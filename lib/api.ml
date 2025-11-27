let ptime_of_string s =
  match Ptime.of_rfc3339 s with
  | Ok (t, _, _) -> Some t
  | Error _ -> None
;;

exception Format_error of string

let ptime_of_string_exn s =
  match ptime_of_string s with
  | Some t -> t
  | None -> raise @@ Format_error s
;;

let timestamp_decoder =
  let module D = Decoder.Yojson.Safe in
  D.map (fun s -> ptime_of_string_exn s) D.string
;;

module type REQUEST_CFG = sig
  val api_url : string
  val api_token : string
end

let make_request_cfg api_url api_token =
  (module struct
    let api_url = api_url
    let api_token = api_token
  end : REQUEST_CFG)
;;

let request_headers api_token =
  let module H = Cohttp.Header in
  H.add_list
    (H.init ())
    [ "Authorization", "Bearer " ^ api_token; "accept", "application/json" ]
;;

type 'a body_decoder = 'a Decoder.Yojson.Safe.decoder
type body_encoder = Encoder.Yojson.Encoder.encoder

type 'a request_method =
  | Get of 'a body_decoder
  | Post of (body_encoder * 'a body_decoder)
  | Delete of 'a body_decoder

type endpoint = string
type request_args = (string * string) list

let make_request_args args = args

type 'a api_request =
  | Api_request of ('a request_method * endpoint * request_args)

let make_api_request request_method endpoint args =
  Api_request (request_method, endpoint, args)
;;

let make_api_get_request ?(args = []) endpoint body_decoder =
  make_api_request (Get body_decoder) endpoint args
;;

let make_api_post_request ?(args = []) endpoint body_encoder body_decoder =
  make_api_request (Post (body_encoder, body_decoder)) endpoint args
;;

let make_api_delete_request ?(args = []) endpoint body_decoder =
  make_api_request (Delete body_decoder) endpoint args
;;

module Response_error = struct
  type t =
    | Http_error of (int * string)
    | Json_decoder_error of Decoder.Yojson.Safe.Error.t
    | Pagination_error of int

  let show = function
    | Http_error (status_code, body) ->
      Printf.sprintf "HTTP status code %i: %s" status_code body
    | Json_decoder_error err -> Decoder.Yojson.Safe.Error.show err
    | Pagination_error total_pages ->
      Printf.sprintf "Pagination error with %i total pages" total_pages
  ;;
end

let return x = Lwt.return_ok x

let bind m f =
  Lwt.bind m (function
    | Ok ok -> f ok
    | Error err -> Lwt.return_error err)
;;

let decode_body m =
  let ( >>= ) = Lwt.bind in
  m
  >>= fun (response, body) ->
  let status_code = Cohttp.(Response.status response |> Code.code_of_status) in
  let lwt_string_body = Cohttp_lwt.Body.to_string body in
  if Cohttp.Code.is_success status_code
  then
    lwt_string_body
    >>= fun body ->
    Lwt.return_ok @@ Yojson.Safe.from_string (if body = "" then "{}" else body)
  else
    lwt_string_body
    >>= fun body ->
    Lwt.return_error (Response_error.Http_error (status_code, body))
;;

let decode_json m json_body_fn =
  let ( >>= ) = Lwt.bind in
  let ( >>>= ) = bind in
  m
  >>>= fun body ->
  body
  |> json_body_fn
  |> Lwt.return
  >>= fun maybe_decoded ->
  match maybe_decoded with
  | Error err -> Lwt.return_error (Response_error.Json_decoder_error err)
  | Ok ok -> Lwt.return_ok ok
;;

let ( --> ) m json_body_fn = decode_json (decode_body m) json_body_fn
let encode_body body_encoder = Cohttp_lwt.Body.of_string (body_encoder ())
let max_page_size = 500

module D = Decoder.Yojson.Safe

let append_json_list json_acc json total_pages =
  match json with
  | `List lst ->
    (match json_acc with
     | `List acc -> Lwt.return_ok (`List (acc @ lst))
     | _ -> Lwt.return_error (Response_error.Pagination_error total_pages))
  | _ -> Lwt.return_error (Response_error.Pagination_error total_pages)
;;

let get_all_pages headers uri total_pages body_decoder =
  let ( >>>= ) = bind in
  let module C = Cohttp_lwt_unix.Client in
  let bodies =
    List.fold_left
      (fun acc page_number ->
        let uri_with_page_argument =
          Uri.add_query_params' uri [ "page", string_of_int page_number ]
        in
        C.get ~headers uri_with_page_argument
        |> decode_body
        >>>= fun body_result ->
        acc >>>= fun acc -> append_json_list acc body_result total_pages)
      (Lwt.return_ok (`List []))
      (List.init total_pages (fun x -> x + 1))
  in
  decode_json bodies @@ body_decoder
;;

let get_with_total_count headers uri body_decoder =
  let ( >>= ) = Lwt.bind in
  let module C = Cohttp_lwt_unix.Client in
  let size_argument = [ "size", string_of_int max_page_size ] in
  let uri_with_size_argument = Uri.add_query_params' uri size_argument in
  let response_m = C.head ~headers uri_with_size_argument in
  response_m
  >>= fun response ->
  let status_code = Cohttp.(Response.status response |> Code.code_of_status) in
  if Cohttp.Code.is_success status_code
  then (
    let total_pages =
      match Cohttp.(Header.get (Response.headers response) "x-total-pages") with
      | Some total_count_str ->
        (match int_of_string_opt total_count_str with
         | Some total_count -> total_count
         | None -> 0)
      | None -> 0
    in
    if total_pages == 0
    then C.get ~headers uri_with_size_argument --> body_decoder
    else get_all_pages headers uri_with_size_argument total_pages body_decoder)
  else C.get ~headers uri_with_size_argument --> body_decoder
;;

let run_request (module RC : REQUEST_CFG) = function
  | Api_request (request_method, endpoint, args) ->
    let headers = request_headers RC.api_token in
    let uri =
      Uri.add_query_params'
        (Printf.sprintf "%s%s" RC.api_url endpoint |> Uri.of_string)
        args
    in
    let module C = Cohttp_lwt_unix.Client in
    (match request_method with
     | Get body_decoder -> get_with_total_count headers uri body_decoder
     | Post (body_encoder, body_decoder) ->
       let body = encode_body body_encoder in
       C.post ~body ~headers uri --> body_decoder
     | Delete body_decoder -> C.delete ~headers uri --> body_decoder)
;;
