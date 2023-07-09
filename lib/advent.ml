open Lwt
open Cohttp
open Cohttp_lwt_unix

let aoc_session_cookie = 
  let ic = open_in "./lib/aoc_session_cookie.txt" in
  try
    input_line ic
  with e ->
    close_in_noerr ic;
    raise e

let get_input day =
    let uri = Uri.of_string (Printf.sprintf "https://adventofcode.com/2022/day/%i/input" day) in
    let header = Header.add_opt None "Cookie" ("session=" ^ aoc_session_cookie) in
    Lwt_main.run(
    Client.get ~headers:header uri >>= 
      fun (_, body) ->
        body |> Cohttp_lwt.Body.to_string >|= (String.split_on_char '\n'))

let extract_test_data raw_html = 
  let new_r = Re.(compile (seq [str "<pre><code>"; group (repn any 0 None); str "</code></pre>"])) in
  Re.Group.get (Re.exec new_r raw_html) 1


let get_test_input day =
    let uri = Uri.of_string (Printf.sprintf "https://adventofcode.com/2022/day/%i" day) in
    let header = Header.add_opt None "Cookie" ("session=" ^ aoc_session_cookie) in
    Lwt_main.run(
    Client.get ~headers:header uri >>= 
      fun (_, body) ->
        body |> Cohttp_lwt.Body.to_string >|= fun raw_html ->
          raw_html |> extract_test_data |> (String.split_on_char '\n')
    )
