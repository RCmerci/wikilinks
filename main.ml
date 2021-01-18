open! Js_of_ocaml
open! Js_of_ocaml_lwt
open Lwt.Infix
open! Core_kernel

(* https://en.wikipedia.org/w/api.php?format=json&action=query&prop=linkshere&redirects=1&titles=linux&lhlimit=20 *)
(* jq expression to extract link titles: .query.pages["6097297"].linkshere[].title *)
let get_wikipedia_links :
  string ->
  string list Lwt.t =
  fun title ->
  XmlHttpRequest.get
    (Printf.sprintf "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=linkshere&redirects=1&titles=%s&lhlimit=10&origin=*" title)
  >>= fun r ->
  Lwt.return r.content >>= fun c ->
  let json = Yojson.Safe.from_string c in
  Lwt.return Yojson.Safe.Util.(json
                               |> member "query"
                               |> member "pages"
                               |> to_assoc |> List.hd_exn |> snd
                               |> member "linkshere"
                               |> to_list
                               |> List.map ~f:(fun v -> member "title" v |> to_string ))


let gen_dot :
  string ->
  string list ->
  string =
  fun title links ->
  let concat_links = List.map links ~f:(fun l -> Printf.sprintf "\"%s\" -> \"%s\"" title l) |> String.concat ~sep:"; " in
  Printf.sprintf "digraph{ %s }" concat_links

let d3_dot (s : Js.js_string Js.t) : unit =
  let expr = "d3.select(\"div:last-child\").graphviz().renderDot" in
  Js.Unsafe.fun_call (Js.Unsafe.js_expr expr) [|Js.Unsafe.inject s|]


let opt_exn e = Option.value_exn (Js.Opt.to_option e)
let d = Dom_html.document
let new_graphdiv () =
  let div = Dom_html.createDiv d in
  div##.style##.textAlign := Js.string "center";
  div

let rec onclick (e:#Dom_html.event Js.t) =
  let body =
    opt_exn (d##querySelector (Js.string "body"))
  in
  let target = opt_exn e##.currentTarget in
  Dom.appendChild body (new_graphdiv ());
  let title = (Js.to_string
                 (opt_exn
                    (opt_exn
                       (target##querySelector (Js.string "title")))##.textContent)) in
  ignore (get_wikipedia_links title >|=
          gen_dot title >|=
          (fun s -> d3_dot (Js.string s)) >|=
          fun _ ->Lwt_js.sleep 0.1 >|=
          fun _ -> setclick());
  Js._true

and setclick () =
  let n = (opt_exn (d##querySelector(Js.string "div:last-child"))) in
  let nodes = n##querySelectorAll(Js.string ".node") in
  for i = 0 to (nodes##.length - 1) do
    let node = nodes##item i in
    (Firebug.console##log node);
    ignore (
      Dom_html.(addEventListener
                  (opt_exn node)
                  Event.click
                  (handler onclick) Js._false));
  done


let onload _ =
  let title =
    List.Assoc.find
      Js_of_ocaml.Url.Current.arguments
      ~equal:String.(=) "q"
    |> Option.value ~default:"emacs" in
  ignore (
    get_wikipedia_links title >|=
    gen_dot title >|=
    (fun s -> d3_dot (Js.string s)) >|=
    fun _ ->Lwt_js.sleep 0.1 >|=
    fun _ -> setclick());
  Js._false

let _ = Dom_html.window##.onload := Dom_html.handler onload
