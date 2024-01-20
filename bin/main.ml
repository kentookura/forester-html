open Core
open Forester
open Render_html
open Render_effect
module Tty = Asai.Tty.Make (Core.Reporter.Message)

let default_transclusion_opts : Sem.transclusion_opts =
  {
    toc = true;
    show_heading = true;
    show_metadata = true;
    title_override = None;
    taxon_override = None;
    expanded = true;
    numbered = true;
  }

let example_tree : Sem.tree =
  {
    title = Some [ { value = Text "Forester + HTML!"; loc = None } ];
    taxon = Some "theorem";
    authors = [ "Kento Okura" ];
    dates = [];
    addr = None;
    metas = [];
    tags = [];
    body =
      [
        {
          value = Link { dest = "foo-0002"; title = None; modifier = None };
          loc = None;
        };
        {
          value = Transclude (default_transclusion_opts, "HelloTransclusion");
          loc = None;
        };
      ];
    source_path = None;
  }

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let forest = load env @@ fun () -> load_forest "./static" in
  let index =
    let open Dream_html in
    let open HTML in
    Forest.complete ~forest "Hello"
    |> List.of_seq
    |> List.map (fun (addr, title) -> div [] [ txt "%s" addr; txt "%s" title ])
  in
  let render f req =
    Dream_html.respond @@ render forest (fun _ -> f (Dream.param req "tree"))
  in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/index.html" (fun _ ->
             Dream_html.respond @@ Render.page example_tree);
         Dream.get "/" (fun _ ->
             Dream_html.respond @@ Dream_html.HTML.div [] index);
         Dream.get "/forest/:tree" @@ render page;
         Dream.get "/tooltip/:tree" @@ render tooltip;
         Dream.get "/static/**" (Dream.static "./static");
       ]

(*
   Dream.post "/query"
   (fun req ->
     match%lwt Dream.form req with
     | `Ok [ ("message", message) ] ->
         Dream_html.respond @@ eval_query message
     | _ -> Dream.empty `Bad_Request)
*)
