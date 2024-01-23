module Tty = Asai.Tty.Make (Core.Reporter.Message)
open Dream_html
open Render_html
open Render_effect

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let forest = Loader.load env @@ fun () -> Loader.forest "./static" in
  let param s f req =
    Dream_html.respond @@ f @@ Dream.param req s
    (* Dream_html.respond @@ render forest (fun _ -> f (Dream.param req s)) *)
  in
  let module Renderer = (val render forest) in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream_html.respond @@ Dream_html.HTML.div [] [ txt "%s" "Hello" ]);
         Dream.get "/forest/:address" @@ param "address" Renderer.page;
         Dream.get "/tooltip/:tree" @@ param "address" (txt "%s");
         Dream.get "/static/**" (Dream.static "./static");
         Dream.get "/graph" (Dream.static "./static");
       ]
