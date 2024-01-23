module Tty = Asai.Tty.Make (Core.Reporter.Message)
open Render_html
open Loader

let () =
  let fatal diagnostics = Tty.display diagnostics in
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let forest = load env @@ fun () -> Loader.forest "./static" in
  let module Forest = struct
    let forest = forest
  end in
  let param s f req = Dream_html.respond @@ f @@ Dream.param req s in
  let module Renderer = Renderer (Forest) in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream_html.respond Renderer.index);
         Dream.get "/forest/:address" @@ param "address" Renderer.page;
         Dream.get "/tooltip/:tree" @@ param "address" Renderer.tooltip;
         ( Dream.get "/diagnostics" @@ fun _ ->
           Dream.websocket (fun websocket ->
               match%lwt Dream.receive websocket with
               | Some _message -> Dream.send websocket "goodbye"
               | None -> Dream.close_websocket websocket) );
         Dream.get "/static/**" (Dream.static "./static");
         Dream.get "/graph" (Dream.static "./static");
       ]
