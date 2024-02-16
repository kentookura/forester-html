module Tty = Asai.Tty.Make (Core.Reporter.Message)
open Render_html
open Loader
open Forester
module A = Analysis
module E = Render_effect.Perform
module M = A.Map

module S = Algaeff.State.Make (struct
  type t = Forest.forest
end)

let () =
  let fatal diagnostics = Tty.display diagnostics in
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let forest = load env @@ fun () -> Loader.forest "./static" in
  let param s f req = Dream_html.respond @@ f @@ Dream.param req s in
  let page addr = render_page addr forest in
  Dream.run ~port:1234 @@ Dream.logger
  @@ Dream.router
       [
         (Dream.get "/" @@ fun _ -> Dream_html.respond index);
         Dream.get "/forest/:address" @@ param "address" page;
         ( Dream.get "/diagnostics" @@ fun _ ->
           Dream.websocket (fun websocket ->
               match%lwt Dream.receive websocket with
               | Some _message -> Dream.send websocket "goodbye"
               | None -> Dream.close_websocket websocket) );
         Dream.get "/static/**" (Dream.static "./static");
       ]
