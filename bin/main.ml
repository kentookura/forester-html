module Tty = Asai.Tty.Make (Core.Reporter.Message)
open Render_html
open Loader
open Forester
open Dream_html
open HTML
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
  (* let param s f req = Dream_html.respond @@ f @@ Dream.param req s in *)
  (* let page addr = render_page addr forest in *)
  Dream.run ~port:1234 @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun req -> Dream_html.respond (index req));
         ( Dream.get "/forest/:address" @@ fun req ->
           Dream_html.respond
             (render_page (Dream.param req "address") forest ~req) );
         ( Dream.get "/diagnostics" @@ fun _ ->
           Dream.websocket (fun websocket ->
               match%lwt Dream.receive websocket with
               | Some _message -> Dream.send websocket "goodbye"
               | None -> Dream.close_websocket websocket) );
         Dream.post "/search" (fun request ->
             match%lwt Dream.form request with
             | `Ok [ ("search", _search_term) ] ->
                 Dream_html.respond (tr [] [ td [] [ txt "hello" ] ])
             | `Ok _ -> Dream_html.respond (tr [] [ td [] [ txt "hello" ] ])
             | `Many_tokens _ -> Dream_html.respond (txt "many tokens")
             | `Missing_token _ -> Dream_html.respond (txt "missing token")
             | `Invalid_token _ -> Dream_html.respond (txt "invalid token")
             | `Wrong_session _ -> Dream_html.respond (txt "wrong session")
             | `Expired _ -> Dream_html.respond (txt "expired")
             | `Wrong_content_type ->
                 Dream_html.respond (txt "wrong content type"));
         Dream.get "/static/**" (Dream.static "./static");
         Dream.get "/favicon.ico" (Dream.static "./static/favicon.ico");
       ]
