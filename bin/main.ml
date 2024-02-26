module Tty = Asai.Tty.Make (Core.Reporter.Message)
open Render_html
open Loader
open Core
open Forester
open Dream_html
open HTML
module A = Analysis
module E = Render_effect.Perform
module M = A.Map

module S = Algaeff.State.Make (struct
  type t = Forest.forest
end)

let base_template stuff =
  html
    [ class_ "h-full" ]
    [
      head []
        [
          meta [ charset "UTF-8" ];
          meta
            [ name "viewport"; content "width=device-width, initial-scale=1.0" ];
          script [ src "https://cdn.tailwindcss.com?plugins=forms" ] "";
          script
            [
              src "https://unpkg.com/htmx.org@1.9.10";
              integrity
                "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC";
              crossorigin `anonymous;
            ]
            "";
          link [ rel "stylesheet"; href "/static/style.css" ];
          link [ rel "stylesheet"; href "/static/prism.css" ];
          link
            [
              rel "stylesheet";
              href
                "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css";
              integrity
                "sha384-n8MVd4RsNIU0tAv4ct0nTaAbDJwPJzDEaqSD1odI+WdtXRGWt2kTvGFasHpSy3SV";
              crossorigin `anonymous;
            ];
          script
            [
              defer;
              src "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js";
              integrity
                "sha384-XjKyOOlGwcjNTAIQHIpgOno0Hl1YQqzUOEleOLALmuqehneUG+vnGctmUb0ZY0l8";
              crossorigin `anonymous;
            ]
            "";
          script
            [
              defer;
              src
                "https://cdnjs.cloudflare.com/ajax/libs/prism/9000.0.1/prism.min.js";
              integrity
                "sha512-UOoJElONeUNzQbbKQbjldDf9MwOHqxNz49NNJJ1d90yp+X9edsHyJoAs6O4K19CZGaIdjI5ohK+O2y5lBTW6uQ==";
              referrerpolicy `no_referrer;
              crossorigin `anonymous;
            ]
            "";
          script
            [
              defer;
              src
                "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/contrib/auto-render.min.js";
              integrity
                "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05";
              crossorigin `anonymous;
              string_attr "onload" "renderMathInElement(document.body);";
            ]
            "";
          body [ class_ "h-full" ] [ stuff ];
        ];
    ]

let recent_trees (forest : Forester.Forest.forest) =
  forest.trees |> M.to_list
  |> List.map (fun ((addr, tree) : M.key * Core.Sem.tree) ->
         match Sem.Util.peek_title tree with
         | Some stuff ->
             li []
               [
                 a
                   [
                     Hx.target "#content";
                     Hx.swap "innerHTML";
                     Hx.get "/forest/%s" addr;
                     Hx.push_url "true";
                   ]
                   [ txt "%s" stuff ];
               ]
         | None -> null [])
  |> ol []

let () =
  let fatal diagnostics = Tty.display diagnostics in
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let forest = load env @@ fun () -> Loader.forest "./static" in
  Dream.run ~port:1234 @@ Dream.logger @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream_html.respond @@ base_template (Index.page @@ null []));
         ( Dream.get "/forest" @@ fun _ ->
           Dream_html.respond @@ recent_trees forest );
         ( Dream.get "/forest/:address" @@ fun req ->
           Dream_html.respond (Page.render (Dream.param req "address") forest)
         );
         ( Dream.get "/diagnostics" @@ fun _ ->
           Dream.websocket (fun websocket ->
               match%lwt Dream.receive websocket with
               | Some _message -> Dream.send websocket "goodbye"
               | None -> Dream.close_websocket websocket) );
         (* Dream.post "/search" (fun request -> *)
         (*     match%lwt Dream.form request with *)
         (*     | `Ok [ ("search", _search_term) ] -> *)
         (*         Dream_html.respond (tr [] [ td [] [ txt "hello" ] ]) *)
         (*     | `Ok _ -> Dream_html.respond (tr [] [ td [] [ txt "hello" ] ]) *)
         (*     | `Many_tokens _ -> Dream_html.respond (txt "many tokens") *)
         (*     | `Missing_token _ -> Dream_html.respond (txt "missing token") *)
         (*     | `Invalid_token _ -> Dream_html.respond (txt "invalid token") *)
         (*     | `Wrong_session _ -> Dream_html.respond (txt "wrong session") *)
         (*     | `Expired _ -> Dream_html.respond (txt "expired") *)
         (*     | `Wrong_content_type -> *)
         (*         Dream_html.respond (txt "wrong content type")); *)
         Dream.get "/static/**" (Dream.static "./static");
         Dream.get "/favicon.ico" (Dream.static "./static/favicon.ico");
       ]
