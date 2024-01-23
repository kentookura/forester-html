open Core
open Forester
open Analysis
module Tty = Asai.Tty.Make (Core.Reporter.Message)
module A = Analysis
module M = A.Map
module Tbl = A.Tbl
module Gph = A.Gph
module S = Set.Make (String)
open Dream_html
open HTML
open Render_html

let _render (forest : Forest.forest) _f =
  let module H : Handler = struct
    let page addr =
      match M.find_opt addr forest.trees with
      | Some t -> base_template @@ render_tree t
      | None -> Render_html.fourohfour addr

    let tooltip addr = div [] [ txt "%s" addr ]
    let query addr = div [] [ txt "%s" addr ]
    let toc addr = div [] [ txt "%s" addr ]
    let author addr = div [] [ txt "%s" addr ]
    let get_doc addr = div [] [ txt "%s" addr ]
    let parents addr = div [] [ txt "%s" addr ]
    let children addr = div [] [ txt "%s" addr ]
    let backlinks addr = div [] [ txt "%s" addr ]
    let transclusion addr = div [] [ txt "%s" addr ]
    (* let parse _addr = div [] [] *)
    (* let load_forest _addr = div [] [] *)
    (* let route _addr = div [] [] *)
  end in
  let get_sorted_trees addrs : Sem.tree list =
    let find addr =
      match M.find_opt addr forest.trees with None -> [] | Some doc -> [ doc ]
    in
    Sem.Util.sort @@ List.concat_map find @@ S.elements addrs
  in

  let _get_parents scope (analysis : A.analysis) =
    get_sorted_trees @@ S.of_list @@ Gph.succ analysis.transclusion_graph scope
  in

  let _get_children scope (analysis : A.analysis) =
    get_sorted_trees @@ S.of_list @@ Gph.pred analysis.transclusion_graph scope
  in

  let _get_contributions scope (analysis : A.analysis) =
    get_sorted_trees @@ S.of_list @@ Tbl.find_all analysis.author_pages scope
  in

  let _get_backlinks scope (analysis : A.analysis) =
    get_sorted_trees @@ S.of_list @@ Gph.succ analysis.link_graph scope
  in
  let module R = Render_html.Run (H) in
  div [] []
(* R.run body *)

let () =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  Eio_main.run @@ fun env ->
  Core.Reporter.run ~emit:Tty.display ~fatal @@ fun () ->
  let forest = Loader.run env @@ fun () -> Loader.forest "./static" in
  let analysis = Lazy.force forest.analysis in
  let _graph = analysis.link_graph in
  let param s f req =
    Dream_html.respond @@ f @@ Dream.param req s
    (* Dream_html.respond @@ render forest (fun _ -> f (Dream.param req s)) *)
  in
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         (* Dream.get "/" (fun _ -> *)
         (* Dream_html.respond @@ Dream_html.HTML.
            div [] index); *)
         (* Dream.get "/forest/:tree" @@ render forest Handler.page; *)
         (* Dream.get "/tooltip/:tree" @@ render forest Handler.tooltip; *)
         Dream.get "/forest/:address" @@ param "address" (txt "%s");
         Dream.get "/tooltip/:tree" @@ param "address" (txt "%s");
         Dream.get "/static/**" (Dream.static "./static");
         Dream.get "/graph" (Dream.static "./static");
         (* Dream.post "/query" (fun req -> *)
         (*     match%lwt Dream.form req with *)
         (*     | `Ok [ ("message", message) ] -> *)
         (*         Dream_html.respond @@ eval_query message *)
         (*     | _ -> Dream.empty `Bad_Request); *)
       ]
