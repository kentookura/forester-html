open Effect
open Forester
open Core
open Effect.Deep
open Dream_html
module Loader = Loader
module S = Set.Make (String)
module A = Analysis
module M = A.Map
module Tbl = A.Tbl
module Gph = A.Gph

type _ Effect.t +=
  | Page : string -> Dream_html.node t
  | Tooltip : string -> Dream_html.node t
  | Query : string -> Dream_html.node t
  | Toc : string -> Dream_html.node t
  | Get_doc : addr -> Dream_html.node t
  | Author : string -> Dream_html.node t
(* | Parents : addr -> Dream_html.node t *)
(* | Parse : string -> Code.node Range.located list t *)
(* | Route : addr -> string t *)
(* | Children : addr -> Dream_html.node t *)
(* | Backlinks : addr -> Sem.tree list t *)

let get_sorted_trees addrs (forest : Forest.forest) : Sem.tree list =
  let find addr =
    match M.find_opt addr forest.trees with None -> [] | Some doc -> [ doc ]
  in
  Sem.Util.sort @@ List.concat_map find @@ S.elements addrs

let _get_parents scope (analysis : A.analysis) =
  get_sorted_trees @@ S.of_list @@ Gph.succ analysis.transclusion_graph scope

let _get_children scope (analysis : A.analysis) =
  get_sorted_trees @@ S.of_list @@ Gph.pred analysis.transclusion_graph scope

let _get_contributions scope (analysis : A.analysis) =
  get_sorted_trees @@ S.of_list @@ Tbl.find_all analysis.author_pages scope

let _get_backlinks scope (analysis : A.analysis) =
  get_sorted_trees @@ S.of_list @@ Gph.succ analysis.link_graph scope

module Render = struct
  let _page addr = perform (Page addr)
  let _get_doc addr = perform (Get_doc addr)
  let _tooltip addr = perform (Tooltip addr)
  let _toc addr = perform (Toc addr)
  let _author addr = perform (Author addr)
end

module type Handler = sig
  val index : node
  val page : string -> node
  val tooltip : string -> node
  val query : string -> node
  (* val toc : string -> node *)
  (* val author : string -> node *)
  (* val get_doc : string -> node *)
  (* val parents : string -> node *)
  (* val children : string -> node *)
  (* val backlinks : string -> node *)
  (* val transclusion : string -> node *)
end

module Run (H : Handler) = struct
  let _run f =
    Effect.Deep.try_with f ()
    @@ {
         effc =
           (fun (type a) (eff : a Effect.t) ->
             let resume x =
               Option.some @@ fun (k : (a, _) Effect.Deep.continuation) ->
               Algaeff.Fun.Deep.finally k @@ fun () -> x ()
             in
             match eff with
             | Page addr -> resume @@ fun () -> H.page addr
             | Tooltip addr -> resume @@ fun () -> H.tooltip addr
             | Query string -> resume @@ fun () -> H.query string
             (* | Toc addr -> resume @@ fun () -> H.toc addr *)
             (* | Forest dir -> resume @@ fun () -> H.load_forest dir *)
             (* | Author addr -> resume @@ fun () -> H.author addr *)
             (* | Route addr -> resume @@ fun () -> string t *)
             (* | Get_doc addr -> resume @@ fun () -> Dream_html.node t *)
             (* | Parents addr -> resume @@ fun () -> Dream_html.node t *)
             (* | Children addr -> resume @@ fun () -> Dream_html.node t *)
             (* | Backlinks addr -> resume @@ fun () -> Sem.tree list t *)
             | _ -> None);
       }
end

let _eval_query q =
  let parse_query q = q in
  parse_query q |> fun a -> perform (Query a)
