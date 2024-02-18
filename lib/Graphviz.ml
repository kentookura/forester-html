(* open Format *)

module V = struct
  type t = string

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Display = struct
  (*  FIXME: uncomment when on forester 3.1*)

  (* include Forester.Analysis.Gph *)
  include Graph.Imperative.Digraph.Abstract (String)

  let vertex_name _v = ""
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Viz = Graph.Graphviz.Dot (Display)

(* let render_graph forest = Viz.fprint_graph std_formatter forest *)
let render_graph (forest : Forester.Forest.forest) =
  let _links = (Lazy.force forest.analysis).link_graph in
  let _transclusions = (Lazy.force forest.analysis).transclusion_graph in
  (* Viz.fprint_graph std_formatter transclusions *)
  ()

let graph forest =
  let _dot = render_graph forest in
  let open Dream_html in
  let open HTML in
  script
    [ type_ "module" ]
    {javascript|
    import { Graphviz } from "https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.js";
    const graphviz = await Graphviz.load();
    const dot = "digraph G { Hello -> World }";
    const svg = graphviz.dot(dot);
    const div = document.getElementById("placeholder");
    |javascript}
