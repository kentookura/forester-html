open Format

module V = struct
  type t = string

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

module Gph = Graph.Imperative.Digraph.Concrete (String)

module Display = struct
  include Gph

  let vertex_name v = V.label v
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Viz = Graph.Graphviz.Dot (Display)

let render_graph forest = Viz.fprint_graph std_formatter forest

let graph forest =
  let _dot = render_graph forest in
  let open Dream_html in
  let open HTML in
  script
    [ type_ "module" ]
    {|
    import { Graphviz } from "https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.js";
    const graphviz = await Graphviz.load();
    const dot = "digraph G { Hello -> World }";
    const svg = graphviz.dot(dot);
    const div = document.getElementById("placeholder");
    |}
