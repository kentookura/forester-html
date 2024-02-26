(* TODO: render frontmatter only when top*)
(* TODO: render taxa in toc *)
open Render
open Prelude
open Core
open Sem
open Dream_html
open HTML
module Render_effect = Render_effect
module Loader = Loader
module Graphviz = Graphviz
module Index = Index
open Forester
module S = Set.Make (String)
module A = Analysis
module M = A.Map
module Tbl = A.Tbl
module Gph = A.Gph
open Forest

module Verbatim = struct
  type cfg = { tex : bool }
  type t = Sem.node

  let rec render_node ~cfg : Sem.node Range.located -> string =
   fun located ->
    match located.value with
    | Sem.Text t -> t
    | Sem.Math (_, xs) -> render ~cfg xs
    | Xml_tag (name, _, body) -> render_tag ~cfg name body
    | Unresolved name -> render_tag ~cfg name []
    | Transclude (_, _) ->
        Dream.log "unhandled verbatim: Transclude";
        ""
    | Query (_, _) ->
        Dream.log "unhandled verbatim: Query";
        ""
    | Link _ ->
        Dream.log "unhandled verbatim: Link";
        ""
    | Embed_tex _ ->
        Dream.log "unhandled verbatim: Embed_tex";
        ""
    | Img _ ->
        Dream.log "unhandled verbatim: Img";
        ""
    | Block (_, _) ->
        Dream.log "unhandled verbatim: Block";
        ""
    | If_tex (_, _) ->
        Dream.log "unhandled verbatim: If_tex";
        ""
    | Prim (_, _) ->
        Dream.log "unhandled verbatim: Prim";
        ""
    | Object _ ->
        Dream.log "unhandled verbatim: Object";
        ""

  (* Reporter.fatalf ?loc:located.loc:  *)
  (*   "Render_verbatim: cannot render this kind of object" *)
  and render_tag ~cfg (name : string) body =
    "\\" ^ name ^ render_arg ~cfg Braces body

  and render_arg ~cfg delim (arg : Sem.t) : string =
    match arg with
    | [] -> ""
    | _ ->
        let l, r =
          match delim with
          | Braces -> ("{", "}")
          | Squares -> ("[", "]")
          | Parens -> ("(", ")")
        in
        String.concat "" [ l; render ~cfg arg; r ]

  and render ~cfg xs = String.concat " " @@ (List.map (render_node ~cfg)) xs
end

type backmatter =
  [ `References | `Context | `Backlinks | `Related | `Contributions ]

type link =
  | Link of {
      dest : string;
      title : Sem.t option;
      modifier : [ `Sentence_case ] option;
    }

type cfg = {
  counter : int ref;
  top : bool;
  in_backmatter : bool;
  seen : addr list;
}

module type Partial = sig
  type t

  val render : cfg:cfg -> t -> forest -> node
end

module type Atomic = sig
  type t

  val render : t -> node
end

module type Transcludable = sig
  type t

  val render : cfg:cfg -> transclusion_opts -> t -> forest -> node
end

module rec Node : (Partial with type t = Core.Sem.node Core.Range.located) =
struct
  type t = Core.Sem.node Core.Range.located

  let rec render : cfg:cfg -> t -> Forester.Forest.forest -> node =
   fun ~cfg tree forest ->
    (* render_node ~cfg tree forest *)
    match tree.value with
    | Sem.Text t -> txt "%s" t
    | Sem.Math (mode, body) -> (
        let _attrs = match mode with Inline -> [] | Display -> [ id "" ] in
        let content = Verbatim.render ~cfg:{ Verbatim.tex = false } body in
        match mode with
        | Display -> txt "\\[%s\\]" content
        | Inline -> txt "\\(%s\\)" content)
    | Sem.Link l ->
        Link.render ~cfg
          (Link { dest = l.dest; title = l.title; modifier = l.modifier })
          forest
    | Sem.Transclude (opts, addr) -> (
        match M.find_opt addr forest.trees with
        | None -> txt "Could not find tree at addr %s" addr
        | Some doc -> Transclusion.render ~cfg opts doc forest)
    | Sem.Query (opts, query) -> Query.render ~cfg opts query forest
    | Sem.Xml_tag (_name, attrs, _xs) ->
        let _attrs =
          attrs
          |> List.map @@ fun (k, v) ->
             let txt = Verbatim.render ~cfg:{ Verbatim.tex = true } v in
             (k, txt)
        in
        div [] []
        (* match (a, b, c) with _ -> div [] []) *)
    | Sem.Embed_tex { source; _ } ->
        (*  TODO: Verify that this works. Prolly not *)
        let code =
          Render_verbatim.Printer.contents
          @@ Render_verbatim.render ~cfg:{ tex = true } source
        in
        let hash = Digest.to_hex @@ Digest.string code in
        (* E.enqueue_latex ~name:hash ~packages ~source:code; *)
        let path = Format.sprintf "resources/%s-web.svg" hash in
        img [ src "%s" path ]
    (* | Sem.Img a -> div [] (match a with _ -> div [] []) *)
    | Sem.Block (title, body) ->
        div [ id "block" ]
        @@ List.map (fun n -> render ~cfg n forest) title
        @ List.map (fun n -> render ~cfg n forest) body
    (*  TODO:  tag headline*)
    | Sem.If_tex (_a, _b) ->
        div [] []
        (* ( *)
        (* match (a, b) with (_ : t), (_ : t) -> div [] [ txt "%s" "iftex" ]) *)
    | Sem.Prim (prim, c) ->
        let tag =
          match prim with
          | `Em -> em
          | `Strong -> strong
          | `Ul -> ul
          | `Li -> li
          | `Blockquote -> blockquote
          | `Code -> code
          | `Ol -> ol
          | `Pre -> pre
          | `P -> HTML.p
        in
        tag [] @@ List.map (fun n -> render ~cfg n forest) c
        (* render_list ~cfg c forest *)
    | Sem.Unresolved a -> (
        match a with _ -> div [] [ txt "unresolved identifier `\\%s`" a ])
    | Sem.Object _o -> div [] [ txt "Can't render object" ]
    | Sem.Img _a -> txt "todo img"
end

and Meta : (Partial with type t = Core.Sem.tree) = struct
  type t = Sem.tree

  let _meta_item ~cfg (_i, (t : Sem.node Range.located list)) forest =
    li [ class_ "meta-item" ]
    @@ (t |> List.map (fun n -> Node.render ~cfg n forest))

  and render ~(cfg : cfg) tree forest =
    div
      [ class_ "metadata" ]
      [
        ul []
        @@ List.map
             (fun d -> li [ class_ "meta-item" ] [ Date.render ~cfg d forest ])
             tree.dates
        @ [
            li
              [ class_ "meta-item" ]
              [
                (*  TODO: get author name effectfully *)
                address [ class_ "author" ]
                @@ [ txt "%s" @@ (String.concat ", ") tree.authors ];
                (*  TODO: with contributions from ... *)
              ];
          ];
      ]
end

and Frontmatter : (Partial with type t = Core.Sem.tree) = struct
  type t = Core.Sem.tree

  let render : cfg:cfg -> t -> Forester.Forest.forest -> node =
   fun ~cfg tree forest ->
    let _ = Sem.sentence_case in
    let taxon =
      match tree.taxon with None -> "" | Some t -> String_util.sentence_case t
    in
    let title =
      match tree.title with
      | None -> []
      | Some ts ->
          List.map
            (fun (t : Sem.node Range.located) -> Node.render ~cfg t forest)
            (Sem.sentence_case ts)
    in
    let slug =
      match tree.addr with
      | None -> div [] []
      | Some s -> a [ class_ "slug"; href "%s" s ] [ txt "[%s]" s ]
    in
    let edit_button =
      match tree.source_path with
      | Some path -> a [ href "nvim://%s" path ] [ txt "[edit]" ]
      | None -> txt ""
    in
    header []
      [
        h1 []
        @@ [ span [ class_ "taxon" ] [ txt "%s" taxon ] ]
        @ title @ [ slug ];
        Meta.render ~cfg tree forest;
        edit_button;
      ]
end

and Contributors : (Partial with type t = string) = struct
  type t = string

  let render : cfg:cfg -> t -> Forester.Forest.forest -> node =
   fun ~cfg tree forest ->
    let _ = cfg in
    let _ = tree in
    let _ = forest in
    header []
      [
        h1 [] []
        (* @@ [ span [ class_ "taxon" ] [ txt "%s" taxon ] ] *)
        (* @ title @ [ slug ]; *)
        (* Meta.render ~cfg tree forest; *)
        (* edit_button; *);
      ]
end

and Mainmatter : (Partial with type t = Core.Sem.tree) = struct
  type t = Core.Sem.tree

  let render : cfg:cfg -> t -> Forester.Forest.forest -> node =
   fun ~cfg tree forest -> div [] @@ [ Body.render ~cfg tree.body forest ]
end

and Backmatter : (Partial with type t = Core.Sem.tree) = struct
  type t = Core.Sem.tree

  let render : cfg:cfg -> t -> Forester.Forest.forest -> node =
   fun ~cfg tree forest ->
    let cfg = { cfg with in_backmatter = true; top = false } in
    let opts =
      Sem.
        {
          title_override = None;
          taxon_override = None;
          toc = false;
          show_heading = true;
          expanded = false;
          numbered = false;
          show_metadata = true;
        }
    in
    let analysis = Lazy.force forest.analysis in

    let get_sorted_trees addrs : Sem.tree list =
      let find addr =
        match M.find_opt addr forest.trees with
        | None -> []
        | Some doc -> [ doc ]
      in
      Sem.Util.sort @@ List.concat_map find @@ S.elements addrs
    in

    let get_all_links scope =
      get_sorted_trees @@ S.of_list @@ Gph.pred analysis.link_graph scope
    in

    let backlinks scope =
      get_sorted_trees @@ S.of_list @@ Gph.succ analysis.link_graph scope
    in

    let related scope =
      get_all_links scope
      |> List.filter @@ fun (doc : Sem.tree) -> doc.taxon <> Some "reference"
    in

    let bibliography scope =
      get_sorted_trees @@ S.of_list
      @@ A.Tbl.find_all analysis.bibliography scope
    in

    let parents scope =
      get_sorted_trees @@ S.of_list
      @@ Gph.succ analysis.transclusion_graph scope
    in

    let contributions scope =
      get_sorted_trees @@ S.of_list @@ Tbl.find_all analysis.author_pages scope
    in

    let item bm =
      let title, content =
        match tree.addr with
        | None -> ("", [])
        | Some addr -> (
            match bm with
            | `References ->
                ( "References",
                  List.map
                    (fun tree -> Tree.render ~cfg opts tree forest)
                    (bibliography addr) )
            | `Context ->
                ( "Context",
                  List.map
                    (fun tree -> Tree.render ~cfg opts tree forest)
                    (parents addr) )
            | `Backlinks ->
                ( "Backlinks",
                  List.map
                    (fun tree -> Tree.render ~cfg opts tree forest)
                    (backlinks addr) )
            | `Related ->
                ( "Related",
                  List.map
                    (fun tree -> Tree.render ~cfg opts tree forest)
                    (related addr) )
            | `Contributions ->
                ( "Contributions",
                  List.map
                    (fun tree -> Tree.render ~cfg opts tree forest)
                    (contributions addr) ))
      in
      section [ class_ "block link-list" ] (h2 [] [ txt "%s" title ] :: content)
    in

    footer []
    @@ ([ `References; `Context; `Backlinks; `Related; `Contributions ]
       |> List.map item)
  (* [ references; context; backlinks; related; contributions ] *)
end

and Body : (Partial with type t = Sem.node Range.located list) = struct
  type t = Sem.node Range.located list

  let render ~cfg bdy forest =
    div []
    @@ List.map
         (fun (n : Sem.node Range.located) -> Node.render ~cfg n forest)
         bdy
end

and Link : (Partial with type t = link) = struct
  type t = link

  let render ~(cfg : cfg) (Link link : t) forest =
    let render_internal ~cfg ~title ~modifier ~addr forest =
      let doc = M.find_opt addr forest.trees in
      let doc_title = Option.bind doc @@ fun d -> d.title in
      let title = Option.fold title ~none:doc_title ~some:Option.some in
      let target_title_attr =
        match doc_title with
        | None -> []
        | Some t ->
            let title_string =
              String_util.sentence_case @@ Render_text.Printer.contents
              @@ Render_text.render t
            in
            [ HTML.title_ "%s" title_string ]
      in
      let title = Option.map (Sem.apply_modifier modifier) title in
      let title =
        Option.value ~default:[ Range.locate_opt None @@ Sem.Text addr ] title
      in
      a ([ href "%s" addr ] @ target_title_attr)
      @@ List.map (fun n -> Node.render ~cfg n forest) title
    in

    let render_external ~cfg ~title ~modifier ~url forest =
      let title = Option.map (Sem.apply_modifier modifier) title in
      let title =
        Option.value ~default:[ Range.locate_opt None @@ Sem.Text url ] title
      in
      a [ href "%s" url ] @@ List.map (fun n -> Node.render ~cfg n forest) title
    in

    match M.find_opt link.dest forest.trees with
    | Some _ ->
        render_internal ~title:link.title ~cfg ~modifier:link.modifier
          ~addr:link.dest forest
    | None ->
        render_external ~title:link.title ~cfg ~modifier:link.modifier
          ~url:link.dest forest
end

and Date : (Partial with type t = Prelude.Date.t) = struct
  type t = Prelude.Date.t

  let render ~(cfg : cfg) date _ =
    let open Prelude.Date in
    let _ = cfg in
    let m =
      match month date with
      | Some n -> (
          match n with
          | 1 -> "January"
          | 2 -> "February"
          | 3 -> "March"
          | 4 -> "April"
          | 5 -> "May"
          | 6 -> "June"
          | 7 -> "July"
          | 8 -> "August"
          | 9 -> "September"
          | 10 -> "October"
          | 11 -> "November"
          | 12 -> "December"
          | _ -> "")
      | None -> ""
    in
    let d = match day date with Some n -> Int.to_string n | None -> "" in
    let y = Int.to_string @@ year date in
    txt "%s" (m ^ " " ^ d ^ ", " ^ y)
end

and Query : (Transcludable with type t = Sem.t Core.Query.t) = struct
  type t = Sem.t Core.Query.t

  let render ~(cfg : cfg) _ _ _ =
    let _ = cfg in
    (* match a with *)
    (* | _, Query.Author _ *)
    (* | _, Query.Tag _ *)
    (* | _, Query.Taxon _ *)
    (* | _, Query.Meta (_, _) *)
    (* | _, Query.Or _ *)
    (* | _, Query.And _ *)
    (* | _, Query.Not _ *)
    (* | _, Query.True -> *)
    div [] []
end

and Transclusion : (Transcludable with type t = Sem.tree) = struct
  type t = Sem.tree

  let render ~cfg opts doc forest =
    let cfg =
      let ctr = cfg.counter in
      let ix = if opts.numbered then !ctr + 1 else !ctr in
      ctr := ix;
      let counter = ref 0 in
      { cfg with counter; top = false }
    in
    Tree.render ~cfg opts doc forest
end

and Tree : (Transcludable with type t = Sem.tree) = struct
  type t = Sem.tree

  let render ~cfg opts (doc : tree) forest =
    let doc =
      match opts.title_override with
      | Some _ as title -> { doc with title }
      | None -> doc
    in
    let doc =
      match opts.taxon_override with
      | Some _ as taxon -> { doc with taxon }
      | None -> doc
    in
    let the_id =
      match doc.addr with
      | None -> id "unknown_tree"
      | Some addr -> id "%s" addr
    in
    let attrs =
      [
        (if opts.expanded then open_ else attr "");
        class_ (if opts.show_heading then "" else "");
        (*  TODO: figure this out *)
        class_ (if opts.show_metadata then "block hide-metadata" else "block");
        class_ (if opts.toc then "" else "");
        class_ (if opts.numbered then "" else "");
      ]
    in
    let _seen =
      match doc.addr with None -> false | Some addr -> List.mem addr cfg.seen
    in
    let _trace k =
      match doc.addr with
      | None -> k ()
      | Some addr ->
          Reporter.tracef "when rendering tree at address `%s` to HTML" addr k
    in
    let cfg =
      match doc.addr with
      | None -> cfg
      | Some addr -> { cfg with seen = addr :: cfg.seen }
    in

    section
      ([ class_ "block"; the_id ] @ attrs)
      [
        details [ open_ ]
          [
            summary [] [ Frontmatter.render ~cfg doc forest ];
            div [ class_ "tree-content" ] [ Mainmatter.render ~cfg doc forest ];
          ];
      ]
end

let fourohfour addr = txt "%s not found" addr
let with_fallback fof f got = match got with Some t -> f t | None -> fof

let rec render_tree_page (doc : Sem.tree) ~cfg ~opts forest =
  div
    [ id "grid-wrapper" ]
    [
      article []
        [ Tree.render ~cfg opts doc forest; Backmatter.render ~cfg doc forest ];
      nav
        [ Dream_html.HTML.id "%s" "toc" ]
        [
          div
            [ class_ "block" ]
            [ h1 [] [ txt "Table of contents" ]; toc ~cfg doc forest ];
        ];
    ]

and toc (doc : Sem.tree) ~cfg forest =
  let item tree =
    match (tree.title, tree.addr) with
    | Some title, Some addr ->
        li []
          [
            a [ href "%s" addr; class_ "bullet" ] [ txt "■" ];
            a
              [ href "#tree-" ]
              [
                span [] @@ List.map (fun n -> Node.render ~cfg n forest) title;
                toc ~cfg tree forest;
              ];
          ]
    | _ -> a [] [ txt "Unnamed tree" ]
  in
  let transclusions (ns : Sem.node Range.located list) =
    ns
    |> List.filter_map (fun n ->
           match n with
           | Range.{ value = Transclude (_, addr); _ } ->
               M.find_opt addr forest.trees
           | _ -> None)
    |> List.map item
  in

  ol [ class_ "block" ] (transclusions doc.body)

module Page = struct
  let render addr forest =
    let cfg =
      { top = true; counter = ref 0; in_backmatter = false; seen = [] }
    in
    let opts =
      Sem.
        {
          title_override = None;
          taxon_override = None;
          toc = false;
          show_heading = true;
          expanded = true;
          numbered = false;
          show_metadata = true;
        }
    in
    match M.find_opt addr forest.trees with
    | Some t -> render_tree_page ~cfg ~opts t forest
    | None -> fourohfour addr
end
(* and render ~cfg : Sem.t -> forest -> node list = *)
(*  fun ns forest -> List.map (fun n -> Node.render ~cfg n forest) ns *)

let tooltip addr = div [] [ txt "%s" addr ]
and query addr = div [] [ txt "%s" addr ]

and render_author (author : string) forest =
  let cfg =
    {
      (* base_url = None; *)
      top = false;
      counter = ref 0;
      in_backmatter = false;
      seen = [];
    }
  in
  match M.find_opt author forest.trees with
  | Some bio -> (
      match bio.addr with
      | None -> txt "%s" author
      | Some addr -> (
          match bio.title with
          | Some title ->
              a [ href "%s" addr ]
              @@ List.map (fun n -> Node.render ~cfg n forest) title
          | _ -> txt "Untitled"))
  | None -> txt "%s" author

and render_authors ~cfg (doc : Sem.tree) forest =
  let contributors =
    match doc.addr with
    | Some addr -> Contributors.render ~cfg addr forest
    | None -> null []
  in
  match (doc.authors, contributors) with
  | [], _ -> null []
  | _authors, _ -> div [] []
