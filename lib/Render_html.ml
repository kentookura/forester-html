open Render
open Prelude
open Core
open Sem
open Dream_html
open HTML
module Render_effect = Render_effect
module Loader = Loader
open Forester

type cfg = {
  counter : int ref;
  top : bool;
  in_backmatter : bool;
  seen : addr list;
}

module type F = sig
  val forest : Forest.forest
end

module Verbatim = struct
  type cfg = { tex : bool }

  let rec render_node ~cfg : Sem.node Range.located -> string =
   fun located ->
    match located.value with
    | Sem.Text t -> t
    | Sem.Math (_, xs) -> render ~cfg xs
    | _node ->
        Reporter.fatalf ?loc:located.loc Type_error
          "Render_verbatim: cannot render this kind of object"

  and render ~cfg xs = String.concat " " @@ (List.map (render_node ~cfg)) xs
end

module Renderer = struct
  module A = Analysis
  module E = Render_effect.Perform
  module M = A.Map
  open Forest

  let get_doc addr forest = M.find_opt addr forest.trees

  let rec page addr forest =
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
    | Some t ->
        base_template ~index:false @@ render_tree_page ~cfg ~opts t forest
    | None -> base_template ~index:false @@ fourohfour addr

  and render ~cfg : Sem.t -> forest -> node list =
   fun ns forest -> List.map (fun n -> render_node ~cfg n forest) ns

  and tooltip addr = div [] [ txt "%s" addr ]
  and query addr = div [] [ txt "%s" addr ]

  and render_node ~cfg tree forest =
    match tree.value with
    | Sem.Text t -> txt "%s" t
    | Sem.Math (mode, body) -> (
        let _attrs = match mode with Inline -> [] | Display -> [ id "" ] in
        let content = Verbatim.render ~cfg:{ Verbatim.tex = false } body in
        match mode with
        | Display -> txt "\\[%s\\]" content
        | Inline -> txt "\\(%s\\)" content)
    | Sem.Link { title; dest; modifier } -> (
        match get_doc dest forest with
        | Some _ -> render_internal_link ~title ~cfg ~modifier ~addr:dest forest
        | None -> render_external_link ~title ~cfg ~modifier ~url:dest forest)
    | Sem.Transclude (opts, addr) -> (
        match M.find_opt addr forest.trees with
        | None -> txt "Could not find tree at addr %s" addr
        | Some doc -> render_transclusion ~cfg ~opts doc forest)
    | Sem.Query (a, b) -> render_query (a, b)
    | Sem.Xml_tag (_name, attrs, _xs) ->
        let _attrs =
          attrs
          |> List.map @@ fun (k, v) ->
             let txt = Verbatim.render ~cfg:{ Verbatim.tex = true } v in
             (k, txt)
        in
        div [] []
        (* match (a, b, c) with _ -> div [] []) *)
    | Sem.Embed_tex { packages; source } ->
        (*  TODO: Verify that this works. Prolly not *)
        let code =
          Render_verbatim.Printer.contents
          @@ Render_verbatim.render ~cfg:{ tex = true } source
        in
        let hash = Digest.to_hex @@ Digest.string code in
        E.enqueue_latex ~name:hash ~packages ~source:code;
        let path = Format.sprintf "resources/%s-web.svg" hash in
        img [ src "%s" path ]
    (* | Sem.Img a -> div [] (match a with _ -> div [] []) *)
    | Sem.Block (title, body) ->
        div [ id "block" ] @@ render ~cfg title forest @ render ~cfg body forest
        (*  TODO:  tag headline*)
    | Sem.If_tex (a, b) -> (
        match (a, b) with (_ : t), (_ : t) -> div [] [ txt "%s" "iftex" ])
    | Sem.Prim (p, c) -> (
        match (p, c) with
        | `Em, t -> em [] @@ render ~cfg t forest
        | `Strong, t -> strong [] @@ render ~cfg t forest
        | `Ul, t -> ul [] @@ render ~cfg t forest
        | `Li, t -> li [] @@ render ~cfg t forest
        | `Blockquote, t -> blockquote [] @@ render ~cfg t forest
        | `Code, t -> code [] @@ render ~cfg t forest
        | `Ol, t -> ol [] @@ render ~cfg t forest
        | `Pre, t -> pre [] @@ render ~cfg t forest
        | `P, t -> HTML.p [] @@ render ~cfg t forest)
    | Sem.Unresolved a -> ( match a with _ -> div [] [ txt "unresolved" ])
    | Sem.Object _o -> div [] [ txt "Can't render object" ]
    | Sem.Img _a -> txt "todo img"
  (* | _ -> Dream_html.txt "%s" "todo" *)

  and render_internal_link ~cfg ~title ~modifier ~addr forest =
    let doc = get_doc addr forest in
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
    a ([ href "%s" addr ] @ target_title_attr) @@ render ~cfg title forest

  and render_external_link ~cfg ~title ~modifier ~url forest =
    let title = Option.map (Sem.apply_modifier modifier) title in
    let title =
      Option.value ~default:[ Range.locate_opt None @@ Sem.Text url ] title
    in
    a [ href "%s" url ] @@ render ~cfg title forest

  and render_transclusion ~cfg ~opts doc forest =
    let cfg =
      let ctr = cfg.counter in
      let ix = if opts.numbered then !ctr + 1 else !ctr in
      ctr := ix;
      let counter = ref 0 in
      { cfg with counter; top = false }
    in
    render_tree ~cfg ~opts doc forest

  and render_query (a, b) =
    let open Core in
    match ((a : transclusion_opts), (b : t Query.t)) with
    | _, Query.Author _
    | _, Query.Tag _
    | _, Query.Taxon _
    | _, Query.Meta (_, _)
    | _, Query.Or _
    | _, Query.And _
    | _, Query.Not _
    | _, Query.True ->
        div [] []

  and toc_item content =
    li []
      [
        a [ href ""; class_ "bullet"; title_ "%s" content ] [ txt "%s" "■" ];
        a [] [ span [ class_ "toc-item-label" ] [ txt "%s" content ] ];
      ]

  and toc (doc : Sem.tree) ~cfg forest =
    let rec transclusions (ns : Sem.node Range.located list) =
      match ns with
      | [] -> []
      | { value = Transclude (_, addr); _ } :: xs -> (
          match get_doc addr forest with
          | Some (t : tree) -> (
              match t.title with
              | Some title ->
                  li []
                    [
                      a [ href "%s" addr; class_ "bullet" ] [ txt "■" ];
                      a
                        [ href "#tree-" ]
                        [
                          span [] @@ render ~cfg title forest;
                          div [] @@ transclusions t.body;
                        ]
                      (* render ~cfg title; *);
                    ]
                  :: transclusions xs
              | None -> a [] [ txt "Unnamed tree" ] :: transclusions xs)
          | None -> [])
      | { value = _; _ } :: xs -> transclusions xs
    in

    ul [ class_ "block" ] (transclusions doc.body)

  and backmatter ~cfg (doc : Sem.tree) forest =
    let cfg = { cfg with counter = ref 0; in_backmatter = true; top = false } in
    let _opts =
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
    let taxon = match doc.taxon with Some t -> [ txt "%s" t ] | None -> [] in
    section [ class_ "block" ]
    @@ taxon
    @ [
        details []
          [
            summary []
              [ frontmatter ~cfg doc forest; mainmatter ~cfg doc forest ];
          ];
      ]

  (* and mainmatter ?mode:_string _tree = div [] [ backmatter _tree ] *)
  and mainmatter ?mode:_string ~cfg tree forest =
    div [] @@ render_body ~cfg tree.body forest

  and addr _ = div [] []
  and source_path _ = div [] []

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
            | Some title -> a [ href "%s" addr ] @@ render ~cfg title forest
            | _ -> txt "Untitled"))
    | None -> txt "%s" author

  (*
  and render_authors (doc : Sem.tree) =
    let contributors =
      match doc.addr with Some addr -> contributors addr | None -> []
    in
    match (doc.authors, contributors) with
    | [] [] -> nil
    | authors, contributors -> div [] []

   and render_date (doc : Sem.tree) =  *)

  and meta_item ~cfg (_i, (t : Sem.node Range.located list)) forest =
    li [ class_ "meta-item" ]
    @@ (t |> List.map (fun n -> render_node ~cfg n forest))

  and render_date (date : Prelude.Date.t) =
    let open Prelude.Date in
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
    m ^ " " ^ d ^ ", " ^ y

  and render_meta (tree : tree) =
    div
      [ class_ "metadata" ]
      [
        ul []
        @@ List.map
             (fun d -> li [ class_ "meta-item" ] [ txt "%s" (render_date d) ])
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

  and frontmatter ~cfg ?(_toc = true) (tree : tree) forest =
    let taxon =
      match tree.taxon with None -> "" | Some t -> String_util.sentence_case t
    in
    let title =
      match tree.title with
      | None -> []
      | Some ts ->
          List.map
            (fun (t : Sem.node Range.located) -> render_node ~cfg t forest)
            ts
    in
    let slug =
      match tree.addr with
      | None -> div [] []
      | Some s -> a [ class_ "slug"; href "%s" s ] [ txt "[%s]" s ]
    in
    header []
      [
        h1 []
        @@ [ span [ class_ "taxon" ] [ txt "%s" taxon ] ]
        @ title @ [ slug ];
        render_meta tree;
      ]

  and render_body ~cfg bdy forest =
    List.map (fun (n : Sem.node Range.located) -> render_node ~cfg n forest) bdy

  and render_tree ~cfg ~opts (doc : tree) forest =
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
        class_ (if opts.expanded then "" else "");
        class_ (if opts.show_heading then "" else "");
        (*  TODO: figure this out *)
        class_
          (if opts.show_metadata then "block hide-metadata"
           else "block hide-metadata");
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
            summary [] [ frontmatter ~cfg doc forest ];
            div [ class_ "tree-content" ] [ mainmatter ~cfg doc forest ];
          ];
      ]

  and fourohfour addr = txt "%s not found" addr
  and with_fallback fof f got = match got with Some t -> f t | None -> fof

  and render_tree_page (doc : Sem.tree) ~cfg ~opts forest =
    div
      [ id "grid-wrapper" ]
      [
        article [] [ render_tree ~cfg ~opts doc forest ];
        nav
          [ Dream_html.HTML.id "%s" "toc" ]
          [
            div
              [ class_ "block" ]
              [ h1 [] [ txt "Table of contents" ]; toc ~cfg doc forest ];
          ];
      ]

  and base_template ~index doc =
    html
      [ lang "en" ]
      [
        head []
          [
            meta [ name "viewport"; content "width=device-width" ];
            title [] "HTMX+Dream+Effects";
            (* script [] *)
            (* "\n\ *)
               (*   \                var socket = new WebSocket(\"ws://\" + \ *)
               (*    window.location.host + \"/diagnostics\");\n\n\ *)
               (*   \    socket.onopen = function () {\n\ *)
               (*   \      socket.send(\"Hello?\");\n\ *)
               (*   \    };\n\n\ *)
               (*   \    socket.onmessage = function (e) {\n\ *)
               (*   \      alert(e.data);\n\ *)
               (*   \    };\n"; *)
            script
              [
                src "https://unpkg.com/htmx.org@1.9.10";
                integrity
                  "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC";
                crossorigin `anonymous;
              ]
              "";
            script [ src "https://unpkg.com/htmx.org/dist/ext/sse.js" ] "";
            link [ rel "stylesheet"; href "/static/style.css" ];
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
                src
                  "https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js";
                integrity
                  "sha384-XjKyOOlGwcjNTAIQHIpgOno0Hl1YQqzUOEleOLALmuqehneUG+vnGctmUb0ZY0l8";
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
          ];
        body []
          [
            header
              [ class_ "header" ]
              [
                nav
                  [ class_ "nav" ]
                  (if not index then
                     [
                       div [ class_ "logo" ] [ a [ href "/" ] [ txt "« Home" ] ];
                     ]
                   else []);
              ];
            doc (* div [ id "grid-wrapper" ] [ doc ]; *);
          ];
      ]

  let index : node =
    base_template ~index:true
      (div
         [ Hx.ext "sse"; Hx.sse_connect "/diagnostics"; Hx.sse_swap "message" ]
         [ txt "Content of this box will be updated!" ])
end
