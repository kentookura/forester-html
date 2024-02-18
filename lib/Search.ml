open Core
open Forester.Forest
open Forester.Analysis
open Wu_Manber

exception Todo

module Pattern : Patterns.PatternWithFoldRight = struct
  type t = forest
  type elem = Sem.tree

  let length forest = Map.to_list forest.trees |> List.length

  let elem_eq (s : Sem.tree) (t : Sem.tree) =
    match (s.addr, t.addr) with Some p, Some q -> p = q | _ -> raise Todo

  let fold_right (f : Sem.tree -> 'a -> 'a) (init : forest) s =
    let list = init.trees |> Map.to_list |> List.map snd in
    List.fold_right f list s
end

module ForestMatcher = Matcher.MakeHashTblMatcher (Pattern)
module FirstMatch = FirstMatch.Make (Pattern) (ForestMatcher)

let search_titles ~k ~pattern ~forest =
  let seq = forest |> Map.to_seq |> Seq.map snd in
  FirstMatch.first_match ~pattern ~k seq
