open! Classroom
module F = Format
module Command = CFG.Command
module ConstraintSet = Constraint.Set

(* ************************************** *
   Extracting Basic Facts
 * ************************************** *)

let extract_cedge src dst set =
  ConstraintSet.add
    (Constraint.CEdge (Command.label_of src, Command.label_of dst))
    set

let extract_source node set =
  match node with
  | Command.Source (l, _) -> ConstraintSet.add (Constraint.Source l) set
  | _ -> set

let extract_sanitizer node set =
  match node with
  | Command.Sanitizer (l, _, _) ->
      ConstraintSet.add (Constraint.Sanitizer l) set
  | _ -> set

let extract_sink node set =
  match node with
  | Command.Sink (l, _) -> ConstraintSet.add (Constraint.Sink l) set
  | _ -> set

let extract_def node set =
  match node with
  | Command.Assign (l, v, _) -> ConstraintSet.add (Constraint.Def (l, v)) set
  | Command.Source (l, v) -> ConstraintSet.add (Constraint.Def (l, v)) set
  | Command.Sanitizer (l, v, _) -> ConstraintSet.add (Constraint.Def (l, v)) set
  | _ -> set

let extract_use node set =
  match node with
  | Command.Assign (l, _, e) ->
      List.fold_left
        (fun acc v -> ConstraintSet.add (Constraint.Use (l, v)) acc)
        set (Command.get_vars e)
  | Command.Sanitizer (l, _, e) ->
      List.fold_left
        (fun acc v -> ConstraintSet.add (Constraint.Use (l, v)) acc)
        set (Command.get_vars e)
  | Command.Sink (l, e) ->
      List.fold_left
        (fun acc v -> ConstraintSet.add (Constraint.Use (l, v)) acc)
        set (Command.get_vars e)
  | _ -> set

let extract_kill node1 node2 set =
  match (node1, node2) with
  | Command.Assign (l1, v1, _), Command.Assign (l2, v2, _)
    when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Assign (l1, v1, _), Command.Source (l2, v2) when v1 = v2 && l1 <> l2
    ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Assign (l1, v1, _), Command.Sanitizer (l2, v2, _)
    when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Source (l1, v1), Command.Assign (l2, v2, _) when v1 = v2 && l1 <> l2
    ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Source (l1, v1), Command.Source (l2, v2) when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Source (l1, v1), Command.Sanitizer (l2, v2, _)
    when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Sanitizer (l1, v1, _), Command.Assign (l2, v2, _)
    when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Sanitizer (l1, v1, _), Command.Source (l2, v2)
    when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | Command.Sanitizer (l1, v1, _), Command.Sanitizer (l2, v2, _)
    when v1 = v2 && l1 <> l2 ->
      ConstraintSet.add (Constraint.Kill (l1, l2)) set
  | _ -> set

let extract : CFG.t -> ConstraintSet.t =
 fun cfg ->
  let set = ConstraintSet.empty in
  let set = CFG.fold_edges extract_cedge cfg set in
  let set =
    CFG.fold_vertex
      (fun node acc ->
        acc |> extract_source node |> extract_sanitizer node
        |> extract_sink node |> extract_def node |> extract_use node)
      cfg set
  in
  CFG.fold_vertex
    (fun node1 acc1 ->
      CFG.fold_vertex (fun node2 acc2 -> extract_kill node1 node2 acc2) cfg acc1)
    cfg set

(* ************************************** *
   Rules for Reaching Definition Analysis
 * ************************************** *)

let derive_out1 cs =
  ConstraintSet.fold
    (fun c acc ->
      match c with
      | Constraint.Def (a, _) -> ConstraintSet.add (Constraint.Out (a, a)) acc
      | _ -> acc)
    cs ConstraintSet.empty

let derive_out2 cs =
  ConstraintSet.fold
    (fun c acc ->
      match c with
      | Constraint.In (a, c) ->
          if not (ConstraintSet.mem (Constraint.Kill (a, c)) cs) then
            ConstraintSet.add (Constraint.Out (a, c)) acc
          else acc
      | _ -> acc)
    cs ConstraintSet.empty

let derive_in cs =
  ConstraintSet.fold
    (fun c1 acc ->
      match c1 with
      | Constraint.CEdge (a, b) ->
          ConstraintSet.fold
            (fun c2 acc2 ->
              match c2 with
              | Constraint.Out (a', c) when a = a' ->
                  ConstraintSet.add (Constraint.In (b, c)) acc2
              | _ -> acc2)
            cs acc
      | _ -> acc)
    cs ConstraintSet.empty

(* ************************************** *
   Rules for Taint Analysis
 * ************************************** *)

let derive_edge cs =
  ConstraintSet.fold
    (fun c1 acc ->
      match c1 with
      | Constraint.Source a ->
          ConstraintSet.fold
            (fun c2 acc2 ->
              match c2 with
              | Constraint.Use (b, v) ->
                  ConstraintSet.fold
                    (fun c3 acc3 ->
                      match c3 with
                      | Constraint.Out (b', a') when b = b' && a = a' ->
                          ConstraintSet.fold
                            (fun c4 acc4 ->
                              match c4 with
                              | Constraint.Def (a'', v') when a = a'' && v = v'
                                ->
                                  ConstraintSet.add
                                    (Constraint.Edge (a, b))
                                    acc4
                              | _ -> acc4)
                            cs acc3
                      | _ -> acc3)
                    cs acc2
              | _ -> acc2)
            cs acc
      | _ -> acc)
    cs ConstraintSet.empty

let derive_path1 cs =
  ConstraintSet.fold
    (fun c acc ->
      match c with
      | Constraint.Edge (a, b) -> ConstraintSet.add (Constraint.Path (a, b)) acc
      | _ -> acc)
    cs ConstraintSet.empty

let derive_path2 cs =
  ConstraintSet.fold
    (fun c1 acc ->
      match c1 with
      | Constraint.Path (a, b) ->
          if not (ConstraintSet.mem (Constraint.Sanitizer b) cs) then
            ConstraintSet.fold
              (fun c2 acc2 ->
                match c2 with
                | Constraint.CEdge (b', c) when b = b' ->
                    ConstraintSet.add (Constraint.Path (a, c)) acc2
                | _ -> acc2)
              cs acc
          else acc
      | _ -> acc)
    cs ConstraintSet.empty

let derive_alarm cs =
  ConstraintSet.fold
    (fun c1 acc ->
      match c1 with
      | Constraint.Path (a, b) ->
          ConstraintSet.fold
            (fun c2 acc2 ->
              match c2 with
              | Constraint.Sink b' when b = b' ->
                  ConstraintSet.add (Constraint.Alarm (a, b)) acc2
              | _ -> acc2)
            cs acc
      | _ -> acc)
    cs ConstraintSet.empty

let rec solve const_set =
  let new_facts =
    ConstraintSet.empty
    |> ConstraintSet.union (derive_out1 const_set)
    |> ConstraintSet.union (derive_out2 const_set)
    |> ConstraintSet.union (derive_in const_set)
    |> ConstraintSet.union (derive_edge const_set)
    |> ConstraintSet.union (derive_path1 const_set)
    |> ConstraintSet.union (derive_path2 const_set)
    |> ConstraintSet.union (derive_alarm const_set)
  in

  let updated_set = ConstraintSet.union const_set new_facts in

  if ConstraintSet.equal updated_set const_set then const_set
  else solve updated_set
