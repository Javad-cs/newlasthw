open! Classroom
open ProsysCil
module F = Format

module Command = struct
  type var = string
  type exp = Var of var | Const of int | BinOp of (exp * exp)
  type label = int * Cil.location

  type t =
    | Assign of label * var * exp
    | Source of label * var
    | Sanitizer of label * var * exp
    | Sink of label * exp
    | Branch of label
    | Skip of label

  let label_of = function
    | Assign (l, _, _)
    | Source (l, _)
    | Sanitizer (l, _, _)
    | Sink (l, _)
    | Branch l
    | Skip l ->
        l

  let get_vars exp =
    let rec loop e acc =
      match e with
      | Var v -> v :: acc
      | Const _ -> acc
      | BinOp (e1, e2) -> acc |> loop e1 |> loop e2
    in
    loop exp []

  let string_of_location (_, loc) =
    let file =
      try
        let idx = String.rindex loc.Cil.file '/' in
        let len = String.length loc.file in
        String.sub loc.file (idx + 1) (len - idx - 1)
      with _ -> loc.file
    in
    file ^ ":" ^ string_of_int loc.line

  let pp_label fmt label = F.fprintf fmt "%d" (label |> fst)

  let pp_exp fmt = function
    | Var v -> F.fprintf fmt "%s" v
    | Const i -> F.fprintf fmt "%d" i
    | vs ->
        (* Vars *)
        let vars = get_vars vs in
        F.fprintf fmt "%s" (String.concat "," vars)

  let pp fmt = function
    | Assign (l, var, e) -> F.fprintf fmt "%a: %s = %a" pp_label l var pp_exp e
    | Source (l, var) -> F.fprintf fmt "%a: %s = source()" pp_label l var
    | Sanitizer (l, var, e) ->
        F.fprintf fmt "%a: %s = sanitizer(%a)" pp_label l var pp_exp e
    | Sink (l, e) -> F.fprintf fmt "%a: sink(%a)" pp_label l pp_exp e
    | Skip l -> F.fprintf fmt "%a: skip" pp_label l
    | Branch l -> F.fprintf fmt "%a: branch" pp_label l
end

module Node = struct
  type t = Command.t

  let compare = compare
  let equal = ( = )
  let hash = Hashtbl.hash

  let rec of_exp exp =
    let open Cil in
    match exp with
    | Lval (Var v, NoOffset) -> Command.Var v.vname
    | Const (CInt64 (i, _, _)) -> Command.Const (Z.to_int i)
    | BinOp (_, e1, e2, _) -> Command.BinOp (of_exp e1, of_exp e2)
    | _ -> failwith "Unsupported syntax"

  let tostring s = Escape.escape_string (Pretty.sprint ~width:0 s)

  let of_stmt stmt =
    let open Cil in
    match stmt.skind with
    | Instr [ Set ((Var vi, NoOffset), e, loc) ] ->
        Command.Assign ((stmt.sid, loc), vi.vname, of_exp e)
    | Instr [ Call (Some (Var vi, _), Lval (Var f, _), _, loc) ]
      when f.vname = "source" ->
        Source ((stmt.sid, loc), vi.vname)
    | Instr [ Call (Some (Var vi, _), Lval (Var f, _), [ e ], loc) ]
      when f.vname = "sanitizer" ->
        Sanitizer ((stmt.sid, loc), vi.vname, of_exp e)
    | Instr [ Call (None, Lval (Var f, _), [ e ], loc) ] when f.vname = "sink"
      ->
        Sink ((stmt.sid, loc), of_exp e)
    | If (_, _, _, loc) -> Branch (stmt.sid, loc)
    | Return (_, loc) | Loop (_, loc, _, _) | Goto (_, loc) | Break loc ->
        Skip (stmt.sid, loc)
    | Instr [] | Block _ -> Skip (stmt.sid, Cil.locUnknown)
    | _ ->
        d_stmt () stmt |> tostring |> prerr_endline;
        failwith "Unsupported syntax"

  let pp = Command.pp
end

module G = Graph.Persistent.Digraph.ConcreteBidirectional (Node)

type t = G.t

let add_edge = G.add_edge
let empty = G.empty
let iter_edges = G.iter_edges
let fold_edges = G.fold_edges
let fold_vertex = G.fold_vertex

let find_main cil =
  Cil.foldGlobals cil
    (fun result glob ->
      match glob with
      | Cil.GFun (fd, _) when fd.svar.vname = "main" -> Some fd
      | _ -> result)
    None

let find_main_exn cil =
  match find_main cil with Some fd -> fd | None -> failwith "main not found"

let of_cfile file =
  let cil = Frontend.parse file in
  let fd = find_main_exn cil in
  Cil.computeCFGInfo fd true;
  List.fold_left
    (fun g stmt ->
      List.fold_left
        (fun g succ -> add_edge g (Node.of_stmt stmt) (Node.of_stmt succ))
        g stmt.Cil.succs)
    empty fd.sallstmts

let pp fmt g =
  iter_edges
    (fun src dst -> F.fprintf fmt "%a -> %a\n" Node.pp src Node.pp dst)
    g

let print cfg =
  let oc = open_out "cfg.txt" in
  F.fprintf (F.formatter_of_out_channel oc) "%a@?" pp cfg;
  close_out oc

let num_of_assignments cfg =
  fold_vertex
    (fun node cnt -> match node with Command.Assign _ -> cnt + 1 | _ -> cnt)
    cfg 0

let num_of_obvious_bugs cfg =
  fold_edges
    (fun src dst cnt ->
      match (src, dst) with
      | Command.Source (_, var), Command.Sink (_, e)
        when Command.get_vars e |> List.mem var ->
          cnt + 1
      | _ -> cnt)
    cfg 0
