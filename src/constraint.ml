open! Classroom
open CFG

module Constraint = struct
  type t =
    | CEdge of Command.label * Command.label
    | CPath of Command.label * Command.label
    | Def of Command.label * Command.var
    | Use of Command.label * Command.var
    | Kill of Command.label * Command.label
    | In of Command.label * Command.label
    | Out of Command.label * Command.label
    | Source of Command.label
    | Sink of Command.label
    | Sanitizer of Command.label
    | Edge of Command.label * Command.label
    | Path of Command.label * Command.label
    | Alarm of Command.label * Command.label

  let compare = compare

  let pp fmt = function
    | CEdge (a, b) ->
        F.fprintf fmt "CEdge (%a, %a)" Command.pp_label a Command.pp_label b
    | CPath (a, b) ->
        F.fprintf fmt "CPath (%a, %a)" Command.pp_label a Command.pp_label b
    | Def (l, v) -> F.fprintf fmt "Def (%a, %s)" Command.pp_label l v
    | Use (l, v) -> F.fprintf fmt "Use (%a, %s)" Command.pp_label l v
    | Kill (a, b) ->
        F.fprintf fmt "Kill (%a, %a)" Command.pp_label a Command.pp_label b
    | In (a, b) ->
        F.fprintf fmt "In (%a, %a)" Command.pp_label a Command.pp_label b
    | Out (a, b) ->
        F.fprintf fmt "Out (%a, %a)" Command.pp_label a Command.pp_label b
    | Source l -> F.fprintf fmt "Source %a" Command.pp_label l
    | Sink l -> F.fprintf fmt "Sink %a" Command.pp_label l
    | Sanitizer l -> F.fprintf fmt "Santizer %a" Command.pp_label l
    | Edge (a, b) ->
        F.fprintf fmt "Edge (%a, %a)" Command.pp_label a Command.pp_label b
    | Path (a, b) ->
        F.fprintf fmt "Path (%a, %a)" Command.pp_label a Command.pp_label b
    | Alarm (a, b) ->
        F.fprintf fmt "Alarm (%a, %a)" Command.pp_label a Command.pp_label b
end

module Set = struct
  include Set.Make (Constraint)

  let pp fmt cs = iter (fun c -> F.fprintf fmt "%a\n" Constraint.pp c) cs

  let print set =
    let oc = open_out "result.txt" in
    F.fprintf (F.formatter_of_out_channel oc) "%a" pp set;
    close_out oc
end

include Constraint
