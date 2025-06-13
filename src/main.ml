open! Classroom
module F = Format
module ConstraintSet = Analyzer.ConstraintSet

let report const_set =
  ConstraintSet.iter
    (fun c ->
      match c with
      | Constraint.Alarm (src, sink) ->
          Format.printf "Potential Error @@ %s (%s)\n"
            (CFG.Command.string_of_location src)
            (CFG.Command.string_of_location sink)
      | _ -> ())
    const_set

let main argv =
  if Array.length argv <> 2 then (
    prerr_endline "analyzer: You must specify one C file";
    exit 1);
  let cfg = CFG.of_cfile argv.(1) in
  CFG.print cfg;
  let result = cfg |> Analyzer.extract |> Analyzer.solve in
  ConstraintSet.print result;
  report result

let _ = main Sys.argv
