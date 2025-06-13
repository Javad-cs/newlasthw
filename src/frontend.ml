open! Classroom
open ProsysCil

let rec remove_temp instrs =
  match instrs with
  | Cil.Call (Some l, f, es, loc) :: Cil.Set (x, Cil.Lval r, _) :: t when l = r
    ->
      Cil.Call (Some x, f, es, loc) :: remove_temp t
  | h :: t -> h :: remove_temp t
  | [] -> []

class removeTempVisitor =
  object
    inherit Cil.nopCilVisitor

    method! vblock b =
      let new_bstmts =
        List.fold_left
          (fun bstmts stmt ->
            match stmt.Cil.skind with
            | Cil.Instr [] when stmt.labels <> [] -> stmt :: bstmts
            | Cil.Instr instrs ->
                stmt.Cil.skind <- Cil.Instr (remove_temp instrs);
                stmt :: bstmts
            | _ -> stmt :: bstmts)
          [] b.Cil.bstmts
      in
      b.Cil.bstmts <- List.rev new_bstmts;
      Cil.DoChildren
  end

class flattenVisitor =
  object
    inherit Cil.nopCilVisitor

    method! vblock b =
      let new_bstmts =
        List.fold_left
          (fun bstmts stmt ->
            match stmt.Cil.skind with
            | Cil.Instr [] when stmt.labels <> [] -> stmt :: bstmts
            | Cil.Instr instrs ->
                (List.map Cil.mkStmtOneInstr instrs |> List.rev) @ bstmts
            | _ -> stmt :: bstmts)
          [] b.Cil.bstmts
      in
      b.Cil.bstmts <- List.rev new_bstmts;
      Cil.DoChildren
  end

let initialize () =
  Cil.initCIL ();
  Cabs2cil.doCollapseCallCast := true

let prepare cfile =
  Cil.iterGlobals cfile (function
    | Cil.GFun (fd, _) -> Cil.prepareCFG fd
    | _ -> ());
  Cil.visitCilFile (new removeTempVisitor) cfile;
  Cil.visitCilFile (new flattenVisitor) cfile

let parse cfile =
  initialize ();
  let cil = Frontc.parse cfile () in
  prepare cil;
  cil
