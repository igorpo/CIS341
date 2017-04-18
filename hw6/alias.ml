(** Alias Analysis *)

open Ll
open Datastructures

(* The lattice of abstract pointers ----------------------------------------- *)
module SymPtr =
  struct
    type t = MayAlias           (* uid names a pointer that may be aliased *)
           | Unique             (* uid is the unique name for a pointer *)
           | UndefAlias         (* uid is not in scope or not a pointer *)

    let compare : t -> t -> int = Pervasives.compare

    let to_string = function
      | MayAlias -> "MayAlias"
      | Unique -> "Unique"
      | UndefAlias -> "UndefAlias"

    let combine_one s t = match s, t with
      | MayAlias, _ | _, MayAlias -> MayAlias
      | Unique, Unique -> Unique
      | UndefAlias, x | x, UndefAlias -> x
  end

(* The analysis computes, at each program point, which UIDs in scope are a unique name
   for a stack slot and which may have aliases *)
type fact = SymPtr.t UidM.t

(* flow function across Ll instructions ------------------------------------- *)
(* - After an alloca, the defined UID is the unique name for a stack slot
   - A pointer returned by a load, call, bitcast, or GEP may be aliased
   - A pointer passed as an argument to a call, bitcast, GEP, or store
     may be aliased
   - Other instructions do not define pointers
 *)
let insn_flow ((u,i):uid * insn) (d:fact) : fact =
  match i with
  | Alloca _ -> UidM.add u SymPtr.Unique d
  | Load (Ptr (Ptr _), _) -> UidM.add u SymPtr.MayAlias d
  | Store (Ptr _,Id v,_) -> UidM.add v SymPtr.MayAlias d
  | Call (t,_,args) -> 
     let d' = match t with Ptr _ -> UidM.add u SymPtr.MayAlias d
                         | _ -> d
     in
     List.fold_left (fun d (t, op) -> match t, op with
                     | Ptr _, Id v -> UidM.add v SymPtr.MayAlias d
                     | _ -> d
                    ) d' args
  | Gep (_,Id v,_)
  | Bitcast (_,Id v,_) -> d |> UidM.add v SymPtr.MayAlias
                            |> UidM.add u SymPtr.MayAlias
  | Bitcast _
  | Gep (_,_,_) -> d |> UidM.add u SymPtr.MayAlias
  | _ -> d

(* The flow function across terminators is trivial: they never change alias info *)
let terminator_flow t (d:fact) : fact = d

(* module for instantiating the generic framework --------------------------- *)
module Fact =
  struct
    type t = fact
    let forwards = true

    let flow (g:Cfg.t) (l:Ll.lbl) (d:fact) : fact = 
      Analysis.block_flow_forwards insn_flow terminator_flow (Cfg.block g l) d

    (* UndefAlias is logically the same as not having a mapping in the fact. To
       compare dataflow facts, we first remove all of these *)
    let normalize : fact -> fact = 
      UidM.filter (fun _ v -> v != SymPtr.UndefAlias)

    let compare (d:fact) (e:fact) : int = 
      UidM.compare SymPtr.compare (normalize d) (normalize e)

    let to_string : fact -> string =
      UidM.to_string (fun _ v -> SymPtr.to_string v)

    (* The alias analysis should take the join over predecessors to compute the
       flow into a node. You may find the UidM.merge function useful *)
    let combine (ds:fact list) : fact = 
      let combine_one d e = 
        UidM.merge (fun k s t -> match s, t with
                    | Some s, Some t -> Some (SymPtr.combine_one s t)
                    | x, None | None, x -> x
                   ) d e
      in
      List.fold_left combine_one UidM.empty ds
  end

(* instantiate the general framework ---------------------------------------- *)
module Graph = Cfg.AsGraph (Fact)
module Solver = Solver.Make (Fact) (Graph)

(* expose a top-level analysis operation ------------------------------------ *)
let analyze (g:Cfg.t) : Graph.t =
  (* the analysis starts with every node set to bottom (the map of every uid 
     in the function to UndefAlias *)
  let init l = UidM.empty in

  (* the flow into the entry node should indicate that any pointer parameter 
     to the function may be aliased *)
  let alias_in = 
    List.fold_right 
      (fun (u,t) -> match t with
                    | Ptr _ -> UidM.add u SymPtr.MayAlias
                    | _ -> fun m -> m) 
      g.Cfg.args UidM.empty 
  in
  let fg = Graph.of_cfg init alias_in g in
  Solver.solve fg

