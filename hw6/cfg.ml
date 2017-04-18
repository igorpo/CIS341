open Ll
open Datastructures

(* control flow graphs ------------------------------------------------------ *)

(* This representation of control-flow graphs is more suited for dataflow
   analysis than the abstract syntax defined in Ll.fdecl

   - a cfg has:
         blocks - a map of labels to  Ll basic block, and
         preds  - a set of labels containing the blocks predecessors           
         ret_ty - the Ll return type of the function
         args   - a list of function parameters with their types

   Representing cfgs as maps makes it simpler to lookup information
   about the nodes in the graph.                                              *)

type cfg = { blocks : Ll.block LblM.t
           ; preds  : LblS.t LblM.t
           ; ret_ty : Ll.ty
           ; args   : (Ll.uid * Ll.ty) list
           } 

let entry_lbl = "_entry"


(* compute a block's successors --------------------------------------------- *)
let block_succs (b:block) : LblS.t =
      match b.terminator with
      | _, Ret _       -> LblS.empty
      | _, Br l        -> LblS.singleton l
      | _, Cbr (_,k,l) -> LblS.of_list [k; l]


(* compute a map from block labels to predecessors -------------------------- *)
let cfg_preds (ast:(lbl * block) list): LblS.t LblM.t =
  let set_add l = LblM.update_or LblS.empty (LblS.add l) in
  List.fold_left (fun m (l, b) -> 
                  m |> LblM.update_or LblS.empty (fun s -> s) l
                    |> LblS.fold (set_add l) (block_succs b)) LblM.empty ast 


(* lookup operations -------------------------------------------------------- *)
let preds (g:cfg) (l:lbl) : LblS.t = LblM.find l g.preds
let succs (g:cfg) (l:lbl) : LblS.t = block_succs @@ LblM.find l g.blocks
let block (g:cfg) (l:lbl) : Ll.block = LblM.find l g.blocks
let nodes (g:cfg) : LblS.t = LblM.bindings g.blocks |> List.map fst |> LblS.of_list
let exits (g:cfg) : LblS.t = LblM.bindings g.blocks 
                             |> List.filter (fun (l, b) -> LblS.is_empty @@ block_succs b )
                             |> List.map fst
                             |> LblS.of_list

(* convert an Ll.fdecl to this representation ------------------------------- *)
let of_ast (fdecl:Ll.fdecl) : cfg =
  let e,bs = fdecl.cfg in
  let ast = (entry_lbl,e)::bs in
  let preds = cfg_preds ast in
  let blocks = List.fold_left 
                 (fun g (l, block) -> LblM.add l block g)
                 LblM.empty ast
  in
  let paramtys, ret_ty = fdecl.fty in
  let args = List.combine fdecl.param paramtys in
  { preds; blocks; ret_ty; args}

(* convert this representation back to Ll.cfg ------------------------------- *)
let to_ast (g:cfg) : Ll.fdecl =
  let e = block g entry_lbl in
  let cfg = e, g.blocks |> LblM.bindings |> List.remove_assoc entry_lbl in
  let param, paramtys = List.split g.args in
  {cfg; param; fty=paramtys,g.ret_ty; }

let add_block (l:lbl) (block:block) (g:cfg) : cfg =
  {g with blocks=LblM.add l block g.blocks }
                  

(* printing functions ------------------------------------------------------- *)
let to_string_annot (annot:lbl -> string) (g:cfg) : string =
  LblM.to_string (fun l block -> annot l ^ Ll.string_of_block block ^ "\n") g.blocks

let to_string : cfg -> string =
  to_string_annot (fun _ -> "\n")

let printer_annot (annot:lbl -> string) (f:Format.formatter) (g:cfg) : unit =
  Format.pp_print_string f (to_string_annot annot g)

let printer : Format.formatter -> cfg -> unit =
  printer_annot (fun _ -> "\n")


(* creating a flow graph froma control flow graph --------------------------- *)

(* Conceptually, this is a view of a cfg annotated with dataflow information
   that should be usable by the generic solver and subsequent optimizations.

   To create a flow graph module for a particular analysis, we need to supply
   several parameters:                                                        *)
module type AS_GRAPH_PARAMS =
  sig 
    (* The type of dataflow facts and the combine operator. This just implements
       the FACT interface from cfg.ml *)
    type t
    val combine : t list -> t
    val to_string : t -> string 

    (* We also need to specify the direction of the analysis and the flow
       function defined on labels of the CFG.
       Note that we consider a label to be a node: the flow function is
       associated with an entire basic block *)
    val forwards : bool 
    val flow : cfg -> lbl -> t -> t
  end

(* This Cfg.AsGraph can be used to create a flow graph for the solver from a
   control flow graph with of_cfg  *)
module AsGraph (D:AS_GRAPH_PARAMS) :
  sig
    (* Implement the DFA_GRAPH signature where facts are defined by the functor
       argument type t and nodes are labels *)
    include Solver.DFA_GRAPH 
            with type fact := D.t
            and  module NodeS = LblS
    
    (* To use the resulting flow graph in optimizations, we need expose a few
       more operations: *)

    (* Create a flow graph for this analysis an initial mapping of facts to 
       labels, a constant flow-in value for the entry or exit labels depending
       on the direction of the analysis, and a CFG *)
    val of_cfg : (node -> D.t) -> D.t -> cfg -> t

    (* The DFA_GRAPH signature only lets you look up the flow out of a node with
       find_fact. When optimizing, it's useful to have the flow in to a node *)
    val find_fact_in : t -> node -> D.t 

    (* For testing purposes, we would like to be able to access the underlying
       map of dataflow facts *)
    val dfa : t -> D.t LblM.t
  end =
  struct
    module NodeS = LblS
                     
    type t = { cfg:cfg
             ; dfa:D.t LblM.t }
    type node = lbl

    (* The label of the logical "boundary" node *)
    let bound_lbl = "__bound"

    (* The only way to create a flow graph is to provide an initial labeling *)
    let of_cfg init flow_in cfg = 
      let dfa = cfg.blocks
                |> LblM.mapi (fun l _ -> init l) 
                |> LblM.add bound_lbl flow_in
      in
      { cfg; dfa }

    (* Access to underlying cfg and facts map  *)
    let block g = block g.cfg

    let nodes g = nodes g.cfg |> NodeS.add bound_lbl

    let dfa g = LblM.remove bound_lbl g.dfa 

    (* Create the dfa successors and predecessors based on the direction of the
       data flow analysis. This also adds the boundary node to the succs/preds  *)
    let extend k v f l = if LblS.mem l k then v else f l 
    let ns = NodeS.singleton

    let dfa_preds = 
      if D.forwards
      then fun g -> (preds g.cfg) 
                    |> extend (ns entry_lbl) (ns bound_lbl)
                    |> extend (ns bound_lbl) NodeS.empty
      else fun g -> (succs g.cfg) 
                    |> extend (exits g.cfg) (ns bound_lbl) 
                    |> extend (ns bound_lbl) NodeS.empty

    let dfa_succs =
      if D.forwards
      then fun g -> extend (ns bound_lbl) (ns entry_lbl) (succs g.cfg)
      else fun g -> extend (ns bound_lbl) (exits g.cfg) (preds g.cfg)

    let preds = dfa_preds
    let succs = dfa_succs

    (* The supplied flow function, plus the boundary value *)
    let flow g (l:lbl) = 
      if Lbl.compare l bound_lbl == 0
      then fun _ -> LblM.find l g.dfa
      else D.flow g.cfg l

    (* Look up and modify facts *)
    let add_fact n d g = { g with dfa=LblM.add n d g.dfa }
        
    let find_fact g n = LblM.find n g.dfa

    let find_fact_in g n =
      let preds = NodeS.elements @@ preds g n in
      let d_outs = List.map (find_fact g) preds in
      D.combine d_outs

    (* Printing functions *)
    let to_string g = 
      to_string_annot (fun l -> D.to_string (find_fact g l) ^ "\n") g.cfg
    let printer f g = 
      printer_annot (fun l -> D.to_string (find_fact g l) ^ "\n") f g.cfg
  end

(* exported type *)
type t = cfg
