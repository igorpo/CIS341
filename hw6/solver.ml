open Datastructures

(* dataflow analysis graph signature ---------------------------------------- *)
(* Interface for dataflow graphs structured in a way to facilitate 
   the general iterative dataflow analysis algorithm.                         

   The AsGraph functor in cfg.ml provides an implementation of this
   DFA_GRAPH signature that converts an LL IR control-flow graph to 
   this representation.
*)
module type DFA_GRAPH =
  sig
    module NodeS : SetS
    type node = NodeS.elt

    (* dataflow facts associated with the nodes of this graph *)
    type fact

    (* the abstract type of dataflow graphs *)
    type t
    val preds : t -> node -> NodeS.t
    val succs : t -> node -> NodeS.t
    val nodes : t -> NodeS.t

    (* the flow function:
       given a graph node and input fact, compute the resulting fact on the 
       output edge of the node                                                *)
    val flow : t -> node -> fact -> fact

    (* lookup / modify the dataflow annotations associated with a node *)    
    val find_fact : t -> node -> fact
    val add_fact : node -> fact -> t -> t

    (* printing *)
    val to_string : t -> string
    val printer : Format.formatter -> t -> unit
  end

(* abstract dataflow lattice signature -------------------------------------- *)
(* The general algorithm works over a generic lattice of abstract "facts".
    - facts can be combined (this is the 'join' operation)
    - facts can be compared                                                   *)
module type FACT =
  sig
    type t
    val combine : t list -> t
    val compare : t -> t -> int
  end


(* generic iterative dataflow solver ---------------------------------------- *)
(* This functor takes two modules:
      Fact  - the implementation of the lattice                                
      Graph - the dataflow anlaysis graph

   It produces a module that has a single function 'solve', which 
   implements the iterative dataflow analysis described in lecture.
      - using a worklist (or workset) nodes 
        [initialized with the set of all nodes]

      - process the worklist until empty:
          . choose a node from the worklist
          . find the node's predecessors and combine their flow facts
          . apply the flow function to the combined input to find the new
            output
          . if the output has changed, update the graph and add the node's
            successors to the worklist                                        

   TASK: complete the [solve] function, which implements the above algorithm.
*)
module Make (Fact : FACT) (Graph : DFA_GRAPH with type fact := Fact.t) =
  struct

    let rec solver workset g = 
      begin match Graph.NodeS.is_empty workset with 
      | true -> g
      | false -> 
        let elt = Graph.NodeS.choose workset in 
        let pre = Graph.NodeS.elements (Graph.preds g elt) in 
        let pre_facts = List.map (fun p -> Graph.find_fact g p) pre in 
        let cmb = Fact.combine pre_facts in 
        let flow_out = Graph.flow g elt cmb in 
        let new_workset = Graph.NodeS.remove elt workset in 
        if (Fact.compare flow_out (Graph.find_fact g elt)) <> 0 then 
          let succ = Graph.succs g elt in 
          let new_new_workset = Graph.NodeS.union new_workset succ in 
          let g' = Graph.add_fact elt flow_out g in 
          solver new_new_workset g'
        else 
          solver new_workset g
      end

    let solve (g:Graph.t) : Graph.t =
      let nodes = Graph.nodes g in 
      solver nodes g
  end

