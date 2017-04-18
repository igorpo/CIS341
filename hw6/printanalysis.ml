open Ll
open Datastructures

let do_live =     ref false
let do_cp =       ref false
let do_alias =    ref false
let do_dce =      ref false
let do_cf =       ref false
let full_passes = ref 0


let print_live args (cfg:Cfg.t) : string =
    Liveness.Graph.to_string @@ Liveness.analyze cfg

let files = ref []

let args = let open Arg in
           [ "-live",  Set do_live,  "print liveness"
           ; "-cp",    Set do_cp,    ""
           ; "-alias", Set do_alias, ""
           ; "-dce",   Set do_dce,   ""
           ; "-cf",    Set do_cf,    ""
           ; "-full",  Set_int full_passes, ""
           ]



let do_file fname print_fn =
  let ll_prog = Driver.parse_ll_file fname in
  ll_prog.fdecls
  |> List.iter @@ fun (g,f) ->
                  let string_of_arg (t,u) = Printf.sprintf "%s %%%s" (Ll.sot t) u in
                  let ts, t = f.fty in
                  Printf.printf "define %s @%s(%s) {\n%s\n}\n" 
                                (Ll.sot t) g 
                                (mapcat ", " string_of_arg List.(combine ts f.param))
                                (print_fn (List.combine ts f.param) (Cfg.of_ast f))

let opt_file opt fname =
  let opt_fdecl (gid,fdecl) = 
    let og = opt (Cfg.of_ast fdecl) in
    gid, Cfg.to_ast og
  in

  let p = Driver.parse_ll_file fname in

  let op = { p with fdecls = List.map opt_fdecl p.fdecls } in

  print_endline @@ string_of_prog op

  

let () = 
  if not !Sys.interactive then begin
      Arg.parse args (fun f -> files := f::!files) "Usage";
      if !do_live  then List.iter (fun f -> do_file f print_live)  !files;

    end
  


    

