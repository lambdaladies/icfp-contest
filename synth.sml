(* First cut attempt at implementing the synthesis algorithm in this paper:
* https://research.microsoft.com/en-us/um/people/sumitg/pubs/icse10_synthesis.pdf
*
*)

structure Synth =
struct

  structure Set = IntBinarySet

  exception Unimplemented

  type input = Int64.int
  type output = Int64.int
  type constraint = input * output
  datatype prop = Constr of constraint | And of prop * prop | Or of prop * prop
                | Top

  (* dummy oracle for now, should ask server for output *)
  fun oracle x = x

  fun phi_func l i o = 

  fun behave bs l =
    foldl (fn (i,o) => And (phi_func l i o)) Top bs
    
  fun distinct bs l i = raise Unimplemented (* ? *)

  fun toProg l = raise Unimplemented

  (* tsat : ('a -> prop) -> 'a *)
  fun tsat propfun = raise Unimplemented

  fun loop behaviors components =
  let
    val l = tsat (behave behaviors) components
    val distinct_input = tsat (distinct behaviors l)
  in
    case distinct_input of
         NONE => SOME (toProg l)
       | SOME input =>
            let
              val behaviors' = Set.add (behaviors, (input, oracle input))
            in
              loop behaviors'
            end
  end

end
