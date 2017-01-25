open Big_int

type t = { value : int64; overflow : bool }

let ok i = { value = i; overflow = false }

exception Overflow

let with_overflow1 g f i =
  let res = f i in 
  { value = res
  ; overflow = not @@ eq_big_int (big_int_of_int64 res) (g @@ big_int_of_int64 i)
  } 
  
let with_overflow2 g f i j =
  let res = f i j in
  { value = res
  ; overflow = not @@ eq_big_int (big_int_of_int64 res) 
		                 (g (big_int_of_int64 i) (big_int_of_int64 j))
  } 

let neg = with_overflow1 minus_big_int Int64.neg
let succ = with_overflow1 succ_big_int Int64.succ
let pred = with_overflow1 pred_big_int Int64.pred

let add = with_overflow2 add_big_int Int64.add
let sub = with_overflow2 sub_big_int Int64.sub
let mul = with_overflow2 mult_big_int Int64.mul
