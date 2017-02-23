declare i64 @ll_atol(i8*)
declare i8* @ll_ltoa(i64)
declare void @ll_puts(i8*)

define i64 @main(i64 %argc, i8** %argv) {
  %p1 = getelementptr i8*, i8** %argv, i64 1
  %p_v1 = load i8*, i8** %p1
  %v1 = call i64 @ll_atol(i8* %p_v1)
  %k = add i64 %v1, 2

  ; calc sizeof
  %size = add i64 %argc, -2
  %right = add i64 %size, 1

  %res = call i64 @select(i8** %argv, i64 2, i64 %right, i64 %k)
  %result = add i64 0, %res

  ;%st = call i8* @ll_ltoa(i64 %res)
  ;call void @ll_puts(i8* %st)

  ret i64 %result
}


define i64 @select(i8** %arr, i64 %left, i64 %right, i64 %k) {
  %cond1 = icmp eq i64 %left, %right
  br i1 %cond1, label %one_element, label %more_than_one_element

one_element:
  %res1 = call i64 @get_from_arr(i8** %arr, i64 %left)
  ret i64 %res1

more_than_one_element:
  %pi = add i64 %left, 0
  %pi2 = call i64 @partition(i8** %arr, i64 %left, i64 %right, i64 %pi)
  %cond2 = icmp eq i64 %k, %pi2

  br i1 %cond2, label %k_eq, label %k_neq

k_eq:
  %res2 = call i64 @get_from_arr(i8** %arr, i64 %k)
  ret i64 %res2

k_neq:
  %cond3 = icmp slt i64 %k, %pi2
  br i1 %cond3, label %k_lt, label %k_gt

k_lt:
  %pi_less_1 = sub i64 %pi2, 1
  %res3 = call i64 @select(i8** %arr, i64 %left, i64 %pi_less_1, i64 %k)
  ret i64 %res3

k_gt:
  %pi_more_1 = add i64 %pi2, 1
  %res4 = call i64 @select(i8** %arr, i64 %pi_more_1, i64 %right, i64 %k)
  ret i64 %res4
}

define i64 @get_from_arr(i8** %arr, i64 %i) {
  %p = getelementptr i8*, i8** %arr, i64 %i
  %p_v = load i8*, i8** %p
  %v = call i64 @ll_atol(i8* %p_v)

  ret i64 %v
}

define i64 @partition(i8** %arr, i64 %left, i64 %right, i64 %k) {
  %pv = call i64 @get_from_arr(i8** %arr, i64 %k)
  call void @swap(i8** %arr, i64 %k, i64 %right)

  %store_index = alloca i64
  store i64 %left, i64* %store_index

  %i = alloca i64
  store i64 %left, i64* %i
  br label %begin_loop_p

begin_loop_p:
  %cur_i = load i64, i64* %i
  %cond = icmp slt i64 %cur_i, %right
  br i1 %cond, label %loop_p, label %end_p

loop_p:
  %val_i = call i64 @get_from_arr(i8** %arr, i64 %cur_i)
  %i_more_one = add i64 %cur_i, 1
  store i64 %i_more_one, i64* %i
  %cond2 = icmp slt i64 %val_i, %pv
  br i1 %cond2, label %doswap, label %begin_loop_p

doswap:
  %store_index_val = load i64, i64* %store_index
  call void @swap(i8** %arr, i64 %store_index_val, i64 %cur_i)
  %add1 = add i64 %store_index_val, 1
  store i64 %add1, i64* %store_index
  br label %begin_loop_p

end_p:
  %store = load i64, i64* %store_index
  call void @swap(i8** %arr, i64 %right, i64 %store)


  %result = call i8* @ll_ltoa(i64 %store)
  ret i64 %store
}

define void @swap(i8** %arr, i64 %i, i64 %j) {
  %i_loc = getelementptr i8*, i8** %arr, i64 %i
  %i_val = load i8*, i8** %i_loc

  %j_loc = getelementptr i8*, i8** %arr, i64 %j
  %j_val = load i8*, i8** %j_loc

  store i8* %j_val, i8** %i_loc
  store i8* %i_val, i8** %j_loc
  ret void
}
