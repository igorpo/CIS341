%arr = type [1 x i64]

@tmp = global %arr [ i64 12 ]

define i64 @main(i64 %argc, i8** %arcv) {
  %val = alloca i64
  %res = call i64 @find_max(i64 0, i64 10)
  store i64 %res, i64* %val
  %ret_val = load i64, i64* %val
  ret i64 %ret_val
}

define i64 @find_max(i64 %start, i64 %size) {
  %1 = icmp eq i64 %size, 1
  %first_ptr = getelementptr %arr, %arr* @tmp, i64 0, i64 %start
  %first = load i64, i64* %first_ptr  ; value of first element
  br i1 %1, label %base, label %continue

continue:
  %new_size = sub i64 %size, 1
  %new_start = add i64 %start, 1
  %second = call i64 @find_max(i64 %new_start, i64 %new_size)
  %cond = icmp sgt i64 %first, %second
  br i1 %cond, label %ret_first, label %lt_case

base:
  br label %ret_first

lt_case:
  ret i64 %second

ret_first:
  ret i64 %first
}