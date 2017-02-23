%arr = type [5 x i64]

@tmp = global %arr [ i64 154, i64 322, i64 -1, i64 534, i64 521 ]

define i64 @sqrt(i64 %n) {
  %i = alloca i64
  store i64 0, i64* %i
  %cmp1 = icmp slt i64 %n, 0
  br i1 %cmp1, label %negative, label %loop
loop:
  %x = load i64, i64* %i
  %x2 = mul i64 %x, %x
  %cmp2 = icmp slt i64 %n, %x2
  br i1 %cmp2, label %output, label %increment
output:
  %o = sub i64 %x, 1
  ret i64 %o
increment:
  %a = add i64 %x, 1
  store i64 %a, i64* %i
  br label %loop
negative:
  ret i64 0
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = getelementptr %arr, %arr* @tmp, i32 0, i32 2
  %2 = load i64, i64* %1
  %3 = call i64 @sqrt(i64 %2)
  ret i64 %3
}
