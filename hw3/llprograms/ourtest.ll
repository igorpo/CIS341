define i64 @baz(i64 %x1) {
  %1 = add i64 %x1, 1
  ret i64 %1
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  store i64 %1 0
  %tmp = call i64 @baz(i64 %1)
  %2 = add i64 %1, %tmp
  ret i64 %1
}