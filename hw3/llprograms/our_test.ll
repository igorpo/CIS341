define i64 @baz(i64 %x1, i64 %x2) {
  %1 = add i64 %x1, %x2
  ret i64 %1
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = add i64 1, 1
  %2 = add i64 %1, 1
  %tmp = call i64 @baz(i64 %1, i64 %2)
  %3 = add i64 %2, %tmp
  ret i64 %3
}


