define i64 @program(i64 %argc, i8** %arcv) {
  br i1 0, label %l1, label %l2
l1:
  br i1 1, label %l2, label %l3
l2:
  ret i64 2500
l3:
  ret i64 0
}

