define i64 @program(i64 %argc, i8** %argv) {
  br i1 0, label %then, label %else
then:
  ret i64 0
else:
  ret i64 1
}

