define i64 @main(i64 %argc, i8** %arcv) {
  br i1 0, label %then, label %else
then:
  ret i64 7
else:
  ret i64 9
}

