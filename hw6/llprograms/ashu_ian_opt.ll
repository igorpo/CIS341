define i64 @program(i64 %argc, i8** %arcv) {
  br i1 1, label %then22, label %else21
else21:
  br label %merge20
merge20:
  ret i64 -1
then22:
  ret i64 5
}

