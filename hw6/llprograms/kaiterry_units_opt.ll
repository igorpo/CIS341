define i64 @main(i64 %argc, i8** %argv) {
  br i1 1, label %then1, label %else1
then1:
  ret i64 1
else1:
  ret i64 0
}

