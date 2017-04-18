define i64 @program(i64 %argc, i8** %arcv) {
  %1 = add i64 0, 64
  %2 = alloca i64
  store i64 %1, i64* %2
  %3 = load i64, i64* %2
  %4 = mul i64 4, 12
  %5 = icmp sgt i64 %4, 52
  br i1 %5, label %then, label %else
then:
  store i64 8, i64* %2
  br label %merge
else:
  store i64 0, i64* %2
  br label %merge
merge:
  %6 = load i64, i64* %2
  ret i64 %6
}

