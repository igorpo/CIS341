define i64 @program(i64 %argc, i8** %argv) {
  %1 = add i64 30, 0
  %2 = sub i64 420, 24
  %3 = add i64 24, %2
  %4 = alloca i64
  store i64 %3, i64* %4
  ret i64 420
}

