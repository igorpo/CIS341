@x = global i64 1
@y = global i64 2

define i64 @program(i64 %argc, i8** %arcv) {
  %1 = load i64, i64* @x
  %2 = mul i64 %1, 7
  %3 = load i64, i64* @y
  %4 = add i64 %3, 1
  store i64 %4, i64* @y
  ret i64 %1
}

