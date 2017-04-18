@x = global i64 1
@y = global i64 2

define i64 @program(i64 %argc, i8** %arcv) {
  %1 = load i64, i64* @x
  ret i64 %1
}

