define i64 @foo(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7, i64 %x8) {
  ret i64 %x6
}

define i64 @bar(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7, i64 %x8) {
  ret i64 %x7
}

define i64 @baz(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7, i64 %x8) {
  ret i64 %x8
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @foo(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8)
  %2 = call i64 @bar(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8)
  %3 = call i64 @baz(i64 1, i64 2, i64 3, i64 4, i64 5, i64 6, i64 7, i64 8)
  %4 = add i64 %1, %2
  %5 = add i64 %4, %3
  ret i64 %5
}

