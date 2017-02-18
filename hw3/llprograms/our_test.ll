;  %1 = add i64 50, 0
;  %2 = add i64 25, 0
;  %3 = add i64 75, 0
;  %4 = add i64 10, 0
;  %5 = add i64 30, 0
;  %6 = add i64 60, 0
;  %7 = call i64 @program(i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6)

define void @program(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f) {
	ret void
}

define void @main(i64 %argc, i8** %arcv) {
  %1 = add i64 50, 0
  %2 = add i64 25, 0
  %3 = add i64 75, 0
  %4 = add i64 10, 0
  %5 = add i64 30, 0
  %6 = add i64 60, 0
  call i64 @program(i64 %1, i64 %2, i64 %3, i64 %4, i64 %5, i64 %6)
  ret void
}

