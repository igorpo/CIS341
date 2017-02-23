define i64 @boo(i64 %x1, i64 %x2, i64 %x3, i64 %x4, i64 %x5, i64 %x6, i64 %x7) {
  %1 = add i64 %x1, %x2
  %2 = add i64 %1, %x3
  %3 = add i64 %2, %x4
  %4 = add i64 %3, %x5
  %5 = add i64 %4, %x6
  %6 = add i64 %5, %x7
  ret i64 %6
}

define i64 @doo(i64 %x) {
  %1 = add i64 2, 0
  %2 = call i64 @boo(i64 %1, i64 %x, i64 %x, i64 %x, i64 %x, i64 %x, i64 %x)
  ret i64 %2
}

define i64 @coo(i64 %x) {
  ret i64 %x
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = alloca i64
  %2 = add i64 1, 0
  %a2 = add i64 0, 0
  %3 = icmp sgt i64 %2, %a2
  br i1 %3, label %then, label %else
then:
  %4 = call i64 @coo(i64 %2) 
  store i64 %4, i64* %1
  br label %end
else:
  %5 = call i64 @doo(i64 %a2)
  store i64 %5, i64* %1 
  br label %end
end:
  %6 = load i64, i64* %1
  ret i64 %6
}

