define i64 @mod(i64 %m, i64 %n) {
  %res = icmp slt i64 %m, %n
  br i1 %res, label %exit, label %recur
recur:
  %v = sub i64 %m, %n
  %r = call i64 @mod(i64 %v, i64 %n)
  ret i64 %r
exit:
  ret i64 %m
}

define i64 @gcd(i64 %m, i64 %n) {
  %remainder = call i64 @mod(i64 %m, i64 %n)
  %flag = icmp eq i64 %remainder, 0
  br i1 %flag, label %exit1, label %recur1
exit1:
  ret i64 %n
recur1:
  %r = call i64 @gcd(i64 %n, i64 %remainder)
  ret i64 %r
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @gcd(i64 1071, i64 462)
  ret i64 %1
}
