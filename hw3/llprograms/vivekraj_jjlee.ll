define i64 @program(i64 %argc, i8** %argv) {
  %a = xor i64 1, 2
  %b = mul i64 -3, 31
  %c = add i64 %b, 99
  %d = icmp slt i64 %c, 0
  br i1 %d, label %then, label %else
then:
  ret i64 0
else:
  ret i64 1
}

