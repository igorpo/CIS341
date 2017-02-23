define i64 @ackermann(i64 %m, i64 %n) {
  %cmp = icmp eq i64 %m, 0
  br i1 %cmp, label %mzero , label %rest
mzero:
  %r = add i64 %n, 1
  ret i64 %r
rest:
  %cmp2 = icmp eq i64 %n, 0
  br i1 %cmp2, label %nzero, label %rest2
nzero:
  %mnext = sub i64 %m, 1
  %reccall = call i64 @ackermann(i64 %mnext, i64 1)
  ret i64 %reccall
rest2:
  %mnext2 = sub i64 %m, 1
  %nnext = sub i64 %n, 1
  %rightcall = call i64 @ackermann(i64 %m, i64 %nnext)
  %lastcall = call i64 @ackermann(i64 %mnext2, i64 %rightcall)
  ret i64 %lastcall
}

define i64 @main(i64 %argc, i8** %arcv) {
  %1 = call i64 @ackermann(i64 3, i64 3)
  %2 = call i8* @ll_ltoa(i64 %1)
  call void @ll_puts(i8* %2)
  ret i64 0
}
