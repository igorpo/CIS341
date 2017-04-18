define i64 @main(i64 %argc, i8** %argv) {
  %one = add i64 0, 1
  %none = sub i64 0, %one
  %five = add i64 0, 5
  %nfive = sub i64 0, %five
  %ten = add i64 0, 10
  %nten = sub i64 0, %ten
  %sixty = add i64 0, 60
  %1 = add i64 %ten, %five
  %2 = sub i64 %ten, %five
  %3 = mul i64 %ten, %five
  %4 = shl i64 %ten, %one
  %5 = lshr i64 %nten, %sixty
  %6 = ashr i64 %nten, %one
  %7 = and i64 %five, %one
  %8 = or i64 %ten, %five
  %9 = xor i64 %ten, %five
  %t1 = icmp eq i64 %1, 15
  %t2 = icmp eq i64 %2, 5
  %t3 = icmp eq i64 %3, 50
  %t4 = icmp eq i64 %4, 20
  %t5 = icmp eq i64 %5, 15
  %t6 = icmp eq i64 %6, %nfive
  %t7 = icmp eq i64 %7, 1
  %t8 = icmp eq i64 %8, 15
  %t9 = icmp eq i64 %9, 15
  %t9a = icmp ne i64 %9, 14
  %t9b = icmp slt i64 %9, 16
  %t9c = icmp sle i64 %9, 15
  %t9d = icmp sgt i64 %9, 14
  %t9e = icmp sgt i64 %9, %none
  %t9f = icmp sge i64 %9, 15
  %r1 = and i1 %t1, 1
  %r2 = and i1 %t2, %r1
  %r3 = and i1 %t3, %r2
  %r4 = and i1 %t4, %r3
  %r5 = and i1 %t5, %r4
  %r6 = and i1 %t6, %r5
  %r7 = and i1 %t7, %r6
  %r8 = and i1 %t8, %r7
  %r9 = and i1 %t9, %r8
  %r9a = and i1 %t9a, %r9
  %r9b = and i1 %t9b, %r9a
  %r9c = and i1 %t9c, %r9b
  %r9d = and i1 %t9d, %r9c
  %r9e = and i1 %t9e, %r9d
  %r9f = and i1 %t9f, %r9e
  br i1 %r9f, label %then1, label %else1
then1:
  ret i64 1
else1:
  ret i64 0
}

