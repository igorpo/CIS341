define i64 @rsa_decrypt(i64 %c) {
  %p = add i64 56807, 0
  %q = add i64 51683, 0
  %n = mul i64 %p, %q
  %d = add i64 1409083253, 0
  %e = add i64 65537, 0
  %_b__3 = alloca i64
  store i64 %c, i64* %_b__3
  %_e__1 = alloca i64
  store i64 %d, i64* %_e__1
  %_res__5 = alloca i64
  store i64 1, i64* %_res__5
  %_i__7 = alloca i64
  store i64 0, i64* %_i__7
  br label %_cond__14
_cond__14:
  %_val___9 = load i64, i64* %_i__7
  %_val___10 = load i64, i64* %_e__1
  %_bop___11 = icmp slt i64 %_val___9, %_val___10
  br i1 %_bop___11, label %_body__13, label %_post__12
_body__13:
  %_val___15 = load i64, i64* %_res__5
  %_val___16 = load i64, i64* %_b__3
  %_bop___17 = mul i64 %_val___15, %_val___16
  store i64 %_bop___17, i64* %_res__5
  %_val___19 = load i64, i64* %_i__7
  %_bop___20 = add i64 %_val___19, 1
  store i64 %_bop___20, i64* %_i__7
  br label %_cond__14
_post__12:
  %_pow_res = load i64, i64* %_res__5
  %_x__25 = alloca i64
  store i64 %_pow_res, i64* %_x__25
  %_y__23 = alloca i64
  store i64 %n, i64* %_y__23
  %_val___27 = load i64, i64* %_x__25
  %_res__28 = alloca i64
  store i64 %_val___27, i64* %_res__28
  br label %_cond__35
_cond__35:
  %_val___30 = load i64, i64* %_res__28
  %_val___31 = load i64, i64* %_y__23
  %_bop___32 = icmp sge i64 %_val___30, %_val___31
  br i1 %_bop___32, label %_body__34, label %_post__33
_body__34:
  %_val___36 = load i64, i64* %_res__28
  %_val___37 = load i64, i64* %_y__23
  %_bop___38 = sub i64 %_val___36, %_val___37
  store i64 %_bop___38, i64* %_res__28
  br label %_cond__35
_post__33:
  %m = load i64, i64* %_res__28
  ret i64 %m
}

