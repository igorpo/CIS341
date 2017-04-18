define i64 @program(i64 %argc, i8** %arcv) {
  %p_lfsr = alloca i64
  store i64 8, i64* %p_lfsr
  br label %loop
loop:
  %iter = load i64, i64* %p_lfsr
  %inc = add i64 %iter, 1
  store i64 %inc, i64* %p_lfsr
  br label %lfsr_step
lfsr_step:
  store i64 4, i64* %p_lfsr
  br label %loop_end
loop_end:
  %lfsr = load i64, i64* %p_lfsr
  ret i64 %lfsr
}

