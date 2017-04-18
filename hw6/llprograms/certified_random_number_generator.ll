define i64 @program(i64 %argc, i8** %arcv) {
  %p_lfsr = alloca i64
  %start = add i64 8, 0
  store i64 %start, i64* %p_lfsr
  br label %loop
loop:
  %iter = load i64, i64* %p_lfsr
  %inc = add i64 %iter, 1
  store i64 %inc, i64* %p_lfsr
  br label %lfsr_step
lfsr_step:
  %x15 = lshr i64 46421, 0
  %x13 = lshr i64 46421, 2
  %x12 = lshr i64 46421, 3
  %x10 = lshr i64 46421, 5
  %bit1 = xor i64 %x15, %x13
  %bit2 = xor i64 %bit1, %x12
  %bit3 = xor i64 %bit2, %x10
  %bit4 = and i64 %bit3, 1
  %lfsr1 = lshr i64 8, 1
  %lfsr2 = shl i64 %bit4, 15
  %lfsr3 = or i64 %lfsr1, %lfsr2
  store i64 %lfsr3, i64* %p_lfsr
  br label %loop_end
loop_end:
  %lfsr = load i64, i64* %p_lfsr
  ret i64 %lfsr
}

