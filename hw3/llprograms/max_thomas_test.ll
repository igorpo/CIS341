define i64 @main(i64 %argc, i8** %argv) {
  %a = alloca i64
  store i64 1, i64* %a
  %b = alloca i64*
  store i64* %a, i64** %b
  %c = alloca i64**
  store i64** %b, i64*** %c
  %d = alloca i64***
  store i64*** %c, i64**** %d
  %e = alloca i64****
  store i64**** %d, i64***** %e
  %f = alloca i64*****
  store i64***** %e, i64****** %f
  %g = alloca i64******
  store i64****** %f, i64******* %g
  %h = alloca i64*******
  store i64******* %g, i64******** %h
  %i = alloca i64********
  store i64******** %h, i64********* %i
  ret i64 120
}

