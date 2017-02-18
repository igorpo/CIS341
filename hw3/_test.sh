#!/bin/bash


rm output/our_test.s && rm output/our_test.o;
./main.native --clang llprograms/our_test.ll;
cat output/our_test.s && echo "";
