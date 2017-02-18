#!/bin/bash

rm output/our_test.s && rm output/our_test.o;
./main.native llprograms/our_test.ll;
cat output/our_test.s && echo "";
