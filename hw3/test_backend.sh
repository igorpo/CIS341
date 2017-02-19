#!/bin/bash

prog=$1;
echo $prog;
rm -rf output/$prog.s && rm output/$prog.o;
./main.native llprograms/$prog.ll;
cat output/$prog.s && echo "";
