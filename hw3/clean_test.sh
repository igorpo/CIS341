#!/bin/bash

prog=$1;
make;
rm -rf output;
main.native llprograms/$prog.ll;
cat output/$prog.s;
