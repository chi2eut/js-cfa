#!/bin/bash

# Define the input file (the one provided as an argument)
input_file=$1
m=$2

echo -e "Analyzing $1 with setting m=$2...\n"

acorn --ecma2025 "$input_file" > out.txt ; racket mcfa2.rkt -v out.txt m ; rm out.txt
