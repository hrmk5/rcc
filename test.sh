#!/bin/bash
try() {
  expected="$1"
  input="$2"

  ./target/debug/rcc.exe "$input" > tmp.s
  gcc -o tmp tmp.s
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$expected expected, but got $actual"
    exit 1
  fi
}

try 0 0
try 42 42
try 28 '10-5+23'
try 1 '5 + 7 - 11'
try 28 '3 + 5 * 5'
try 30 '(4 + 6) * 3'
try 3 '12 / 4'
try 36 '31 + +3 + -6 - -8'
try 1 '3 < 5'
try 1 '5 <= 7 - 2'
try 1 '2 == 2'
try 0 '3 != 3'
try 0 '1 > (3 + 2)'
try 0 '5 >= 21'

echo OK
