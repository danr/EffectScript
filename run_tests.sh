#!/bin/bash
for i in $(ls test/*esc); do
  diff <(boot/Main $i) test/$(basename $i .esc).out
  echo $? $i
done
