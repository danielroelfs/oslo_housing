#!/bin/bash

source ~/.profile

for i in `ls *.Rmd`
do

Rmarkdown $i

done
