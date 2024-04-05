#!/bin/bash

if [[ -d ./compareDB ]] ; then
  rm -rf ./compareDB
fi


mkdir -p compareDB/step_0004
for i in pl ml sfc  ; do
  for j in "0" "1" ; do
    mkdir -p "./compareDB/step_0004/${i}_${j}";
  done;
done
