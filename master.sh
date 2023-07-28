#!/bin/bash


for i in $(ls -d */); do
  root=$(sed 's:\/::g' <<< ${i})

  python3 get_tensors.py ${root}

  [[ -d "${root}_tensors/" ]] || mkdir ${root}_tensors/
  make clean
  make
  ./tensors.x < filelist.in
  mv *.csv *.dat ${root}_tensors/
done
