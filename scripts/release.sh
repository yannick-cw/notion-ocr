#!/bin/bash

echo "Running script to create release .zip"

if [ `uname` = "Darwin" ]
then
  mkdir release  
  cp .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/notion-ocr-exe/notion-ocr-exe ./release/notion-ocr &&\
  cd release &&\
  zip notion-ocr_mac.zip notion-ocr
  tar -zcvf notion-ocr_mac.tar.gz notion-ocr
  rm notion-ocr
  cd ..
else
  mkdir release &&\
  cp .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/notion-ocr-exe/notion-ocr-exe ./release/notion-ocr &&\
  cd release &&\
  zip notion-ocr_x86_64-linux.zip notion-ocr
  rm notion-ocr
  cd ..
fi 
