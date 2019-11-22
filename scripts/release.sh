#!/bin/bash
if [ `uname` = "Darwin" ]
then
  mkdir release  
  mv .stack-work/dist/x86_64-osx/Cabal-2.4.0.1/build/notion-ocr-exe/notion-ocr-exe ./release/notion-ocr &&\
  cd release &&\
  zip notion-ocr_mac.zip notion-ocr
  cd ..
else
  mkdir release &&\
  mv .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/notion-ocr-exe/notion-ocr-exe ./release/notion-ocr &&\
  cd release &&\
  zip notion-ocr_x86_64-linux.zip notion-ocr
  cd ..
fi 
