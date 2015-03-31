@echo off
set path=%path%;c:\98ddk\bin;c:\98ddk\bin\win98;c:\98ddk\inc\win98
set INCLUDE=c:\98ddk\inc\win98
set ML=-coff -DBLD_COFF -DIS_32 -W2 -c -Cx -Zm -DMASM6 -Cu
set LFLAGS=/VXD /NOD
