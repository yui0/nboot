@echo off

ml -coff -c -Cx -DMASM6 -DBLD_COFF -DIS_32 %1.asm
link -ignore:4078 -ignore:4039 -vxd -def:%1.def %1.obj

pause
