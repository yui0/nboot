@echo off

ml %1.asm
link -ignore:4078 -ignore:4039 -def:%1.def %1.obj
