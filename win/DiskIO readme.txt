This DLL was compiled with Borland C++ 4.5
The original code is from the Microsoft KnowledgeBase.
Thanks also go to Ralf Brown for his interrupt list.  I thougt I had seen the last of that a long time ago, but it is amazing how DOS comes back to haunt you.

Basicly, it is not possible to read raw sectors of a hard disk from an Win32 application under Windows 95.  Microsoft suggest that the workaround is to thunk a 16 bit DLL which uses DPMI to do an interrupt 0x13.  

Anybody who has a better solution, such as a VDX, I would love you to share it.

Update July 21 1999
===================

This DLL now has support for the Int 0x13 Extentions.  Thanks go again to Ralph's interrupt list, and of course Microsoft and IBM who invented the extentions, and then kept them a secret.

John Newbigin
jn@it.swin.edu.au