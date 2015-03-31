//---------------------------------------------------------
//	ÉfÉBÉXÉNêßå‰
//
//		ÅiÇbÅjÇQÇOÇOÇPÅ@ÇmÇ`ÇjÇ`ÇcÇ`
//---------------------------------------------------------

#include <windows.h>
#include <mem.h>
#include "ss.h"
#include "discio.h"

#define	SECTOR_SIZE	512


//---------------------------------------------------------
//	calling VWin32.vxd Win32-Service to issue DPMI call
//---------------------------------------------------------

// DPMI registers
#define IR(n) union { dword e##n; word n; }
#define XR(n) union { dword e##n##x; word n##x; struct { byte n##l, n##h; }; }
#pragma pack(1)
struct DPMI_REG {
	// registers
	IR(di); IR(si); IR(bp);		// EDI, ESI, EBP
	unsigned long reserved;
	XR(b); XR(d); XR(c); XR(a);	// EBX, EDX, ECX, EAX
	unsigned short Flags, es, ds, fs, gs, ip, cs, sp, ss;

	// (E)Flags and bits
	enum { CarryBit=0x0001 };
	inline bool CF() const { return 0 != (Flags & CarryBit); }

	// ctor
	DPMI_REG() {
		memset(this, 0, sizeof *this);	//!!! BAD CODE !!!
	}
};
#pragma pack()

// VWin32.vxd Win32-Service number (for the generic VWin32 version)
const unsigned long	VWin32_Get_Version = 0x002A0000;
unsigned long		VWin32_Issue_Int21 = 0x002A0010;
unsigned long		VWin32_Issue_Int31 = 0x002A0029;

// VWin32 Win32-Service initialization
void init_vwin32_service()
{
	const unsigned short VER_OSR21=0x0104;	// USB Supplement
	const unsigned short VER_WIN98=0x040A;	// Win98
	unsigned short ver;

	_asm {
		pushad
		push	VWin32_Get_Version
		call	VxDCall
		mov		[ver],ax
		popad
	}
	if (VER_OSR21 <= ver && ver < VER_WIN98) {
		VWin32_Issue_Int21 = 0x002A000D;
		VWin32_Issue_Int31 = 0x002A0020;
	}

	return;
}

// the only instance (initializer)
struct VWin32Init {
	VWin32Init() { init_vwin32_service(); }
} vwin32_initializer;


//---------------------------------------------------------
//	dpmi_realmode_int: Simulate Real Mode Interrupt
//---------------------------------------------------------

long dpmi_realmode_int(unsigned int n, DPMI_REG *reg)
{
	long errcode = 0;

	_asm {
		pushad

		mov		ebx,[n]		// BL=int-no, BH=flags(0)
		mov		edi,[reg]	// register buffer
		push	0			// ECX (# of stack-words)
		push	0300h		// EAX (DPMI function #)
		push	VWin32_Issue_Int31
#if defined(_DEBUG)
		clc
#else
		stc
#endif
		call	VxDCall
		jnc		_end		// jump when success

		movzx	eax,ax
		test	eax,eax
		jnz		_err
		mov		eax,-1
_err:	mov		[errcode],eax

_end:	popad
	}

	return errcode;
}


//---------------------------------------------------------
//	íºê⁄ÉfÉBÉXÉNÇ…ÉAÉNÉZÉX
//---------------------------------------------------------

int DiscAccess(int d, int f, int c, int h, int s, int n, byte *buff, int size)
{
	DPMI_REG reg;

	reg.ax = f+n;				// BIOS read or write, n sector
	reg.ch = (byte)c;			// Cylinder
	c = (c>>2) & 0xc0;			// high two bits of cylinder
	reg.cl = (byte)c|(s&0x3f);	// Sector
	reg.dh = h;					// Head
	reg.dl = d;					// Drive
	reg.es = bufseg;			// Segment of sector buffer
	reg.bx = bufoff;			// Offset of sector buffer

	dpmi_realmode_int(INT_DISKBIOS, &reg);

	if (r.CF()) return reg.ah;

	return 0;
}
