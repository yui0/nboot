//---------------------------------------------------------
//	ディスク制御
//
//		（Ｃ）２００１　ＮＡＫＡＤＡ
//---------------------------------------------------------

#include <windows.h>
#include <mem.h>
#include "ss.h"
#include "discio.h"

//---------------------------------------------------------
//	Code in the 16-bit DLL

#define	SECTOR_SIZE	512
#define	CARRY_FLAG	0x0001


//---------------------------------------------------------
//	構造体定義
//---------------------------------------------------------

/*struct DiskAddressPacket {
	byte size;
	byte reserved;
	word count;
	dword buffer;
	dword startLo;
	dword startHi;
};

struct DriveParameters {
	word size;
	word flags;
	dword cylinders;
	dword heads;
	dword sectors;
	dword sectorsLo;
	dword sectorsHi;
	word bytesPerSector;
//---v2.0+ ---
	dword EDDptr;
//---v3.0 ---
	word signature;
	byte v3size;
	byte reserved[3];
	byte bus[4];
	byte interface[8];
	byte interfacePath[8];
	byte devicePath[8];
	byte reserved2;
	byte checksum;
};*/

struct RMCS {
	dword edi, esi, ebp, RESERVED, ebx, edx, ecx, eax;
	word  flag, es, ds, fs, gs, ip, cs, sp, ss;
};


//---------------------------------------------------------
//	レジスタを初期化
//---------------------------------------------------------

void BuildRMCS(RMCS FAR *reg)
{
	_fmemset(reg, 0, sizeof(RMCS));
}


//---------------------------------------------------------
//	DPMIを通して割り込み
//---------------------------------------------------------

int SimulateRM_Int(byte n, RMCS FAR *reg)
{
	sint r=FALSE;

	_asm {
		push	di

		mov		ax,0x300	// DPMI Simulate Real Mode Int
		mov		bl,n		// Number of the interrupt to simulate
		mov		bh,1		// Bit 0 = 1; all other bits must be 0
		xor		cx,cx		// No words to copy
		les		di,reg
		int		0x31		// Call DPMI
		jc		end			// CF set if error occurred
		mov		r,TRUE
end:
		pop di
	}

	return r;
}


//---------------------------------------------------------
//	ディスク初期化
//---------------------------------------------------------

int far PASCAL _export ResetDisk(int d)
{
	RMCS reg;

	BuildRMCS(&reg);

	reg.eax = 0;	// BIOS Reset disk system
	reg.edx = d;

	return SimulateRM_Int(0x13, &reg);
}


//---------------------------------------------------------
//	直接ディスクにアクセス(AccessPhysicalSector)
//---------------------------------------------------------

int far PASCAL _export DiscAccess(int d, int f, int c, int h, int s, int n, byte *buff, int size)
{
	int r;
	RMCS reg;
	dword gda;		// Return value of GlobalDosAlloc()
	byte *rbuff;	// Real-mode buffer pointer
	byte *pbuff;	// Protected-mode buffer pointer

	if (!buff || size < (SECTOR_SIZE * n)) return FALSE;

	gda = GlobalDosAlloc(size);
	if (!gda) return FALSE;

	rbuff = (byte*)MAKELONG(0, HIWORD(gda));
	pbuff = (byte*)MAKELONG(0, LOWORD(gda));

	BuildRMCS(&reg);

	reg.eax = f+n;				// BIOS read or write, n sector
	reg.ecx = (c<<8) + s;		// Sector s, Cylinder c
	reg.edx = (h<<8) + d;		// Head 0, Drive #
	reg.ebx = LOWORD(rbuff);	// Offset of sector buffer
	reg.es	= HIWORD(rbuff);	// Segment of sector buffer

	if (f & DIO_WRITE) _fmemcpy(pbuff, buff, size);
	r = SimulateRM_Int(0x13, &reg);
	if ((f & DIO_READ) && r) _fmemcpy(buff, pbuff, size);

	// Free the sector data buffer this function allocated
	GlobalDosFree(LOWORD(gda));

	if (!r || (reg.flag & CARRY_FLAG)) return FALSE;

	return TRUE;
}
