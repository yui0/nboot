/*
* ---------------------------------------------------------------------
*	int13: Sample code to issue real(v86)-mode int 13h interrupt.
* ---------------------------------------------------------------------
* Author:	Takehiko Morita (morry)  http://tech.millto.net/~morry/
* Description:
*	This is a sample code to issue real(v86)-mode interrupt via
*	DPMI "Simulate real mode interrupt" service (DPMI 0300h).
* Note:
*	This program uses VxDCall() UNDOCUMENTED API and VWin32.vxd
*	UNDOCUMENTED Win32-Service.
*
* History:
* 99/08/21 v00.01.00	morry	created
*
*	Copyright (C) 1999 Takehiko Morita (morry)
* ---------------------------------------------------------------------
*/
#define ProgName	"int13"
const char	UsageText[] = 
  ProgName " - issue int 13h via DPMI call (C) 1999 Takehiko Morita (morry)\n"
  "Usage: " ProgName " [options] drive head cylinder sector\n"
  "  - \'drive\' is a drive id such as 0x00 (1'st FDD), 0x80 (1'st HDD).\n"
  "Options:\n"
  "  -n sectnum   number of sectors to read (default: 1)\n"
  "  -s size      sector size (default: 512)\n"
  "  -o filename  filename to save sector data (default: dump to screen)\n"
  "  -v           display verbose information\n"
  "Ex)  " ProgName " -n 2 -o sector.bin  0x80  1 0 1\n"
;
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>

/* the dos memory block allocator */
#include "../include/DosMem32.h"
#pragma comment(lib, "../lib/DosMem32.lib")

//translate the real-mode pointer (seg:off) to the win32 pointer (0:32)
inline void * flatptr( unsigned short seg, unsigned short off=0 ) {
	return (void *)( ((unsigned long)seg << 4) + off );
} //flatptr

/*
* ---------------------------------------------------------------------
*	DPMI client register set
* ---------------------------------------------------------------------
*/
#define IR(n) union { DWORD E##n; WORD n; }
#define XR(n) union { DWORD E##n##X; WORD n##X; struct { BYTE n##L, n##H; }; }
#pragma pack(1)
struct DPMIRegs {
	//registers
	IR(DI); IR(SI); IR(BP);		//EDI, ESI, EBP
	unsigned long	reserved;
	XR(B); XR(D); XR(C); XR(A);	//EBX, EDX, ECX, EAX
	unsigned short	Flags, ES, DS, FS, GS, IP, CS, SP, SS;

	//(E)Flags and bits
	enum { CarryBit=0x0001 };
	inline bool CF( void ) const { return 0 != (Flags & CarryBit); }

	//ctor
	DPMIRegs( void ) { 
		memset(this, 0, sizeof *this);	//!!! BAD CODE !!!
	}
}; //DPMIRegs
#pragma pack()

/*
* ---------------------------------------------------------------------
*	int 13h definitions
* ---------------------------------------------------------------------
*/
//int 13h AH=08h "Get Drive Parameters"
struct DriveParam {
	unsigned char	type;		//drive type (BL)
	unsigned char	drvnum;		//number of drives (DL)
	unsigned char	head;		//maximum head number (DH)
	unsigned short	cylinder;	//maximum cylinder number (CH)
	unsigned char	sector;		//maximum sector number (CL)
}; //DriveParam

struct IDNAME {		//"id(value) - string" mapping table
	unsigned long	id;
	const char *	name;
}; //IdName
const IDNAME	driveType[] = {		//BL (drive type)
	{ 1, "360K" },	{ 2, "1.2M" },	{ 3, "720K" },	{ 4, "1.44M" },
	{ 6, "2.88M" },	{ 0x10, "ATAPI removable" }
};
void i13_get_drive_parameter(
	unsigned char	drive,		//drive number
	DriveParam *	p		//o: drive parameter
);

//int 13h AH=02h "Read Sectors"
void i13_read_sector(
	unsigned char	drive,		//drive number
	unsigned char	head,		//head number
	unsigned short	cylinder,	//cylinder number
	unsigned char	sector,		//sector number
	unsigned char	sectnum,	//number of sectors to read
	unsigned short	bufseg,		//segment of real-mode buffer
	unsigned short	bufoff		//offset of real-mode buffer
);

const unsigned long	Size64k = 0x00010000;
const int	INT_DISKBIOS = 0x13;

/*
* ---------------------------------------------------------------------
*	helpers
* ---------------------------------------------------------------------
*/
void *  dos_alloc(		//returns 32bit protected-mode pointer
	unsigned long	size,		//i: buffer allocation size
	unsigned short*	rmseg,		//o: real-mode segment
	unsigned short*	pmsel		//o: protected-mode selector
);
void dpmi_realmode_int( unsigned int intno, DPMIRegs* pregs );
void save( const char* fname, const void* buf, size_t size );
void dump( const void* buf, size_t size );

#define  len_of(a)	( sizeof(a) / sizeof(*(a)) )

/*
* ---------------------------------------------------------------------
*	command line parser
* ---------------------------------------------------------------------
*/
class BadArg {		//"Invalid Argument" exception
 public:
	BadArg( const char* sz=NULL ) : message( NULL==sz ? _Default : sz ){}
	const char *	message;
 protected:
	static const char *	_Default;
};
const char *	BadArg::_Default = "Mandatory argument not present";

// generate char/short/int/long value from the string (throws BadArg exception)
template <class T> void mkint( const char* sz, T* pv ) {
	if ( NULL == sz ) throw BadArg();
	unsigned int	i;
	if ( 1 != sscanf(sz, "%i", &i) ) throw BadArg(sz);
	*pv = (T)i;
	if ( i != (unsigned int)*pv ) throw BadArg(sz);
}

/*
* ---------------------------------------------------------------------
*	main
* ---------------------------------------------------------------------
*/
int  main( int ac, char** av )
{
	unsigned int	drive, head, cylinder, sector, sectnum=1;
	unsigned int	sectSize = 512;
	const char *	output=NULL;
	bool		Verbose = false;

	//process commnad line arguments
	try {
		for ( ++av; NULL != *av && '-' == **av; ++av ) {
			++av;
			switch( av[-1][1] ) {
			case  '?':
				puts(UsageText);
				return  0;
			case  'n':		//number of sectors
				mkint(*av, &sectnum);
				break;
			case  's':		//sector size
				mkint(*av, &sectSize);
				break;
			case  'o':		//output file
				if (NULL == (output=*av)) throw BadArg(av[-1]);
				break;
			case  'v':		//verbose
				Verbose = true;
				av--;
				break;
			default:
				throw  BadArg(av[-1]);
				break;
			}
		}
		mkint(*av, &drive);
		mkint(*++av, &head);
		mkint(*++av, &cylinder);
		mkint(*++av, &sector);
		if ( NULL != *++av )	throw BadArg(*av);
	}//try
	catch( BadArg& badarg ) {
		fprintf(stderr, "Invalid Argument [%s]", badarg.message);
		fputs(UsageText, stderr);
		return  ERROR_INVALID_PARAMETER;
	}

	unsigned short	rmseg=0, pmsel=0;
	long		rc = NO_ERROR;
	try {
		//if verbose mode, display drive information
		if ( Verbose ) {
			DriveParam	dp;
			i13_get_drive_parameter(drive, &dp);

			const char *	typestr = "Unknown";
			for ( int i = 0; i < len_of(driveType); i++ ) {
				if ( dp.type == driveType[i].id ) {
					typestr = driveType[i].name;
					break;
				}
			}

			printf(	"[%02hX] type=%02X(%s) drivenum=%u"
				" head=%u cylinder=%u sector=%u\n",
				drive, dp.type, typestr, dp.drvnum,
				dp.head, dp.cylinder, dp.sector);
		} //if Verbose

		//allocate real-mode buffer
		unsigned long	bufsize = sectSize * sectnum;
		if ( Size64k < bufsize ) {
			fprintf(stderr, "sector buffer exceeds 64k\n");
			throw  long(ERROR_AUTODATASEG_EXCEEDS_64k);
		}
		void *	rmbuf = dos_alloc(bufsize, &rmseg, &pmsel);

		//read sector
		i13_read_sector(drive, head, cylinder, sector, sectnum,
			rmseg, 0);

		//dump sector data
		if ( NULL == output ) {
			dump(rmbuf, bufsize);
		}
		else {
			save(output, rmbuf, bufsize);
		}
	} //try
	catch( long errcode ) {
		fprintf(stderr, "error #%ld(%04X)\n", errcode, errcode);
		rc = errcode;
	}
	catch( ... ) {
		fprintf(stderr, "internal error has occured\n");
		rc = ERROR_INTERNAL_ERROR;
	}

	if ( pmsel != 0 ) GlobalDosFree32(pmsel);

	return  rc;
} //main

/*
* ---------------------------------------------------------------------
*	i13_get_drive_parameter: issue real-mode int 13h to get drive parameter
* ---------------------------------------------------------------------
*/
void  i13_get_drive_parameter(
	unsigned char	drive,		//drive number
	DriveParam *	p		//o: drive parameter
)
{
	//IN	AH=08h, DL=drive (bit 7 set for hard disk)
	DPMIRegs	r;
	r.AH = 0x08;
	r.DL = drive;

	dpmi_realmode_int(INT_DISKBIOS, &r);

	if ( r.CF() || r.AH != 0 ) {
		fprintf(stderr, "get-drive-parameter error #%02X\n", r.AH);
		throw  long(r.AH);
	}

	//OUT	AH=00h, BL=drive type, CH=max cylinder number (LO 8bit)
	//	CL=max sector number (b5-b0)
	//		+ high 2 bits of max cylinder number (b7-b6)
	//	DH=max head number, DL=number of drives,
	//	ES:DI=drive parameter table (floppies only)
	p->type = r.BL;
	p->cylinder = r.CL & 0xC0;
	p->cylinder <<= 2;
	p->cylinder += r.CH;
	p->sector = r.CL & 0x3F;
	p->head = r.DH;
	p->drvnum = r.DL;
} //i13_get_drive_parameter

/*
* ---------------------------------------------------------------------
*	i13_read_sector: issue real-mode int 13h to read disk sector
* ---------------------------------------------------------------------
*/
void i13_read_sector(
	unsigned char	drive,		//drive number
	unsigned char	head,		//head number
	unsigned short	cylinder,	//cylinder number
	unsigned char	sector,		//sector number
	unsigned char	sectnum,	//number of sectors to read
	unsigned short	bufseg,		//segment of real-mode buffer
	unsigned short	bufoff		//offset of real-mode buffer
)
{
	//IN	AH = 02h, AL = number of sectors to read
	//	CH = low eight bits of cylinder number
	//	CL = sector number (b0-b5) + high two bits of cylinder (b6-b7)
	//	DH = head number, DL = drive number (bit 7 set for hard disk)
	//	ES:BX -> data buffer
	DPMIRegs	r;
	r.AH = 0x02;
	r.AL = sectnum;
	r.CH = (unsigned char)cylinder;
	cylinder >>= 2;
	cylinder &= 0xC0;	//high two bits of cylinder
	r.CL = (unsigned char)cylinder | (sector & 0x3F);
	r.DH = head;
	r.DL = drive;
	r.ES = bufseg;
	r.BX = bufoff;

	dpmi_realmode_int(INT_DISKBIOS, &r);

	//OUT	CF=1, AH=status code (when error)
	if ( r.CF() ) {
		fprintf(stderr, "disk read error #%02X\n", r.AH);
		throw  long(r.AH);
	}
} //i13_read_sector

/*
* ---------------------------------------------------------------------
*	calling VWin32.vxd Win32-Service to issue DPMI call
* ---------------------------------------------------------------------
*/
#pragma optimize("", off)	//disable optimization for the inline asm
extern "C" void VxDCall( void );	//issues VxD Win32-Services
#pragma comment(lib, "VxDCall.lib")	//built by makefile

//VWin32.vxd Win32-Service number (for the generic VWin32 version)
extern "C" {
  const unsigned long	VWin32_Get_Version = 0x002A0000;
  unsigned long		VWin32_Issue_Int21 = 0x002A0010;
  unsigned long		VWin32_Issue_Int31 = 0x002A0029;
};

//VWin32 Win32-Service initialization
void  init_vwin32_service( void )
{
	//note:	If the VWin32 version is 0104h, then Win32-Service
	//	number maybe different from other versions.
	const unsigned short	VER_OSR21=0x0104;  //USB Supplement
	const unsigned short	VER_WIN98=0x040A;  //Win98
	unsigned short	vwin32ver;	//VWin32 version #
	__asm {
		pushad
		push	VWin32_Get_Version
		call	VxDCall
		mov	[vwin32ver], ax
		popad
	}
	if ( VER_OSR21 <= vwin32ver && vwin32ver < VER_WIN98 ) {
		VWin32_Issue_Int21 = 0x002A000D;
		VWin32_Issue_Int31 = 0x002A0020;
	}
} //init_vwin32_service

struct VWin32Init {
	VWin32Init( void ) {  init_vwin32_service(); }
}  vwin32_initializer;	//the only instance (initializer)

/*
* ---------------------------------------------------------------------
*	dpmi_realmode_int: Simulate Real Mode Interrupt
* ---------------------------------------------------------------------
*/
void  dpmi_realmode_int( unsigned int intno, DPMIRegs* preg )
{
	long	errcode = 0;

	__asm {
		pushad

	;DPMI #0300h: Simulate Real Mode Interrupt
	; AX	= 0300H
	; BL	= interrupt number
	; BH	= flags (should be 0)
	; CX	= # of words to copy from protected mode to real mode stack
	; ES:(E)DI	= selector:offset of real mode register data structure
		mov	ebx, [intno]	;BL=int-no, BH=flags(0)
		mov	edi, [preg]	;register buffer
		push	0		;ECX (# of stack-words)
		push	0300h		;EAX (DPMI function #)
		push	VWin32_Issue_Int31
	#if defined(_DEBUG)
		clc
	#else
		stc
	#endif
		call	VxDCall
		jnc	Done		;jump when success
	;DPMI #0300h result (error)
	; AX	= DPMI error code (v1.0)
		movzx	eax, ax
		test	eax, eax
		jnz	_Error
		mov	eax, -1
	  _Error:
		mov	[errcode], eax

	  Done:
		popad
	}
	if ( 0 != errcode )  throw errcode;
} /* dpmi_realmode_int */

/*
* ---------------------------------------------------------------------
*	dos_alloc: Allocate DOS memory block
* ---------------------------------------------------------------------
* Note:	When error, this function sends the exception
*	long(ERROR_NOT_ENOUGH_MEMORY).
* ---------------------------------------------------------------------
*/
void *  dos_alloc(		//returns 32bit protected-mode pointer
	unsigned long	size,		//i: buffer allocation size
	unsigned short*	rmseg,		//o: real-mode segment
	unsigned short*	pmsel		//o: protected-mode selector
){
	void *	win32ptr;
	if ( NULL == (win32ptr = GlobalDosAllocPtr(size, rmseg, pmsel)) ) {
		//error
		throw  long(ERROR_NOT_ENOUGH_MEMORY);
	}
	return  win32ptr;
} //dos_alloc

//save buffer to the file
void save( const char* fname, const void* buf, size_t size )
{
	FILE*	fp = fopen(fname, "wb");
	if ( NULL == fp ) throw  long(ERROR_WRITE_FAULT);

	try {
		if ( size != fwrite(buf, 1, size, fp) ) {
			throw  long(ERROR_WRITE_FAULT);
		}
	}
	catch( ... ) {
		fclose(fp);
		throw;
	}
	fclose(fp);
} //save

//dump buffer
#include <mbstring.h>
bool inline dumpable( unsigned int c ) { return 0 != _ismbcprint(c); }
void dump( const void* buf, size_t size )
{
	const unsigned char *	pb = (const unsigned char *)buf;
	const unsigned char *	end = pb + size;
	const size_t	bytesPerLine = 16;

	while ( pb < end ) {
		printf("%08lX:", size_t(pb)-size_t(buf));

		size_t	linesize = min(end-pb, bytesPerLine);
		size_t	col;
		for ( col = 0; col < linesize; col++ ) {
			printf("%c%02X",
				col == bytesPerLine/2 ? '-' : ' ', pb[col]);
		}
		for ( ; col < bytesPerLine; col++ ) {
			putchar(' ');
			putchar(' ');
			putchar(' ');
		}
		putchar(' ');
		putchar(' ');
		for ( col = 0; col < linesize; col++ ) {
			if ( col == bytesPerLine/2 )  putchar(' ');
			putchar( dumpable(pb[col]) ? pb[col] : '.' );
		}

		putchar('\n');
		pb += linesize;
	}
} //dump
//eof
