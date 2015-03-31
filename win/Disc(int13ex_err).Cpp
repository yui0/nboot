//---------------------------------------------------------
//	ÉfÉBÉXÉNêßå‰
//
//		ÅiÇbÅjÇQÇOÇOÇPÅ@ÇmÇ`ÇjÇ`ÇcÇ`
//---------------------------------------------------------

#include <stdio.h>
#include <windows.h>

#define VWIN32_DIOC_DOS_IOCTL	1
#define VWIN32_DIOC_DOS_INT25	2
#define VWIN32_DIOC_DOS_INT26	3
#define VWIN32_DIOC_DOS_INT13	4

struct DIOC_REGISTERS {
	DWORD ebx;
	DWORD edx;
	DWORD ecx;
	DWORD eax;
	DWORD edi;
	DWORD esi;
	DWORD flag;
};

#define DIO_FLOPPY	0
#define DIO_HARDDISK	0x80
#define DIO_READ	0x4200
#define DIO_WRITE	0x4300


#pragma pack(1)
struct LBA_TBL {
	BYTE size;
	BYTE reserved;
	WORD count;
	DWORD offset;
	DWORD lo;
	DWORD hi;
};
#pragma pack()


//---------------------------------------------------------
//	ÉfÉBÉXÉNêßå‰
//---------------------------------------------------------

bool DiscIO(int d, int f, int c, int s, int n, unsigned char *buff)
{
	HANDLE hDevice;
	DIOC_REGISTERS reg;
	DWORD cb;
	bool r;

	hDevice = CreateFile("\\\\.\\vwin32",
	    0, 0, NULL, 0, FILE_FLAG_DELETE_ON_CLOSE, NULL);

	LBA_TBL tbl;
	tbl.size=16;
	tbl.reserved=0;
	tbl.count=1;
	tbl.offset=(DWORD)buff;
	tbl.lo=1;
	tbl.hi=0;

	reg.eax = f;		// BIOS read or write, n sector
	reg.edx = 0;
	reg.esi = (DWORD)&tbl;
	reg.flag = 0x0001;	// assume error (carry flag is set)

	r = DeviceIoControl(hDevice,
		VWIN32_DIOC_DOS_INT13, &reg, sizeof(reg), &reg, sizeof(reg), &cb, 0);

	CloseHandle(hDevice);
	printf("%d %d\n",r,reg.flag);

	if (!r || (reg.flag & 0x0001)) return FALSE;

	return TRUE;
}


//---------------------------------------------------------
//	Ç”ÇÎÇ¡Ç“Å[Ç©ÇÁãNìÆÇ≈Ç´ÇÈÇÊÇ§Ç…
//	- saves nboot in a floppy disk -
//---------------------------------------------------------

void InstallFloppy()
{
	BYTE mbr[512], buff[27648];

	if (DiscIO(DIO_FLOPPY, DIO_READ, 0, 1, 1, mbr)) {
		mbr[0]=0xeb;	// jmp code
		mbr[1]=0x3c;	// to preserve the floppy info
		mbr[2]=0x90;	// 0x903c
		mbr[510]=0x55;	// save the boot signature
		mbr[511]=0xaa;	// 0xaa55

		FILE *fp;
		fp=fopen("NBoot.com", "rb");
		fread(buff, sizeof(buff), 1, fp);
		fclose(fp);

		// code for floppies
		memcpy(&mbr[0x3e], &buff[0xc33], 170);	// loadf c33

		// save the entire program and configuration
		DiscIO(DIO_FLOPPY, DIO_WRITE, 0, 1, 1, mbr);
		DiscIO(DIO_FLOPPY, DIO_WRITE, 1, 1, 18, buff+3);
		DiscIO(DIO_FLOPPY, DIO_WRITE, 2, 1, 18, buff+3+512*18);
		DiscIO(DIO_FLOPPY, DIO_WRITE, 3, 1, 18, buff+3+512*18*2);
	}
}


//---------------------------------------------------------
//	ÉÅÉCÉì
//---------------------------------------------------------

void main()
{
//	InstallFloppy();

	BYTE buff[27648];
	FILE *fp;
	fp=fopen("debug.txt","wb");

//	DiscIO(DIO_FLOPPY, DIO_READ, 0, 1, 1, buff);
	printf("%d\n", DiscIO(DIO_HARDDISK, DIO_READ, 0, 1, 1, buff));
	fwrite(buff, 512, 1, fp);

/*	DiscIO(DIO_FLOPPY, DIO_READ, 1, 1, 18, buff);
	DiscIO(DIO_FLOPPY, DIO_READ, 2, 1, 18, buff+512*18);
	DiscIO(DIO_FLOPPY, DIO_READ, 3, 1, 18, buff+512*18*2);
	fwrite(buff, 27648, 1, fp);
*/
	fclose(fp);

	return;
}

/*
// Important: All MS_DOS data structures must be packed on a one-byte boundary.
#pragma pack(1)
typedef struct _MID {
    WORD  midInfoLevel;
    DWORD midSerialNum;
    BYTE  midVolLabel[11];
    BYTE  midFileSysType[8];
} MID, *PMID;
#pragma pack()
*/

void main_()
{
	HANDLE hDevice;
	DIOC_REGISTERS reg;
//	MID mid;
	bool r;
	DWORD cb;
//	int nDrive = 3;  // Drive C:

	hDevice = CreateFile("\\\\.\\vwin32",
	    0, 0, NULL, 0, FILE_FLAG_DELETE_ON_CLOSE, NULL);

//	reg.reg_EAX = 0x440D;	    /* IOCTL for block devices		 */
//	reg.reg_EBX = nDrive;	    /* zero-based drive identifier	 */
//	reg.reg_ECX = 0x0866;	    /* Get Media ID command		 */
//	reg.reg_EDX = (DWORD) &mid; /* receives media identifier info.	 */
//	reg.reg_Flags = 0x0001;     /* assume error (carry flag is set)  */

/*	fResult = DeviceIoControl(hDevice, 
	    VWIN32_DIOC_DOS_IOCTL,
	    &reg, sizeof(reg), 
	    &reg, sizeof(reg), 
	    &cb, 0);

	if (!fResult || (reg.reg_Flags & 0x0001)) ;	// error if carry flag is set */

	char buff[1024];

	reg.eax = 0x201;		// BIOS read, 1 sector
	reg.ebx = (DWORD)buff;		// Offset of sector buffer
	reg.ecx = 1;			// Sector 1, Cylinder 0
//	reg.edx = 0x8000;		// Head 0, Drive #
	reg.edx = 0;			// Head 0, Drive #
	reg.flag = 0x0001;		// assume error (carry flag is set)

	r = DeviceIoControl(hDevice,
		VWIN32_DIOC_DOS_INT13, &reg, sizeof(reg), &reg, sizeof(reg), &cb, 0);

	printf("%s", buff);

	CloseHandle(hDevice);
}
