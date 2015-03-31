/*
* ---------------------------------------------------------------------
*	IOCTL: An Example for VWin32 DeviceIoControl() Programming.
* ---------------------------------------------------------------------
* Author: Takehiko Morita (morry)
*	e-mail:	morry@tech.millto.net
*	Web:	http://tech.millto.net/~morry/
*
* Description:
*	* This code demonstrates sector access programming 
*	  by using VWin32 DeviceIoControl() + DOS IOCTL.
*
* Note:
*	* Generally, you should lock the drive by IOCTL CL=4Bh
*	  (not described in this sample). See SDK document.
*
* Usage:	ioctl [options] drive: head sylinder sector
*	-s(size)	sector size (default: 512)
*	-n(num)		# of sectors to read (default: 1)
*	-o(file)	output file name (default: dump to stdout)
*	Ex.  ioctl  -n2  a:  0  0  0
*
* History:
*	97/01/05 v01.00.00	morry  Created by Takehiko Morita
*	98/11/28 v01.00.01	morry  Re-write to upload to my homepage.
*
*	Copyright (C) 1998 Takehiko Morita (morry)
* ---------------------------------------------------------------------
*/
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
* ---------------------------------------------------------------------
*	VWin32 DeviceIoControl() definitions
* ---------------------------------------------------------------------
*/
/* VWIN32 VxD name */
#define	VWIN32	"\\\\.\\VWIN32"

/* VWIN32 DeviceIoControl function code */
#define	VWIN32_DIOC_DOS_IOCTL	(1)
#define	VWIN32_DIOC_DOS_INT25	(2)
#define	VWIN32_DIOC_DOS_INT26	(3)
#define	VWIN32_DIOC_DOS_INT13	(4)
#define	VWIN32_DIOC_DOS_DRIVEINFO	(6)	/* OSR2 or later */

/* DIOC_REGISTERS: Used to request VWIN32 to issue DOS call. */
typedef struct DIOCRegs {
    DWORD   reg_EBX;   // EBX register
    DWORD   reg_EDX;   // EDX register
    DWORD   reg_ECX;   // ECX register
    DWORD   reg_EAX;   // EAX register
    DWORD   reg_EDI;   // EDI register
    DWORD   reg_ESI;   // ESI register
    DWORD   reg_Flags; // Flags register
} DIOC_REGISTERS;

#define	CARRY_BIT	(0x0001)

/*
* ---------------------------------------------------------------------
*	function prototypes
* ---------------------------------------------------------------------
*/
int  read_sector( HANDLE hvxd, char drive, UINT head, UINT cylinder,
					UINT sector, UINT num, void* pbuf );
int  display_device_parameter( HANDLE hvxd, char drive );
int  save( const char * fname, void * pbuf, size_t cbbuf );
void  dump( BYTE * pbuf, size_t cbbuf );

void  error_exit( void ) {
	fprintf(stderr,
		"ioctl [options]  drive:  head  cylinder  sector\n"
		"  -s(size)\tsector size\n"
		"  -n(num)\tsector num\n"
		"  -o(fname)\toutput file\n"
	);
	exit(ERROR_INVALID_PARAMETER);
}

/*
* ---------------------------------------------------------------------
*	main
* ---------------------------------------------------------------------
*/
int  main( int ac, char** av )
{
	char	drive;
	UINT	head, cylinder, sector;
	UINT	num = 1;
	UINT	sector_size = 512;
	char *	fname = NULL;
	void *	pbuf;
	HANDLE	hvxd;
	int	rc;

	/* parse command line */
	for ( av++; NULL != *av && '-' == **av; av++ ) {
		char *	pch = *av + 2;
		switch ( pch[-1] ) {
		case  'n':		/* # of sectors to read */
			sscanf(pch, "%i", &num);
			break;
		case  's':		/* sector size */
			sscanf(pch, "%i", &sector_size);
			break;
		case  'o':		/* output file name */
			fname = pch;
			break;
		}
	}
	if ( NULL == *av || ! isalpha( drive = **av ) )	error_exit();
	++av;
	if ( NULL == *av || 1 != sscanf(*av, "%i", &head) )	error_exit();
	++av;
	if ( NULL == *av || 1 != sscanf(*av, "%i", &cylinder) )	error_exit();
	++av;
	if ( NULL == *av || 1 != sscanf(*av, "%i", &sector) )	error_exit();

	/* open vwin32 vxd */
	hvxd = CreateFile(VWIN32, 0, 0, NULL, 0,
			FILE_FLAG_DELETE_ON_CLOSE, NULL);
	if ( INVALID_HANDLE_VALUE == hvxd ) {
		rc = GetLastError();
		fprintf(stderr, "failed to open vxd (%d)", rc);
		return  rc;
	}

	/* display drive information (another DeviceIoControl example) */
	display_device_parameter(hvxd, drive);

	/* allocate sector buffer */
	sector_size *= num;
	pbuf = malloc(sector_size);
	memset(pbuf, 0, sector_size);

	/* read sector (demonstration main topic) */
	rc = read_sector(hvxd, drive, head, cylinder, sector, num, pbuf);
	if ( NO_ERROR != rc ) {
		fprintf(stderr, "failed to read sector (%d)", rc);
		return  rc;
	}

	/* dump */
	if ( NULL == fname ) {
		dump(pbuf, sector_size);
	}
	else {
		save(fname, pbuf, sector_size);
	}

	/* termination */
	free(pbuf);
	CloseHandle(hvxd);

	return  NO_ERROR;
} /* main */

/*
* ---------------------------------------------------------------------
*	read_sector: call DOS IOCTL via VWin32 DeviceIoControl
* ---------------------------------------------------------------------
*/
/* DOS IOCTL function code */
#define	IOCTL_BLOCK_DEVICE	(0x440D)	/* AX */
#define	IOCTL_CATEGORY_DISK	(0x0800)	/* CH */

/* parameter block for IOCTL_TRACK_READ (DS:DX) */
#define	IOCTL_TRACK_READ	(0x0061)	/* CL */
#pragma pack(1)
typedef struct  tagTRACKIO {
	BYTE	func;		/* 0 */
	WORD	head;		/* head no. */
	WORD	cylinder;	/* cylinder no. */
	WORD	sector;		/* sector no. */
	WORD	num;		/* # of sectors to read */
	void*	offset32;	/* transfer address (32bit offset) */
	WORD	sel16;		/* transfer address (16bit segment) */
} TRACKIO;
#pragma pack()

int  read_sector( HANDLE hvxd, char drive, UINT head, UINT cylinder,
		UINT sector, UINT num, void* pbuf )
{
	TRACKIO		param;
	DIOC_REGISTERS	regs;
	BOOL		b;
	DWORD		retsize;
	int		rc;

	/* make parameter block for IOCTL subfunction 61h */
	memset(&param, 0, sizeof param);
	param.head = head;
	param.cylinder = cylinder;
	param.sector = sector;
	param.num = num;
	param.offset32 = pbuf;
	_asm {
		mov	ax, ds
		mov	word ptr [param.sel16], ax
	}

	/* make register set to issue DOS IOCTL 61h */
	memset(&regs, 0, sizeof regs);
	regs.reg_EAX = IOCTL_BLOCK_DEVICE;		/* 440Dh */
	regs.reg_EBX = toupper(drive) - 'A' + 1;	/* BL=drive */
	regs.reg_ECX = IOCTL_CATEGORY_DISK + IOCTL_TRACK_READ;
	regs.reg_EDX = (DWORD)&param;		/* DS:DX=parameter block */

	/* call vwin32 */
	b = DeviceIoControl(hvxd, VWIN32_DIOC_DOS_IOCTL,
		&regs, sizeof regs, &regs, sizeof regs, &retsize, NULL);
	if ( ! b ) {
		/* DeviceIoControl() error */
		rc = GetLastError();
		return  rc;
	}

	/* check the return from DOS */
	if ( regs.reg_Flags & CARRY_BIT ) {
		/* DOS error */
		return  regs.reg_EAX;	/* DOS error code */
	}

	return  NO_ERROR;
} /* read_sector */

/*
* ---------------------------------------------------------------------
*	display_device_parameter: display device parameter
* ---------------------------------------------------------------------
*/
#pragma pack(1)
typedef struct  tagBPB {	/* bios parameter block */
	WORD	sector_size;		/* sector size in bytes */
	BYTE	sector_per_cluster;	/* # of sectors per cluster */
	WORD	reserved_sector;	/* # of reserved sectors */
	BYTE	fat_num;		/* # of FATs */
	WORD	root_entry;		/* # of root directory entries */
	WORD	total_sector;		/* # of total sectors */
	BYTE	media_id;		/* media ID */
	WORD	sector_per_fat;		/* # of sectors per FAT */
	WORD	sector_per_track;	/* # of sectors per track */
	WORD	head_num;		/* # of heads */
	DWORD	hidden_sector;		/* # of hidden sectors */
	DWORD	logical_sector;		/* # of logical sectors */
	BYTE	reserved[6];
} BPB;
typedef struct tagTRACK_LAYOUT {	/* track layout */
	WORD	number;		/* sector number (1 to be first) */
	WORD	size;		/* sector size */
} TRACK_LAYOUT;

/* parameter block for GET_DEVICE_PARAMETER (DS:DX) */
#define	IOCTL_GET_DEVICE_PARAMETER	(0x0060)	/* CL */
typedef struct  tagDEVICE_PARAMETER {
	BYTE	func;			/* special functions */
	BYTE	device_type;		/* device type */
	WORD	device_attr;		/* device attributes */
	WORD	cylinder_num;		/* number of cylinders */
	BYTE	media_type;		/* media type */
	BPB	bpb;			/* bios parameter block */
	/* --- following fields are used in SET-DEVICE-PARAMETER (40h) --- */
	WORD	sector_per_track;	/* number of sectors per track */
	TRACK_LAYOUT	tracks[];	/* tack layout */
} DEVICE_PARAMETER;
#pragma pack()

int  display_device_parameter( HANDLE hvxd, char drive )
{
	DEVICE_PARAMETER	param;
	DIOC_REGISTERS		regs;
	BOOL			b;
	DWORD			retsize;
	int			rc;

	memset(&param, 0, sizeof param);

	/* make register set to issue DOS IOCTL 60h */
	memset(&regs, 0, sizeof regs);
	regs.reg_EAX = IOCTL_BLOCK_DEVICE;		/* 440Dh */
	regs.reg_EBX = toupper(drive) - 'A' + 1;	/* BL=drive */
	regs.reg_ECX = IOCTL_CATEGORY_DISK + IOCTL_GET_DEVICE_PARAMETER;
	regs.reg_EDX = (DWORD)&param;		/* DS:DX=parameter block */

	/* call vwin32 */
	b = DeviceIoControl(hvxd, VWIN32_DIOC_DOS_IOCTL,
		&regs, sizeof regs, &regs, sizeof regs, &retsize, NULL);
	if ( ! b ) {
		/* DeviceIoControl() error */
		rc = GetLastError();
		return  rc;
	}

	/* check the return from DOS */
	if ( regs.reg_Flags & CARRY_BIT ) {
		/* DOS error */
		return  regs.reg_EAX;	/* DOS error code */
	}

	/* display result (drive information) */
	/* this is your homework :-) */

	return  NO_ERROR;
} /* display_device_parameter */

/*
* ---------------------------------------------------------------------
*	save
* ---------------------------------------------------------------------
*/
int  save( const char * fname, void * pbuf, size_t cbbuf )
{
	FILE *	fp = NULL;
	int	rc = NO_ERROR;

	if ( ! (fp = fopen(fname, "wb")) ) {
		/* ERROR */
		rc = ERROR_WRITE_FAULT;
		fprintf(stderr, "failed to open [%s]\n", fname);
		return  rc;
	}
	if ( cbbuf != fwrite(pbuf, 1, cbbuf, fp) ) {
		rc = ERROR_WRITE_FAULT;
		fprintf(stderr, "failed to write to [%s]\n", fname);
	}
	if ( EOF == fclose(fp) ) {
		rc = ERROR_WRITE_FAULT;
		fprintf(stderr, "failed to write to [%s]\n", fname);
	}
	return  rc;
} /* save */
/*
* ---------------------------------------------------------------------
*	dump
* ---------------------------------------------------------------------
*/
void  dump( BYTE * pbuf, size_t cbbuf )
{
	int	i;

	i = 16;
	while ( cbbuf-- ) {
		if ( ! i-- ) {
			i = 15;
			printf("\n");
		}
		printf("%02X ", *pbuf);
		pbuf++;
	}
} /* dump */

/* EOF */
