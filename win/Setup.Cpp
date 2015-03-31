//---------------------------------------------------------
//	Installer for NBoot
//
//		(C)2001 NAKADA
//---------------------------------------------------------

#include <windows.h>
#include <stdio.h>
#include "discio.h"


//---------------------------------------------------------
//	Ç”ÇÎÇ¡Ç“Å[Ç©ÇÁãNìÆÇ≈Ç´ÇÈÇÊÇ§Ç…
//	- saves nboot in a floppy disk -
//---------------------------------------------------------

/*void InstallFloppy()
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
}*/


//---------------------------------------------------------
//	ÉÅÉCÉì
//---------------------------------------------------------

void main()
{
//	InstallFloppy();

	BYTE buff[27648];
	FILE *fp;
	fp=fopen("debug.txt","wb");

	DiscAccess(DIO_FLOPPY, DIO_READ, 0, 0, 1, 1, buff, 512);
//	printf("%d\n", DiscIO(DIO_HARDDISK, DIO_READ, 0, 1, 1, buff));
	fwrite(buff, 512, 1, fp);

/*	DiscIO(DIO_FLOPPY, DIO_READ, 1, 1, 18, buff);
	DiscIO(DIO_FLOPPY, DIO_READ, 2, 1, 18, buff+512*18);
	DiscIO(DIO_FLOPPY, DIO_READ, 3, 1, 18, buff+512*18*2);
	fwrite(buff, 27648, 1, fp);
*/
	fclose(fp);

	return;
}
