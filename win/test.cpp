#include <windows.h>
#include <stdio.h>
#include <mem.h>

void main()
{
	char *p, buff[512];

	p=(char*)(65536+1);
	memcpy(buff, p, 512);
	memset(p, 0, 512);
	memcpy(buff, p, 512);

/*	void *p;
	HANDLE a;
//	a=GlobalAlloc(0x8000, 512);
	a=GlobalAlloc(0, 512);
	p=GlobalLock(a);
	printf("handle %d address %x\n", a, p);
	GlobalUnlock(a);
	GlobalFree(a);*/

/*	char *p, buff[512];

	p=buff;

	asm {
		mov	ebx,p
		mov	edx,0		//; drive and head
		mov	ecx,1		//; sector and cilinder
		mov	eax,0x201	//; 1 sector bios_load_sector
		int	0x13
	}

	FILE *fp;
	fp=fopen("debug.txt","wb");
	fwrite(buff, 512, 1, fp);
	fclose(fp);

	return;*/
}
