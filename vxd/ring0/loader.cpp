//---------------------------------------------------------
//	Vxd Loader
//
//		(C)2002 NAKADA
//---------------------------------------------------------

#include <stdio.h>
#include <windows.h>


struct DIOC_REGISTERS {
	DWORD ebx;
	DWORD edx;
	DWORD ecx;
	DWORD eax;
	DWORD edi;
	DWORD esi;
	DWORD flag;
};

#pragma pack(1)
struct DISC_ACCESS {
	unsigned char drive;
	unsigned char *buff;
	int sector;
	int n;
};
#pragma pack()
	char buff[512];


//---------------------------------------------------------
//	ディスク制御
//---------------------------------------------------------

//bool DiscIO(int d, int f, int c, int s, int n, unsigned char *buff)
void main()
{
	HANDLE hDevice;
	DIOC_REGISTERS reg;
	DWORD cb;
	int r;

	hDevice = CreateFile("\\\\.\\DISC.VXD",
		0, 0, NULL, 0, FILE_FLAG_DELETE_ON_CLOSE, NULL);

//	reg.esi = (DWORD)buff;
//	r = DeviceIoControl(hDevice,
//		1, &reg, sizeof(reg), &reg, sizeof(reg), &cb, 0);

	DISC_ACCESS d;
	d.drive=0;
	d.buff=buff;
	d.sector=0;
	d.n=1;
//	r = DeviceIoControl(hDevice, 1, &d, sizeof(d), 0, 0, &cb, 0);
	r = DeviceIoControl(hDevice, 1, &d, sizeof(d), &d, sizeof(d), &cb, 0);
	printf("%x,%x,%x,%x\n",d.drive,d.buff,d.sector,d.n);

//	asm {
//		mov ax,4300h						//;test for softice
//		int 68h								//;jesli AX=F386h - si found
//	}

	CloseHandle(hDevice);

	printf("%x,%x\n%s", r, cb, buff);

	FILE *fp;
	fp=fopen("debug.txt","wb");
	fwrite(buff, 512, 1, fp);
	fclose(fp);
}
