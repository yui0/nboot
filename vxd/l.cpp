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
	unsigned char *data;
	int sector;
	unsigned char blocks;
};
#pragma pack()


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
	char buff[512];

	hDevice = CreateFile("\\\\.\\DISC.VXD",
		0, 0, NULL, 0, FILE_FLAG_DELETE_ON_CLOSE, NULL);

	DISC_ACCESS d;
	d.drive=0x80;
	d.data=buff;
	d.sector=0;
	d.blocks=1;
//	r = DeviceIoControl(hDevice, 1, &d, sizeof(d), 0, 0, &cb, 0);
	r = DeviceIoControl(hDevice, 2, &d, sizeof(d), &d, sizeof(d), &cb, 0);
	printf("%x,%x,%x,%x\n",d.drive,d.data,d.sector,d.blocks);

	CloseHandle(hDevice);

	printf("%x,%x\n%s", r, cb, buff);

	FILE *fp;
	fp=fopen("debug.txt","wb");
	fwrite(buff, 512, 1, fp);
	fclose(fp);
}
