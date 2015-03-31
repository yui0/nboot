//---------------------------------------------------------
//	ƒfƒBƒXƒN§Œä
//
//		i‚bj‚Q‚O‚O‚P@‚m‚`‚j‚`‚c‚`
//---------------------------------------------------------

#define DIO_FLOPPY	0
#define DIO_HARDDISK	0x80
#define DIO_READ	0x200
#define DIO_WRITE	0x300


//---------------------------------------------------------
//	ŠÖ”’è‹`
//---------------------------------------------------------

#ifdef __cplusplus
extern "C" {		/* Assume C declarations for C++ */
#endif

int far PASCAL _export ResetDisk(int d);
int far PASCAL _export DiscAccess(int d, int f, int c, int h, int s, int n, unsigned char *buff, int size);

#ifdef __cplusplus
}
#endif
