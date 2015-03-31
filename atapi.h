; ---------------------------------------------------------
;	ATA LOW LEVEL I/O DRIVER
;
;		(C)2000	NAKADA
; ---------------------------------------------------------

; This code is based on the ATA-2, ATA-3 and ATA-4 standards and
; on interviews with various ATA controller and drive designers.
;
; This code has been run on many ATA (IDE) drives and
; MFM/RLL controllers.  This code may be a little
; more picky about the status it sees at various times.  A real
; BIOS probably would not check the status as carefully.

%ifndef _ATAIO_H
%define _ATAIO_H
%define ATA_DRIVER_VERSION "13J"

; Global defines -- ATA register and register bits.
;
; command block & control block regs
;
; these are the offsets into pio_reg_addrs[]

%define CB_DATA  0   ; data reg         in/out pio_base_addr1+0
%define CB_ERR   1   ; error            in     pio_base_addr1+1
%define CB_FR    1   ; feature reg         out pio_base_addr1+1
%define CB_SC    2   ; sector count     in/out pio_base_addr1+2
%define CB_SN    3   ; sector number    in/out pio_base_addr1+3
%define CB_CL    4   ; cylinder low     in/out pio_base_addr1+4
%define CB_CH    5   ; cylinder high    in/out pio_base_addr1+5
%define CB_DH    6   ; device head      in/out pio_base_addr1+6
%define CB_STAT  7   ; primary status   in     pio_base_addr1+7
%define CB_CMD   7   ; command             out pio_base_addr1+7
%define CB_ASTAT 8   ; alternate status in     pio_base_addr2+6
%define CB_DC    8   ; device control      out pio_base_addr2+6
%define CB_DA    9   ; device address   in     pio_base_addr2+7

; error reg (CB_ERR) bits

%define CB_ER_ICRC 0x80    ; ATA Ultra DMA bad CRC
%define CB_ER_BBK  0x80    ; ATA bad block
%define CB_ER_UNC  0x40    ; ATA uncorrected error
%define CB_ER_MC   0x20    ; ATA media change
%define CB_ER_IDNF 0x10    ; ATA id not found
%define CB_ER_MCR  0x08    ; ATA media change request
%define CB_ER_ABRT 0x04    ; ATA command aborted
%define CB_ER_NTK0 0x02    ; ATA track 0 not found
%define CB_ER_NDAM 0x01    ; ATA address mark not found

%define CB_ER_P_SNSKEY 0xf0   ; ATAPI sense key (mask)
%define CB_ER_P_MCR    0x08   ; ATAPI Media Change Request
%define CB_ER_P_ABRT   0x04   ; ATAPI command abort
%define CB_ER_P_EOM    0x02   ; ATAPI End of Media
%define CB_ER_P_ILI    0x01   ; ATAPI Illegal Length Indication

; ATAPI Interrupt Reason bits in the Sector Count reg (CB_SC)

%define CB_SC_P_TAG    0xf8   ; ATAPI tag (mask)
%define CB_SC_P_REL    0x04   ; ATAPI release
%define CB_SC_P_IO     0x02   ; ATAPI I/O
%define CB_SC_P_CD     0x01   ; ATAPI C/D

; bits 7-4 of the device/head (CB_DH) reg

%define CB_DH_DEV0 0xa0    ; select device 0
%define CB_DH_DEV1 0xb0    ; select device 1

; status reg (CB_STAT and CB_ASTAT) bits

%define CB_STAT_BSY  0x80  ; busy
%define CB_STAT_RDY  0x40  ; ready
%define CB_STAT_DF   0x20  ; device fault
%define CB_STAT_WFT  0x20  ; write fault (old name)
%define CB_STAT_SKC  0x10  ; seek complete
%define CB_STAT_SERV 0x10  ; service
%define CB_STAT_DRQ  0x08  ; data request
%define CB_STAT_CORR 0x04  ; corrected
%define CB_STAT_IDX  0x02  ; index
%define CB_STAT_ERR  0x01  ; error

; digital output reg (CB_DC) bits

%define CB_DC_HD15   0x08  ; bit should always be set to one
%define CB_DC_SRST   0x04  ; soft reset
%define CB_DC_NIEN   0x02  ; disable interrupts

;**************************************************************

; Most mandtory and optional ATA commands (from ATA-3),

%define CMD_CFA_ERASE_SECTORS            0xC0
%define CMD_CFA_REQUEST_EXT_ERR_CODE     0x03
%define CMD_CFA_TRANSLATE_SECTOR         0x87
%define CMD_CFA_WRITE_MULTIPLE_WO_ERASE  0xCD
%define CMD_CFA_WRITE_SECTORS_WO_ERASE   0x38
%define CMD_CHECK_POWER_MODE1            0xE5
%define CMD_CHECK_POWER_MODE2            0x98
%define CMD_DEVICE_RESET                 0x08
%define CMD_EXECUTE_DEVICE_DIAGNOSTIC    0x90
%define CMD_FLUSH_CACHE                  0xE7
%define CMD_FORMAT_TRACK                 0x50
%define CMD_IDENTIFY_DEVICE              0xEC
%define CMD_IDENTIFY_DEVICE_PACKET       0xA1
%define CMD_IDENTIFY_PACKET_DEVICE       0xA1
%define CMD_IDLE1                        0xE3
%define CMD_IDLE2                        0x97
%define CMD_IDLE_IMMEDIATE1              0xE1
%define CMD_IDLE_IMMEDIATE2              0x95
%define CMD_INITIALIZE_DRIVE_PARAMETERS  0x91
%define CMD_INITIALIZE_DEVICE_PARAMETERS 0x91
%define CMD_NOP                          0x00
%define CMD_PACKET                       0xA0
%define CMD_READ_BUFFER                  0xE4
%define CMD_READ_DMA                     0xC8
%define CMD_READ_DMA_QUEUED              0xC7
%define CMD_READ_MULTIPLE                0xC4
%define CMD_READ_SECTORS                 0x20
%define CMD_READ_VERIFY_SECTORS          0x40
%define CMD_RECALIBRATE                  0x10
%define CMD_SEEK                         0x70
%define CMD_SET_FEATURES                 0xEF
%define CMD_SET_MULTIPLE_MODE            0xC6
%define CMD_SLEEP1                       0xE6
%define CMD_SLEEP2                       0x99
%define CMD_STANDBY1                     0xE2
%define CMD_STANDBY2                     0x96
%define CMD_STANDBY_IMMEDIATE1           0xE0
%define CMD_STANDBY_IMMEDIATE2           0x94
%define CMD_WRITE_BUFFER                 0xE8
%define CMD_WRITE_DMA                    0xCA
%define CMD_WRITE_DMA_QUEUED             0xCC
%define CMD_WRITE_MULTIPLE               0xC5
%define CMD_WRITE_SECTORS                0x30
%define CMD_WRITE_VERIFY                 0x3C

%define REG_CONFIG_TYPE_NONE  0
%define REG_CONFIG_TYPE_UNKN  1
%define REG_CONFIG_TYPE_ATA   2
%define REG_CONFIG_TYPE_ATAPI 3

%define FAILBIT15 0x8000   ; extra interrupts detected
%define FAILBIT14 0x4000
%define FAILBIT13 0x2000
%define FAILBIT12 0x1000
%define FAILBIT11 0x0800
%define FAILBIT10 0x0400
%define FAILBIT9  0x0200
%define FAILBIT8  0x0100   ; SC( CD/IO bits) wrong at end of cmd
%define FAILBIT7  0x0080   ; byte count odd at data packet xfer time
%define FAILBIT6  0x0040   ; byte count wrong at data packet xfer time
%define FAILBIT5  0x0020   ; SC (IO bit) wrong at data packet xfer time
%define FAILBIT4  0x0010   ; SC (CD bit) wrong at data packet xfer time
%define FAILBIT3  0x0008   ; byte count wrong at cmd packet xfer time
%define FAILBIT2  0x0004   ; SC wrong at cmd packet xfer time
%define FAILBIT1  0x0002   ; got interrupt before cmd packet xfer
%define FAILBIT0  0x0001   ; slow setting BSY=1 or DRQ=1 after AO cmd

%define ATATYPE_MD 0x1     ; Direct-access device (e.g. magnetic disk)
%define ATATYPE_MT 0x1     ; Sequential-access device (e.g. magnetic tape)
%define ATATYPE_PT 0x2     ; Printer device
%define ATATYPE_PR 0x3     ; processor device
%define ATATYPE_CDR 0x4    ; Write-once device
%define ATATYPE_CD 0x5     ; CD-ROM device
%define ATATYPE_SC 0x6     ; Scanner device
%define ATATYPE_OM 0x7     ; Optical memory device
%define ATATYPE_MC 0x8     ; Medium changer device
%define ATATYPE_CT 0x9     ; Communications device

%define SIZE_OF_ATAPI_DEVINFO struc_atapi_devinfo.end_of_struc

struc struc_atapi_devinfo
	.dev_type	: resb 1
	.dev_flags	: resb 1
	.vender_id	: resb 8
	.product_id	: resb 16
	.product_rev	: resb 4
	.reserved	: resb 2
	.end_of_struc
endstruc


%macro	outbyte	0-2
%if %0>1
	mov	al,%2
%endif
%if %0>0
	mov	dx,[reg_addr+%1*2]
%endif
	out	dx,al
%endmacro

%macro	outword	0-2
%if %0>1
	mov	ax,%2
%endif
%if %0>0
	mov	dx,[reg_addr+%1*2]
%endif
	out	dx,ax
%endmacro


%macro outbytes 2-*
%assign LAST_AL 0x1000
%rep %0 / 2
    %ifnum %2	
	%if LAST_AL != %2
		outbyte %1, %2
		%assign LAST_AL %2
	%else
		outbyte %1
	%endif
    %else
	outbyte %1, %2
    %endif
    %rotate 2
%endrep
%endmacro

%macro inbyte 1-2
	mov	dx,[reg_addr+%1*2]
	in	al,dx
	%if %0 > 1
		mov	%2,al
	%endif
%endmacro

%macro inword 1-2
	mov	dx,[reg_addr+%1*2]
	in	ax,dx
	%if %0 > 1
		mov	%2,ax
	%endif
%endmacro


%endif
