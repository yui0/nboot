; ---------------------------------------------------------
;	Hard Disk I/O
;
;		(C)2000	NAKADA
; ---------------------------------------------------------


%define DRVFLAG_DRIVEOK    0x0001       ;
%define DRVFLAG_CHSVALID   0x0002       ; used in driveinfo structure
%define DRVFLAG_REMOVABLE  0x0004       ; and bootrecord structure 
%define DRVFLAG_EXTOK      0x0008       ;
%define DRVFLAG_ISCDROM    0x0010       ; 0000,0000,0001,0000B

%define DRVFLAG_MASK       0x0015       ; 0000,0000,0001,0101B

%define INT13H_EXT_INSTCHECK 0x41
%define INT13H_EXT_READ      0x42
%define INT13H_EXT_WRITE     0x43
%define INT13H_EXT_GETINFO   0x48
%define INT13H_EXT_LOCK      0x45
%define INT13H_RESET         0X00
%define INT13H_READ          0X02
%define INT13H_WRITE         0X03
%define INT13H_GETINFO       0X08
%define INT13H_GETTYPE       0x15

%define EXT_SUBSET_FIXED     0x01
%define EXT_SUBSET_REMOVABLE 0x02
%define EXT_SUBSET_EDD       0x04

%define DRV_TYPE_FIXED       0x03

%define MIN_HD_ID            0X80
%define EXTPARAM_SIZE        0x42

%define MAX_CYLS             1023

; Structure for calling int 13h ext Read / Write functions
struc struc_int13ext
      .pack_size    : resb  1      ; ==16 size of struct Int13ExtData
      .reserved     : resb  1      ; ==0
      .blk_count    : resb  1      ; number of blocks to transfer <= 127
      .reserved1    : resb  1      ; ==0
      .buf_addr_off : resw  1      ; address of transfer buffer(segment:offset)
      .buf_addr_seg : resw  1
      .blk_num_low1 : resw  1      ; starting absolute block number
      .blk_num_low2 : resw  1
      .blk_num_high1: resw  1
      .blk_num_high2: resw  1
      .end_of_struc
endstruc

; structure for calling int 13h ext get drive parameters functions (0x48)
struc struc_extparam
      .pack_size         : resw  1
      .flags             : resw  1
      .cylinders         : resd  1
      .heads             : resd  1      ; <= 255
      .sectors           : resd  1      ; <= 63
      .total_sectors     : resd  2
      .bytes_per_sect    : resw  1
      .dpte_addr         : resd  1      ; device parameter table extension
      .dpi_key           : resw  1      ; 0xBEDD - Key, indicates presence
                                        ; of Device Path Information
      .dpi_length        : resb  1      ; Length of Device Path Information 
                                        ; including the key. = 36
      .reserved1         : resb  1
      .reserved2         : resw  1
      .host_bus_type     : resb  4      ; Host bus type, 4 bytes
                                        ; PCI    PCI Local Bus
                                        ; ISA    Legacy 16 bit fixed bus
      .interface_type    : resb  8      ; Interface type, 8 bytes
                                        ; ATA   ATA/ATAPI-4 compliant device using ATA commands
                                        ; ATAPI ATA/ATAPI-4 compliant device using ATAPI commands
                                        ; SCSI  SCSI compliant device
                                        ; USB   USB Mass Storage compliant device
                                        ; 1394  1394 Mass Storage device
                                        ; FIBRE Fibre Channel
      .interface_path    : resq  1
      .device_path       : resq  1
      .reserved3         : resb  1
      .checksum          : resb  1
      .end_of_struc
endstruc

; Device parameter table extension
%define SIZE_OF_DPTE struc_dpte.end_of_struc
struc struc_dpte
	.base_io	: resw 1
	.ctrl_io	: resw 1
	.flags		: resb 1
	.vender_spec	: resb 1
	.irq		: resb 1
	.blk_count	: resb 1
	.dma		: resb 1
	.pio		: resb 1
	.bios_spec	: resw 1
	.reserved	: resw 1
	.revision	: resb 1
	.checksum	: resb 1
	.end_of_struc
endstruc

;Return cdrom boot catalog command packet
%define SIZE_OF_CDBC_CMD struc_cdbc_cmd.end_of_struc
struc struc_cdbc_cmd
	.pack_size	: resb 1
	.sector_count	: resb 1
	.buf_addr_off	: resw 1
	.buf_addr_seg	: resw 1
	.begnning_sect	: resw 1
	.end_of_struc
endstruc

;CD Emulation Specification Packet
%define SIZE_OF_CDEMU_SPEC struc_cdemu_spec.end_of_struc
struc struc_cdemu_spec
	.pack_size	: resb 1
	.media_type	: resb 1
	.emu_drvid	: resb 1
	.controller_id	: resb 1
	.image_lba	: resd 1
	.device_spec	: resw 1
	.user_bufseg	: resw 1
	.load_seg	: resw 1
	.sect_count	: resw 1
	.cylinders	: resb 1
	.sectors	: resb 1
	.heads		: resb 1
	.end_of_struc
endstruc


;Structure of boot catalog entry
%define SIZE_OF_BOOT_CATALOG struc_boot_catalog.end_of_struc
struc struc_boot_catalog
	.indicator	: resb 1
	.media_type	: resb 1
	.load_seg	: resw 1
	.sys_type	: resb 1
	.reserved	: resb 1
	.sect_count	: resw 1
	.load_rba	: resd 1
	.reserved1	: resb 20
	.end_of_struc
endstruc


; structure for record drive informations
struc struc_driveinfo
      .id                : resb  1
      .flags             : resb  1
      .cylinders         : resw  1
      .heads             : resw  1
      .sectors           : resw  1
      .sector_size       : resw  1
      .end_of_struc
endstruc



%define SIZE_OF_DRIVEINFO (struc_driveinfo.end_of_struc)
%define SIZE_OF_EXTPARAM (struc_extparam.end_of_struc)
%define SIZE_OF_INT13EXT (struc_int13ext.end_of_struc)
