; ---------------------------------------------------------
;	Hard Disk I/O
;
;		(C)2000	NAKADA
; ---------------------------------------------------------

; Interface to BIOS int 0x13 functions

%ifndef HAVE_HD_IO

;%ifndef MAIN
;%include "macros.h"
;%include "hd_io.h"
;%include "utils.asm"
;%endif
;=============================================================================
;itoa ---- convert integer to ascii string (the string is zero ending)
;input:
;      ax = the integer to be converted
;      cl = max length of the integer
;      es:di -> buffer
;output:
;      none
;=============================================================================
itoa:
        pusha
        xor ch, ch
        add di, cx
        mov byte [di], 0
        mov bx, 10
.loop_itoa:
        xor dx, dx
        dec di
        div bx
        add dl, '0'
        mov [di], dl
        dec cx
        or ax, ax
        jz .end_itoa
        or cx, cx
        jnz .loop_itoa
.end_itoa:
        or cx, cx
        jz .end
.loop_fillspace:
        dec di
        mov byte [di], ' '
        loop .loop_fillspace
.end:
        popa
        ret


%define HAVE_HD_IO

%define RETRY_TIMES     3

        bits 16
;==============================================================================
;check_int13ext ---- Check if the extension of int13h is presents for a driver.
;input:
;      dl = driver id
;output:
;      cf = 0, int13ext is presents
;          cl = interface support bitmap.
;      cf = 1, int13ext is not presents
;==============================================================================
check_int13ext:
        cmp byte [use_int13_ext], 0
        jz .not_present

        mov bx, 0x55aa
        mov ah, INT13H_EXT_INSTCHECK
        int 0x13                        ; Check if int13h extension is presents
        jc .not_present
        cmp bx, 0xaa55
        jne .not_present
        test cl, EXT_SUBSET_FIXED | EXT_SUBSET_EDD
					; Check if this drive supports extended
        jz .not_present                 ; read/write
        clc
        ret
.not_present:
        stc
        ret

;==============================================================================
;get_drive_info ---- Get drive informations
;input:
;      dl = drive id
;      es:di -> buffer for drive info (struc struc_driveinfo)
;output:
;      cf = 0 ok
;      cf = 1 drive is not presents
;==============================================================================
get_drive_info:
        pusha
        
        mov [di + struc_driveinfo.id], dl
        mov byte [di + struc_driveinfo.flags], 0 ; clear the flags

        push dx
        call check_int13ext
        pop dx
        jnc .ext_present

        mov ah, INT13H_GETTYPE
        push dx
        int 0x13
        mov [disk_errno], ah
        pop dx
        jc .drive_not_present
        or ah, ah                           ; ah = drive type, = 0 no such drive
        jz .drive_not_present
        
        push es
        push di
        push ax                             ; save drive type
        mov ah, INT13H_GETINFO
        int 0x13
        mov [disk_errno], ah
        pop ax
        pop di
        pop es
        jc .drive_not_present

        push ax                             ; drive type.
        push bx                             ; floppy type.
        
        movzx ax, cl                             ;
        and al, 0x3f                             ; get sectors per track
        mov [di + struc_driveinfo.sectors], ax   ;

        xchg ah, ch                              ;
        shl cx, 2                                ;
        mov cl, ah                               ; get cylinders
        inc cx                                   ;
        mov [di + struc_driveinfo.cylinders], cx ;

	movzx ax, dh                             ; get heads
        inc ax                                   ;
        mov [di + struc_driveinfo.heads], ax     ;

        pop bx
        pop ax

        mov dh, DRVFLAG_DRIVEOK
        cmp ah, DRV_TYPE_FIXED                  ; fixed drive.
        je .fixed_drive
        or dh, DRVFLAG_REMOVABLE
        
.fixed_drive:
        or dh, DRVFLAG_CHSVALID
        mov [di + struc_driveinfo.flags], dh
	mov word [di + struc_driveinfo.sector_size], 0x200
        jmp .endok

.drive_not_present:
        popa
        stc
        ret

.ext_present:
;Int13h ext is presents, use int13h ext to get drive info
        mov ah, INT13H_EXT_GETINFO
        lea si, [tmp_extparam]
        mov word [si + struc_extparam.pack_size], SIZE_OF_EXTPARAM
        push dx
        int 0x13
        mov [disk_errno], ah
        pop dx
        jc .drive_not_present
        
        mov ax, [si + struc_extparam.cylinders]
        mov [di + struc_driveinfo.cylinders], ax
        mov ax, [si + struc_extparam.heads]
        mov [di + struc_driveinfo.heads], ax
        mov ax, [si + struc_extparam.sectors]
        mov [di + struc_driveinfo.sectors], ax
	mov ax, [si + struc_extparam.bytes_per_sect]
	mov [di + struc_driveinfo.sector_size], ax

        mov al, [si + struc_extparam.flags]              ;
        and al, (DRVFLAG_CHSVALID + DRVFLAG_REMOVABLE)   ; fill in the
        or al, DRVFLAG_EXTOK + DRVFLAG_DRIVEOK           ; struc_driveinfo.flags

%ifndef DISABLE_CDBOOT
	cmp byte [si + struc_extparam.pack_size], SIZE_OF_EXTPARAM
	jb .no_edd30
	cmp word [si + struc_extparam.dpi_key], 0xBEDD
	jne .no_edd30
	cmp byte [si + struc_extparam.dpi_length], 36
	jne .no_edd30
	cmp dword [si + struc_extparam.interface_type], 'ATAP'
	jne .not_cdrom
	cmp byte [si + struc_extparam.interface_type+4], 'I'
	jne .not_cdrom
	or al, DRVFLAG_ISCDROM

.no_edd30:
	cmp byte [si + struc_extparam.pack_size], 30
	jb .not_cdrom
	mov bx, [si + struc_extparam.dpte_addr+2]
	cmp bx, 0xffff
	je .not_cdrom
	mov si, [si + struc_extparam.dpte_addr]
	cmp si, 0xffff
	je .not_cdrom
	push ds
	mov ds, bx
	test byte [ds:si + struc_dpte.bios_spec], 0x40  ; check if is atapi dev
	pop ds
	jz .not_cdrom
	or al, DRVFLAG_ISCDROM

.not_cdrom:
%endif
        mov [di + struc_driveinfo.flags], al

.endok:
        popa
        clc
        ret
        
;==============================================================================
;get_drive_flags ---- get drive flags
;input:
;      dl = drive id
;output:
;      cf = 0 ok, al = drive flags
;      cf = 1 error, al = 0
;==============================================================================
get_drive_flags:
        push di
        lea di, [tmp_driveinfo]
        call get_drive_info
        mov al, [di + struc_driveinfo.flags]
        pop di
        ret

;;;;;;;;;;;;;;;;;;; perhaps these will be used in future ;;;;;;;;;;;;;;;;;;;
%if 0
;==============================================================================
;init_drives_info ---- get all drives info
;input:
;      es:di -> buffer to store the info of each drives
;               should have enought space to store 255 drives' info
;output:
;      dh = the number of drives (include the floppy)
;==============================================================================
init_drives_info:
        push es
        push di
        mov cx, 256
        xor dx, dx
.loop_query:
        push cx
        push dx
        call get_drive_info
        pop dx
        pop cx
        jc .bad_drive
        inc dh
        
.bad_drive:
        add di, SIZE_OF_DRIVEINFO
        inc dl
        loop .loop_query
        
        pop di
        pop es
        ret

%endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;==============================================================================
;lba2chs ---- lba to chs translation
;input:
;      ebx = lba address 
;      ds : si -> drive info struc
;output:
;      cf = 0 success
;            ch = low eight bits of cylinder number
;            cl = sector number 1-63 (bits 0-5)
;                 high two bits of cylinder (bits 6-7, hard disk only)
;            dh = head number
;      cf = 1 error ( cylinder exceed )
;==============================================================================
lba2chs:
        push ax
	mov eax, ebx

        movzx ecx, word [si + struc_driveinfo.sectors] ; calculate sector:

        or cx, cx                              ; sectors per track cannot equal
        jz .convert_failed                     ; to zero.
        
        xor edx, edx
        div ecx                                ; sector =  lba % sects_per_track + 1
        inc dx                                 ; lba1 = lba / sects_per_track
        mov cx, dx                             ;

                                               ; calculate head and cylinder:
                                               ; head = lba1 % num_of_heads
        movzx ebx, word [si + struc_driveinfo.heads] 
                                               ; cylinder = lba1 / num_of_heads

        or bl, bl                              ; heads cannot equal to zero.
        jz .convert_failed                     ;
        
        xor edx, edx
        div ebx                                ;
        
        mov dh, dl                             ; head = dl <= 255
        cmp ax, MAX_CYLS                       ; cylinder = ax
        ja .convert_failed                     ; cylinder is too big.

        xchg al, ah                            ;
        shl al, 6                              ; fill cylinder and sector into
        or cx, ax                              ; cx
        pop ax
        clc
        ret
        
.convert_failed:
        mov byte [disk_errno], 0xFE             ; invalid LBA address
        pop ax
        stc
        ret
        
;==============================================================================
;disk_access_low ---- read / write sectors on disk
;input:
;      ah      = function id, ( 02 = read, 03 = write )
;      al      = number of sectors to be accessed
;      ebx     = lba address
;      ds : si -> drive info struc
;      es : di -> buffer to get/store data
;output:
;      cf = 0 success
;      cf = 1 error
;==============================================================================
disk_access_low:
        pusha
        
        mov dh, [si + struc_driveinfo.flags]
        test dh, DRVFLAG_DRIVEOK ; check if drive ok
        jz .access_error
        
        test dh, DRVFLAG_EXTOK
        jz .no_ext
        
;int13h extension is present, use it
        
        mov dl, [si + struc_driveinfo.id]    ; get drive id
        add ah, 0x40                         ; ext read func id = 0x42
        lea si, [tmp_int13ext]
        xor ecx, ecx
        mov byte [si + struc_int13ext.pack_size], 0x10 ;
        mov [si + struc_int13ext.blk_num_high1], ecx ; clear and set
        mov [si + struc_int13ext.reserved], cl       ; some stuff.
        mov [si + struc_int13ext.reserved1], cl      ;

        mov [si + struc_int13ext.buf_addr_off], di
        mov [si + struc_int13ext.buf_addr_seg], es
        mov [si + struc_int13ext.blk_num_low1], ebx

.retry_ext_read:
        mov [si + struc_int13ext.blk_count], al
        
        push ax
        push dx
        push si
        int 0x13
        mov [disk_errno], ah
        pop si
        pop dx
        pop ax
        jnc .access_ok
        call reset_drive
        inc dh
        cmp dh, RETRY_TIMES                 ; retry 3 times
        jb .retry_ext_read
        jmp short .access_error

;int13h extension is not ok, use old int13h
.no_ext:
        call lba2chs
        jc .access_error
        mov dl, [si + struc_driveinfo.id]   ; dl = drive id
        mov bx, di                          ; es : bx -> buffer
        xor di, di
        
.retry_read:
        push ax
        push dx
        push bx
        int 0x13
        mov [disk_errno], ah
        pop bx
        pop dx
        pop ax
        jnc .access_ok
        call reset_drive
        inc di
        cmp di, RETRY_TIMES                 ; retry 3 times
        jb .retry_read

.access_error:
        stc
.access_ok:
        popa
        ret

;==============================================================================
;disk_access ---- read / write sectors on disk
;input:
;      ah = function id, ( 02 = read, 03 = write )
;      al = number of sectors to be accessed
;      ebx = lba address
;      dl = drive id
;      es : di -> buffer
;output:
;      cf = 0 success
;      cf = 1 error
;==============================================================================
disk_access:
        push si
        push di
	push ebx

        push es
        push ds                         ; ds -> es, tmp_driveinfo is in ds segment.
        pop es
        
        lea di, [tmp_driveinfo]
        call get_drive_info
        
        pop es
	pop ebx
        pop di
        
        jc .access_error
        lea si, [tmp_driveinfo]
        call disk_access_low
.access_error:
        pop si
        ret


%if 0
;==============================================================================
;get_cdrom_devinfo ---- get the CDROM's device info, e.g. io port
;input: dl = drive id
;output: cf = 0 success, ax = base io port, bx = ctrl port,
;        cx = device specification
;==============================================================================
get_cdrom_devinfo:
	push si
	call get_drive_flags
	test al, DRVFLAG_ISCDROM
	jz .not_cdrom

	mov si, tmp_extparam
	mov word [si], SIZE_OF_EXTPARAM
	mov ah, 0x48
	int 0x13

	jc .not_cdrom
	cmp byte [si], 30
	jb .not_cdrom
	mov ax, [si + struc_extparam.dpte_addr + 2]
	cmp ax, 0xffff
	je .not_cdrom

	push es
	push di
	mov es, ax
	mov di, [si + struc_extparam.dpte_addr]
	mov ax, [es:di + struc_dpte.base_io]
	mov bx, [es:di + struc_dpte.ctrl_io]
	pop di
	pop es

	cmp byte [si], SIZE_OF_EXTPARAM
	jb .not_cdrom
	mov cx, [si + struc_extparam.device_path]
	clc
	jmp short .ok

.not_cdrom:
	stc
.ok:
	pop si
	ret

%endif

%ifndef DISABLE_CDBOOT
;==============================================================================
;get_cdrom_boot_catalog ---- get the CDROM's boot catalog
;input:
;	dl = drive id
;	es:di -> buffer
;output:
;	cf =0 success
;	cf =1 error
;==============================================================================
get_cdrom_boot_catalog:
	pusha
	mov si, tmp_cdbc_cmd
	mov byte [si], SIZE_OF_CDBC_CMD
	mov byte [si+struc_cdbc_cmd.sector_count], 1
	mov [si+struc_cdbc_cmd.buf_addr_off], di
	mov [si+struc_cdbc_cmd.buf_addr_seg], es
	xor bx, bx
	mov [si+struc_cdbc_cmd.begnning_sect], bx
	mov ax, 0x4d00
	int 0x13
	mov [disk_errno], ah
	popa
	ret
%endif

;==============================================================================
;reset_drive ---- reset the drive
;input:
;      dl = drive id
;output:
;      cf = 0 success
;      cf = 1 error
;==============================================================================
reset_drive:
        pusha
        xor ax, ax
        int 0x13
        popa
        ret

;==============================================================================
;get_drvid_str ---- get driver id string
;input:
;	dl = drive id
;	es:di -> buffer
;output:
;	es:di -> point to the end of the string
;==============================================================================
get_drvid_str:
	pusha

%ifdef SHOW_DRV_NAME
	call get_drive_flags
	cmp dl, MIN_HD_ID
	jb .floppy
	test al, DRVFLAG_REMOVABLE
	jz .harddisk
	test al, DRVFLAG_ISCDROM
	jnz .cdrom
	mov ax, 'RD'
	jmp short .show_drv_name
.floppy:
	mov ax, 'FD'
	jmp short .show_drv_name
.harddisk:
	mov ax, 'HD'
	jmp short .show_drv_name
.cdrom:
	mov ax, 'CD'
.show_drv_name:
	stosb
	mov al, ah
	stosb

	movzx ax, dl
	and al, 0x0F
	mov cl, 1
	call htoa
%else
        movzx ax, dl                                ; fill drive id
        mov cl, 3                                   ;
        call itoa                                   ;
%endif
	popa
	add di, 3
	ret



use_int13_ext  db 1

;%ifndef MAIN
;%include "tempdata.asm"
;%endif

%endif	;End of HAVE_HD_IO
