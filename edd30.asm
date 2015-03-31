; ---------------------------------------------------------
;	CD-ROM Boot Extension
;
;		(C)2000	NAKADA
; ---------------------------------------------------------

%define jmpz  jz near
%define jmpnz jnz near
%define jmpe  je near
%define jmpne jne near
%define jmpc  jc near
%define jmpnc jnc near
%define jmpa  ja near
%define jmpna jna near
%define jmpb  jb near
%define jmpnb jnb near
;%include "macros.h"
%include "atapi.h"
%include "hd_io.h"
%include "tool.h"

;%ifdef DEBUG
;  %include "debug.h"
;%endif

%define DELAY_MSECS 55			; about 1 millisecond

;%define EDD_3_0
%define CDSECTOR_SIZE 0x800
%define sane_check
%define check_extra_fail

%define MIN_CDROM_ID 0xE0
%define REG_ATAPI_MAX_BYTES 32768

%ifndef DISABLE_CDBOOT
  %define EDD30_SIG1	'BM'
  %define EDD30_SIG2	'CB'
%else
  %define EDD30_SIG1	'BM'
  %define EDD30_SIG2	'SD'
%endif

	org 0
	section .text

	jmp edd30_main
	nop

copyright_msg	db 'CD-ROM Booting Module 1.00',0x0d,0x0a
		db 'Copyright (C) 2000-2002 Yuichiro Nakada',0x0d,0x0a,0x0a
		db 'Booting from CD-ROM...',0x0d,0x0a,0

edd30_main:
;input ah = 0, install my int 13h
;      ah = 1, uninstall my int 13h
;      ah = 2, initialize atapi
;      ah = 3, set drive map
;           bx , cx , drive map
;      ah = 4, set atapi io ports
;           bx , cx , atapi io ports

	pusha
	
	push es
	push ds
	push cs
	pop ds
	cld

	xor dx, dx
	mov es, dx

	xor al, al

	or ah, ah
	je .inst_int13h

	cmp ah, 1
	je .uninst_int13h

%ifndef DISABLE_CDBOOT
	cmp ah, 2
	je .init_atapi

	cmp ah, 4
	je .set_io_ports
%endif

	cmp ah, 3
	je .set_drvmap

.end:
	pop ds
	pop es
	popa
	retf

.inst_int13h:
	cmp word [es:0x13*4], new_int13h
	je .end

	push es
	push cs
	pop es
	mov di, start_of_emu_data
	mov cx, end_of_emu_data - start_of_emu_data
	rep stosb
	pop es

	cli
	mov ax, new_int13h
	xchg ax, [es:0x13*4]
	mov [int13h_old_off], ax
	mov ax, cs
	xchg ax, [es:0x13*4+2]
	mov [int13h_old_seg], ax
	sti
	jmp short .end

.uninst_int13h:
	cmp word [es:0x13*4], new_int13h
	jne .end

%ifndef DISABLE_CDBOOT
	call reset_atapi_cdroms
%endif

	cli
	mov ax, [int13h_old_off]
	mov [es:0x13*4], ax
	mov ax, [int13h_old_seg]
	mov [es:0x13*4+2], ax
	sti
	jmp short .end

.set_drvmap:
	mov [drive_map], bx
	mov [drive_map+2], cx
	xor ax, ax
	mov [drive_map+4], ax
	jmp short .end

%ifndef DISABLE_CDBOOT
.set_io_ports:
	mov [reg_base_addr_append], bx
	mov [reg_base_addr_append+2], cx
	xor al, al

.init_atapi:
	push cs
	pop es
	mov di, start_of_atapi_data
	mov cx, end_of_atapi_data - start_of_atapi_data
	rep stosb

	dec word [atapi_cur_dev]
%ifdef SLOW_ATAPI_DEVICE
	call init_timer
%endif
	call init_atapi_cdroms
	jmp short .end
%endif


; ---------------------------------------------------------
;	My New INT 13h handler starts here!
; ---------------------------------------------------------

new_int13h:
	cmp	ax,0x6666
	jne	.common_func
	cmp	bx,EDD30_SIG1
	jne	.common_func
	cmp	cx,EDD30_SIG2
	jne	.common_func

	xchg	bx,cx
	iret

.common_func:
	cld			; clear direction, only need one time
	push    ax		; save AX (contains function code in AH)
	push    bp		; need BP to mess with stack
	mov     bp,sp
	; Stack layout:
	;
	;   +8  INT flags
	;   +6  INT CS
	;   +4  INT IP
	;   +2  AX
	; BP+0 BP

%ifndef DISABLE_CDBOOT
	cmp	byte [cs:atapi_dev_count], 0
	jz	.no_cdrom

	cmp	dl, MIN_CDROM_ID	; 0xe0
	jb	.not_cdrom_drv

	push	dx
	sub	dl, MIN_CDROM_ID
	cmp	dl, [cs:atapi_dev_count]
	pop	dx
	jae	.not_cdrom_drv

	jmp	edd30_for_cdrom

.not_cdrom_drv:
	cmp byte [cs:emu_disk_type], 0
	je .not_emu_drv

	cmp dl,[cs:edd30_cdemu_spec + struc_cdemu_spec.emu_drvid]
	je cdemu_int13h

.not_emu_drv:

.no_cdrom:
%endif

        pushf                   ; push flags (to act like interrupt)
        push    si
        mov     si, drive_map

.mapfl: mov ax, [cs:si]         ; get next entry
        inc si
        inc si
        or      ax, ax          ; at end ?
        jz      .nomap          ; yes -> do not map
        cmp     dl, al          ; match ?
        jne     .mapfl          ; no -> continue
        mov     dl, ah          ; map drive
.nomap: pop     si              ; restore SI
        mov     [bp+8], ax      ; overwrite old flags (to remember mapping)
        mov     ax, [bp+2]      ; restore AX
        mov     bp, [bp]        ; restore BP
        db      0x9a            ; CALL FAR

int13h_old_off  dw   0
int13h_old_seg  dw   0

        push    bp              ; save BP again
        mov     bp,sp
        ; New stack layout:
        ;
        ;   +10 mapping (was flags)
        ;   +8  INT CS
        ;   +6  INT IP
        ;   +4  AX
        ;   +2  obsolete BP
        ; BP+0  BP
        xchg    ax, [bp+4]      ; save AX and get command
        pushf                   ; fix driver number, if necessary
        cmp     ah, 8           ; do not fix
        je      .done13
        mov     ax, [bp+10]     ; no mapping ?
        or      ax, ax
        jz      .done13
        mov     dl, al          ; fix mapping
.done13:mov     ax, [bp+4]      ; restore AX
        pop     word [bp+10]    ; restore flags
        pop     bp              ; get BP
        add     sp, 4           ; fix SP
        iret                    ; done


%ifndef DISABLE_CDBOOT
;=============================================================================
cdemu_int13h:
	; Stack layout:
        ;   +8  INT flags
        ;   +6  INT CS
        ;   +4  INT IP
        ;   +2  AX
        ; BP+0 BP
	;   -2 ax
	;   -4 cx
	;   -6 dx
	;   -8 bx
	;   -10 sp
	;   -12 bp
	;   -14 si
	;   -16 di
	;   -18 ds
	;   -20 es
	;   -24 edx
	pusha
	push ds
	push es
	push edx
	push cs
	pop ds

	push ax
	movzx ax, byte [cs:emu_cdrom_id]
	sub al, MIN_CDROM_ID 
	call select_atapi
	pop ax

	jnc .sel_atapi_ok
	mov ah, 0xaa
	jmp edd30_for_cdrom.fail_out

.sel_atapi_ok:
	mov cx, (cdemu_act_table.end_of_table - cdemu_act_table)
	xor di, di 

.loop_search:
	cmp [cdemu_act_table + di], ah
	je .found_act
	inc di
	loop .loop_search
	jmp edd30_for_cdrom.invalid_cmd

.found_act:
	mov cx, [bp-4]		; restore cx
	shl di, 1
	jmp [cdemu_jmp_table + di]

;=============================================================================
.stop_disk_emu:
	mov di, si
	push word [bp-18]
	pop es
	mov si, edd30_cdemu_spec
	mov cx, SIZE_OF_CDEMU_SPEC
	rep movsb
	or al, al
	jnz near edd30_for_cdrom.success_out

.terminate_emu:
	mov byte [emu_disk_type], al
	jmp edd30_for_cdrom.success_out

.emu_get_param:
	mov ah, [emu_disk_type]
	mov [bp-8], ah		; bl = drive type
	mov cx, [edd30_cdemu_spec + struc_cdemu_spec.cylinders]
	xchg ch, cl
	dec ch
	and cl, 0x3f
	mov dh, [edd30_cdemu_spec + struc_cdemu_spec.heads]
	dec dh
	mov dl, 0x02
	mov [bp-4], cx
	mov [bp-6], dx
	xor ax, ax
	jmp edd30_for_cdrom.success_out

.emu_get_type:
	mov ah, 0x02
	jmp edd30_for_cdrom.success_out_no_ah

.emu_read:
	or al, al
	jmpz .emu_read_fail

	cmp dh, [emu_head]
	jmpnb .emu_read_fail

	mov dl, cl		; dl = sector number
	and dl, 63
	or dl, dl
	jmpz .emu_read_fail
	dec dl

	cmp dl, [emu_sec]
	jmpnb .emu_read_fail

	movzx cx, ch
	cmp cx, [emu_cyl]
	jmpnb .emu_read_fail

	mov ax, cx		; ax = cylinder
	mul byte [emu_head]	; (cylinder*head
	mov cl, dh
	add ax, cx		; (cylinder*head + head)
	mov cl, dl		; cl = sector
	mul word [emu_sec]	; (cylinder*head + head)*sect_per_track
	add ax, cx		; (cyl*head + head)*sect_p_t + sector

	mov cl, [bp+2]		; number of sectors

	mov di, [emu_buf_off]
	xor edx, edx
	mov dword [emu_last_read], edx

.emu_loop_read:
	push ax
	push bx
	push cx
	push di

	movzx edx, ax

	shr edx, 2
	add edx, [edd30_cdemu_spec + struc_cdemu_spec.image_lba]

	cmp edx, [emu_last_read]
	je .emu_have_read
	mov [emu_last_read], edx

	mov cx, 1
	push es
	push word [emu_buf_seg]
	pop es
	call read_atapi
	pop es
	jc .emu_atapi_fail

.emu_have_read:
	and ax, byte 3			; sector = sector % 4
	shl ax, byte 9			; sector = sector * 512
	mov si, [emu_buf_off]
	add si, ax
	mov di, bx

	push ds
	push word [emu_buf_seg]
	pop ds
	mov cx, 512
	rep movsb
	pop ds

	pop di
	pop cx
	pop bx
	pop ax
	add bx, 512
	inc ax
	loop .emu_loop_read
	jmp edd30_for_cdrom.success_out

.emu_atapi_fail:
	pop di
	pop cx
	pop bx
	pop ax

.emu_read_fail:
	mov ah, 0x04
	jmp edd30_for_cdrom.fail_out


;=============================================================================

edd30_for_cdrom:
	; Stack layout:
        ;   +8  INT flags
        ;   +6  INT CS
        ;   +4  INT IP
        ;   +2  AX
        ; BP+0 BP
	;   -2 ax
	;   -4 cx
	;   -6 dx
	;   -8 bx
	;   -10 sp
	;   -12 bp
	;   -14 si
	;   -16 di
	;   -18 ds
	;   -20 es
	;   -24 edx
	pusha
	push	ds
	push	es
	push	edx
	push	cs
	pop	ds

	push	ax
	movzx	ax, dl
	sub	al, MIN_CDROM_ID 
	call	select_atapi
	pop	ax

	jnc	.sel_atapi_ok
	mov	ah, 0xaa
	jmp	.fail_out

.sel_atapi_ok:
	mov	cx, (edd30_act_table.end_of_table - edd30_act_table)
	xor	bx, bx

.loop_search:
	cmp	[edd30_act_table + bx], ah	; run command of ax
	je	.found_act
	inc	bx
	loop	.loop_search
	jmp	.invalid_cmd

.found_act:
	shl	bx, byte 1
	jmp	[edd30_jmp_table + bx]


; ---------------------------------------------------------
;	Boot from CD-ROM (0x4c00)
; ---------------------------------------------------------

.init_disk_emu_and_boot:
	call	edd30_init_disk_emu
	jc	near .fail_out

	mov	ax, [edd30_cdemu_spec + struc_cdemu_spec.load_seg]
	mov	es, ax
	xor	di, di
	mov	cx, [edd30_cdemu_spec + struc_cdemu_spec.sect_count]
	add	cx, 3		; convert virtual sect count
	shr	cx, 2		; to cdrom sect count
	mov	edx, [edd30_cdemu_spec + struc_cdemu_spec.image_lba]
	call	read_atapi
	jnc	.emu_boot_read_ok
	mov	ah, 0x04
	jmp	.fail_out

.emu_boot_read_ok:
	mov	si, copyright_msg
	call	draw_string_tty

	call	read_bios_time
	mov	dx, ax
	add	dx, byte 18*2

	sti
.loop_delay:
	call	read_bios_time
	cmp	dx, ax
	ja	.loop_delay
	cli

	mov	dl, [edd30_cdemu_spec + struc_cdemu_spec.emu_drvid]
	mov	[bp-6], dl		; set the driver id to dl
	mov	ax, es
	shl	ax, byte 4
	mov	[bp+4], ax		; set new ip
	xor	ax, ax
	mov	[bp+6], ax		; set new cs
	mov	[bp], ax		; clear bp 
	mov	ax, 0xaa55
	mov	[bp+2], ax
	jmp	short .success_out_no_ah

.init_disk_emu:
	call edd30_init_disk_emu
	jnc .success_out
	jmp short .fail_out

.return_boot_catalog:
	call edd30_return_boot_catalog
	jnc .success_out
	jmp short .fail_out

.get_drv_param:
	call edd30_get_cdrom_param
	jnc .success_out
	jmp short .invalid_cmd

.ext_write:
	mov ah, 0x03
	jmp short .fail_out

.ext_read:
	call edd30_read_cdrom
	jnc .success_out
	jmp short .fail_out

.install_check:
	mov word [bp-8], 0xaa55	; bx=0xaa55

%ifdef EDD_3_0
	mov ah, 0x30		; ax=0x30  edd-3.0
%else
	mov ah, 0x21		; ax=0x21  edd-2.1
%endif

	mov byte [bp-4], 0x07	; cx= 0x01 | 0x04, ext disk access and edd ok
	jmp short .success_out_no_ah

.get_disk_type:
	mov ah, 0x02
	jmp short .success_out_no_ah

.get_last_stat:
	mov ah, [int13_last_stat]
	jmp short .success_out_no_ah

.reset:
	call reg_reset
	mov ax, [atapi_cur_dev]
	call select_atapi_force

.success_out:
	xor ah,ah

.success_out_no_ah:
	and	byte [bp+8],0xfe
	jmp	short .done

.invalid_cmd:
	mov ah, 0x01

.fail_out:
	or byte [bp+8],1

.done:
	mov [int13_last_stat],ah
	mov [bp+3], ah
	pop edx
	pop es
	pop ds
	popa
	pop bp
	pop ax
	iret

;=============================================================================
edd30_read_cdrom:
;return cf=0 success, cf=1 fail, ah = fail code
	call test_atapi_ready
	jnc .atapi_ok
	mov ah, 0xaa
	jmp short .fail_out

.atapi_ok:
	push ds
	mov ax, [bp-18]		; restore ds
	mov ds, ax
	cmp byte [si], 16
	je short .packet_ok
	pop ds
	setne ah
.fail_out:
	stc
	ret

.packet_ok:
	mov bx, [si + struc_int13ext.buf_addr_seg]
	mov cx, [si + struc_int13ext.blk_count]
	mov edx, [si + struc_int13ext.blk_num_low1]
	mov di, [si + struc_int13ext.buf_addr_off]
	pop ds
	mov es, bx
	call read_atapi
	jc .read_fail
	ret

.read_fail:
	mov ah, 0x0c
	stc
	ret

;=============================================================================
edd30_get_cdrom_param:
;return cf =0 ok, cf =1 fail
	push ds
	mov ax, [bp-18]		; restore ds
	mov ds, ax
	mov ax, [si]
	cmp ax, byte 26
	jae .packet_ok
	pop ds
	setb ah
.fail_out:
	stc
	ret

.packet_ok:

%ifdef EDD_3_0
	cmp ax, byte 66
	jb .below_3_0
	mov ax, 66
	jmp .set_packet_size
.below_3_0:
%endif

	cmp ax, byte 30
	jb .below_2_0
	mov ax, 30
	jmp .set_packet_size
.below_2_0:
	mov ax, 26
.set_packet_size:
	mov [si], ax
	mov word [si+ struc_extparam.flags], 0x74  ; removable, lock, chg line
	mov word [si+ struc_extparam.bytes_per_sect], CDSECTOR_SIZE
	xor bx, bx
	dec bx
	mov [si+ struc_extparam.cylinders], bx
	mov [si+ struc_extparam.heads], bx
	mov [si+ struc_extparam.sectors], bx

%ifdef EDD_3_0
	cmp ax, byte 66
	jb .no_dpi
	mov word [si+ struc_extparam.dpi_key], 0xBEDD  ; dpi signature
	mov word [si+ struc_extparam.dpi_length], 0x24 ; dpi length
	mov dword [si+ struc_extparam.host_bus_type], 'ISA'
	mov dword [si+ struc_extparam.interface_type], 'ATAP'
	mov word [si+ struc_extparam.interface_type+4], 'I'

	mov ax, [cs:atapi_cur_dev]
	call get_atapi_base_io
	mov [si+ struc_extparam.device_path], cx
	mov [si+ struc_extparam.interface_path], bx

	push si
	add si, struc_extparam.dpi_key
	mov cx, struc_extparam.checksum - struc_extparam.dpi_key
	call edd30_checksum
	mov [si], ah
	pop si
.no_dpi:
%endif

	cmp ax, byte 30
	jb .no_dpte
        mov word [si+ struc_extparam.dpte_addr], atapi_dpte_buffer
        mov word [si+ struc_extparam.dpte_addr+2], cs

.no_dpte:
	pop ds
        mov ax, [atapi_cur_dev]
        call get_atapi_base_io
	mov si, atapi_dpte_buffer
        mov [si], bx
        mov [si+2], dx
        mov al, 0xe0
        shl cl, 4
        or al, cl
        mov [si+struc_dpte.flags], al  ; LBA enable, bit 4 = slave drv
	mov byte [si+struc_dpte.bios_spec], 0x60 ;ATAPI and removable
	mov cx, struc_dpte.checksum
	call edd30_checksum
	mov [si], ah
	clc
	ret

edd30_checksum:
	xor ah, ah

.loop_checksum:
	lodsb
	add ah, al
	loop .loop_checksum
	neg ah
	ret
	

; ---------------------------------------------------------
;	Boot Catalog (0x4d00)
;
;	return cf =0 ok, cf =1 fail
; ---------------------------------------------------------

edd30_return_boot_catalog:
	call	test_atapi_ready
	jnc	.atapi_ok
	mov	ah, 0xaa
	jmp	short .fail_out

.atapi_ok:
	push	ds
	mov	ax, [bp-18]		; restore ds
	mov	ds, ax
	cmp	byte [si], 8
	pop	ds
	jae	.packet_ok
	setb	ah

.fail_out:
	stc
	ret

.packet_ok:
	push	cs
	pop	es
	mov	di, edd30_disk_buffer
	xor	cx, cx
	inc	cx
	mov	dx, 0x11
	movzx	edx,dx
	call	read_atapi			; read boot record volume descriptor
	jc	.read_fail
	cmp	byte [di], 0
	jne	.non_bootable
	cmp	dword [di+7], 'EL T'
	jne	.non_bootable
	cmp	dword [di+11], 'ORIT'
	jne	.non_bootable

	mov	edx, [di+0x47]
	or	edx, edx
	jz	.non_bootable

	push	ds
	mov	ax, [bp-18]		; restore ds
	mov	ds, ax
	movzx	eax, word [si + struc_cdbc_cmd.begnning_sect] ; begnning sector
	add	edx, eax

	mov	cl, [si + struc_cdbc_cmd.sector_count]
	mov	bx, [si + struc_cdbc_cmd.buf_addr_seg]
	mov	di, [si + struc_cdbc_cmd.buf_addr_off]
	pop	ds
	mov	es, bx
	call	read_atapi
	jc	.read_fail
	ret

.non_bootable:
.read_fail:
	mov ah, 0x0c
	stc
	ret


; ---------------------------------------------------------
;	Init Disk Emu (0x0000)
;	input: si:cdemu_spec
; ---------------------------------------------------------

edd30_init_disk_emu:
	call test_atapi_ready
	jnc .atapi_ok
	mov ah, 0xaa
	jmp short .fail_out

.atapi_ok:
	push ds
	mov ax, [bp-18]		; restore ds
	mov ds, ax
	cmp byte [si], SIZE_OF_CDEMU_SPEC
	jae .packet_ok
	pop ds

.invalid_cmd:
	setb ah
.fail_out:
	stc
	ret

.packet_ok:
	push es
	push cs
	pop es
	mov di, edd30_cdemu_spec
	push di
	mov cx, SIZE_OF_CDEMU_SPEC
	rep movsb
	pop si
	pop es
	pop ds

	mov [emu_cdrom_id], dl
	mov al, [si + struc_cdemu_spec.emu_drvid]
	mov al, [si + struc_cdemu_spec.media_type]
	and al, 0x0f
	cmp al, 4
	jae .invalid_cmd
	shl al, 1
	mov [emu_disk_type], al
	mov ax, [si + struc_cdemu_spec.user_bufseg]
	or ax, ax
	jnz .has_user_buf
	mov [emu_buf_seg], cs
	mov ax, edd30_disk_buffer
	mov [emu_buf_off], ax
	jmp short .cont

.has_user_buf:
	mov [emu_buf_seg], ax
	xor ax, ax
	mov [emu_buf_off], ax
.cont:
	mov ax, [si + struc_cdemu_spec.load_seg]
	or ax, ax
	jnz .has_load_seg
	mov word [si + struc_cdemu_spec.load_seg], 0x07c0

.has_load_seg:
	mov ah, [si + struc_cdemu_spec.sectors]
	mov bl, ah
	shl ah, byte 6
	mov al, [si + struc_cdemu_spec.cylinders]
	mov [emu_cyl], ax
	and bx, byte 63
	mov [emu_sec], bx
	mov bl, [si + struc_cdemu_spec.heads]
	mov [emu_head], bx

%if 1
	cmp byte [emu_disk_type], 0
	jz .no_disk_swap
	cmp byte [si + struc_cdemu_spec.emu_drvid], 0
	jnz .no_disk_swap
	xor ax, ax
	mov [drive_map+2], ax
	inc al
	mov [drive_map], ax		; install the swap drive map

;increase the floppy number
	push es
	push word 0x0040
	pop es
	mov bx, 0x0010
	or byte [es:bx], 0x41
	pop es

%endif
.no_disk_swap:

	clc
	ret

;=============================================================================
;draw_string_tty ---- Draw a string ending by zero ( tty mode )
;input:
;      ds:si -> string
;output:
;      none
;=============================================================================
draw_string_tty:
.draw1:
        lodsb
        or al, al
        jz .end
        mov bx,7
        mov ah,0x0e
        int 0x10
        jmp short .draw1
.end:
        ret


cdemu_act_table:
	db 0x0, 0x1, 0x2, 0x4, 0x8, 0x15, 0x16, 0x4b
.end_of_table

cdemu_jmp_table:
	dw edd30_for_cdrom.success_out		; 0 reset 
	dw edd30_for_cdrom.get_last_stat	; 1 get last state
	dw cdemu_int13h.emu_read		; 2 read
	dw cdemu_int13h.emu_read		; 4 verify
	dw cdemu_int13h.emu_get_param		; 8 get param
	dw cdemu_int13h.emu_get_type		; 0x15 get type
	dw edd30_for_cdrom.success_out		; 0x16 detect disk change
	dw cdemu_int13h.stop_disk_emu		; 0x4b stop disk emu
.end_of_table


edd30_act_table:
	db 0x0, 0x1, 0x15, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4D, 0x4A, 0x4C, 0x4E, 0x4B
.end_of_table

edd30_jmp_table:
	dw edd30_for_cdrom.reset
	dw edd30_for_cdrom.get_last_stat
	dw edd30_for_cdrom.get_disk_type
	dw edd30_for_cdrom.install_check
	dw edd30_for_cdrom.ext_read
	dw edd30_for_cdrom.ext_write
	dw edd30_for_cdrom.ext_read
	dw edd30_for_cdrom.success_out		; lock / unlock
	dw edd30_for_cdrom.success_out		; eject
	dw edd30_for_cdrom.success_out		; extended seek
	dw edd30_for_cdrom.get_drv_param
	dw edd30_for_cdrom.success_out		; check media change
	dw edd30_for_cdrom.return_boot_catalog
	dw edd30_for_cdrom.init_disk_emu
	dw edd30_for_cdrom.init_disk_emu_and_boot
	dw edd30_for_cdrom.success_out		;set hardware configuration
	dw cdemu_int13h.stop_disk_emu		; 0x4b stop disk emu
.end_of_table


;=============================================================================
; IDE ATAPI driver
;=============================================================================

;*************************************
;timer relate functions
;*************************************
%ifdef SLOW_ATAPI_DEVICE
proc	init_timer
	call read_bios_time
	mov bx, ax
	xor ecx, ecx
	repeat
		call read_bios_time
		inc ecx
	until {cmp bx, ax}, ne
	mov eax, ecx
	xor edx, edx
	xor ecx, ecx
	mov cx, DELAY_MSECS
	div ecx
	mov [delay_repeat_num], eax
endp
%endif

proc	read_bios_time
;	return dx:ax as the long time
	save ds
	sub ax,ax
	mov ds,ax
	repeat
		mov eax,[0x46c]
	until {cmp eax,[0x46c]}, e
endp

proc	set_timeout
	save eax
	call read_bios_time
	add ax, 5*18	; 20 seconds
	 		; FIXME: Midnight overflow
	mov [time_out],ax
endp

proc	check_timeout
; you need to setup the timeout first
%define is_timeout a
%define cc_nottimeout na
	call read_bios_time
	cmp ax,[time_out]
%ifdef DEBUG
	if is_timeout
		debug_print "timeout accord!!!"
	endif
%endif
endp

proc	sub_xfer_delay
%ifdef SLOW_ATAPI_DEVICE
	save ecx
	pushf
	sti
	mov ecx, [delay_repeat_num]
	repeat
		call read_bios_time
		dec ecx
	until {or ecx, ecx}, z
	popf
%endif
endp

proc	sub_atapi_delay
%ifdef SLOW_ATAPI_DEVICE
; delay a few clicks
;	save ecx
	if {cmp byte [atapi_delay_flag],0},nz 
;		pushf
;		sti
;		mov ecx, [delay_repeat_num]
;		repeat
;			call read_bios_time
;			dec ecx
;		until {or ecx, ecx}, z
;		popf
	call sub_xfer_delay
	endif
%endif
endp

proc	delay400ns
	inbyte CB_ASTAT
	%rep 3
	in al,dx
	%endrep
endp

proc	reg_poll_busy
; need to setup the timeout first
;return ax=0	ok
;	ax = 1	timeout
	while {inbyte CB_STAT},{and al,CB_STAT_BSY},nz
		call check_timeout
		return if,is_timeout
	endwhile
endp

proc	__reg_select_dev
; ax = dev
; simpile version of the select dev
	save bx
	mov bx,ax
	outbyte CB_DH,[bx+cmd_select_dev]
	call delay400ns
endp
	
proc	reg_select_dev
; ax = dev
; set_timeout first
	save bx
	mov bx,ax
	if {cmp byte[reg_dev_info+bx], REG_CONFIG_TYPE_ATA},ae
		call reg_poll_busy
		return if,{or ax,ax},nz
		mov ax,bx
		call __reg_select_dev
%ifndef USE_ATA
		call reg_poll_busy
%else
		while
			inbyte CB_STAT
			if {cmp byte[reg_dev_info+bx],REG_CONFIG_TYPE_ATA},e
				and al, CB_STAT_BSY | CB_STAT_RDY | CB_STAT_SKC
				return if,{cmp al,CB_STAT_RDY|CB_STAT_SKC},e
			else
				return if,{test al,CB_STAT_BSY},z
			endif
		endwhile
%endif 
	else
		call __reg_select_dev
	endif
endp

proc	reg_packet,withlocal,dir,packet_seg,packet_off,packet_len
; input:
; return ax = 0 noerror, ah = error code al= error bit
;	 cx = len
	local status,1,reason,1,bcnt,2,pre_fail_bit7,1
	save all
%define NON_DATA 0x80
%define cmd_buff atapi_cmd_buffer
%define cmd_buff_len 12
%define cmd_DC	  CB_DC_HD15 
%define cmd_DC_ni CB_DC_HD15 | CB_DC_NIEN
	call set_timeout
	mov byte [.pre_fail_bit7],0
	

;	outbytes CB_DC,cmd_DC_ni,CB_FR,0,CB_SC,0,CB_SN,0,
	outbytes CB_DC,cmd_DC,CB_FR,0,CB_SC,0,CB_SN,0,
	outbytes CB_CL,[.packet_len],CB_CH,[.packet_len+1]
	outbyte CB_DH,[reg_cur_dev]
	outbyte CB_CMD,CMD_PACKET
	call delay400ns
	call sub_atapi_delay
	sub bx,bx
;	while {inbyte CB_ASTAT},{test al,CB_STAT_BSY|CB_STAT_ERR|CB_STAT_DRQ},z
	while {inbyte CB_ASTAT},{test al,CB_STAT_BSY},z
		if 
		orblock
			condiction {test al, CB_STAT_DRQ}, nz
			condiction {test al, CB_STAT_ERR}, nz
		endblock then
			break
		endif
		or bl,FAILBIT0
		break if, {call check_timeout},is_timeout
	endwhile
	
	while {inbyte CB_ASTAT},{test al, CB_STAT_BSY}, nz
;		inbyte CB_ASTAT
;		break if, {test al,CB_STAT_BSY},z
		if {call check_timeout},is_timeout
			mov word [.dir],-1
			mov bh, 51
			break
		endif
	endwhile
	
	goton .skip_out,if,{cmp bh,0},nz
	
	
	inbyte CB_STAT,[.status]
	inbyte CB_SC, [.reason]
	inbyte CB_CL, [.bcnt]
	inbyte CB_CH, [.bcnt+1]

	mov al,[.status]
	and al,CB_STAT_BSY | CB_STAT_DRQ
	if {cmp al,CB_STAT_DRQ},nz
		mov bh,52
%ifdef DEBUG
		print_stat [.status],"%b %s check BSY=0 DRQ=1 now\n",bx,STAT_BUF
%endif
		goto .skip_out
	endif

%ifdef sane_check	
	mov al,[.reason]
	if
	orblock
		condiction  {test al,CB_SC_P_TAG | CB_SC_P_REL | CB_SC_P_IO},nz
		condiction  {test al,CB_SC_P_CD},z
	endblock then
		or bl,FAILBIT2
%ifdef DEBUG
		print_stat [.status],"%b %s %b check reason\n",bx,STAT_BUF,[.reason]
%endif
	endif
	
	mov ax,[.bcnt]
	if {cmp ax,[.packet_len]},nz
		or bl,FAILBIT3
%ifdef DEBUG
		print_stat [.status],"%b %s 0x%x check packet_len\n",bx,STAT_BUF,[.bcnt]
%endif
	endif
%endif

%ifdef DEBUG
	debug_print "sending cmd buffer"
%endif
	mov si,cmd_buff
	mov cx,cmd_buff_len
	shr cx,1
	mov dx,[reg_addr+CB_DATA]
	cld
	rep outsw
	
	call delay400ns

	sub cx,cx
	while

%ifdef DEBUG
		debug_print "  data transfer ----------------------------\n"
%endif
		call sub_atapi_delay
	    
		while {inbyte CB_ASTAT},{test al,CB_STAT_BSY},nz
			call check_timeout
			if is_timeout
				mov bh,54
				goto .skip_out
			endif
		endwhile

%ifdef DEBUG
		print_stat al,"%b %s wait ASTAT BSY=0\n",bx,STAT_BUF
%endif

		; Data transfer loop
		; read the primary state register
		inbyte CB_STAT, [.status]
		inbyte CB_SC, [.reason]
		inbyte CB_CL, [.bcnt]
		inbyte CB_CH, [.bcnt+1]

%ifdef DEBUG
		print_stat [.status],"%b %s pre-data reason=%b len=%d\n",bx,STAT_BUF,[.reason],[.bcnt]

		print_stat [.status],"  stat "
		debug_print "  check the device said end of command"
%endif

		if {test byte[.status],CB_STAT_BSY | CB_STAT_DRQ},z
			or byte[.dir], NON_DATA
			goto .skip_out
		endif

%ifdef DEBUG
		debug_print "  device want transfer data BSY = 0 DRQ =1"
%endif

		if {mov al,[.status]},{and al,CB_STAT_BSY|CB_STAT_DRQ},{cmp al,CB_STAT_DRQ},nz
			mov bh,55
			goto .skip_out
		endif

%ifdef sane_check
%ifdef DEBUG
		print_stat al,"  stat "
		debug_print "  check: C/nD=0, IO=1 (read) or IO=0 (write)"
%endif
		if {test byte[.reason],CB_SC_P_TAG|CB_SC_P_REL|CB_SC_P_CD},nz
			or bl,FAILBIT4

%ifdef DEBUG
			print_stat al, "  FAIL:%b %s reason=%b C/nD=0, IO=1 (read) or IO=0 (write)\n",bx,STAT_BUF,[.reason],
%endif
		endif
		if {test byte[.reason],CB_SC_P_IO},nz
			if {cmp byte[.dir],0},nz
				or bl,FAILBIT5
%ifdef DEBUG
				print_stat al, "  FAIL:%b %s reason=%b dir=%d\n",bx,STAT_BUF,[.reason],[.dir]
%endif
			endif
		endif
%endif	
	    ; do the slow data transfer

%ifdef DEBUG
		debug_print "  do slow delay"
%endif

		if {cmp byte[reg_slow_xfer_flag],0},nz
			call sub_xfer_delay
		endif


%ifdef DEBUG
		debug_print "  check data len zero"
%endif

		mov ax,[.bcnt]
		if {or ax,ax},z
			mov bh,60
			or byte[.dir],NON_DATA
			goto .skip_out
		endif
   	    
%ifdef DEBUG
		debug_print "  check the buffer len" 
%endif

%ifdef sane_check 
		if {cmp ax,REG_ATAPI_MAX_BYTES},a
			or bl,FAILBIT6
		endif
%endif  
		mov dl,[.pre_fail_bit7]
		or bl,dl
		test al, 1
		setnz [.pre_fail_bit7]
    
	   
		mov dx,cx 
		add dx,ax 
		if {cmp dx,[.packet_len]},a
			mov bh,59
			or byte[.dir],NON_DATA
			goto .skip_out
		endif

		push dx
		mov cx,ax
		inc cx
		shr cx,1
		mov dx,[reg_addr+CB_DATA*2]
		cld
		if {cmp byte[.dir],0},nz
			push ds
			push word [.packet_seg]
			pop ds
			mov si,[.packet_off]
			rep outsw
			pop ds
		else
			push es
			push word [.packet_seg]
			pop es
			mov di,[.packet_off]
			rep insw
			pop es
		endif
		pop cx

		add [.packet_off],ax
		call delay400ns
	endwhile	
	
	if {test byte [.dir], NON_DATA},z
		call sub_atapi_delay
		if is_timeout
			mov bh,57
%ifdef DEBUG
			debug_print "  end of command, wait for BSY=0\n"
%endif

			goto .skip_out
		endif
	endif
	

	inbyte CB_STAT, ; [.status]
	if {test al,CB_STAT_BSY|CB_STAT_DRQ|CB_STAT_ERR},nz 
		mov bh,58
%ifdef DEBUG
		debug_print "Error: final check for stat al\n"
%endif
		goto .skip_out
	endif
	
	inbyte CB_SC, [.reason]
.skip_out:
%ifdef check_extra_fail
	mov al,[.reason]
	if
	orblock
		condiction {test al,CB_SC_P_TAG|CB_SC_P_REL},nz
		condiction {test al,CB_SC_P_IO },z
		condiction {test al,CB_SC_P_CD},z
	endblock then
		or bx, FAILBIT8
%ifdef DEBUG
		debug_print "FAIL:final check for protocol failures C/nD=1 IO=1\n"
%endif
	endif
%endif	
;	outbyte CB_DC,cmd_DC

%ifdef DEBUG
	debug_print " end of command, ec:failbit=%x, %d bytes data\n",bx,cx
%endif

	mov [__AX],bx
	mov [__CX],cx
endp

proc	reg_setup_base_addr
; input:  bx = base addr pointer
	save all
	cld
	mov di,reg_addr
	mov ax, [bx]
	forcx 8
		stosw
		inc ax
	endforcx
	mov ax, [bx+2]
	stosw
	inc ax
	stosw
endp

proc	reg_probe_dev_exist
; input ax=dev
; return ax = 1: exist
	call __reg_select_dev
	outbytes CB_SC,0x55,CB_SN,0xaa,CB_SC,0xaa,CB_SN,0x55,CB_SC,0x55
	outbyte CB_SN ,0xaa 
	inbyte CB_SC,ah
	inbyte CB_SN
	cmp ax, 0x55aa
	sete al
endp

proc	reg_probe_exist
; new fixed
	outbyte CB_DC, cmd_DC

	mov_ax 0
	call reg_probe_dev_exist
	mov [reg_dev_info],al
	mov ax,1
	call reg_probe_dev_exist
	mov [reg_dev_info+1],al
endp

proc	reg_check_dev_type
; input ax=dev
; call after a reset
	call __reg_select_dev
	inbyte CB_SC, ah
	inbyte CB_SN

	if {cmp ax, 0x0101}, ne
		mov ax, 1
		return
	endif

	inbyte CB_CL,ah
	inbyte CB_CH

	if {cmp ax, 0x14eb}, e
		mov ax, REG_CONFIG_TYPE_ATAPI
		return
	endif

	if {or al,al},z
	    inbyte CB_STAT
		if {or al, al}, nz
			mov ax, REG_CONFIG_TYPE_ATA
			return
		endif
	endif
	mov ax, REG_CONFIG_TYPE_UNKN
endp

proc	reg_reset
; call after reg_probe_exist
	save all

;	mov_ax 0
;	call __reg_select_dev

	mov al,cmd_DC
	or al,CB_DC_SRST
	outbyte CB_DC
	call delay400ns
	outbyte CB_DC, cmd_DC
	call delay400ns

	call set_timeout
	if {cmp byte [reg_dev_info],REG_CONFIG_TYPE_NONE},ne
		call sub_atapi_delay
		call reg_poll_busy
	endif
	if {cmp byte [reg_dev_info+1],REG_CONFIG_TYPE_NONE},ne
		call sub_atapi_delay
		while
			mov ax,1
			call __reg_select_dev
			call delay400ns
			inbyte CB_SC,ah
			inbyte CB_SN
			break if,{cmp ax,0x0101},e
			call check_timeout
			break if,is_timeout
		endwhile 
	endif
endp

proc	reg_probe
; return cx = number of atapi devices
	save ax, bx, si, di 
	cld
	mov word [atapi_dev_count], 0
	mov bx,reg_base_addr
	mov di,atapi_dev_base
	while {cmp word [bx], byte 0},nz
		call reg_setup_base_addr
		call reg_probe_exist
		call reg_reset
		xor si,si
		while {cmp si, byte 2}, b
			if {cmp byte[reg_dev_info+si],0},ne
				mov ax,si
				call reg_check_dev_type
				mov [reg_dev_info],al
				if {cmp al,REG_CONFIG_TYPE_ATAPI},e
					; Add it to the list
					inc word [atapi_dev_count]
					mov ax,bx
					stosw
					mov ax,si
					stosw
				endif
			endif
			inc si
		endwhile
		inc bx
		inc bx
	endwhile
	mov cx, [atapi_dev_count]
endp


select_atapi:
	cmp ax, [atapi_cur_dev]
	jne select_atapi_force
	clc
	ret

proc select_atapi_force
; input: ax = dev number
; return: cf =0 success, cf =1 failed
	save all
	push es
	push cs
	pop es

	cmp ax, [atapi_dev_count]
	jb .continue
	stc
	jmp short .end

.continue:
	mov [atapi_cur_dev], ax

	mov si, atapi_dev_base
	shl ax, 2
	add si, ax
	mov bx, [si]
	call reg_setup_base_addr
	mov bx,[si+2]
	mov ax,bx
	call set_timeout
	call reg_select_dev
	mov ah,byte [bx+cmd_select_dev]
	mov [reg_cur_dev],ah
.ok:
	clc
.end:
	pop es
endp

proc clear_atapi_buffer
	save all
	cld
	push es
	push cs
	pop es
	mov di, atapi_tmp_buffer
	mov cx, 128
	xor al, al
	stosb
	mov di, atapi_cmd_buffer
	mov cx, 16
	stosb
	pop es
endp

proc get_atapi_sense
; return: cf =0 success, al = sense key, bl = asc, bh = ascq
;	  cf =1 failed 
	call clear_atapi_buffer
	mov byte [atapi_cmd_buffer], 0x03
	mov byte [atapi_cmd_buffer+4], 32
	invoke reg_packet,byte,0,cs, atapi_tmp_buffer, 128

	or ax, ax
	jnz .fail

	mov al, [atapi_tmp_buffer]
	and al, 0x7f
	if
	orblock
		condiction {cmp al, 0x70}, e
		condiction {cmp al, 0x71}, e
	endblock then
		mov al, [atapi_tmp_buffer+2]            ; get sense key
		and al, 0x0f
		xor bx, bx
		if {cmp byte [atapi_tmp_buffer+7], 0x06}, ae
			mov bx, [atapi_tmp_buffer+12]
		endif
		clc
		return
	endif
.fail:
	stc
endp

proc test_atapi_ready
; return: cf =0 ready, cf =1 not ready
	save all
	mov cx, 2

.loop_try:
	push cx
	call clear_atapi_buffer
	invoke reg_packet,byte,0,cs, atapi_tmp_buffer, 128
	or ax, ax
	jnz .try_again
	call get_atapi_sense
	jc .try_again
	or al, al
	jnz .try_again
	pop cx
	clc
	jmp short .end
.try_again:
	pop cx
	loop .loop_try
	stc
.end:
endp

%if 1
proc inquiry_atapi
;input: es:di -> atapi_devinfo
;return: cf =0 success, al = device type, 
;        cf =1 fail
;	save si, di, cx
	call clear_atapi_buffer
	mov byte [atapi_cmd_buffer], 0x12
	mov byte [atapi_cmd_buffer+4], 128
	invoke reg_packet,byte,0,cs, atapi_tmp_buffer, 128
	or ax, ax
	jnz .fail

	mov al, [atapi_tmp_buffer]
	test al, 0xe0
	jnz .fail

	and al, 0x1f

%if 0
	mov [es:di + struc_atapi_devinfo.dev_type], al

	mov ah, [atapi_tmp_buffer+7]
	mov [es:di + struc_atapi_devinfo.dev_flags], ah

	add di, struc_atapi_devinfo.vender_id
	mov si, atapi_tmp_buffer + 8
	mov cx, 24
	cld
	rep movsb
%endif
	clc
;	jmp short .end
	return
.fail:
	stc
.end:
endp

proc check_atapi_cdrom
;return: cf =0 is cdrom, cf =1 not cdrom
	save all
;	push es
;	push cs
;	pop es
;	mov di, atapi_devinfo
	call inquiry_atapi
	jc .end
	clc
	cmp al, ATATYPE_CD
	je .end
	cmp al, ATATYPE_CDR
	je .end
	stc
.end:
;	pop es
endp
%endif


proc get_atapi_base_io
;input: ax = dev 
;return: bx = base io 1, dx = base io 2, cx = device number
	save si, ax
	shl ax, byte 2
	mov si, atapi_dev_base
	add si, ax
	mov bx, [cs:si]
	mov cx, [cs:si+2]
	mov dx, [cs:bx+2]
	mov bx, [cs:bx]
endp

proc read_atapi
;input: es:di -> buffer, cx = sector count, edx = lba address
;return: cf =0 success, cx = number of bytes actually read
	save all
	or cx, cx
	jz .no_data

.loop_read:
	push cx
	push edx

	call clear_atapi_buffer
	mov byte [atapi_cmd_buffer], 0x28

	bswap edx
	mov [atapi_cmd_buffer+2], edx
	mov byte [atapi_cmd_buffer+8], 1
	invoke reg_packet,byte, 0, es, di, REG_ATAPI_MAX_BYTES
	or ax, ax
	jnz .fail
	cmp cx, CDSECTOR_SIZE
	jne .fail
	pop edx
	inc edx
	add di, cx
	pop cx
	loop .loop_read

	clc
	jmp short .end

.fail:
	pop edx
	pop cx

.no_data:
	stc
.end:
endp

proc init_atapi_cdroms
; return: cf =0 success, cx =number of cdroms
;         cf =1 failed, no cdrom found
;	push cs
;	pop es

	call reg_probe
	or cx, cx
	jnz .find_cdrom

	call reg_probe
	or cx, cx
	jz .nocdrom

.find_cdrom:
%if 1
	mov si, atapi_dev_base
	mov di, atapi_dev_base_bak
	cld
	push si
	push di
	xor ax, ax
	xor bx, bx
	forcx 
		call select_atapi_force
		jc .chk_next
		call check_atapi_cdrom
		jc .chk_next
		inc bx
		movsw
		movsw
		jmp short .loop_next
		.chk_next:
		add si, 4
		.loop_next:
		inc ax
	endforcx

	pop si
	pop di

	mov cx, 64
	rep movsb

	mov [atapi_dev_count], bx
	mov cx, bx
%endif

	clc
	return

.nocdrom:
	stc
endp

proc reset_atapi_cdroms
	mov cx, [atapi_dev_count]
	xor ax, ax

	if {or cx, cx}, z
		return
	endif

	forcx
		call select_atapi
		call reg_reset
		inc ax
	endforcx
endp

	section .data

%ifdef SLOW_ATAPI_DEVICE
atapi_delay_flag	db 1
reg_slow_xfer_flag	db 1
%else
atapi_delay_flag	db 0
reg_slow_xfer_flag	db 0
%endif

cmd_select_dev		db CB_DH_DEV0,CB_DH_DEV1
reg_base_addr		dw 0x1f0,0x3f6, 0x170,0x376
			dw 0x180,0x386, 0x6b00,0x6f00
			dw 0x7300,0x7700

reg_base_addr_append	dw 0,0,0,0

%endif			; DISABLE_CDBOOT

	section .bss

%ifndef DISABLE_CDBOOT
start_of_atapi_data:

atapi_cur_dev		resw 1
reg_cur_dev		resb 1
time_out		resw 1
reg_dev_info		resb 2

atapi_dev_count		resw 1 
atapi_dev_base		resw 2*16	; first word is the base pointer,
					; second word is the device
atapi_dev_base_bak	resw 2*16	; first word is the base pointer,
					; second word is the device
reg_addr		resw 10
atapi_cmd_buffer	resb 16
atapi_tmp_buffer	resb 256
atapi_devinfo		resb SIZE_OF_ATAPI_DEVINFO
delay_repeat_num	resd 1

end_of_atapi_data:
%endif

start_of_emu_data:

drive_map 	resw 3

%ifndef DISABLE_CDBOOT
emu_buf_off	resw 1
emu_buf_seg	resw 1
emu_cdrom_id	resb 1
emu_disk_type	resb 1	;1=360 2=1.2 3=720 4=1.44 6=2.88 10h=atapi
emu_cyl		resw 1
emu_sec		resw 1
emu_head	resw 1
emu_last_read	resd 1

int13_last_stat resb 1

edd30_cdemu_spec resb SIZE_OF_CDEMU_SPEC
atapi_dpte_buffer resb SIZE_OF_DPTE
edd30_disk_buffer resb 0x800
%endif

end_of_emu_data:
