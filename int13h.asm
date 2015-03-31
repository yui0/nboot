; ---------------------------------------------------------
;	INT 13H FUNCTION
;
;		(C)2000	NAKADA
; ---------------------------------------------------------

; install / uninstall func for my int13h

%ifndef HAVE_MYINT13H
%define HAVE_MYINT13H

%ifndef DISABLE_CDBOOT
  %define SIZE_OF_EDD30	8	; my EDD3.0 and ATAPI driver need 6K mem
%else
  %define SIZE_OF_EDD30	1	; my SWAP Driver ID wrapper needs 1K mem
%endif

%ifndef DISABLE_CDBOOT
  %define EDD30_SIG1	'BM'
  %define EDD30_SIG2	'CB'
%else
  %define EDD30_SIG1	'BM'
  %define EDD30_SIG2	'SD'
%endif

;=============================================================================
;install_myint13h ---- install my int13h handler
;=============================================================================
install_myint13h:
	push	es
	xor	ax,ax
	mov	es,ax

	call	check_myint13h
	jc	.not_inst
	mov	ax,[es:0x13*4+2]
	mov	[module_edd30_seg],ax

	jmp	.end
	
.not_inst:
	sub	word [es:0x413],SIZE_OF_EDD30	; allocate memory for module edd30
	int	0x12
	shl	ax,6
	push	ax
	pop	es
	xor	di,di
	mov	si,module_edd30
	mov	cx,end_of_mod_edd30 - module_edd30
	cld
	rep	movsb

	mov	[module_edd30_seg],ax

%ifndef DISABLE_CDBOOT
	mov	ah,2
	call	far [module_edd30_off]	;initialize cdrom drivers
%endif

	xor	ax,ax
	call	far [module_edd30_off]	;install my int13h

.end:
	pop	es
	ret

;=============================================================================
;uninstall_myint13h ---- uninstall my int13h handler
;=============================================================================
uninstall_myint13h:
	call	check_myint13h
	jc	.not_inst
	xor	ah,ah
	inc	ah
	call	far [module_edd30_off]
	push	es
	xor	ax,ax
	mov	es,ax
	add	word [es:0x413],SIZE_OF_EDD30 ; free memory
	pop	es
.not_inst:
	ret

;=============================================================================
;set_drive_map
;input: bx, cx drive map
;=============================================================================
set_drive_map:
	call check_myint13h
	jc .end
	mov ah, 3

	call far [module_edd30_off]
.end:
	ret

;=============================================================================
;set_io_ports
;input: bx, cx io ports
;=============================================================================
set_io_ports:
	call check_myint13h
	jc .end
	mov ah, 4

	call far [module_edd30_off]
.end:
	ret

;=============================================================================
;check_myint13h ---- check if myint13h is present
;=============================================================================
check_myint13h:
	pusha
	mov ax, 0x6666
	mov bx, EDD30_SIG1
	mov cx, EDD30_SIG2
	clc
	int 0x13
	jc .absent
	cmp bx, EDD30_SIG2
	jne .absent
	cmp cx, EDD30_SIG1
	je .end
.absent:
	stc
.end:
	popa
	ret

module_edd30:
incbin "edd30.bin"
;%include "edd30.asm"
end_of_mod_edd30:

;%ifndef MAIN
;	section .bss
;%include "tempdata.asm"
;%endif

%endif
