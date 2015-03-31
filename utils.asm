; ---------------------------------------------------------
;	NBoot
;
;		(C)2000	NAKADA
; ---------------------------------------------------------


; ---------------------------------------------------------
;	power_off ---- turn the power off
;
;	output:
;	never return if successful.
;	cf = 1 on error.
; ---------------------------------------------------------

power_off:
	pusha
	call	check_apm_bios
	jc	.end

	mov	ax,0x5301
	xor	bx,bx
	int	0x15
	jc	.end

	mov	ax,0x5380
	mov	bh,0x8c
	int	0x15

	mov	ax,0x40
	mov	bx,0xd8
	push	ds
	mov	ds,ax
	or	byte [ds:bx],0x10
	pop	ds

	mov	ax,0x5307
	mov	bx,1
	mov	cx,3
	int	0x15
.end:
	popa
	ret


; ---------------------------------------------------------
;	check_apm_bios ---- check if the apm bios present
;
;	output:
;       cf = 1 error, cf = 0 ok
; ---------------------------------------------------------

check_apm_bios:
	pusha
	mov	ax,0x5300
	xor	bx,bx
	int	0x15		; check if apm present
	jc	.end
	cmp	bx,0x504d
	jnz	.none
	test	cx,1
	jnz	.end
.none:	stc
.end:	popa
	ret


; ---------------------------------------------------------
;	reboot ---- reboot the computer
; ---------------------------------------------------------

reboot:
	mov	bx,0x40
	push	bx
	pop	ds
	mov	ax,0x1234
	mov	[0x0072],ax
	jmp	0xffff:0x0000


; ---------------------------------------------------------
;	Check BIOS INT13h extensions
;	in  dl(drive)
;	out cf = 1 error, cf = 0 ok
;	use ax,bx,cx,dx
; ---------------------------------------------------------

check_lba:
	mov	ah,0x41
	mov	bx,0x55aa
	int	0x13		; test if we have BIOS INT13h extensions
	jc	.nolba
	cmp	bx,0xaa55
	jne	.nolba
	test	cx,1		; Does it support packet structure?
	jz	.nolba
	clc			; lba is ok
	ret
.nolba:	stc			; use csh
	ret
