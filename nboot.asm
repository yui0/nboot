; ---------------------------------------------------------
;	NBoot
;
;		(C)2000-2004 NAKADA
; ---------------------------------------------------------

%include "macro.h"
%include "key.h"


%define BOOT_OFF	0x7c00		; boot sector startup offset
%define PART_OFF	0x0600		; partition table offset
%define KERNEL_SEG	0x1000		; kernel startup


	org	100h
	jmp	main

; ---------------------------------------------------------
;	NBoot signature
; ---------------------------------------------------------

start:	db	"NBoot",0


; ---------------------------------------------------------
;	メイン
;	main program
; ---------------------------------------------------------

main:
;	call	install_myint13h
;	mov	bx,[cdrom_ioports]	; 0x1F0, 0x170
;	mov	cx,[cdrom_ioports+2]	; 0x3F6, 0x376
;	mov	bx,0x170
;	mov	cx,0x376
;	call	set_io_ports

	mov	ax,13h			; graphic mode 13h
	int	10h

	mov	si,palette		; set palette
	xor	ax,ax
	mov	cx,255
	call	setpal

	mov	dx,3ceh			; Don't understand
	mov	al,0
	out	dx,al
	inc	dx
	mov	al,0
	out	dx,al
	dec	dx
	mov	al,1
	out	dx,al
	inc	dx
	mov	al,0
	out	dx,al
	dec	dx
	mov	al,3
	out	dx,al
	inc	dx
	mov	al,0
	out	dx,al
	dec	dx
	mov	al,5
	out	dx,al
	inc	dx
	in	al,dx
	and	al,11111100b
	out	dx,al

	push	cs			; set segment
	pop	ds
	mov	ax,0a000h		; vram segment
	mov	es,ax

	xor	ax,ax
	mov	[counter],ah
	mov	[count],ax		; reset the timer counter
	mov	al,[time]		; timer enabled ?
	or	al,al			; has to start the timer?
	jz	mainlp
	mov	bl,36			; ax=al*36
	mul	bl			; ax has the increment for the system clock
	mov	[count],ax
	in	al,61h
	or	al,1			; allows the timer 2 to count,
	and	al,0fdh			; but without sound in the speaker.
	out	61h,al			; if we don't do this, the timer doesn't count :-?
	mov	al,0b0h			; timer 2, mode 0, binary
	out	43h,al
	mov	al,0ffh
	out	42h,al
	mov	al,0ffh
	out	42h,al			; start the timer 2
	mov	byte [counter],1	; starts the counter

mainlp:	call	mainmenu
.keylp:	call	waitkey

.setup:
	mov	bl,al
	or	bl,0x20			; convert the keystroke to lowercase
	cmp	bl,keysetup		; if keysetup is pressed, goes to setup
	jne	.up
	jmp	tsetup
.up:
	cmp	ah,0x48			; ↑
	jne	.down
	mov	al,-1
	call	select
	jmp	.keylp
.down:
	cmp	ah,0x50			; ↓
	jne	.chk_flag
	mov	al,1
	call	select
	jmp	.keylp

.chk_flag:
	cmp	byte [flag],1		; Where does NBoot load from ?
	jne	.dos
.reboot:
	cmp	ax,kbDel		; if del key is pressed, reboot.
	jne	.poweroff
;	call	reboot
	call	do_boot_record		; テスト
.poweroff:
	cmp	ax,kbEnd		; if end key is pressed, power off.
;	jne	.quit
	jne	.enter
	call	power_off
.quit:
;	cmp	al,27			; esc is quit
;	jne	.enter
;	int	0x18			; return to BIOS
.dos:
	cmp	al,27			; esc is quit
	jne	.enter
	call	uninstall_myint13h	; テスト
	mov	ax,3			; text mode
	int	0x10
	mov	ax,0x4c00		; exit to dos
	int	0x21

.enter:
	cmp	al,13			; return ?
	jne	.boot
	mov	bl,[slpos]
	mov	bh,[slmax]
	dec	bh
	mov	al,bl			; boot number
	add	al,'1'
	cmp	bl,bh
	jne	.boot
	jmp	tsetup
.boot:
	cmp	al,'1'
	jl	.keylp			; if is greatter than ascii '0'
	cmp	al,'9'
	jg	.keylp			; and is lower or equal than ascii '9'
	sub	al,'0'			; is a valid boot option, so we load the partition

	mov	si,ostabl
.loop:	cmp	byte [si],0xff
	jz	mainlp			; if the icon is ffh, it's not a valid boot option
	sub	al,1			; フラグのため
	jz	.pass
	add	si,28h			; next entry
	jmp	.loop
.pass:
	mov	[tempo],si
	add	si,20
	cmp	byte [si],0		; has password this entry?
	je	.check
	inc	si
	mov	di,si
	call	ask_pass
.check:
	mov	si,[tempo]
	test	byte [si+39],1		; check read mode
	jnz	.lba

	mov	dx,[si+16]		; drive and head
	mov	[csh_tbl.head],dh
	mov	cx,[si+18]		; sector and track
	mov	[csh_tbl.sc],cx
	mov	byte [disc_mode],2	; chs mode
	jmp	.read
.lba:
	mov	dl,[si+16]		; drive
	mov	cx,[si+17]
	mov	[lba_tbl.startlo],cx
	mov	cx,[si+37]
	mov	[lba_tbl.startlo+2],cx
	mov	cl,[si+19]
	mov	[lba_tbl.starthi],cl
	xor	cx,cx
	mov	[lba_tbl.starthi+1],cl
	mov	[lba_tbl.starthi+2],cx
	mov	byte [disc_mode],1	; lba mode

.read:
	mov	eax,BOOT_OFF		; address
	mov	[lba_tbl.off],eax
	mov	word [lba_tbl.count],1	; load one sector
	mov	cx,3			; load boot sector (trying three times max)
.lp:	mov	ah,2			; read
	push	cx
	call	disc_access
	pop	cx
	jnc	.success
	loop	.lp
.msg:	mov	si,merrms
	mov	ax,100h
	call	message
	jmp	mainlp

.success:
	xor	ax,ax
	mov	es,ax
	mov	ax,[es:7dfeh]		; last two bytes must be aa55h
	cmp	ax,0aa55h
	jnz	.msg			; to error transaction
	mov	ax,3
	int	10h			; changes to text mode
	mov	ah,2
	mov	bh,0
	mov	dx,0
	int	10h			; puts the screen coordinates to 0,0
	mov	dl,[tempo+16]		; drive
	or	dl,dl			; if NBoot is booting the floppy (drive 0) it doesn't
	je	.floppy			; test any mbr

	push	dx
	mov	eax,PART_OFF		; address 0:600
	mov	[lba_tbl.off],eax
	mov	ah,2			; read mode
	push	ax
	call	access_mbr		; loads the mbr of the selecter hd in 0000:0600
	pop	ax
	pop	dx
	mov	cx,[tempo+18]		; sector and track
	push	dx			; saves in the stack the drive
	call	updmbr			; tests the hidden and visible partitions
	pop	dx			; restore the drive from the stack
	push	dx
	push	cx
	push	ax
	call	access_mbr		; loads or saves the mbr as needed
	pop	ax
	pop	cx
	pop	dx
	call	updmbr2			; test the active partition
	call	access_mbr		; loads or saves the mbr as needed
.floppy:
	call	uninstall_myint13h	; テスト
	mov	si,[tempo]		; tests if we have to swap drives
	mov	al,[si+36]
	or	al,al
	je	.run
	mov	[swapr],al		; pass to the swap routine the drive to swap
	xor	ax,ax
	mov	es,ax
	dec	word [es:413h]		; allocate 1kbyte
	int	12h			; gets the available amount of memory
	mov	cl,6
	shl	ax,cl			; obtains the segment for our swap routine
	push	ax			; and saves it in the stack
	mov	bx,ax
	xchg	bx,[es:4eh]		; take the segment for int 13h
	mov	[swapsg],bx		; and saves it in the swap routine
	mov	bx,8h
	xchg	bx,[es:4ch]		; take the offset for int 13h
	mov	[swapof],bx		; now int 13h points to lastseg:0008
	cld
	pop	es			; segment for the swap routine
	mov	si,swapr
	mov	di,0			; swap routine in lastseg:0000
	mov	cx,0ffh			; 255 bytes
	repz
	movsb				; copies the swap routine
.run:
	xor	ax,ax
	mov	ds,ax			; data segment to 0000
	mov	es,ax			; extra segment to 0000
	jmp	0:BOOT_OFF		; and jump to the boot sector


; ---------------------------------------------------------
;	swapr
;	this is the swap routine, that allows to swap
;	hard disk drives
; ---------------------------------------------------------

swapr	db	0			; here we store the drive to swap
	db	0			; here we store the dl register
	dw	0
	dw	0
	dw	0			; temporal place
	mov	[cs:2],ah		; saves the subcall made to the int13h
	mov	[cs:1],dl		; stores the drive
	cmp	dl,80h			; first hard disk?
	jne	swsig1
	mov	dl,[cs:0]		; if true, use the swaped disk
	jmp	swsig2
swsig1:	cmp	dl,[cs:0]		; swaped hard disk?
	jne	swsig2
	mov	dl,80h			; if true, use the first hard disk
swsig2:	pushf				; the iret in the bios routine needs the flags in the stack
	db	9ah			; far call
swapof	dw	0			; offset
swapsg	dw	0			; segment
	pushf
	cmp	byte [cs:2],8		; subcall 'get drive parameters'?
	je	swsig3			; if true, don't change the drive when return
	cmp	byte [cs:2],15h		; subcall 'get disk type'?
	je	swsig3			; if true, don't change the drive when return
	mov	dl,[cs:1]		; loads the old dl value
swsig3:	popf
	mov	[cs:2],ax		; saves in the temporal place ax
	pop	ax
	mov	[cs:4],ax		; ip
	pop	ax
	mov	[cs:6],ax		; and cs
	pop	ax			; pops the old flags
	pushf				; pushes the new ones
	mov	ax,[cs:6]		; and returns the rest of datas
	push	ax
	mov	ax,[cs:4]
	push	ax
	mov	ax,[cs:2]
	iret				; and return


; ---------------------------------------------------------
;	updmbr
;	updates the hidden and visible partitions. cx and dx must have
;	the drive, sector, head and track. returns:
;	ah=2 -> mbr doesn't need modifications ah=3 -> it needs.
; ---------------------------------------------------------

updmbr:	mov	ah,2
	mov	byte [pfoun],0
	mov	byte [phidd],0
	mov	bh,4			; 4 entries
	mov	si,PART_OFF+0x1be	; first entry
upbuc3:	mov	al,[es:si+4]		; partition type
	or	al,al
	jz	upntry
	cmp	dh,[es:si+1]		; head matchs?
	jnz	upnex1
	cmp	cl,[es:si+2]		; sector and cilinder matchs?
	jnz	upnex1
	cmp	ch,[es:si+3]		; sector and cilinder matchs?
	jnz	upnex1

	mov	di,activ		; tests if this partition is in the group of the
upbuc0:	cmp	byte [di],0		; hideable partitions
	je	upnexb			; not found, continue
	cmp	al,[di]
	je	upnex0			; if it's in the group, mark it in pfoun
	inc	di
	jmp	upbuc0
upnex0:	mov	byte [pfoun],1		; the partition to boot is primary and hiddeable
	jmp	upntry			; go next entry
upnexb:	mov	di,inact
upbuc1:	cmp	byte [di],0
	je	upntry			; not found, go next entry
	cmp	al,[di]
	je	upnex2			; found a hide partition
	inc	di
	jmp	upbuc1
upnex2:	mov	ah,3			; mbr needs to be resaved
	and	al,0efh			; resets bit 4 (unhides the partition)
	mov	[es:si+4],al
	jmp	upntry
upnex1:	mov	di,activ
upbuc2:	cmp	byte [di],0
	je	upntry			; not found, go next entry
	cmp	al,[di]
	je	upnex3			; found an unhide partition
	inc	di
	jmp	upbuc2
upnex3:	or	al,10h			; sets bit 4 (hides the partition)
	mov	[es:si+4],al
	mov	byte [phidd],1		; we had hidded a partition
upntry:	add	si,10h			; next entry
	sub	bh,1
	jnz	upbuc3

	; now we ensure that only one primary partition is visible

	cmp	byte [pfoun],1		; the partition to boot is primary and hideable?
	jne	upend			; if true, we must to hide the others
	cmp	byte [phidd],1		; had we hidded a partition?
	jne	upend			; if not, ends, and avoid to save the mbr without changes
	mov	ah,3			; mbr needs to be resaved to hides the other partitions
upend:	ret


; ---------------------------------------------------------
;	updmbr2
;	updates the active and inactive partitions. cx and dx must have
;	the drive, sector, head and track. returns:
;	ah=2 -> mbr doesn't need modifications ah=3 -> it needs.
; ---------------------------------------------------------

updmbr2:
	mov	ah,2
	mov	bh,4			; 4 entries
	mov	si,PART_OFF+0x1be	; first entry
ubuc3:	mov	al,[es:si+4]		; type
	or	al,al
	jz	untry
	cmp	dh,[es:si+1]		; head matchs?
	jnz	unex1
	cmp	cl,[es:si+2]		; sector and cilinder matchs?
	jnz	unex1
	cmp	ch,[es:si+3]		; sector and cilinder matchs?
	jnz	unex1
	mov	ah,3			; mbr needs to be resaved
	mov	byte [es:si],80h	; marks partition as active
	jmp	untry
unex1:	mov	byte [es:si],0		; marks partition as inactive
untry:	add	si,10h			; next entry
	sub	bh,1
	jnz	ubuc3
	ret


; ---------------------------------------------------------
;	loadmbr
;	loads the sector indicated in exten1 and exten2
; ---------------------------------------------------------

loadmbr:
	push	es
	push	bx
	push	ax
	push	di
	mov	bx,mbr
	push	ds
	pop	es
	mov	dx,[exten1]	; drive and head
	mov	cx,[exten2]	; sector and cilinder
	mov	ax,0201h	; 1 sector bios_load_sector
	int	13h		; load the mbr in es:mbr
	pop	di
	pop	ax
	pop	bx
	pop	es
	ret


; ---------------------------------------------------------
;	yes and no
;	prints a question string pointed by si
;	and returns al=0 if the user answers yes
;	or al=1 if the user asks no
; ---------------------------------------------------------

yesno:
	mov	ax,0a000h
	mov	es,ax
	xor	ax,ax
	mov	di,47+158*320
	mov	cx,29
	mov	dx,(273-47)/2
	call	fill
	call	window

	mov	bx,gr+blue
	mov	dx,20*8+4
	call	printc
	mov	si,myesno
	mov	dx,21*8+5
	call	printc
.loop:	call	waitkey
	or	al,20h			; converts keystroke to lowercase
	cmp	al,key.yes
	jz	.yes
	cmp	al,key.no
	jnz	.loop
	mov	al,1			; user pressed no
	ret
.yes:	mov	al,0			; user pressed yes
	ret


; ---------------------------------------------------------
;	message
;	prints a message pointed by si
;	ax:flag, si:msg
; ---------------------------------------------------------

message:
	push	ax
	mov	ax,0a000h
	mov	es,ax
	xor	ax,ax
	mov	di,47+158*320
	mov	cx,29
	mov	dx,(273-47)/2
	call	fill
	call	window
	pop	ax

	mov	bx,gr+purple
	mov	dx,20*8+4
	push	ax
	or	al,al
	jnz	msgjp
	or	ah,ah
	jz	msgjp2
	mov	dx,20*8
msgjp:	push	dx
	call	printc
	pop	dx
	add	dx,9
msgjp2:	push	dx
	call	printc
	pop	dx
	add	dx,9
	pop	ax
	or	al,al
	jnz	msgjp3
	mov	si,mok
	call	printc
msglp:	call	waitkey
	cmp	al,13
	jnz	msglp
msgjp3:	ret


; ---------------------------------------------------------
;	tsetup
;	tests if there's configuration password.
;	if not, goes to setup
; ---------------------------------------------------------

tsetup:
	mov	byte [slpos],0
	cmp	byte [ispaswd],0
	je	setup			; if there's no password, go to setup
	mov	di,tpaswd		; ask for the password
	call	ask_pass
	jmp	setup


; ---------------------------------------------------------
;	ask_pass
;	ask_pass ask for a password, given in di.
;	if the typed password is equal to and jumps to mainlp.
; ---------------------------------------------------------

ask_pass:
	mov	byte [prpaswd],1	; don't print the keystrokes
.msg:	mov	si,mentpw
	push	di
	call	input
	pop	di
	mov	byte [prpaswd],0	; print the keystrokes
	mov	si,buff
	mov	cx,14
tsbuc1:	mov	ah,[si]
	cmp	ah,[di]
	jne	notequ
	inc	si
	inc	di
	loop	tsbuc1
	ret				; password ok
notequ:	pop	ax			; empty the stack
msg140:	mov	si,mincor
	xor	ax,ax
	call	message
	jmp	mainlp


; ---------------------------------------------------------
;	setup
;	setup menu routine
; ---------------------------------------------------------

setup:
	call	pnboot			; title

	mov	bx,white		; message
	mov	si,msg.add
	mov	cx,7*8
	mov	dx,9*8
	call	print
	mov	si,msg.del
	mov	cx,7*8
	mov	dx,10*8
	call	print
	mov	si,msg.savefd
	mov	cx,7*8
	mov	dx,11*8
	call	print
	mov	si,msg.savehd
	mov	cx,7*8
	mov	dx,12*8
	call	print
	mov	si,msg.btimer
	mov	cx,7*8
	mov	dx,13*8
	call	print
	mov	si,msg.pass
	mov	cx,7*8
	mov	dx,14*8
	call	print
	mov	si,msg.option
	mov	cx,7*8
	mov	dx,15*8
	call	print
	mov	si,msg.return
	mov	cx,7*8
	mov	dx,16*8
	call	print
	call	textwav

	mov	byte [slmax],8
	mov	word [slbpos],5*8+(9*8+7)*320
	mov	word [slbwd],240
	mov	word [slspos],5*8+8*320
	mov	byte [slcl],0
	mov	ax,blue*256+purple
	mov	bx,gr+yellow
	call	select_

setlp:	call	waitkey
	cmp	al,27			; esc key ?
	jnz	setjp
	jmp	mainlp
setjp:	cmp	ah,48h			; ↑
	jnz	setjp2
	mov	al,-1
	call	select
	jmp	setlp
setjp2:	cmp	ah,50h			; ↓
	jnz	setjp3
	mov	al,1
	call	select
	jmp	setlp
setjp3:	mov	cl,[slpos]		; select position
	mov	bx,.tbl			; table address

.comd:
	cmp	ax,kbEnter		; enter key ?
	jnz	.key
	xor	ah,ah
	mov	al,cl
	sal	ax,2			; *4
	add	bx,ax
.find:	add	bx,2
	jmp	[cs:bx]
.key:	xor	ah,ah
	or	al,20h			; convert the keystroke to lowercase
.lp:	cmp	ax,[bx]
	jz	.find
	cmp	byte [bx],0		; None select
	jz	.find
	add	bx,4
	jmp	.lp

.tbl:
	dw	key.add,	add_os	; Add an OS
	dw	key.del,	delete	; Delete an OS
	dw	key.fd,		flopy	; Save in Floppy
	dw	key.hd,		harddk	; Save in Hard disk
	dw	key.timer,	btimer	; Boot timer
	dw	key.pass,	passwd	; Setup Password
	dw	key.op,		option	; Option
	dw	key.ret,	mainlp	; Return to main menu
	dw	0,		setlp	; None select


; ---------------------------------------------------------
;	option
; ---------------------------------------------------------

option:
	call	mclear			; clear
	mov	ax,101h
msg001:	mov	si,mmenu		; 英語だとおかしい表現
	call	message

	mov	bx,gr+blue		; message
	mov	si,msg.lang		; language
	mov	dx,9*8
	call	printc
	mov	bx,white
	mov	si,msg.eng
	mov	dx,10*8
	call	printc
	mov	si,msg.jp
	mov	dx,11*8
	call	printc

	mov	byte [slpos],0
	mov	byte [slmax],2
	mov	word [slbpos],90+(10*8+7)*320
	mov	word [slbwd],140
	mov	word [slspos],90+2*8*320
	mov	byte [slcl],0
	mov	ax,blue*256+purple
	mov	bx,gr+yellow
	call	select_

oplp:	call	waitkey
	cmp	al,27			; esc key ?
	jnz	opjp
	mov	byte [slpos],6		; select option
	jmp	setup
opjp:	cmp	ah,48h			; ↑
	jnz	opjp2
	mov	al,-1
	call	select
	jmp	oplp
opjp2:	cmp	ah,50h			; ↓
	jnz	opjp3
	mov	al,1
	call	select
	jmp	oplp
opjp3:	cmp	al,13			; enter key ?
	jnz	oplp
	mov	bl,[slpos]
	cmp	bl,1
	jg	oplp
	jnz	opjp4
	jmp	opjp5
opjp4:	mov	byte [multil],0
	mov	word [mainmenu.msg+1],mmenu_
	mov	word [msg001+1],mmenu_
	mov	word [msg010+1],mossel_
	mov	word [msg020+1],mosdel_
	mov	word [add_lp.msg2+1],mopsel_
	mov	word [add_lp.msg3+1],mpartt_
	mov	word [msg050+1],mtimer_
	mov	word [msg060+1],mdescr_
	mov	word [ask_pass.msg+1],mentpw_
	mov	word [msg080+1],mnewpw_
	mov	word [msg090+1],mbdisa_
	mov	word [msg100+1],mberr_
	mov	word [msg110+1],merrls_
	mov	word [msg111+1],merrls_
	mov	word [mainlp.msg+1],merrms_
	mov	word [msg130+1],mgraba_
	mov	word [sherr +1],mgraba_
	mov	word [msg131+1],mgraba_
	mov	word [msg140+1],mincor_
	mov	word [msg150+1],mnoos_
	mov	word [add_os.msg+1],mnoent_
	mov	word [msg170+1],mpsdis_
	mov	word [msg180+1],msuces_
	mov	word [msg181+1],msuces_
	mov	word [msg190+1],mswap_
	mov	byte [slpos],6
	jmp	setup
opjp5:	mov	byte [multil],1
	mov	word [mainmenu.msg+1],mmenu
	mov	word [msg001+1],mmenu
	mov	word [msg010+1],mossel
	mov	word [msg020+1],mosdel
	mov	word [add_lp.msg2+1],mopsel
	mov	word [add_lp.msg3+1],mpartt
	mov	word [msg050+1],mtimer
	mov	word [msg060+1],mdescr
	mov	word [ask_pass.msg+1],mentpw
	mov	word [msg080+1],mnewpw
	mov	word [msg090+1],mbdisa
	mov	word [msg100+1],mberr
	mov	word [msg110+1],merrls
	mov	word [msg111+1],merrls
	mov	word [mainlp.msg+1],merrms
	mov	word [msg130+1],mgraba
	mov	word [sherr +1],mgraba
	mov	word [msg131+1],mgraba
	mov	word [msg140+1],mincor
	mov	word [msg150+1],mnoos
	mov	word [add_os.msg+1],mnoent
	mov	word [msg170+1],mpsdis
	mov	word [msg180+1],msucces
	mov	word [msg181+1],msucces
	mov	word [msg190+1],mswap
	mov	byte [slpos],6
	jmp	setup


; ---------------------------------------------------------
;	delete
;	removes an os from the list
; ---------------------------------------------------------

delete:
	mov	byte [prstp],1		; removes an os from the list
	call	mclear
	mov	ax,101h			; "Press 1-9 to delete an OS or ESC to abort"
msg020:	mov	si,mosdel
	call	message
	call	puticon
	cmp	byte [ostabl],0ffh	; is there more os?
	jnz	dinic
msg150:	mov	si,mnoos		; "There aren't OS to delete"
	xor	ax,ax
	call	message
	jmp	setup
dinic:	call	waitkey
	cmp	al,27			; esc key?
	jnz	msig1
	jmp	setup
msig1:	cmp	al,49
	jl	dinic			; if is greatter than ascii '0'
	cmp	al,57
	jg	dinic			; and is lower or equal than ascii '9'
	sub	al,48			; is a valid option
	mov	si,ostabl
debuc:	mov	ah,[si]
	cmp	ah,0ffh
	jz	dinic			; if the icon is ffh, it's not a valid boot option
	sub	al,1
	jz	dsig2
	add	si,28h			; next entry
	jmp	debuc
dsig2:	mov	di,si			; now si and di point to the entry to delete
	add	si,28h			; si points to the next entry
debuc2:	mov	ah,[si]
	mov	[di],ah
	inc	si
	inc	di
	cmp	ah,0ffh			; last entry?
	jne	debuc2
	mov	cx,28h
debuc3:	mov	[di],ah			; deletes the last entry
	inc	di
	loop	debuc3
	cmp	byte [time],0		; boot timer enabled?
	je	dfine
	jmp	bdisab			; disables the timer
dfine:	jmp	setup


; ---------------------------------------------------------
;	btimer
;	configures the boot timer
; ---------------------------------------------------------

btimer:
msg050:	mov	si,mtimer
	mov	byte [prpaswd],2	; allow to type numbers only
	call	input			; gets the number in ascii in buff
	mov	byte [prpaswd],0	; allows again all characters
	cmp	byte [buff+2],32	; more than two digits?
	jz	btsig1
	xor	ax,ax
msg100:	mov	si,mberr
	call	message
	jmp	setup
btsig1:	cmp	byte [buff],32		; empty string?
	jnz	btsig2
bdisab:	mov	byte [time],0		; disables the timer
	xor	ax,ax
msg090:	mov	si,mbdisa
	call	message
	jmp	setup
btsig2:	mov	bx,0
	cmp	byte [buff+1],32	; only one digit?
	je	btsig3
	mov	bl,0ah
	mov	ah,0
	mov	al,[buff]
	sub	al,48
	mul	bl			; multiply 10*first digit
	mov	bl,[buff+1]
	sub	bl,48
	add	al,bl
	jmp	btsig4
btsig3:	cmp	byte [buff],48		; time is zero?
	je	bdisab			; disables the timer
	mov	al,[buff]
	sub	al,48
btsig4:	mov	[time],al		; stores the new time
	call	mclear
	mov	ax,101h
msg010:	mov	si,mossel
	call	message
	mov	byte [prstp],1
	call	puticon
btinic:	call	waitkey
	cmp	al,27			; esc
	jnz	btsig5
	mov	byte [time],0
	jmp	setup
btsig5:	cmp	al,49
	jl	btinic			; if is lower or equal than ascii '0'
	cmp	al,57
	jg	btinic			; or is greater than ascii '9', close the loop
	push	ax
	sub	al,48			; is a valid boot option, so we select it
	mov	si,ostabl
btbuc:	mov	ah,[si]
	cmp	ah,0ffh
	jnz	btsig7			; if the icon is ffh, it's not a valid option
	pop	ax
	jmp	btinic
btsig7:	sub	al,1
	jz	btsig6
	add	si,28h			; next entry
	jmp	btbuc
btsig6:	pop	ax			; is a valid option
	mov	[toboot],al		; stores it
	jmp	setup


; ---------------------------------------------------------
;	passwd
;	changes the setup password
; ---------------------------------------------------------

passwd:
	mov	si,ispaswd
	mov	di,tpaswd
	call	chpwd
	cmp	byte [ispaswd],0
	jne	rpaswd
	xor	ax,ax
msg170:	mov	si,mpsdis
	call	message
rpaswd:	jmp	setup


; ---------------------------------------------------------
;	chpwd
;	accept a password from keyboard, returning 1 in si if a password
;	is typed, or 0 if only return is pressed, and storing a pointer to
;	the password in di.
; ---------------------------------------------------------

chpwd:
	push	si
	push	di
msg080:	mov	si,mnewpw		; enter the new password
	call	input
	pop	di
	mov	si,buff
	mov	cx,15
	mov	al,0
psbuc1:	mov	ah,[si]
	mov	[di],ah
	cmp	ah,32
	je	pspace
	mov	al,1			; if there's a character not equal to space, puts al=1
pspace:	inc	si
	inc	di
	loop	psbuc1
	pop	si
	cmp	al,0			; all spaces?
	je	pdisab
	mov	byte [si],1
	ret
pdisab:	mov	byte [si],0
	ret


; ---------------------------------------------------------
;	Disc Access
;	in  ah(flag), dl(drive)
;	use ax,bx,cx,dx,si
; ---------------------------------------------------------

disc_access:
	cmp	byte [disc_mode],1
	jz	disc_access_lba
	ja	disc_access_csh		; disc_mode > 1 ?
	push	ax
	call	check_lba
	pop	ax
	jnc	disc_access_lba

; ah(flag), dl(drive)
disc_access_csh:
	push	es
	mov	al,[lba_tbl.count]
	mov	bx,[lba_tbl.off]
	mov	cx,[lba_tbl.seg]
	mov	es,cx
	mov	cx,[csh_tbl.sc]
	mov	dh,[csh_tbl.head]
	int	0x13
	pop	es
	ret

; ah(flag), dl(drive)
disc_access_lba:
	or	ah,0x40
	mov	si,lba_tbl
	int	0x13
	ret

disc_mode	db 0			; 0 no check, 1 lba, 2 csh

csh_tbl:
.sc		dw 1			; sector/cylinder
.head		db 0			; head

lba_tbl:
.size		db 16
.reserved	db 0
.count		dw 1			; read count sector (for chs/lba mode)
.off		dw 0			; buffer offset (for chs/lba mode)
.seg		dw 0			; buffer segment (for chs/lba mode)
.startlo	dd 0
.starthi	dd 0


; ---------------------------------------------------------
;	Load MBR
;	in dl(drive)
; ---------------------------------------------------------

load_mbr:
	mov	ax,mbr
	mov	[lba_tbl.off],ax
	mov	ax,ds
	mov	[lba_tbl.seg],ax
	mov	ah,2			; read mode

access_mbr:
	xor	ebx,ebx
;	mov	[lba_tbl.startlo],ebx
;	mov	[lba_tbl.starthi],ebx
	mov	[csh_tbl.head],bl
	inc	bx
	mov	[csh_tbl.sc],bx
	mov	[lba_tbl.count],bx

;	jmp	disc_access
	jmp	disc_access_csh


; ---------------------------------------------------------
;	add_os
;	adds a new partition to the o.s. list
; ---------------------------------------------------------

add_os:
	mov	si,ostabl		; searchs for a free entry in the table of o.s.
	mov	ah,0
.lp:	mov	al,[si]
	cmp	al,0ffh
	je	.find
	inc	ah
	add	si,28h			; next entry
	cmp	ah,9			; last entry?
	jne	.lp			; no, close loop
	xor	ax,ax			; "no entry" err
.msg:	mov	si,mnoent
	call	message
	jmp	setup

.find:
	push	si			; si points to a free entry
	mov	cx,28h
.lp2:	mov	byte [si],0		; clears the entry to delete spare
	inc	si
	loop	.lp2
	pop	si

	push	si
	mov	dl,80h			; first hard disk
add_lp:
	mov	[drives],dl
;	mov	byte ptr [extend],0	; no LBA base
;	mov	byte ptr [extens],0	; we are with the MBR

	call	mclear			; "select os" msg
	mov	ax,101h
.msg2:	mov	si,mopsel
	call	message
	call	clmsg			; we need buff to print the letters

	mov	di,mbr			; clears the buffer
	mov	cx,200h
.lp:	mov	byte [di],0
	inc	di
	loop	.lp

	mov	dl,[drives]		; loads the mbr
;	push	dx
	mov	dh,0
	mov	[exten1],dx		; drive and head
	mov	word [exten2],0001h	; sector and cilinder
;	mov	word ptr [exten5],0
;	mov	word ptr [exten6],0
;	mov	byte ptr [exten7],0
;	call	loadmbr
	call	load_mbr

.msg3:	mov	si,mpartt		; "Partition type"
	mov	bx,white
	mov	cx,13*8
	mov	dx,8*8
	call	print
	mov	si,mdisk		; "A  Boot from floppy"
	mov	cx,10*8
	mov	dx,9*8
	call	print

	mov	di,ptable		; table where I save the temporal partition data
	mov	word [di],0		; adds to the table the first floppy disk
	inc	di
	inc	di
	mov	word [di],1		; sector and cilinder
	inc	di
	inc	di
	xor	eax,eax
	mov	[di],eax		; Relative sector
	add	di,4
	mov	[di],ax			; partition number
	add	di,2
	mov	byte [di],0ffh		; mark the end of table

	mov	bh,'B'			; b is the first letter (a is for floppy)
	mov	bl,0			; line where print the next partition
add_lp2:
	mov	ah,4			; four entries in each sector
	mov	byte [exten],0		; no extended partitions found yet
;	mov	si,mbr			; point to the mbr
;	add	si,1bfh			; first entry
	mov	si,mbr+0x1be		; point to the partition table
add_lp3:
;	pop	dx

;	push	dx			; gets in dl the drive
	mov	dl,[drives]		; gets in dl the drive
	mov	dh,[si+1]		; head
	mov	cx,[si+2]		; sector and cilinder
	mov	al,[si+4]		; partition type
	cmp	al,0			; not used?
	jnz	.next
	jmp	anext
.next:	cmp	al,5			; extended?
	jz	.exten
	cmp	al,0fh			; windows extended?
	jnz	.print

.exten:	mov	byte [exten],1		; we have now an extended partition
	mov	[exten1],dx		; drive and head
	mov	[exten2],cx		; cilinder and sector

;	cmp	byte ptr [extend],0	; no lba use?
;	jne	.exlba
;	mov	byte ptr [extend],1
;	push	dx
;	mov	dx,[si+8]		; 1
;	mov	[exten3],dx		; lba base for extended partitions
;	mov	[exten5],dx		; next partition sector (lba)
;	mov	dx,[si+10]		; 2
;	mov	[exten4],dx
;	mov	[exten6],dx
;	mov	byte ptr [exten7],0	; 3
;	pop	dx
;	jmp	.exend
;.exlba:	push	dx
;	mov	dx,[si+8]
;	mov	[exten5],dx		; next partition sector (lba)
;	mov	dx,[si+10]
;	mov	[exten6],dx
;	mov	byte ptr [exten7],0
;	push	si
;	mov	si,offset exten3
;	call	sumalba
;	pop	dx
;	pop	si
;.exend:

	jmp	anext

.print:	mov	[di],dx			; add to the table the found partition
	inc	di			; head and drive
	inc	di
	mov	[di],cx			; cylinder and sector
	inc	di
	inc	di
;---------------------------------------
;	push	dx
;	cmp	byte ptr [extens],0	; extend mbr ?
;	jne	.lbanx
	mov	edx,[si+8]		; Relative sectors
	mov	[di],edx
	add	di,4
;	mov	dh,[partit]		; number of primary partition (1 to 4)
	mov	dx,0x100
	mov	[di],dx
	inc	di
	inc	di
;	pop	dx
;	jmp	.lbaend
;.lbanx:	push	si
;	add	si,8
;	call	sumalba
;	pop	si
;	mov	dx,[exten5]
;	mov	[di],dx
;	inc	di
;	inc	di
;	mov	dx,[exten6]
;	mov	[di],dx
;	inc	di
;	inc	di
;	mov	dl,[exten7]
;	mov	[di],dl
;	inc	di
;	mov	byte ptr [di],0		; extended partition
;	inc	di
;	pop	dx
;.lbaend:
;---------------------------------------
	mov	byte [di],0ffh		; end of table

	push	si
	push	di
	push	bx
	push	ax
	call	clmsg			; clears the temporal msg buffer
	mov	[buff],bh		; actual letter
	push	ax
	shr	al,4
	call	toasc
	mov	[buff+3],al
	pop	ax
	call	toasc
	mov	[buff+4],al
	mov	byte [buff+5],"h"
	mov	byte [buff+7],0
	mov	si,buff
	mov	cx,10*8
	mov	dh,0
	mov	dl,bl
	add	dx,dx
	add	dx,dx
	add	dx,dx
	add	dx,10*8
	mov	bx,white
	call	print
	pop	ax
	pop	bx
	push	bx
	push	ax
	mov	cx,17*8
	mov	dh,0
	mov	dl,bl
	add	dx,dx
	add	dx,dx
	add	dx,dx
	add	dx,10*8
	mov	bx,white

	cmp	al,1
	jz	.dos
	cmp	al,4
	jz	.dos
	cmp	al,6
	jz	.dos
	cmp	al,11h
	jz	.dos
	cmp	al,14h
	jz	.dos
	cmp	al,16h
	jz	.dos
	cmp	al,7
	jz	.os2
	cmp	al,17h
	jz	.os2
	cmp	al,0bh
	jz	.win
	cmp	al,0ch
	jz	.win
	cmp	al,0eh
	jz	.win
	cmp	al,1bh
	jz	.win
	cmp	al,1ch
	jz	.win
	cmp	al,1eh
	jz	.win
	cmp	al,0ah
	jz	.bm
	cmp	al,0a5h
	jz	.bsd
	cmp	al,0ebh
	jz	.beos
	cmp	al,83h
	jnz	.perr
	mov	si,mlinux
	jmp	.ptype
.bsd:	mov	si,mbsd
	jmp	.ptype
.dos:	mov	si,mdos
	jmp	.ptype
.win:	mov	si,mwin
	jmp	.ptype
.bm:	mov	si,mbm
	jmp	.ptype
.beos:	mov	si,mbeos
	jmp	.ptype
.os2:	mov	si,mos2
.ptype:	call	print
.perr:	pop	ax
	pop	bx
	pop	di
	pop	si
	inc	bh			; next letter
	inc	bl			; next row
anext:	add	si,16			; next entry (every 16B)
;	cmp	bl,27			; mov bl,9 -> 27-9
	cmp	bl,18
	je	afine			; there's no room in the screen to show more partitions
;	inc	byte ptr [partit]
	sub	ah,1			; 4 times
	jz	.next5
	jmp	add_lp3
.next5:	mov	al,[exten]
	cmp	al,0			; are there more extended partitions?
	je	afine
;	mov	byte ptr [extens],1
	call	loadmbr			; reads the next sector
	jmp	add_lp2
;afine:	pop	dx
afine:	mov	dl,[drives]		; gets in dl the drive

.keylp:	call	waitkey			; waits for a keystroke
	cmp	al,"8"			; is a '8'?
	jg	.key
	cmp	al,"1"			; is a '1'?
	jl	.keylp
	add	al,79			; converts the keystroke into a number betwen 80h and 88h
	mov	dl,al			; selects the new hd
;	mov	[drives],dl		; and stores the actual drive in drives to know if we
	jmp	add_lp			; need to swap or not
.key:	or	al,20h			; convert the keystroke to lowercase
	cmp	al,60h
	jle	.keylp

	sub	al,61h			; search data
	mov	si,ptable
.elp:	or	al,al
	je	.save			; entry located
;	add	si,4			; next entry
	add	si,10			; next entry
	cmp	byte [si],0ffh		; end of ptable?
	je	.keylp			; if true, try again
	dec	al
	jmp	.elp

;.save:
;	mov	al,[si+9]		; partition number
;	cmp	al,0
;	je	.anxt7
;	sub	al,1			; partition number (0 - 3)
;	rol	al,1
;	rol	al,1
;	and	al,0ch			; partition number in bits 2 and 3
;	or	al,2			; bit 1 set
;.anxt7:	cmp	byte ptr [whatdo],1
;	je	anextn

.save:	mov	dl,[si]			; drive unit
	or	dl,dl			; Floppy disk drive?
	je	.save_csh		; If true, use CSH
	mov	cx,[si+4]		; LBA0 and LBA1
	mov	bx,[si+6]		; LBA2 and LBA3
	mov	dh,[si+8]		; LBA4
	pop	si
	mov	[si+10h],dl		; drive unit
	mov	[si+11h],cx		; LBA0 and LBA1
	mov	[si+13h],dh		; LBA4
	mov	[si+25h],bx		; LBA2 and LBA3
;	or	al,1			; sets the bit 0 (LBA mode)
	mov	al,1			; sets the bit 0 (LBA mode)
	mov	[si+27h],al
	jmp	.cont

.save_csh:
	mov	dx,[si]			; drive/head
	mov	cx,[si+2]		; track/sector
	pop	si
	mov	[si+10h],dx		; saves the drive, sector, head and track
	mov	[si+12h],cx
;	and	al,0xfe			; clear the bit 0 (CSH mode)
	mov	al,0			; clear the bit 0 (CSH mode)
	mov	[si+0x27],al
	mov	byte [si],3		; icon
.cont:
	push	si
	cmp	byte [drives],80h	; actual drive is the first hard disk?
	je	aask			; if yes, don't ask for swap drives
msg190:	mov	si,mswap
	call	yesno			; ask for drive swapping
	pop	si
	push	si
	mov	byte [si+36],0		; no swap
	mov	ah,[drives]
	cmp	al,0			; user answered yes?
	jne	aask
	mov	[si+36],ah		; drive to swap

aask:
msg060:	mov	si,mdescr
	call	input			; ask for the o.s.'description
	pop	si
	push	si
	inc	si
	mov	di,buff
	mov	cx,15
.lp:	mov	ah,[ds:di]
	mov	[ds:si],ah
	inc	si
	inc	di
	loop	.lp			; copies the description
	pop	si

	push	si
	add	si,20
	mov	di,si
	inc	di
	call	chpwd			; ask for a password
;	call	shwicn			; ask for an icon
;	mov	si,mlett2		; "Press A-Z to select an icon"
;	mov	bx,white
;	mov	dx,100
;	call	printc
;abuc7:	call	waitkey
;	or	al,20h
;	cmp	al,61h
;	jl	abuc7
;	sub	al,60h
;	cmp	al,[icons]		; icons=11
;	jg	abuc7
	mov	al,1
	pop	si
	mov	[si],al
	jmp	setup


; ---------------------------------------------------------
;	ふろっぴーから起動できるように
;	floppy	- saves nboot in a floppy disk -
; ---------------------------------------------------------

flopy:
	mov	dx,0
	push	es
	push	ds
	pop	es
	push	dx
	mov	cx,3
sfabun:	push	cx
	mov	ax,201h
	mov	cx,0001h
	mov	bx,mbr
	int	13h				; load the boot sector (trying three times max)
	jnc	sfaend				; success
	pop	cx
	loop	sfabun
	pop	dx
	pop	es
msg110:	mov	si,merrls			; error
	xor	ax,ax
	call	message
	jmp	setup
sfaend:	pop	dx				; empty the stack
	mov	word [mbr+510],0aa55h		; saves the boot signature
	mov	byte [mbr],0ebh			; jmp code
	mov	word [mbr+1],903ch		; saves the jump (to preserve the floppy info)
	pop	dx
	push	dx
	mov	si,loadf			; code for floppies
	mov	di,mbr				; mbr + 0x3e
	add	di,03eh
	mov	cx,170				; copies only 170 bytes
sfbuc1:	mov	ah,[si]
	mov	[di],ah
	inc	si
	inc	di
	loop	sfbuc1
	mov	bx,mbr
	mov	cx,0001h			; starting in the sector 1, track 0
	pop	dx
	push	dx
	mov	ax,0301h			; save 1 sector (512b)
	int	13h
	jc	sferror1
	pop	dx
	mov	bx,start			; saves the entire program and configuration
	mov	ax,0101h			; starting in the sector 1, track 1
	mov	cx,3				; save 3 tracks
sfbuc2:	push	cx
	push	bx
	push	ax
	mov	cx,ax				; sector and track
	mov	ax,0312h			; saves 18 sectors
	int	13h
	jc	sferror3
	pop	ax
	pop	bx
	pop	cx
	inc	ah				; next track
	add	bx,9216				; next memory address (512*18)
	loop	sfbuc2
	pop	es
msg180:	mov	si,msucces
	xor	ax,ax
	call	message
	jmp	setup
sferror1:
	pop	dx
sferror2:
	pop	es
msg130:	mov	si,mgraba
	xor	ax,ax
	call	message
	jmp	setup
sferror3:
	pop	bx
	pop	cx
	jmp	sferror2


; ---------------------------------------------------------
;	はーどでぃすくから起動できるように
;	hard disk  - saves nboot in the hard disk -
; ---------------------------------------------------------

harddk:
	mov	dx,80h				; first, we test if the hard disk has
	mov	ah,08h				; the needed number of sectors per track
	push	es
	int	13h
	and	cl,3fh				; gets the 6 lower bits
	cmp	cl,nsect
	jle	sherr				; if is lower or equal, we return an error (equal because we must
	jmp	shcnt				; count the mbr too, wich is one more sector)
sherr:	mov	si,mgraba
	xor	ax,ax
	call	message
	jmp	setup
shcnt:	mov	dx,80h
	mov	byte [loadh.sflag],0		; default: we can.
	mov	si,ostabl			; we search for an entry with password
shsbuc:	cmp	byte [si],0ffh			; end of table?
	je	shaves2				; if true, continue
	add	si,20				; here is the flag that tells if this entry
	cmp	byte [si],0			; has or not a password.
	jne	shsend				; if is 1, this entry has password.
	add	si,20				; next entry
	jmp	shsbuc
shsend:	mov	byte [loadh.sflag],1		; we can't use safeboot: there's entries with passwords.
shaves2:
	push	es
	push	ds
	pop	es
	push	dx
	mov	cx,3
shabun:	push	cx
	mov	ax,201h
	mov	cx,0001h
	mov	bx,mbr
	int	13h				; load the boot sector (trying three times max)
	jnc	shaend
	pop	cx
	loop	shabun
	pop	dx
	pop	es
msg111:	mov	si,merrls
	xor	ax,ax
	call	message
	jmp	setup
shaend:	pop	dx				; empty the stack
	mov	word [mbr+510],0aa55h		; saves the boot signature
	mov	byte [mbr],0ebh
	pop	dx
	push	dx
	mov	di,mbr
	mov	si,loadh			; code for hard disks
	mov	cx,255				; copies only 255 bytes
shbuc1:		mov	ah,[si]
	mov	[di],ah
	inc	si
	inc	di
	loop	shbuc1
	mov	bx,mbr
	mov	cx,0001h
	pop	dx
	push	dx
	mov	ax,0301h			; save 1 sector
	int	13h
	jc	sherror1
	pop	dx
	mov	bx,start			; saves the entire program and configuration
	mov	cx,0002h			; starting in the sector 2
	mov	ah,03
	mov	al,nsect			; saves nsect sectors
	int	13h
	jc	sherror2
	pop	es
msg181:	mov	si,msucces
	xor	ax,ax
	call	message
	jmp	setup
sherror1:
	pop	dx
sherror2:
	pop	es
msg131:	mov	si,mgraba
	xor	ax,ax
	call	message
	jmp	setup


; ---------------------------------------------------------
;	ふろっぴーに書き込むこーど
;	load from floppy
;	this is the code saved in the mbr of the diskettes
; ---------------------------------------------------------

%define OFF(x)		(PART_OFF+(x-loadh))

loadf:
	cld
	xor	ax,ax
	mov	ds,ax
	mov	es,ax
	mov	si,7c55h			; start address of the code
	mov	di,PART_OFF			; address 0000:0600h
	mov	cx,100h				; 256 words
	repz
	movsw					; relocates the code
	jmp	0:0x616				; far jump to 0000:0616

.copy:	db	"NBoot: Now Loading...",90h	; address 0000:0600h

	mov	di,600h				; start address of the message 'NBoot: '
.loop:	mov	ah,0eh
	mov	bx,7
	mov	al,[di]
	cmp	al,90h				; end of the message?
	je	.boot				; if true, continue.
	push	di
	int	10h				; if false, print the letter
	pop	di
	inc	di
	jmp	.loop				; and loop

; loads	NBoot

.boot:
	mov	bx,start			; offset where NBoot is loaded
	mov	ax,KERNEL_SEG			; segment where NBoot is loaded
	mov	ds,ax
	mov	es,ax
	mov	ax,0101h			; sector 1, track 1
	mov	cx,3				; read three tracks
.track:	push	cx
	push	bx
	mov	cx,3				; try 3 times max
.load:	push	cx
	push	ax
	mov	dx,0h				; floppy disk 0, head 0
	mov	cx,ax				; sector and track
	mov	ax,212h				; 18 sectors, bios_load_sector
	int	13h				; load the mbr
	pop	ax
	pop	cx
	jnc	.suc
	loop	.load
	mov	al,'1'				; error 1, error reading a sector!
	jmp	.error
.suc:	inc	ah				; next track
	pop	bx
	add	bx,9216				; address where load the next track
	pop	cx
	loop	.track				; read next track

.run:	mov	ax,KERNEL_SEG			; NBoot's segment
	mov	es,ax
	mov	al,'3'				; error 3, NBoot is not in the disk!
	cmp	dword [es:start],'NBoo'		; tests for NBoot signature
	jne	.error
	cmp	word [es:start+4],0074h
	jne	.error
	mov	byte [es:flag],1		; load from bios
	jmp	KERNEL_SEG:main			; far jump to 1000:0120

.error:	mov	bx,7
	mov	ah,0eh
	int	10h				; prints the error passed in al
.halt:	jmp	.halt				; and locks the machine to allows user to read it.


; ---------------------------------------------------------
;	はーどでぃすくに書き込むこーど
;	load from hard disk
;	this is the code saved in the mbr of the hard disks
; ---------------------------------------------------------

loadh:
	cld
	xor	ax,ax
	mov	ds,ax
	mov	es,ax
	mov	si,BOOT_OFF			; start address of the code
	mov	di,PART_OFF			; address 0000:0600h
	mov	cx,100h				; 256 words
	repz
	movsw					; relocates the code
	jmp	0:OFF(.main)			; far jump to 0000:062e

.copy:	db	"NBoot: Now Loading...",90h	; address 0000:0617h
.sflag:	db	0				; address 062dh. if 1, don't allow safeboot
.ulba:	db	16,0,1,0			; address 062eh. lba table
	dw	BOOT_OFF,0			; address 0632h. segment:offset
.uls:	dd	0,0				; address 0636h. logical sector

.main:
	mov	di,OFF(.copy)			; start address of the message 'NBoot: '
.loop:	mov	ah,0eh
	mov	bx,7
	mov	al,[di]
	cmp	al,90h				; end of the message?
	je	.check				; if true, continue.
	push	di
	int	10h				; if false, print the letter
	pop	di
	inc	di
	jmp	.loop				; and close de loop

.check:
	mov	di,OFF(.sflag)
	cmp	byte [di],0			; if is 1, don't allow safeboot at this point
	jne	.boot				; to avoid the 'security hole'.
	mov	ah,2
	int	16h				; read keyboard flags
	test	ax,000fh			; Shift, Ctrl or Alt key pressed?
	jz	.boot				; if none is pressed, loads NBoot normally

; loads NBoot

.boot:
	mov	bx,start			; offset where NBoot is loaded
	mov	ax,KERNEL_SEG			; segment where NBoot is loaded
	mov	ds,ax
	mov	es,ax
	mov	cx,3				; try 3 times max
.load:	push	cx
	mov	dx,80h				; hard disk 0, head 0
	mov	cx,2				; sector 2, track 0
	mov	ah,2				; bios_load_sector
	mov	al,nsect			; nsect sectors to be loaded
	int	13h				; load the mbr
	pop	cx
	jnc	.run
	loop	.load
	mov	al,'1'				; error 1, error reading a sector!
.err:	jmp	.error

.run:	mov	ax,KERNEL_SEG
	mov	es,ax				; nboot's segment
	mov	al,'3'				; error 3, NBoot is not in the disk!
	cmp	dword [es:start],'NBoo'		; tests for NBoot signature
	jne	.err
	cmp	word [es:start+4],0074h
	jne	.err
	mov	byte [es:flag],1		; load from bios
	jmp	KERNEL_SEG:main			; far jump to 1000:0120

; Safe Boot

.safeboot:
	xor	ax,ax				; if not, enter safeboot
	mov	ds,ax
	mov	es,ax
	mov	si,7dbeh			; first entry in the partition table
	mov	cx,4				; max. 4 entries
.sloop:	cmp	byte [ds:si],80h		; active?
	jz	.safe				; boot that partition
	add	si,10h				; next entry
	loop	.sloop
	mov	al,'2'				; error 2, no active partition!
	jmp	.error
.safe:	mov	dl,80h				; here we load the boot sector of the active partition
	mov	ah,0x41				; check for BIOS extensions
	mov	bx,0x55aa
	int	0x13
	jc	.chs				; to CHS mode
	cmp	bx,0xaa55
	jnz	.chs				; to CHS mode
.lba:
;	mov	di,(.uls)			; LBA mode
	mov	di,OFF(.uls)			; LBA mode
	mov	cx,[ds:si+8]			; logical sector
	mov	[ds:di],cx
	mov	cx,[ds:si+10]			; logical sector
	mov	[ds:di+2],cx
	mov	ah,0x42				; extend read
;	mov	si,(.ulba)
	mov	si,OFF(.ulba)
	mov	bx,3				; try 3 times max
.sload:	push	ax
	push	si
	push	bx
	mov	dl,0x80
	int	0x13
	pop	bx
	pop	si
	pop	ax
	jnc	.srun
	dec	bx
	jnz	.sload
	mov	al,'1'				; error 1, error reading a sector!
	jmp	.error
.chs:
	mov	dl,80h
	mov	dh,[ds:si+1]			; head
	mov	cx,[ds:si+2]			; sector and track
	mov	bx,3				; try 3 times max
.cload:	push	bx
	push	cx
	push	dx
	mov	bx,BOOT_OFF			; address where we load the boot sector
	mov	ax,0201h			; one sector
	int	13h
	pop	dx
	pop	cx
	pop	bx
	jnc	.srun
	dec	bx
	jnz	.cload
	mov	al,'1'				; error 1, error reading a sector!
	jmp	.error
.srun:
	cmp	word [ds:7dfeh],0aa55h		; mbr signature?
	mov	al,'4'				; error 4, no boot sector found!
	jnz	.error
	jmp	0:BOOT_OFF

.error:
	mov	bx,7
	mov	ah,0eh
	int	10h				; prints the error passed in al
.halt:	jmp	.halt				; and locks the machine to allows user to read it.


; ---------------------------------------------------------
;	input
;	allows to type a string of up to 15 characters
;	and returns it in buff. prints the string
;	pointed by si in the column dl
; ---------------------------------------------------------

input:
	call	mclear
	push	si
	call	clmsg
	pop	si
	mov	byte [buff+15],0
	mov	byte [buff+16],0
	mov	byte [buff],"_"			; cursor
	mov	bx,gr+pink
	mov	dx,9*8
	call	printc
	mov	di,buff
	mov	bh,0				; 0 caracters entered
	call	refresh
.lp:
	push	bx
	call	waitkey
	pop	bx
	cmp	al,13				; cr?
	jnz	.del
	mov	byte [di],' '			; 32
	mov	byte [buff+15],0
	ret
.del:
	cmp	al,8				; delete?
	jnz	.dig
	cmp	bh,0				; head ?
	jz	.lp
	mov	byte [di],' '			; 32
	dec	di
	dec	bh
	mov	byte [di],"_"
	jmp	.print
.dig:	cmp	al,32				; ascii code < 32?
	jl	.lp
	cmp	byte [prpaswd],2		; if prpaswd=2, the user can type numbers only
	jne	.chr
	cmp	al,'0'				; 48
	jl	.lp
	cmp	al,'9'				; 57
	jg	.lp
.chr:	cmp	bh,15				; end ?
	je	.lp
	mov	[di],al
	inc	di
	inc	bh				; charactrer length
	mov	byte [di],"_"
.print:	call	refresh
	jmp	.lp

refresh:
	cmp	byte [prpaswd],1		; if prpaswd=1, input doesn't do echo
	je	.ret
	push	bx
	push	di
	push	dx
	xor	ax,ax				; clear
	mov	di,11*8*320
	mov	cx,8
	mov	dx,320/2
	call	fill
	mov	si,buff				; print
	mov	bx,white
	mov	cx,12*8
	mov	dx,11*8
	call	print
	pop	dx
	pop	di
	pop	bx
.ret:	ret


; ---------------------------------------------------------
;	メッセージエリアをクリア
;	Clear the message area
;	di:start postion, dx:width, cx:height, ax:color
; ---------------------------------------------------------

mclear:
	xor	ax,ax
	mov	di,58*320
	mov	cx,200-58-50
	mov	dx,320/2
;	call	fill
;	ret


; ---------------------------------------------------------
;	塗りつぶす
;	Fill
;	di:start postion, dx:width, cx:height, ax:color
; ---------------------------------------------------------

fill:
	push	cx
	push	di
	mov	cx,dx
	cld
	rep
	stosw
	pop	di
	pop	cx
	add	di,320
	loop	fill
	ret


; ---------------------------------------------------------
;	画面クリア
;	cls
; ---------------------------------------------------------

cls:
	cld
	xor	ax,ax
	xor	di,di
	mov	cx,320*200/2
	rep
	stosw
	ret


; ---------------------------------------------------------
;	一文字表示(Pro)
;	ax:character, bh:flag, bl:color, cx:x, dx:y
; ---------------------------------------------------------

putchar:
	xor	ah,ah
	sal	ax,3			; *8
	add	ax,character		; character table
	mov	si,ax

	xor	al,al			; vram address
	mov	ah,dl			; *256
	sal	dx,6			; *64
	add	dx,ax			; *256 + *64 = *320
	add	dx,cx
	mov	di,dx

	xor	ah,ah
	mov	cx,8
.lp:	push	cx
	mov	al,[si]			; font data
	inc	si
	or	ah,al			; for pro

	mov	cx,8
.loop:	sal	al,1			; left shift
	jnb	.next
	mov	[es:di],bl		; pset
.next:	inc	di
	loop	.loop

	add	di,312
	pop	cx
	add	bl,bh
	loop	.lp

	ret


; ---------------------------------------------------------
;	文字列表示(Pro)
;	si:string, bx:color, cx:x, dx:y
; ---------------------------------------------------------

print:
	mov	al,[si]
	inc	si
	or	al,al			; end ?
	retz
	cmp	al,' '			; space ?
	jz	.sp
	push	bx
	push	cx
	push	dx
	push	si
	call	putchar			; print
	mov	cx,8
	mov	al,9
.lp:	sar	ah,1			; right shift
	jb	.end			; carry flag is 1
	dec	al
	loop	.lp
.end:	pop	si
	pop	dx
	pop	cx
	pop	bx
	xor	ah,ah
	add	cx,ax
	jmp	print
.sp:	add	cx,7			; x+=7
	jmp	print


; ---------------------------------------------------------
;	真中に文字を表示
;	print center
;	prints a string pointed by SI at dx=row, but centered.
; ---------------------------------------------------------

;printc:
;	push	si
;	mov	cx,40			; 40 columns
;.lp:	cmp	byte [si],0
;	jz	.prc
;	inc	si
;	dec	cx
;	jmp	.lp
;.prc:	sal	cx,2			; divide by 2 (*2)
;	pop	si
;	jmp	print


; ---------------------------------------------------------
;	真中に文字を表示(Pro)
;	print center
;	prints a string pointed by SI at dx=row, but centered.
; ---------------------------------------------------------

printc:
	push	si
	xor	cx,cx
.loop:
	mov	al,[si]
	inc	si
	or	al,al			; end ?
	jz	.calc
	cmp	al,' '			; space ?
	jz	.sp

	xor	ah,ah
	sal	ax,3			; *8
	add	ax,character		; character table
	mov	di,ax
	mov	ah,[di]
	inc	di
	or	ah,[di]
	inc	di
	or	ah,[di]
	inc	di
	or	ah,[di]
	inc	di
	or	ah,[di]
	inc	di
	or	ah,[di]
	inc	di
	or	ah,[di]
	inc	di
	or	ah,[di]

	push	cx			; get one character size
	mov	cx,8
	mov	al,9
.lp:	sar	ah,1			; right shift
	jb	.end			; carry flag is 1
	dec	al
	loop	.lp
.end:	pop	cx
	xor	ah,ah			; add one character size
	add	cx,ax
	jmp	.loop
.sp:
	add	cx,7			; add space size
	jmp	.loop

.calc:
	mov	ax,40*8			; 40 columns
	sub	ax,cx			; 320-size
	sar	ax,1			; divide by 2
	mov	cx,ax
	pop	si
	jmp	print


; ---------------------------------------------------------
;	アイコン一つを表示
;	ah:icon, al:position
;	print icon
;	prints the icon given in ah at the position given by al
;	and the name, given in buff
;	if prinam is 1, doesnt print the name
; ---------------------------------------------------------

picon:
	mov	cx,11*8
	mov	dx,8*8
	xor	ah,ah
	add	dx,ax
	mov	byte [buff+17],0
	mov	bx,white
	mov	si,buff
	call	print
;	cmp	byte [prinam],0		; has to print the names?
;	je	picon2
	ret
;picon2:	mov	cx,10
;	add	dx,8
;	mov	si,mthek		; key
;	jmp	print			; calls prstr and return


; ---------------------------------------------------------
;	アイコンの表示
;	put icon
;	puts the os in the screen. only if prstp=0, prints the setup icon.
; ---------------------------------------------------------

puticon:
;	mov	byte [prinam],0			; print icons and names
	mov	al,0				; first position
	mov	bl,'1'				; key 1
	mov	si,ostabl
putlp:	mov	ah,[si]				; icon number
	cmp	ah,0ffh				; quit
	jz	putic2
	inc	si
	mov	di,buff
	mov	[di],bl
	inc	di
	mov	byte [di],' '
	inc	di
	mov	cx,0fh
putlp2:	mov	bh,[si]
	mov	[di],bh
	inc	si
	inc	di
	loop	putlp2
	push	si
	push	ax
	push	bx
;	mov	[thekey],bl
	call	picon
	pop	bx
	pop	ax
	pop	si
	add	si,24
	inc	bl
	add	al,8
	jmp	putlp
putic2:	sub	bl,'0'				; for select function
	mov	[slmax],bl
	cmp	byte [prstp],0
	jne	putic3
	mov	di,buff
	mov	word [di],2003h			; ' S'
	inc	di
	inc	di
	mov	si,msetp2
	mov	cx,10h
putlp3:	mov	bh,[si]
	mov	[di],bh
	inc	si
	inc	di
	loop	putlp3
;	mov	byte [thekey],keysetup		; setup key
	call	picon				; setup icon
putic3:	ret


; ---------------------------------------------------------
;	タイトル表示
; ---------------------------------------------------------

pnboot:
	call	cls

	mov	bx,gr+pink
	mov	si,mnboot			; NBoot version 1.00
	mov	cx,11*8
	mov	dx,10
	call	print
	mov	si,mcopy			; (C)2000 Yuichiro Nakada
	mov	cx,8*8
	mov	dx,18
	call	print

	cld					; window
	mov	ax,white-7
	mov	cx,19*8/2
	mov	di,57*320+84
	rep
	stosw
	mov	cx,19*8/2
	mov	di,(200-50)*320+84
	rep
	stosw

	mov	si,picture			; picture
	mov	ax,280
	mov	bx,152
	mov	dx,32
	mov	cx,40
	call	draw

	ret


; ---------------------------------------------------------
;	メインメニュー
;	main menu  - prints the main menu, with the icons -
; ---------------------------------------------------------

mainmenu:
	call	pnboot				; title
	mov	ax,101h
.msg:	mov	si,mmenu
	call	message
	mov	byte [prstp],0
	call	puticon
	call	textwav

	mov	al,[toboot]
	sub	al,'1'
	mov	byte [slpos],al
	mov	word [slbpos],10*8+(8*8+7)*320
	mov	word [slbwd],160
	mov	word [slspos],10*8
	mov	byte [slcl],0
	mov	ax,blue*256+purple
	mov	bx,gr+yellow
	call	select_

	ret


; ---------------------------------------------------------
;	window
; ---------------------------------------------------------

window:
	cld
	mov	ax,(blue+7)*256+(blue+7)
	mov	cx,(273-47)/2
	mov	di,157*320+47
	rep
	stosw
	mov	ax,blue*256+blue
	mov	cx,(273-47)/2
	mov	di,(157+30)*320+47
	rep
	stosw
	mov	ax,blue+7
	mov	cx,27
	mov	di,45+159*320
.lp:	mov	[es:di],al
	add	di,274-45
	mov	[es:di],al
	add	di,320-(274-45)
	inc	ah
	cmp	ah,4
	jne	.next
	mov	ah,0
	dec	al
.next:	loop	.lp
	mov	[es:46+158*320],al
	mov	[es:273+158*320],al
	mov	[es:46+186*320],al
	mov	[es:273+186*320],al
	ret


; ---------------------------------------------------------
;	select
;	ax:line color, bx:text color
; ---------------------------------------------------------

select:
	push	ax				; erase
	mov	ax,white
	mov	byte [slcl],blue
;	xor	ax,ax
	xor	bx,bx
	call	select_
	pop	ax
	add	al,[slpos]			; locate
	cmp	al,0ffh
	jz	sljmp
	cmp	al,[slmax]
	jl	sljmp2
	xor	al,al
	jmp	sljmp2
sljmp:	mov	al,[slmax]
	dec	al
sljmp2:	mov	[slpos],al
	mov	ax,blue*256+purple
	mov	byte [slcl],0
;	mov	ax,purple*256+purple
	mov	bx,gr+yellow

select_:
	push	ax
	mov	al,1				; ☆
;	mov	bx,gr+yellow
;	mov	cx,10*8
	mov	cx,[slspos]
	xor	dh,dh
	mov	dl,[slpos]
	add	dx,dx
	add	dx,dx
	add	dx,dx
	push	dx
	add	dx,8*8
	call	putchar
	pop	dx
	pop	ax

;	mov	ax,purple*256+purple
;	mov	cx,160/2
;	mov	di,10*8 + (8*8+7)*320
	mov	cx,[slbwd]
	mov	di,[slbpos]
	add	dx,dx				; *2
	add	dx,dx				; *4
	add	dx,dx				; *8
	add	dx,dx				; *16
	add	dx,dx				; *32
	add	dx,dx				; *64
	add	di,dx
	add	dx,dx				; *128
	add	dx,dx				; *256
	add	di,dx
;	rep
;	stosw

sellp:	mov	bh,al
	mov	bl,[es:di]
	cmp	bl,[slcl]
	jz	seljp
	mov	bh,ah
seljp:	mov	[es:di],bh
	inc	di
	loop	sellp

	ret


slpos	db	0				; selected position
slmax	db	2				; max selection
slbpos	dw	10*8 + (8*8+7)*320		; bar position
slbwd	dw	160				; bar width
slspos	dw	10*8				; star position
slcl	db	0				; color


; ---------------------------------------------------------
;	graph
; ---------------------------------------------------------

graph:
	mov	ax,[count]			; count * 100 / 36
	mov	cx,100
	mul	cx
	mov	cx,36
	div	cx

	mov	cl,[time]			; count / time
	div	cl
	mov	ah,0
	mov	dx,ax
	sar	dx,1

	cld
	mov	ax,(yellow)*256+(yellow)
	mov	di,5*8*320+110
	mov	cx,8
.lp:	push	cx
	mov	cx,dx
	rep
	stosw
	add	ax,808h
	mov	cx,100/2
	sub	cx,dx
	rep
	stosw
	sub	ax,808h
	add	di,320-100
	add	ax,101h
	pop	cx
	loop	.lp

	ret


; ---------------------------------------------------------
;	キー入力を待つ
;	waitkey
;	waits for a keystroke and return it in al.
;	also decrements the boot timer counter
;	and returns the ascii in toboot if it reaches 0
; ---------------------------------------------------------

waitkey:
	cmp	byte [counter],0	; timer started?
	je	wnotim
	in	al,42h
	in	al,42h			; reads the high byte of the timer
	cmp	al,127			; is greater than 127?
	jl	wnotim
	mov	ax,[count]
	dec	ax
	cmp	ax,1
	jnz	wkjmp
	mov	byte [counter],0	; stops the timer
	mov	al,[toboot]		; and returns the ascii of the os to boot
	ret
wkjmp:	mov	[count],ax
	mov	al,0b0h
	out	43h,al
	mov	al,0ffh
	out	42h,al
	mov	al,0ffh
	out	42h,al
	call	graph
wnotim:	mov	ah,1			; push key?
	int	16h
	jz	waitkey
	mov	byte [counter],0	; stops the timer
readkey:
	mov	ah,0
	int	16h

; code for keyboard translation, used to add support for azerty and qwertz keyboards.

	push	si
	mov	si,kbtabl
azlp:	cmp	byte [si],0		; end of table?
	jz	azend			; end loop
	cmp	al,[ds:si]		; key found?
	jz	azfnd
	inc	si
	inc	si			; next key in the table
	jmp	azlp			; close loop
azfnd:	inc	si
	mov	al,[ds:si]		; change the readed key for the translated key
azend:	pop	si

; end of keyboard translation support code

endkey:	ret


; ---------------------------------------------------------
;	tables for qwertz and azerty support
;	uncoment it to add support.
; ---------------------------------------------------------

kbtabl:
	; kbtabl points to the translation tables. them contains the ascii code	readed
	; from the keyboard and the ascii code translated.

; table for azerty keyboards. uncoment it to add support.
;	db	"a","q"		; 'a' key is translated as 'q' key
;	db	"q","a"		; 'q' key is translated as 'a' key
;	db	"a","q","q","a","w","z","z","w","w","z","z","w"
;	db	":","m","m",":",";","m","m",";"

; table for qwertz keyboards. uncoment it to add support.
;	db	"z","y"		; 'z' key is translated as 'y' key
;	db	"y","z"		; 'y' key is translated as 'z' key
;	db	"z","y","y","z"

	db	0		; end of table


; ---------------------------------------------------------
;	toasc
;	converts the value of al into an hex ascii code
; ---------------------------------------------------------

toasc:
	and	al,0fh
	cmp	al,9
	jbe	.jp
	add	al,'A'-10		; 55
	ret
.jp:	add	al,'0'			; 48
	ret


; ---------------------------------------------------------
;	clmsg
;	clears the temporary buffer with spaces
; ---------------------------------------------------------

clmsg:	mov	si,buff
	mov	cx,39
.loop:	mov	byte [si],32
	inc	si
	loop	.loop
	mov	byte [si],0
	ret


; ---------------------------------------------------------
;	垂直帰線を待つ
; ---------------------------------------------------------

vsync:
	mov	dx,3dah
.vrt:	in	al,dx
	test	al,8
	jnz	.vrt			; wait until Verticle Retrace starts
.novrt:	in	al,dx
	test	al,8
	jz	.novrt			; wait until Verticle Retrace Ends
	ret


; ---------------------------------------------------------
;	パレット設定
;	si:palette, al:first palette, cx:n
; ---------------------------------------------------------

setpal:
	mov	dx,3c8h
	out	dx,al
	inc	dx
.lp:	mov	al,[si]		; r
	inc	si
	shr	al,2
	out	dx,al
	mov	al,[si]		; g
	inc	si
	shr	al,2
	out	dx,al
	mov	al,[si]		; b
	inc	si
	shr	al,2
	out	dx,al
	loop	.lp
	ret


; ---------------------------------------------------------
;	絵を表示
;	si:picture, ax:x, bx:y, cx:sy, dx:sx
; ---------------------------------------------------------

draw:
	mov	di,ax			; vram address
	mov	ah,bl			; *256
	xor	al,al
	add	di,ax
	sal	bx,6			; *64
	add	di,bx			; *256 + *64 = *320
	cld
.lp:	push	cx
	push	di
	mov	cx,dx
	rep
	movsb
	pop	di
	pop	cx
	add	di,320
	loop	.lp
	ret


; ---------------------------------------------------------
;	フェードイン
; ---------------------------------------------------------

textfin:
	mov	bh,20
	mov	bl,bh
	sub	bl,cl
	mov	al,white
	mov	dx,03c8h
	out	dx,al
	inc	dx
	mov	al,49		; 255 - 58
	mul	bl
	div	bh
	add	al,14
	out	dx,al
	mov	al,36		; 255 - 110
	mul	bl
	div	bh
	add	al,27
	out	dx,al
	mov	al,21		; 255 - 170
	mul	bl
	div	bh
	add	al,42
	out	dx,al
	ret


; ---------------------------------------------------------
;	波
;	di:wave table
; ---------------------------------------------------------

wave:
	mov	si,10
;	mov	cx,200
;	mov	si,10+58*320
;	mov	cx,200-58-51
	mov	cx,200-49
	xor	bx,bx
	cld

wavelp:	push	cx
	push	di
	mov	al,[di+bx]
	cbw
	inc	bx
	and	bx,15

	push	si
	mov	di,buff
	mov	cx,300/2
	push	es
	push	ds
	pop	es
	pop	ds
	rep
	movsw
	pop	di

	push	di
	add	di,ax
	mov	si,buff
	mov	cx,300/2
	push	es
	push	ds
	pop	es
	pop	ds
	rep
	movsw
	pop	si

	add	si,320
	pop	di
	pop	cx
	loop	wavelp
	ret


; ---------------------------------------------------------
;	水文字
; ---------------------------------------------------------

textwav:
	mov	di,wavetbl
	mov	cx,20
.lp:	push	cx
	call	textfin
	call	wave
	call	vsync
	pop	cx
	add	di,16
	loop	.lp
	ret


; ---------------------------------------------------------
;	include asm file
; ---------------------------------------------------------

%include "atapi.h"
%include "hd_io.h"
%include "tool.h"
%include "hd_io.asm"
%include "int13h.asm"
%include "data.asm"

%include "utils.asm"


; ---------------------------------------------------------
;	データ
; ---------------------------------------------------------

; define of colour

gr	equ	0ff07h	; gradation
white	equ	255
pink	equ	248
purple	equ	240
blue	equ	232
yellow	equ	224


; messages of nboot

%include "nboot.msg"

mok	db	1," OK ",1,0

; messages of title

mnboot	db	"NBoot version 1.03",0
mcopy	db	"@ 2000-2004 Yuichiro Nakada ",2,0

flag	db	0		; Where is NBoot from ?
;buff	dup	300
buff	times 300 db 0


tpaswd	dup	15,' '		; here we save the password for setup
ispaswd	db	0		; 0 if there's no password. 1 if is defined


; activ contains the partition types that can be hidden during a boot

activ:	db	01h,04h,06h,07h,0bh,0ch,0eh,0h


; inactiv contains the partition types that can be unhidden during a boot

inact:	db	11h,14h,16h,17h,1bh,1ch,1eh,0h


; the character table of nboot. each character is 8x8 pixels

character:
%include "nboot.fnt"

%include "opstar.asm"	; The picture table of NBoot


; wave table

wavetbl:
	db	 0, 1, 2, 3, 3, 3, 2, 0, 0,-2,-3,-3,-3,-2,-1, 0
	db	 1, 1, 1, 0, 0,-1,-2, 0,-2,-1, 0, 0, 1, 1, 1, 1
	db	 1, 1, 0, 0,-1,-2, 0,-2,-1, 0, 0, 1, 1, 1, 1, 1
	db	 1, 0, 0,-1,-2, 0,-2,-1, 0, 0, 1, 1, 1, 1, 1, 1
	db	 0, 0,-1,-2, 0,-2,-1, 0, 0, 1, 1, 1, 1, 1, 1, 0
	db	 0,-1,-2, 0,-2,-1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0
	db	-1,-2, 0,-2,-1, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0,-1
	db	-2, 0,-1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0,-1,-2,-2
	db	 0,-1,-1,-1, 0, 1, 1, 1, 1, 1, 0, 0,-1,-1,-1, 0
	db	-1,-1, 0, 1, 1, 1, 1, 1, 1, 0, 0,-1,-1,-1, 0,-1
	db	-1, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0,-1,-1, 0,-1,-1
	db	 0, 0, 1, 1, 1, 0, 0, 0, 0, 0,-1,-1, 0,-1,-1, 0
	db	 0, 1, 1, 1, 0, 1, 1, 0,-1,-1,-1, 0,-1, 0, 0, 0
	db	 1, 1, 0, 0, 1, 0,-1,-1, 0,-1, 0,-1, 0, 0, 1, 1
	db	 1, 0, 0, 1, 0, 0, 0,-1,-1, 0, 0, 0, 0, 0, 0, 1
	db	 0, 0, 0, 0, 0, 0,-1, 0, 0, 0,-1, 0, 0, 1, 1, 0
	db	 0, 0, 0,-1,-1,-1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0
	db	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
	db	 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0


multil	db	1		; flag for multi languages

time	db	0		; if time=0, nboot doesn't use timer. if not, waits time seconds
toboot	db	49		; here nboot saves the default os in ascii (49 for 1, 50 for 2...)

mbr	dup	512		; temporal place to put the partition tables
ptable	dup	200		; temporal place to put the data boot
				; (20 partitions with 4 bytes each one)

prstp	db	0		; temporal place to decide if prints the setup icon

drives	db	0		; temporal place to save the actual drive

exten	db	0		; tells if i have to load extended partitions and where
exten1	dw	0
exten2	dw	0

prpaswd	db	0		; if 1, input don't display the keystrokes.

counter	db	0		; this variable decides if we count time or not
count	dw	0		; here we saves the counter

tempo	dw	0		; temporal place
pfoun	db	0
phidd	db	0

nsect	equ	38		; number of sectors to save in hard disk


cdrom_ioports	dw	0,0
%if 0
; structure for boot record, including removable drives and partitions
struc struc_bootrecord
	.flags		: resw 1 ; type flags of this record, see INFOFLAG_x
	.drive_id	: resb 1 ; drive id = 0 to 255
				 ; partition id used in linux,
	.part_id	: resb 1 ; 1-4 for primary partitions,
				 ; > 5 for logical partitions,
				 ; 0 for driver or special bootrecord.
	.type		: resb 1  ; partition type, = 0 : not a partition
	.reserved	: resb 1  ;
	.father_abs_addr : resd 1  ; father's LBA address
	.abs_addr	: resd 1  ; partition's abs LBA address
	.password	: resd 1  ; password of this record
	.schedule_time	: resd 1  ; schedule time
	.name		: resb 16 ; name of this record, zero ending.
	.keystrokes	: resw 13 ; keystrokes to be preloaded.
	.end_of_struc
endstruc

;%define SIZE_OF_BOOTRECORD (struc_bootrecord.end_of_struc)
;%define MAX_RECORD_NUM      32

; buffer to store boot records
;boot_records	times MAX_RECORD_NUM * SIZE_OF_BOOTRECORD db 0

%endif

;tmp_cdemu_spec	resb SIZE_OF_CDEMU_SPEC
tmp_cdemu_spec	dup SIZE_OF_CDEMU_SPEC

;disk_buf	resb 512
disk_buf	dup 512
;disk_buf1	resb 2048
disk_buf1	dup 2048

; ---------------------------------------------------------
;	boot
;	ax =  the boot record number.
; ---------------------------------------------------------

do_boot_record:
;	mov	bl, SIZE_OF_BOOTRECORD
;	mul	bl

;	lea	si, [boot_records]
;	add	si, ax

;	mov bx, [si + struc_bootrecord.flags]

;	test bx, INFOFLAG_ISSPECIAL
;	jz .boot_drv_part

;	call do_special_record
;	jmp .end_clean

	mov	dl,0xe0

.boot_drv_part:
%ifndef DISABLE_CDBOOT
;	test bx, DRVFLAG_ISCDROM
;	jz .normal_boot

;	mov	dl, [si + struc_bootrecord.drive_id]
	mov	di, disk_buf1
	call	get_cdrom_boot_catalog		; boot 情報を取得
	jc	.disk_error

	push	si
	mov	si, di				; buffer
	mov	di, disk_buf			; entries buffer
	call	find_boot_catalog
	pop	si

	or	cx, cx				; no cdrom
	jz	.no_system
	cmp	cx, 1				; one cdrom images
	je	.go_boot_cdrom

;	push	si				; choose cdrom images
;	mov	si, di
;	call	choose_cdimg
;	pop	si
;	jc	.end_clean

;	mov	cl, SIZE_OF_BOOT_CATALOG
;	mul	cl

;	add	di, ax

.go_boot_cdrom:
;	push dx
;	push di
;	call preload_keystrokes     ; preload the keystrokes into key buffer.
;	call reset_video_mode
;	pop di
;	pop dx
	call boot_cdrom
	jmp short .boot_fail

%endif

;.normal_boot:
;	call boot_the_record

.boot_fail:
	or al, al
	jz .no_system

.disk_error:
;	call show_disk_error
	mov	ax,101h
	mov	si,.err
	call	message
	ret

.err	db	"disc error",0

.no_system:
;	mov si, [str_idx.no_system]
;	call error_box
	mov	ax,101h
	mov	si,.nosys
	call	message
	ret

.nosys	db	"no system",0

.end_clean:
;	call draw_screen
	ret


%ifndef DISABLE_CDBOOT
; ---------------------------------------------------------
;	boot cdrom driver
;
;	ds:di -> boot catalog
;	dl = cdrom drvid
; ---------------------------------------------------------

boot_cdrom:
	mov	al,[di+1]
	and	al,0x0f

	mov	si,tmp_cdemu_spec
	mov	byte [si],SIZE_OF_CDEMU_SPEC
	mov	[si + struc_cdemu_spec.media_type],al

	xor	ah,ah
	or	al,al
	jnz	.floppy_emu
	mov	ah,dl

.floppy_emu:
	mov	byte [si + struc_cdemu_spec.emu_drvid],ah	; drive id
	mov	ebx,[di+0x08]
	mov	[si + struc_cdemu_spec.image_lba], ebx
	mov	bx, [di+0x02]
	mov	[si + struc_cdemu_spec.load_seg], bx
	mov	bx, [di+0x06]
	mov	[si + struc_cdemu_spec.sect_count], bx
	mov	byte [si + struc_cdemu_spec.cylinders], 0x50
	mov	byte [si + struc_cdemu_spec.heads], 2

	mov	bl,al
	xor	bh,bh
	mov	ah,[.sect_nums + bx]

	mov	byte [si + struc_cdemu_spec.sectors], ah
	xor	ax,ax
	mov	[si + struc_cdemu_spec.user_bufseg], ax

;Boot it!
	mov	ax,0x4c00	; si:cdemu_spec
	int	0x13

	ret

.sect_nums  db	0, 0x0f, 0x12, 0x24


; ---------------------------------------------------------
;find_boot_catalog ---- find boot catalog entry from buffer
;input: ds:si -> buffer  es:di -> entries buffer
;return: cx = number of entries
; ---------------------------------------------------------

find_boot_catalog:
	push	si
	push	di
	push	ax
	cld

	xor	cx, cx

	cmp	word [si], 0x0001
	jne	.end
	cmp	word [si+0x1e], 0xaa55
	jne	.end

.loop_find:
	mov	al, [si + struc_boot_catalog.indicator]
	or	al, al
	jz	.end

	cmp	al, 0x88
	jne	.loop_next

	mov	al, [si + struc_boot_catalog.media_type]
	and	al, 0x0f
	cmp	al, 4
	jae	.loop_next

	push	cx
	push	si
	mov	cx, SIZE_OF_BOOT_CATALOG
	rep	movsb
	pop	si
	pop	cx
	inc	cx

.loop_next:
	add	si, SIZE_OF_BOOT_CATALOG
	jmp short	.loop_find

.end:
	pop	ax
	pop	di
	pop	si
	ret

%endif
