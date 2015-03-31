; ---------------------------------------------------------
;	NBoot
;
;		(C)2000-2004 NAKADA
; ---------------------------------------------------------

;These are the messages of NBoot. This is the file you need to
;translate if you want to have NBoot in your own language.

;The text must end in zero. That's why there's the ',0' at the
;end of each one.

;each type of text has a maximum length.

%include "hiragana.h"


; ---------------------------------------------------------
;	Messages 'presentation' (up to 25 characters)
; ---------------------------------------------------------

mmenu_	db	"Select ",24,",",26," key,",0,"And ",10," key to boot",0
mossel_	db	"Press 1-9 to select an OS",0,"or ESC to abort",0
mosdel_	db	"Press 1-9 to delete an OS",0,"or ESC to abort",0
mopsel_	db	"Press A-Z to select an OS",0,"or 1-8 to select a Hard Disk",0
mpartt_	db	"Partition type",0

mmenu	db	24,",",26,_de,_toten,_se,_n,_ta,_ku,_shi,_te,0,10,_de,_toten,_ke,_xtu,_te,_i,_shi,_ma,_su,_kuten,0
mossel	db	"1-9",_de,_toten,"OS",_wo,_e,_ra,_n,_de,_ku,_da,_sa,_i,_kuten,0
	db	"ESC",_de,_toten,_ki,_xya,_n,_se,_ru,_shi,_ma,_su,_kuten,0
mosdel	db	"1-9",_de,_toten,_ke,_su,"OS",_wo,_e,_ra,_n,_de,_ku,_da,_sa,_i,_kuten,0
	db	"ESC",_de,_toten,_ki,_xya,_n,_se,_ru,_shi,_ma,_su,_kuten,0
mopsel	db	"A-Z",_de,_toten,"OS",_wo,_e,_ra,_n,_de,_ku,_da,_sa,_i,_kuten,0
	db	"1-8",_de,_toten,_ha,_kara,_do,_de,_xi,_su,_ku,_wo,_ka,_e,_ra,_re,_ma,_su,_kuten,0
mpartt	db	pa_,_kara,te_,xi_,shi_,xyo_,n_,_no,_shi,_xyu,_ru,_i,0


; ---------------------------------------------------------
;	messages 'input' (up to 36 characters)
; ---------------------------------------------------------

mtimer_	db	"Seconds? (",10,",0 disables)",0
mdescr_	db	"Type a description (up to 15 chars)",0
mentpw_	db	"Enter password",0
mnewpw_	db	"Password? (RETURN to none)",0

mtimer	db	_na,_n,_bi,_xyo,_u,_ni,_shi,_ma,_su,_ka,"? (",10,",0",_de,_ki,_xya,_n,_se,_ru,")",0
mdescr	db	_to,_u,_ro,_ku,_me,_i,_wo,_i,_re,_te,_ku,_da,_sa,_i,_kuten,"(15",_mo,_ji,_ma,_de,")",0
mentpw	db	_pa,_su,_wa,_kara,_do,_wo,_i,_re,_te,_ku,_da,_sa,_i,_kuten,0
mnewpw	db	_pa,_su,_wa,_kara,_do,_wo,_i,_re,_te,_ku,_da,_sa,_i,_kuten,"(RETURN",_de,_ki,_xya,_n,_se,_ru,")",0


; ---------------------------------------------------------
;	messages 'error' (up to 34 characters)
; ---------------------------------------------------------

mbdisa_	db	"Boot timer disabled ",2,0
mberr_	db	"Maximum time is 99 seconds ",2,0
merrls_	db	"Disk drive not ready ",4,0
merrms_	db	"Boot sector not found",0,"or invalid ",4,0
mgraba_	db	"Disk error ",5,0
mincor_	db	"Password incorrect ",4,0
mnoos_	db	"There aren't OS to delete",0
mnoent_	db	"No more entries availables",0
mpsdis_	db	"Password disabled ",2,0
msuces_	db	"NBoot installed succesfully",0
mswap_	db	"Exchange drive letters?",0

mbdisa	db	bu_,_kara,to_,ta_,i_,ma_,_kara,_ha,_ki,_xya,_n,_se,_ru,_shi,_ma,_shi,_ta,2,0
mberr	db	_sa,_i,_da,_i,_ji,_ka,_n,_ha,"99",_bi,_xyo,_u,_de,_su,2,0
merrls	db	_de,_xi,_su,_ku,do_,ra_,i_,bu_,_no,_ji,_xyu,_n,_bi,_ga,_de,_ki,_te,_i,_ma,_se,_n,4,0
merrms	db	bu_,_kara,to_,se_,ku_,ta_,_ga,_mi,_tu,_ka,_ri,_ma,_se,_n,_kuten,0
	db	_yo,_me,_ma,_se,_n,_de,_shi,_ta,5,0
mgraba	db	_de,_xi,_su,_ku,e_,ra_,_kara,5,0
mincor	db	_pa,_su,_wa,_kara,_do,_ga,_chi,_ga,_i,_ma,_su,4,0
mnoos	db	_na,_ni,_mo,_to,_u,_ro,_ku,_sa,_re,_te,_i,_ma,_se,_n,4,0
mnoent	db	_sa,_i,_da,_i,_to,_u,_ro,_ku,_su,_u,_wo,_ko,_e,_ma,_shi,_ta,4,0
mpsdis	db	_pa,_su,_wa,_kara,_do,_ha,_ki,_xya,_n,_se,_ru,_shi,_ma,_shi,_ta,2,0
msucces	db	"NBoot",_no,i_,n_,su_,to_,_kara,ru_,_ni,_se,_i,_ko,_u,_shi,_ma,_shi,_ta,2,0
mswap	db	do_,ra_,i_,bu_,_wo,_he,_n,_ko,_u,_shi,_ma,_su,_ka,"?",0


msg:
; ---------------------------------------------------------
;	Setup menu (Max. 30 characters)
; ---------------------------------------------------------

.add	db	"Add a new Operating System",0
.del	db	"Delete an Operating System",0
.savefd	db	"Save in Floppy",0
.savehd	db	"Save in Hard disc",0
.btimer	db	"Boot timer",0
.pass	db	"Setup password",0
.option	db	"Options",0
.return	db	"Return to main menu",0


; ---------------------------------------------------------
;	Option Menu
; ---------------------------------------------------------

.lang	db	1," Language ",1,0
.eng	db	"English",0
.jp	db	_ni,_ho,_n,_go,0


; ---------------------------------------------------------
;	Yes/No
; ---------------------------------------------------------

myesno	db	"Yes/No",0


; ---------------------------------------------------------
;	Other messages
; ---------------------------------------------------------

mdisk	db	"A  Boot from floppy",0		; up to 36 chars. WARNING with the spaces
msetp2	db	"Setup NBoot",0			; up to 15 chars
mdos	db	"MS-DOS FAT",0
mwin	db	"Windows FAT32",0
mos2	db	"WinXP NTFS/HPFS",0
mbm	db	"OS/2 Boot Manager",0
mlinux	db	"Linux EXT3",0
mbsd	db	"FreeBSD",0
mbeos	db	"BeOS",0


; ---------------------------------------------------------
;	Menu keys
; ---------------------------------------------------------

;These are the keys for each menu option.
;You must put the same letters than
;in the HotKeys, and must be in lowercase

key.add		equ	"a"	; Add an OS
key.del		equ	"d"	; Delete an OS
key.fd		equ	"f"	; Save in Floppy
key.hd		equ	"h"	; Save in Hard disk
key.timer	equ	"b"	; Boot timer
key.pass	equ	"p"	; Setup Password
key.ret		equ	"r"	; Return to main menu
key.yes		equ	"y"	; Key for 'Yes'
key.no		equ	"n"	; Key for 'No'
key.op		equ	"o"	; Option


; ---------------------------------------------------------
;	Setup keys
; ---------------------------------------------------------

;here is the key used to enter setup from the
;main menu

keysetup	equ	"s"	; Setup key in Lowercase


; ---------------------------------------------------------
;	table of Operating Systems
; ---------------------------------------------------------

; 9 entries with 40 bytes each one

	db	1			; 1 byte: icon (0FFh=entry not used)
	db	"Diskette       "	; 15 bytes: OS'name (completed with blank spaces)

	db	0			; 1 byte: tells if there's or not password
	dup	15			; 15 bytes: stores the password

	db	0			; 1 byte: drive unit (0=A: 1=B: 80h=HD1 81h=HD2 ...)
	db	0			; 1 byte: swap drive (if different to 0)
	db	0			; 1 byte: access mode
	db	0			; 1 byte: start head
	db	1			; 1 byte: start sector
	db	0			; 1 byte: start track
	dd	0			; 4 bytes: relative sector
	dd	0			; 4 bytes: reserved

ostabl:
	db	1			; 1 byte: icon (0FFh=entry not used)
	db	"Diskette       "	; 15 bytes: OS'name (completed with blank spaces)

	db	0			; 1 byte: drive unit (0=A: 1=B: 80h=HD1 81h=HD2 ...)
	db	0			; 1 byte: start head
	db	1			; 1 byte: start sector
	db	0			; 1 byte: start track
	db	0			; 1 byte: tells if there's or not password
	dup	15			; 15 bytes: stores the password
	db	0			; 1 byte: swap drive (if different to 0)
	db	0			; 2 bytes: reserved
	db	0
	db	0			; 1 byte: access mode

	dup	320,0xff		; rest of entries (40*8)
	db	0ffh			; end of table
