; GAG
; el Gestor de Arranque Grafico (this means: 'the Graphical Boot Manager')

Code    segment public
        ASSUME CS:Code, DS:Code, ES:Code
        org 100h

;if TEST_VAR is set to 1, Return to DOS code will be added
;(only for testing purposes. Remove in distribution files)
TEST_VAR = 0

Prog1   PROC    NEAR
        JMP start2

        org 11Bh

; ***********************************
; *         GAG signature           *
; ***********************************

start:  DB 0 ; keyboard type. 0=QWERTY  1=AZERTY  2=QWERTZ
        DB "GAG",0


; ***************************
; *     Main Program        *
; ***************************


start2: MOV AH,0h
        MOV AL,12h ; graphic mode 12h
        INT 10h

        MOV DX,3CEh
        MOV AL,0
        OUT DX,AL
        INC DX
        MOV AL,0
        OUT DX,AL
        DEC DX
        MOV AL,1
        OUT DX,AL
        INC DX
        MOV AL,0
        OUT DX,AL
        DEC DX
        MOV AL,3
        OUT DX,AL
        INC DX
        MOV AL,0
        OUT DX,AL
        DEC DX
        MOV AL,5
        OUT DX,AL
        INC DX
        IN AL,DX
        AND AL,11111100b
        OUT DX,AL


        PUSH CS
        POP DS
        MOV AX,0A000h
        MOV ES,AX

        MOV [counter],AH
        MOV [count1],AX ; reset the timer counter
        MOV AL,[time]
        CMP AL,0 ; has to start the timer?
        JE notimer
        MOV BL,36
        MUL BL ; AX has the increment for the system clock
        MOV [count1],AX
        IN AL,61h
        OR AL,01h ; allows the timer 2 to count,
        AND AL,0FDh ; but without sound in the speaker.
        OUT 61h,AL ; If we don't do this, the timer doesn't count :-?
        MOV AL,0B0h; timer 2, mode 0, binary
        OUT 43h,AL
        MOV AL,0FFh
        OUT 42h,AL
        MOV AL,0FFh
        OUT 42h,AL ; start the timer 2
        MOV byte ptr [counter],1 ; starts the counter
notimer:
        MOV AX,0A000h
        MOV ES,AX
        CALL main
        MOV AL,[time]
        CMP AL,0 ; has to start the timer?
        JE minic
        MOV byte ptr [timertem],0
        MOV BL,9
        MUL BL ; AX has the increment for the system clock
        MOV CX,AX
        MOV AL,7
        CALL setcolor
        MOV SI,4000
inbus:  MOV byte ptr ES:[SI],255
        INC SI
        LOOP inbus
        DEC SI
        MOV [tpos],SI
        JMP minic


IF TEST_VAR EQ 1
finel:  MOV AH,0h ; routine to return to DOS (only for testing)
        MOV AL,3h
        INT 10h ; change to text mode
        RET
ENDIF

cierra: CALL main
minic:  CALL waitkey

IF TEST_VAR EQ 1

        CMP AL,65 ; return if uppercase A is pressed
        JNZ msig5b
        JMP finel

ENDIF

msig5b: CMP AL,13 ; Return?
        JNE msig5
        CMP byte ptr [time],0 ; Timer enabled?
        JE msig5 ; if not, don't use Return
        MOV AL,[toboot] ; if timer is enabled, emulate the press of the default.
msig5:  CMP AL,keysetu2 ; if keysetup (lowercase or uppercase) is pressed
        JNZ msig8       ; goes to SETUP
        JMP tsetup
msig8:  CMP AL,keysetup
        JNZ msig9
        JMP tsetup
msig9:  CMP AL,49
        JL minic        ; if is greatter than ASCII '0'
        CMP AL,57
        JG minic        ; and is lower or equal than ASCII '9'
        SUB AL,48       ; is a valid boot option, so we load the partition
        MOV SI,offset ostabl
mabuc:  MOV AH,[SI]
        CMP AH,0FFh
        JZ cierra       ; if the icon is FFh, it's not a valid boot option
        SUB AL,1
        JZ msig2
        ADD SI,28h      ; next entry
        JMP mabuc
msig2:  MOV [tempo],SI
        ADD SI,20
        CMP byte ptr [SI],0 ; has password this entry?
        JE msig2b
        INC SI
        MOV DI,SI
        CALL tkepsw

msig2b: MOV SI,[tempo]

        MOV DL,[SI+39]
        AND DL,01h      ; LBA or CSH?
        CMP DL,0
        JNZ mlba
        MOV DX,[SI+16]  ; drive and head
        MOV [exten1],DX
        MOV [drives],DL
        MOV CX,[SI+18]  ; sector and track
        MOV [exten2],CX
        MOV byte ptr whatdo,1 ; Work in CSH mode
        JMP msig2f

mlba:   MOV DL,[SI+16]  ; drive unit
        MOV [drives],DL
        MOV CX,[SI+17]
        MOV [exten5],CX
        MOV CX,[SI+37]
        MOV [exten6],CX
        MOV CL,[SI+19]
        MOV [exten7],CL ; LBA parameters
        MOV byte ptr whatdo,2 ; Work in LBA mode

msig2f: PUSH CX         ; saves in the stack the drive where it's booting
        PUSH DX
        MOV AX,0
        MOV ES,AX
        MOV SI,offset merrms
        MOV CX,3
msig2g: CALL loadmbr  ; loads the wanted Boot Sector (trying three times max)
        CMP [lderr],0
        JE mend2
        LOOP msig2g
        POP CX
        POP CX          ; empty the stack
        CALL merror
        JMP cierra
mend2:  PUSH SI
        PUSH DI
        MOV SI,offset mbr
        MOV DI,7C00h
        MOV CX,100h
mend3:  MOV DX,DS:[SI] ; copy the Boot Sector to 0000:7C00h
        MOV ES:[DI],DX
        INC SI
        INC SI
        INC DI
        INC DI
        LOOP mend3
        POP DI
        POP SI
mend:   MOV SI,offset merrms
        MOV AX,ES:[7DFEh] ; last two bytes must be AA55h
        CMP AX,0AA55h
        JZ mnexta
        POP CX
        POP CX          ; empty the stack
        CALL merror
        JMP cierra
mnexta: MOV AH,0h
        MOV AL,3h
        INT 10h         ; changes to text mode
        MOV AH,2h
        MOV BH,0
        MOV DX,0h
        INT 10h         ; puts the screen coordinates to 0,0
        MOV DL,[drives]
        CMP DL,0        ; if GAG is booting the floppy (drive 0) it doesn't
        JE mflopy       ; test any MBR
        MOV AH,2
        CALL loadmbr2   ; loads the MBR of the selecter HD in 0000:0600
        POP DX
        POP CX
        MOV DL,[drives]
        PUSH DX         ; saves in the stack the drive
        PUSH AX
        MOV SI,[tempo]
        MOV AL,[SI+39] ; puts in CH the partition to boot (1 to 4)
        AND AL,2 ; or 0 if we want to boot an extended partition
        CMP AL,0
        JE prueb1
        MOV AL,[SI+39]
        ROR AL,1
        ROR AL,1
        AND AL,3
        INC AL ; converts the value from 0-3 to 1-4
        MOV CH,AL
        JMP prueb2
prueb1: MOV CH,0
prueb2: POP AX
        PUSH CX         ; saves the partition number
        PUSH DX
        CALL updmbr     ; tests the hidden and visible partitions
        POP DX          ; restore the drive from the stack
        POP CX
        PUSH CX
        PUSH DX
        CALL loadmbr2   ; loads or saves the MBR as needed
        POP DX
        POP CX
        PUSH DX
        PUSH CX
        CALL updmbr2    ; test the active partition
        POP CX
        POP DX
        PUSH DX
        PUSH CX
        CALL loadmbr2   ; loads or saves the MBR as needed
        POP CX
        POP DX
mflopy: MOV SI,[tempo]  ; tests if we have to swap drives
        MOV AL,[SI+36]
        CMP AL,0
        JE mrunn
        MOV [swapr],AL  ; pass to the swap routine the drive to swap
        XOR AX,AX
        MOV ES,AX
        DEC word ptr ES:[413h] ; allocate 1KByte
        INT 12h ; gets the available amount of memory
        MOV CL,6
        SHL AX,CL ; obtains the segment for our swap routine
        PUSH AX ; and saves it in the stack
        MOV BX,AX
        XCHG BX,ES:[4Eh] ; take the segment for INT13h
        MOV [swapsg],BX ; and saves it in the swap routine
        MOV BX,8h
        XCHG BX,ES:[4Ch] ; take the offset for INT13h
        MOV [swapof],BX ; now INT13h points to lastseg:0008
        CLD
        POP ES ; segment for the swap routine
        MOV SI,offset swapr
        MOV DI,0        ; swap routine in lastseg:0000
        MOV CX,0FFh      ; 255 bytes
        REPZ
        MOVSB           ; copies the swap routine
mrunn:  MOV AX,0
        MOV DS,AX       ; data segment to 0000
        MOV ES,AX       ; extra segment to 0000
        DB 0EAh
        DW 7C00h
        DW 0h           ; and jump to the Boot Sector
        RET


; *************************************************
; *                     SWAPR                     *
; * This is the swap routine, that allows to swap *
; * hard disk drives                              *
; *************************************************

swapr   DB 0    ; here we store the drive to swap
        DB 0    ; here we store the DL register
        DW 0
        DW 0
        DW 0    ; temporal place
        MOV CS:[2],AH ; saves the subcall made to the INT13h
        MOV CS:[1],DL ; stores the drive
        CMP DL,80h ; First hard disk?
        JNE swsig1
        MOV DL,CS:[0] ; if true, use the swaped disk
        JMP swsig2
swsig1: CMP DL,CS:[0] ; swaped hard disk?
        JNE swsig2
        MOV DL,80h ; if true, use the first hard disk
swsig2: PUSHF ; the IRET in the BIOS routine needs the FLAGS in the stack
        DB 9Ah  ; FAR CALL
swapof  DW 0 ; offset
swapsg  DW 0 ; segment
        PUSHF
        CMP byte ptr CS:[2],8 ; Subcall 'GET DRIVE PARAMETERS'?
        JE swsig3 ; if true, don't change the drive when return
        CMP byte ptr CS:[2],15h ; Subcall 'GET DISK TYPE'?
        JE swsig3 ; if true, don't change the drive when return
        MOV DL,CS:[1] ; loads the old DL value
swsig3: POPF
        MOV CS:[2],AX ; saves in the temporal place AX
        POP AX
        MOV CS:[4],AX ; IP
        POP AX
        MOV CS:[6],AX ; and CS
        POP AX        ; POPs the old FLAGs
        PUSHF         ; PUSHes the new ones
        MOV AX,CS:[6] ; and returns the rest of datas
        PUSH AX
        MOV AX,CS:[4]
        PUSH AX
        MOV AX,CS:[2]
        IRET ; and return


; ******************************************************************
; *                            UPDMBR                              *
; * Updates the hidden and visible partitions. CH contains the     *
; * partition number to boot (0 for extended). Returns:            *
; * AH=2 -> MBR doesn't need modifications AH=3 -> it needs.       *
; * MBR must be loaded in 0000:0600h, and ES must be 0000          *
; ******************************************************************

updmbr: PUSH SI
        PUSH DI
        PUSH BX
        MOV BH,0
        MOV AH,2 ; by default, we don't modify
        CMP CH,0
        JNE upd2
        POP BX
        POP DI
        POP SI
        RET ; it's an extended partition. Don't update the MBR
upd2:   MOV SI,0600h
        ADD SI,446 ; start of partition table
updlp:  PUSH DX
        CMP byte ptr [whatdo],1
        JNE updl1
        MOV DX,[exten1]
        CMP ES:[SI+1],DH ; head matches?
        JNE updl2
        MOV DX,[exten2]
        CMP ES:[SI+2],DX ; sector & track matches?
        JNE updl2
        POP DX
        JMP bootth ; If all matches, go to BOOTTH
updl1:  MOV DX,[exten5]
        CMP ES:[SI+8],DX ; first & second LBA params. matches?
        JNE updl2
        MOV DX,[exten6]
        CMP ES:[SI+10],DX ; third & fourth LBA params. matches?
        JNE updl2
        POP DX
        JMP bootth ; If all matches, go to BOOTTH
updlp9: JMP updlp
updl2:  POP DX
        MOV DI,offset activ
        MOV DH,ES:[SI+4]
upd4:   CMP byte ptr DS:[DI],0 ; end of list?
        JE upd6
        CMP DH,DS:[DI] ; this partition is in the ACTIV list?
        JE upd5
        INC DI ; next entry
        JMP upd4 ; close the loop
upd5:   OR BH,1 ; we have hidden a partition (bit 0 set)
        MOV BL,ES:[SI+4]
        OR BL,10h ; hidde it
        MOV ES:[SI+4],BL
        JMP upd6
bootth: MOV DI,offset inact
        MOV DH,ES:[SI+4]
upd8:   CMP byte ptr DS:[DI],0 ; end of list?
        JE upd10
        CMP DH,DS:[DI] ; this partition is in the INACTIV list?
        JE upd9
        INC DI ; next entry
        JMP upd8 ; close the loop
upd9:   MOV AH,3 ; we have unhidden a partition
        MOV BL,ES:[SI+4]
        AND BL,0EFh ; unhidde it
        MOV ES:[SI+4],BL
        JMP upd6
upd10:  MOV DI,offset activ
        MOV DH,ES:[SI+4]
upd11:  CMP byte ptr DS:[DI],0 ; end of list?
        JE upd6
        CMP DH,DS:[DI] ; this partition is in the ACTIV list?
        JE upd12 ; if true, we have to mark it
        INC DI ; next entry
        JMP upd11 ; close the loop
upd12:  OR BH,2 ; bootable partition can be hidden 
upd6:   ADD SI,16 ; next entry
        INC CL
        CMP CL,5
        JNE updlp9
        CMP BH,3 ; The partition to boot can be hidden and we have hidden
        JNE upd13 ; at least one partition?
        MOV AH,3 ; If true, we must modify the MBR
upd13:  POP BX
        POP DI
        POP SI
        RET


; ******************************************************************
; *                            UPDMBR2                             *
; * Updates the active and inactive partitions. CH contains the    *
; * partition number to boot (0 for extended). Returns:            *
; * AH=2 -> MBR doesn't need modifications AH=3 -> it needs.       *
; * MBR must be loaded in 0000:0600h, and ES must be 0000          *
; ******************************************************************

updmbr2:
        PUSH SI
        PUSH DI
        PUSH BX
        MOV BH,0
        MOV AH,2 ; by default, we don't modify
        CMP CH,0
        JNE upd2b
        POP BX
        POP DI
        POP SI
        RET ; it's an extended partition. Don't update the MBR
upd2b:  MOV SI,0600h
        ADD SI,446 ; start of partition table

updlpb: PUSH DX
        CMP byte ptr [whatdo],1
        JNE updlb1
        MOV DX,[exten1]
        CMP ES:[SI+1],DH ; head matches?
        JNE updlb2
        MOV DX,[exten2]
        CMP ES:[SI+2],DX ; sector & track matches?
        JNE updlb2
        POP DX
        JMP botthb ; If all matches, go to BOOTTH
updlb1: MOV DX,[exten5]
        CMP ES:[SI+8],DX ; first & second LBA params. matches?
        JNE updlb2
        MOV DX,[exten6]
        CMP ES:[SI+10],DX ; third & fourth LBA params. matches?
        JNE updlb2
        POP DX
        JMP botthb ; If all matches, go to BOOTTH
updlb2: POP DX
        CMP byte ptr ES:[SI],80h ; has it the BOOTABLE flag?
        JNE upd6b
        MOV byte ptr ES:[SI],0 ; if true, change it
        JMP upd6b
botthb: CMP byte ptr ES:[SI],80h ; has it the BOOTABLE flag?
        JE upd6b
        MOV byte ptr ES:[SI],80h ; if false, change it
        MOV AH,3
upd6b:  ADD SI,16 ; next entry
        INC CL
        CMP CL,5
        JNE updlpb
        POP BX
        POP DI
        POP SI
        RET


; ***************************************************************
; *                         LOADMBR2                            *
; * this loads or saves the MBR pointed by DL in 0000:0600      *
; * AH=2 to load, AH=3 to save. ES register MUST point to 0000. *
; ***************************************************************

loadmbr2:
        MOV DH,0
        MOV DL,[drives]
        MOV BX,0600h
        MOV CX,01h
        MOV AL,1
        INT 13h
        RET


; *********************************************
; *                  YESNO                    *
; * prints a question string pointed by SI    *
; * and returns AL=0 if the user answers YES  *
; * or AL=1 if the user asks NO               *
; *********************************************

yesno:  MOV AX,0A000h
        MOV ES,AX
        PUSH SI
        PUSH DX
        MOV SI,13604
        MOV BX,68
        MOV AX,56
        CALL window
        POP DX
        POP SI
        MOV DH,0Ah
        MOV BX,1210h
        CALL prcen
        MOV SI,offset myesno
        MOV DH,0Ch
        MOV BX,1210h
        CALL prcen
mynbuc: CALL waitkey
        OR AL,20h ; converts keystroke to lowercase
        CMP AL,keyyes
        JZ mynyes
        CMP AL,keyno
        JNZ mynbuc
        MOV AL,1 ; User pressed NO
        RET
mynyes: MOV AL,0 ; User pressed YES
        RET


; ****************************************
; *                MERROR                *
; * prints an error string pointed by SI *
; ****************************************

merror: MOV AX,0A000h
        MOV ES,AX
        PUSH SI
        PUSH DX
        MOV SI,13525
        MOV BX,68
        MOV AX,60
        CALL window
        MOV SI,16116
        MOV BX,6
        MOV AX,24
        CALL window
        POP DX
        POP SI
        MOV DH,0Ah
        MOV BX,1215h
        CALL prcen
        MOV SI,offset mok
        MOV DH,0Ch
        MOV BX,1210h
        CALL prcen
merbuc: CALL waitkey
        CMP AL,0Dh
        JNZ merbuc
        RET


; ************************************************
; *                 WINDOW                       *
; * creates a window with AX pixels of heigh and *
; * BX chars of width, starting in SI.           *
; ************************************************

window: PUSH DX
        PUSH BX
        PUSH AX
        PUSH SI
        MOV DX,AX
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],0C0h
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],03Fh
        MOV AL,9
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wnbuc1: MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wnbuc1
        MOV byte ptr ES:[SI],0FCh
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],03h
        MOV AL,9
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP SI
        ADD SI,80 ; next line
        PUSH SI
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],0C0h
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],03Fh
        MOV AL,9
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wnbuc2: MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wnbuc2
        MOV byte ptr ES:[SI],0F8h
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],03h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],04h
        MOV AL,8
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP SI
        ADD SI,80 ; next line
        PUSH SI
        MOV CX,DX
wnbuc3: PUSH CX
        MOV AL,04
        CALL setcolor
        MOV byte ptr ES:[SI],30h
        MOV AL,02
        CALL setcolor
        MOV byte ptr ES:[SI],0CFh
        MOV AL,09
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wnbuc4: MOV AL,02h
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        MOV AL,0Dh
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        LOOP wnbuc4
        MOV AL,01h
        CALL setcolor
        MOV byte ptr ES:[SI],0Ch
        MOV AL,02h
        CALL setcolor
        MOV byte ptr ES:[SI],0F3h
        MOV AL,0Ch
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP CX
        POP SI
        ADD SI,80
        PUSH SI
        LOOP wnbuc3
        POP SI
        PUSH SI
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],0C0h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],1Fh
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],20h
        MOV AL,8
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wnbuc5: MOV AL,00001110b
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wnbuc5
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],03h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],0FCh
        MOV AL,12
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP SI
        ADD SI,80
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],0C0h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],3Fh
        MOV AL,12
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wnbuc6: MOV AL,14
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wnbuc6
        MOV byte ptr ES:[SI],0FCh
        MOV AL,2
        CALL setcolor
        MOV byte ptr ES:[SI],03h
        MOV AL,12
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP AX
        POP BX
        POP DX
        RET


; ***************************************************
; *                    SETMASK                      *
; * Set the pixel mask to the value indicated in AL *
; ***************************************************

setmask:
        PUSH DX
        PUSH AX
        MOV AX,8
        MOV DX,3CEh
        OUT DX,AL
        POP AX
        INC DX
        OUT DX,AL
        POP DX
        RET

; ****************************************************
; *                     SETCOLOR                     *
; * Set the pixel color to the value indicated in AL *
; ****************************************************

setcolor:
        PUSH DX
        PUSH AX
        MOV AX,2
        MOV DX,3C4h
        OUT DX,AL
        POP AX
        INC DX
        OUT DX,AL
        POP DX
        RET


; ************************************************************
; *                         TSETUP                           *
; * Tests if there's configuration password. If not, goes to *
; * SETUP.                                                   *
; ************************************************************

tsetup: CMP byte ptr [ispaswd],0
        JE setup                ; if there's no password, go to setup
        MOV DI,offset tpaswd    ; ask for the password
        CALL tkepsw
        JMP setup


; *****************************************************************************
; *                             TKEPSW                                        *
; * tkepsw ask for a password, given in DI. If the typed password is equal to *
; * the given password, returns. If not, empties the stack, prints an error   *
; * and jumps to cierra.                                                      *
; *****************************************************************************

tkepsw: MOV byte ptr [prpaswd],1 ; don't print the keystrokes
        MOV SI,offset mentpsw
        PUSH DI
        CALL intro
        POP DI
        MOV byte ptr [prpaswd],0 ; print the keystrokes
        MOV SI,offset mtemp
        MOV CX,14
tsbuc1: MOV AH,[SI]
        CMP AH,[DI]
        JNE notequ
        INC SI
        INC DI
        LOOP tsbuc1
        RET             ; password OK
notequ: POP AX          ; empty the stack
        MOV SI,offset mincor
        CALL merror
        JMP cierra


; ***************************
; *         SETUP           *
; * Setup menu routine      *
; ***************************

setup:  CALL presen
        MOV AX,24
        MOV BX,60
        MOV SI,10969
        CALL window
        MOV SI,13529
        CALL window
        MOV SI,16089
        CALL window
        MOV SI,18649
        CALL window
        MOV SI,21209
        CALL window
        MOV SI,23769
        CALL window
        MOV SI,26329
        CALL window

        MOV BX,1210h ; default color
        MOV SI,offset madd
        MOV DH,08h
        CALL prcen
        MOV SI,offset mdel
        MOV DH,0Ah
        CALL prcen
        MOV SI,offset msavfd
        MOV DH,0Ch
        CALL prcen
        MOV SI,offset msavhd
        MOV DH,0Eh
        CALL prcen
        MOV SI,offset mbotim
        MOV DH,10h
        CALL prcen
        MOV SI,offset mpassw
        MOV DH,12h
        CALL prcen
        MOV SI,offset mretur
        MOV DH,14h
        CALL prcen

tsebu1: CALL waitkey
        OR AL,20h ; convert the keystroke to lowercase
        CMP AL,keyreturn
        JNZ tsesg1
        JMP cierra
tsesg1: CMP AL,keyadd
        JNZ tsesg2
        JMP tadd
tsesg2: CMP AL,keyfloppy
        JNZ tsesg3
        JMP flopy
tsesg3: CMP AL,keydelete
        JNZ tsesg4
        JMP delete
tsesg4: CMP AL,keytimer
        JNZ tsesg5
        JMP btimer
tsesg5: CMP AL,keypassw
        JNZ tsesg6
        JMP passwd
tsesg6: CMP AL,keyhd
        JNZ tsesg7
        JMP harddk
tsesg7: JMP tsebu1


; *******************************
; *           DELETE            *
; * Removes an OS from the list *
; *******************************

delete: MOV byte ptr [prstp],1 ; removes an OS from the list
        CALL presen
        MOV BX,1016h ; ink green, background black
        MOV SI,offset mdelnm
        MOV DH,3h
        CALL prcen
        MOV SI,offset mabort
        MOV DH,4h
        CALL prcen
        CALL puticon
        CMP byte ptr [ostabl],0FFh ; is there more OS?
        JNZ dinic
        MOV SI,offset mnoos
        CALL merror
        JMP setup
dinic:  CALL waitkey
        CMP AL,27 ; ESC key?
        JNZ msig1
        JMP setup
msig1:  MOV [wtemp],AL ; store the keystroke
        CMP AL,49
        JL dinic ; if is greatter than ASCII '0'
        CMP AL,57
        JG dinic ; and is lower or equal than ASCII '9'
        SUB AL,48 ; is a valid option
        MOV SI,offset ostabl
debuc:  MOV AH,[SI]
        CMP AH,0FFh
        JZ dinic ; if the icon is FFh, it's not a valid boot option
        SUB AL,1
        JZ dsig2
        ADD SI,28h ; next entry
        JMP debuc
dsig2:  MOV DI,SI ; now SI and DI point to the entry to delete
        ADD SI,28h ; SI points to the next entry
debuc2: MOV AH,[SI]
        MOV [DI],AH
        INC SI
        INC DI
        CMP AH,0FFh ; last entry?
        JNE debuc2
        MOV CX,28h
debuc3: MOV [DI],AH ; deletes the last entry
        INC DI
        LOOP debuc3
        CMP byte ptr [time],0 ; boot timer enabled?
        JE dfine
        MOV AL,[wtemp] ; reload the keystroke
        CMP AL,[toboot] ; Are we deleting the default Operating System?
        JNE dfine2 ; If true...
        JMP bdisab ; disables the timer
dfine2: CMP AL,[toboot] ; if the OS to delete is greater than the default...
        JG dfine ; we do nothing
        MOV AL,[toboot]
        DEC AL ; if not, we decrement the keystroke to boot it
        MOV [toboot],AL
dfine:  JMP setup


; *****************************
; *         BTIMER            *
; * Configures the boot timer *
; *****************************

btimer: MOV SI,offset mboot
        MOV byte ptr [prpaswd],2 ; allow to type numbers only
        CALL intro ; gets the number in ASCII in mtemp
        MOV byte ptr [prpaswd],0 ; allows again all characters
        CMP byte ptr [mtemp+2],32 ; more than two digits?
        JZ btsig1
        MOV SI,offset mberr
        CALL merror
        JMP setup
btsig1: CMP byte ptr [mtemp],32 ; empty string?
        JNZ btsig2
bdisab: MOV byte ptr [time],0 ; disables the timer
        MOV SI,offset mbdisa
        CALL merror
        JMP setup
btsig2: MOV BX,0
        CMP byte ptr [mtemp+1],32 ; only one digit?
        JE btsig3
        MOV BL,0Ah
        MOV AH,0
        MOV AL,[mtemp]
        SUB AL,48
        MUL BL ; multiply 10*first digit
        MOV BL,[mtemp+1]
        SUB BL,48
        ADD AL,BL
        JMP btsig4
btsig3: CMP byte ptr [mtemp],48 ; time is zero?
        JE bdisab ; disables the timer
        MOV AL,[mtemp]
        SUB AL,48
btsig4: MOV [time],AL ; stores the new time
        CALL presen ; shows the OS list
        MOV BX,1016h ; ink green, background black
        MOV SI,offset mbsel
        MOV DH,4h
        CALL prcen
        MOV byte ptr [prstp],1
        CALL puticon
btinic: CALL waitkey
btsig5: CMP AL,49
        JL btinic ; if is lower or equal than ASCII '0'
        CMP AL,57
        JG btinic ; or is greater than ASCII '9', close the loop
        PUSH AX
        SUB AL,48 ; is a valid boot option, so we select it
        MOV SI,offset ostabl
btbuc:  MOV AH,[SI]
        CMP AH,0FFh
        JNZ btsig7 ; if the icon is FFh, it's not a valid option
        POP AX
        JMP btinic
btsig7: SUB AL,1
        JZ btsig6
        ADD SI,28h ; next entry
        JMP btbuc
btsig6: POP AX ; is a valid option
        MOV [toboot],AL ; stores it
        JMP setup


; *******************************
; *             PASSWD          *
; * changes the setup password  *
; *******************************

passwd: MOV SI,offset ispaswd
        MOV DI,offset tpaswd
        CALL chpwd
        CMP byte ptr [ispaswd],0
        JNE rpaswd
        MOV SI,offset mpsdisa
        CALL merror
rpaswd: JMP setup


; **********************************************************************
; *                             CHPWD                                  *
; * Accept a password from keyboard, returning 1 in SI if a password   *
; * is typed, or 0 if only RETURN is pressed, and storing a pointer to *
; * the password in DI.                                                *
; **********************************************************************

chpwd:  PUSH SI
        PUSH DI
        MOV SI,offset mnewpsw ; enter the new password
        CALL intro
        POP DI
        MOV SI,offset mtemp
        MOV CX,15
        MOV AL,0
psbuc1: MOV AH,[SI]
        MOV [DI],AH
        CMP AH,32
        JE pspace
        MOV AL,1 ; if there's a character not equal to SPACE, puts AL=1
pspace: INC SI
        INC DI
        LOOP psbuc1
        POP SI
        CMP AL,0 ; All SPACEs?
        JE pdisab
        MOV byte ptr [SI],1
        RET
pdisab: MOV byte ptr [SI],0
        RET


; *****************************************
; *                  TADD                 *
; * adds a new partition to the O.S. list *
; *****************************************

tadd:   MOV SI,offset ostabl
        MOV AH,0
abuc1:  MOV AL,[SI] ; searchs for a free entry in the table of O.S.
        CMP AL,0FFh
        JE asig1b
        INC AH
        ADD SI,28h ; next entry
        CMP AH,9 ; last entry?
        JNE abuc1 ; no, close loop
        MOV SI,offset mnoent
        CALL merror
        JMP setup
asig1b: PUSH SI ; SI points to a free entry
        MOV CX,28h
abuc1n: MOV byte ptr [SI],0 ; clears the entry to delete spare
        INC SI
        LOOP abuc1n
        POP SI
asig1:  PUSH SI
        MOV DL,80h ; first hard disk
        MOV [drives],DL

abuc8:  PUSH DX

        PUSH AX
        PUSH BX
        PUSH CX
        MOV AH,41h
        MOV BX,55AAh
        MOV DL,[drives]
        INT 13h ; test if we have BIOS INT13h extensions
        JC nobiosx
        CMP BX,0AA55h
        JNE nobiosx
        AND CX,1 ; Does it support packet structure?
        CMP CX,1
        JNE nobiosx
        MOV byte ptr [whatdo],2 ; use LBA
        JMP biosnx
nobiosx:
        MOV byte ptr [whatdo],1 ; use CSH
biosnx: POP CX
        POP BX
        POP AX
        POP DX

        PUSH DX
        CALL presen
        CALL prlett
        CALL prnum
        CALL clmsg ; we need mtemp to print the letters
        MOV byte ptr [extend],0 ; no LBA base
        MOV byte ptr [extens],0 ; we are with the MBR
        MOV DI, offset MBR
        MOV CX,200h
abuc9:  MOV byte ptr [DI],0 ; clears the buffer
        INC DI
        LOOP abuc9
        POP DX
        PUSH DX
        MOV DH,0
        MOV [exten1],DX ; drive and head
        MOV word ptr [exten2],0001h ; sector and cilinder
        MOV word ptr [exten5],0
        MOV word ptr [exten6],0
        MOV byte ptr [exten7],0
        CALL loadmbr ; loads the MBR
        MOV SI,offset mopti
        MOV DX,0701h
        MOV BX,1217h ; ink bright red, background normal white
        CALL prstr ; options
        MOV BX,1210h ; ink black, background normal white
        MOV SI,offset mdisk
        MOV DX,0803h
        CALL prstr ; mesage "boot from floppy disk"
        MOV DI,offset ptable ; table where I save the temporal partition data
        MOV word ptr [DI],0h ; adds to the table the first floppy disk
        INC DI
        INC DI
        MOV word ptr [DI],0001h
        INC DI
        INC DI
        PUSH CX
        MOV CX,6
abucot: MOV byte ptr [DI],0
        INC DI
        LOOP abucot
        POP CX
        MOV byte ptr [DI],0FFh ; mark the end of table
        MOV BH,66 ; B is the first letter (A is for floppy)
        MOV BL,9 ; line where print the next partition
abuc3:  MOV SI,offset MBR ; point to the MBR
        MOV AH,4 ; four entries in each sector
        MOV byte ptr [partit],1
        MOV byte ptr [exten],0 ; no extended partitions found yet
        ADD SI,1BFh ; first entry
abuc2:  POP DX
        PUSH DX ; gets in DL the drive
        MOV DH,[SI] ; head
        INC SI
        MOV CX,[SI] ; sector and cilinder
        INC SI
        INC SI
        MOV AL,[SI] ; partition type
        CMP AL,0h ; not used?
        JNE anext4
        JMP anext
anext4: CMP AL,5h ; extended?
        JZ aexten
        CMP AL,0Fh ; Windows extended?
        JNZ aprint
aexten: MOV byte ptr [exten],1 ; we have now an extended partition
        MOV [exten1],DX ; drive and head
        MOV [exten2],CX ; cilinder and sector
        CMP byte ptr [extend],0
        JNE lbanxt
        MOV byte ptr [extend],1
        PUSH SI
        PUSH DX
        INC SI
        INC SI
        INC SI
        INC SI
        MOV DX,[SI]
        MOV [exten3],DX ; LBA base for extended partitions
        MOV [exten5],DX ; next partition sector (LBA)
        INC SI
        INC SI
        MOV DX,[SI]
        MOV [exten4],DX
        MOV [exten6],DX
        MOV byte ptr [exten7],0
        POP DX
        POP SI
        JMP lbanx0
lbanxt: PUSH SI
        PUSH DX
        INC SI
        INC SI
        INC SI
        INC SI
        MOV DX,[SI]        
        MOV [exten5],DX ; next partition sector (LBA)
        INC SI
        INC SI
        MOV DX,[SI]
        MOV [exten6],DX
        MOV byte ptr [exten7],0
        MOV SI,offset exten3
        CALL sumalba
        POP DX
        POP SI

lbanx0: JMP anext
aprint: MOV [DI],DX ; add to the table the partition found
        INC DI
        INC DI
        MOV [DI],CX
        INC DI
        INC DI
        PUSH SI
        PUSH DX
        INC SI
        INC SI
        INC SI
        INC SI
        CMP byte ptr [extens],0
        JNE lbanx1
        MOV DX,[SI]
        MOV [DI],DX
        INC DI
        INC DI
        INC SI
        INC SI
        MOV DX,[SI]
        MOV [DI],DX
        INC DI
        INC DI
        MOV byte ptr [DI],0
        INC DI
        PUSH AX
        MOV AL,[partit]
        MOV [DI],AL ; number of primary partition (1 to 4)
        POP AX
        INC DI
        POP DX
        POP SI
        JMP lbanx2
lbanx1: CALL sumalba
        MOV DX,[exten5]
        MOV [DI],DX
        INC DI
        INC DI
        MOV DX,[exten6]
        MOV [DI],DX
        INC DI
        INC DI
        MOV DL,[exten7]
        MOV [DI],DL
        INC DI
        MOV byte ptr [DI],0 ; extended partition
        INC DI
        POP DX
        POP SI

lbanx2: MOV byte ptr[DI],0FFh ; end of table
        PUSH SI
        PUSH DI
        PUSH BX
        PUSH AX
        CALL clmsg ; clears the temporal msg buffer
        MOV [mtemp+1],BH ; actual letter
        PUSH AX
        SHR AL,1
        SHR AL,1
        SHR AL,1
        SHR AL,1
        CALL toasc
        MOV [mtemp+6],AL
        POP AX
        CALL toasc
        MOV [mtemp+7],AL
        MOV byte ptr [mtemp+8],"h"
        MOV byte ptr [mtemp+10],0
        MOV SI,offset mtemp
        MOV DH,BL
        MOV DL,2
        CMP [extens],0
        JNE lbas20
        MOV BX,1210h ; ink black, background normal white
        JP lbas30
lbas20: MOV BX,121Ah ; ink blue, background normal white
lbas30: CALL prstr ; prints the letter and the partition type (hex number)
        POP AX
        POP BX
        PUSH BX
        PUSH AX
        MOV DH,BL
        MOV DL,0Ch
        CMP [extens],0
        JNE lbas2
        MOV BX,1210h ; ink black, background normal white
        JP lbas3
lbas2:  MOV BX,121Ah ; ink blue, background normal white
lbas3:  CMP AL,1
        JZ prdos
        CMP AL,4
        JZ prdos
        CMP AL,6
        JZ prdos
        CMP AL,11h
        JZ prdos
        CMP AL,14h
        JZ prdos
        CMP AL,16h
        JZ prdos
        CMP AL,7h
        JZ pros2
        CMP AL,17h
        JZ pros2
        CMP AL,0Bh
        JZ prwin
        CMP AL,0Ch
        JZ prwin
        CMP AL,0Eh
        JZ prwin
        CMP AL,1Bh
        JZ prwin
        CMP AL,1Ch
        JZ prwin
        CMP AL,1Eh
        JZ prwin
        CMP AL,0Ah
        JZ prbm
        CMP AL,0A5h
        JZ prbsd
        CMP AL,0EBh
        JZ prbeos
        CMP AL,83h
        JNZ anext2
        MOV SI,offset mlinux
        JMP anext3
prbsd:  MOV SI,offset mbsd
        JMP anext3
prdos:  MOV SI,offset mdos
        JMP anext3
prwin:  MOV SI,offset mwin
        JMP anext3
prbm:   MOV SI,offset mbm
        JMP anext3
prbeos: MOV SI,offset mbeos
        JMP anext3
pros2:  MOV SI,offset mos2
anext3: CALL prstr
anext2: POP AX
        POP BX
        POP DI
        POP SI
        INC BH ; next letter
        INC BL ; next row
anext:  ADD SI,0Dh ; next entry
        CMP BL,27
        JE afine ; there's no room in the screen to show more partitions
        PUSH AX
        MOV AL,[partit]
        INC AL
        MOV [partit],AL
        POP AX
        SUB AH,1
        JZ anext5
        JMP abuc2
anext5: MOV AL,[exten]
        CMP AL,0 ; are there more extended partitions?
        JE afine
        MOV byte ptr [extens],1
        CALL loadmbr ; reads the next sector
        JMP abuc3
afine:  POP DX
abuc4:  CALL waitkey ; waits for a keystroke
        CMP AL,"8" ; is a '8'?
        JG anext7
        CMP AL,"1" ; is a '1'?
        JL abuc4
        ADD AL,79 ; converts the keystroke into a number betwen 80h and 88h
        MOV DL,AL ; selects the new HD
        MOV [drives],DL ; and stores the actual drive in DRIVES to know if we
        JMP abuc8 ; need to swap or not
anext7: OR AL,20h ; convert the keystroke to lowercase
        CMP AL,60h
        JLE abuc4
        SUB AL,61h
        MOV SI,offset ptable
abuc5:  CMP AL,0
        JE anext6 ; entry located
        ADD SI,10 ; next entry
        CMP byte ptr [SI],0FFh ; End of ptable?
        JE abuc4 ; If true, try again
        DEC AL
        JMP abuc5
anext6:
        MOV AL,[SI+9] ; partition number
        CMP AL,0
        JE anxt7
        SUB AL,1 ; partition number (0 - 3)
        ROL AL,1
        ROL AL,1
        AND AL,0Ch ; partition number in bits 2 and 3
        OR AL,2 ; bit 1 set
anxt7:  CMP byte ptr [whatdo],1
        JE anextn

        MOV DL,[SI] ; drive unit
        CMP DL,0 ; Floppy disk drive?
        JE anextn ; If true, use CSH
        MOV CX,[SI+4] ; LBA0 and LBA1
        MOV BX,[SI+6] ; LBA2 and LBA3
        MOV DH,[SI+8] ; LBA4
        POP SI
        MOV [SI+10h],DL ; drive unit
        MOV [SI+11h],CX ; LBA0 and LBA1
        MOV [SI+13h],DH ; LBA4
        MOV [SI+25h],BX ; LBA2 and LBA3
        OR AL,1h ; sets the bit 0 (LBA mode)
        MOV [SI+27h],AL
        JMP anxt2

anextn: MOV DX,[SI] ; we saves the data in the standard (CSH) mode
        MOV CX,[SI+2]
        POP SI
        MOV [SI+10h],DX
        MOV [SI+12h],CX ; saves the drive, sector, head and track
        AND AL,0FEh ; clear the bit 0 (CSH mode)
        MOV [SI+27h],AL

anxt2:  MOV byte ptr [SI],3
        PUSH SI
        MOV byte ptr [SI+36],0 ; no swap
        CMP byte ptr [drives],80h ; actual drive is the first hard disk?
        JE anext9 ; if true, don't ask for swap drives
        MOV SI,offset mswap
        CALL yesno ; ask for drive swapping
        POP SI
        PUSH SI
        MOV AH,[drives]
        CMP AL,0 ; user answered yes?
        JNE anext9
        MOV [SI+36],AH ; drive to swap

anext9: MOV SI,offset mdescr
        CALL intro ; ask for the O.S.'description
        POP SI
        PUSH SI
        INC SI
        MOV DI,offset mtemp
        MOV CX,15
abuc6:  MOV AH,DS:[DI]
        MOV DS:[SI],AH
        INC SI
        INC DI
        LOOP abuc6 ; copies the description
        POP SI
        PUSH SI
        ADD SI,20
        MOV DI,SI
        INC DI
        CALL chpwd ; ask for a password
        CALL presen
        CALL shwicn ; ask for an icon
        MOV SI,offset mlett2
        MOV DH,04h
        MOV BX,1016h ; ink green, background black
        CALL prcen
abuc7:  CALL waitkey
        OR AL,20h
        CMP AL,61h
        JL abuc7
        SUB AL,60h
        CMP AL,[icons]
        JG abuc7
        POP SI
        MOV [SI],AL
        JMP setup

; ***********************************
; *             SUMALBA             *
; * Adds the four bytes pointed by  *
; * SI to the four bytes stored in  *
; * EXTEN5 and EXTEN6, and stores   *
; * it in EXTEN5, EXTEN6 and EXTEN7 *
; ***********************************

sumalba:
        PUSH AX
        PUSH SI
        MOV AX,[SI]
        ADD AX,[exten5]
        MOV [exten5],AX
        INC SI
        INC SI
        MOV AX,[SI]
        ADC AX,[exten6]
        MOV [exten6],AX
        MOV AL,0
        ADC AL,[exten7]
        MOV [exten7],AL
        POP SI
        POP AX
        RET



; ******************************
; *         FLOPY              *
; * saves GAG in a floppy disk *
; ******************************

flopy:  MOV DX,0
        PUSH ES
        PUSH DS
        POP ES
        PUSH DX
        MOV CX,3
sfabun: PUSH CX
        MOV AX,201h
        MOV CX,0001h
        MOV BX,offset MBR
        INT 13h ; load the Boot Sector (trying three times max)
        JNC sfaend
        POP CX
        LOOP sfabun
        POP DX
        POP ES
        MOV SI,offset merrls
        CALL merror
        JMP setup
sfaend: POP DX ; empty the stack
        MOV word ptr [MBR+510],0AA55h ; saves the boot signature
        MOV byte ptr [MBR],0EBh
        MOV word ptr [MBR+1],903Ch ; saves the jump (to preserve the floppy info)
        POP DX
        PUSH DX
        MOV SI,offset load1 ; code for floppies
        MOV DI, offset MBR
        ADD DI,03Eh
        MOV CX,170 ; copies only 170 bytes
sfbuc1: MOV AH,[SI]
        MOV [DI],AH
        INC SI
        INC DI
        LOOP sfbuc1
        MOV BX,offset MBR
        MOV CX,0001h
        POP DX
        PUSH DX
        MOV AX,0301h ; Save 1 sector
        INT 13h
        JC sferror1
        POP DX
        MOV BX,offset start ; saves the entire program and configuration
        MOV AX,0101h ; starting in the sector 1, track 1
        MOV CX,3 ; save 3 tracks
sfbuc2: PUSH CX
        PUSH BX
        PUSH AX
        MOV CX,AX ; sector and track
        MOV AX,0312h ; saves 18 sectors
        INT 13h
        JC sferror3
        POP AX
        POP BX
        POP CX
        INC AH ; next track
        ADD BX,9216 ; next memory address
        LOOP sfbuc2
        POP ES
        MOV SI,offset msucces
        CALL merror
        JMP setup
sferror1:
        POP DX
sferror2:
        POP ES
        MOV SI,offset mgraba
        CALL merror
        JMP setup
sferror3:
        POP BX
        POP CX
        JMP sferror1


; ******************************
; *         HARDDK             *
; * saves GAG in the hard disk *
; ******************************

harddk: MOV DX,80h ; first, we test if the hard disk has
        MOV AH,08h ; the needed number of sectors per track
        PUSH ES
        INT 13h
        AND CL,3Fh ; gets the 6 lower bits
        CMP CL,nsect
        JLE sherr ; if is lower or equal, we return an error (equal because we must
        JMP shcnt ; count the MBR too, wich is one more sector)
sherr:  MOV SI,offset mgraba
        CALL merror
        JMP setup
shcnt:  MOV DX,80h
        MOV byte ptr usesf,0 ; default: we can.
        MOV SI,offset ostabl ; we search for an entry with password
shsbuc: CMP byte ptr [SI],0FFh ; end of table?
        JE shaves2 ; if true, continue
        ADD SI,20 ; here is the flag that tells if this entry
        CMP byte ptr [SI],0 ; has or not a password.
        JNE shsend ; if is 1, this entry has password.
        ADD SI,20 ; next entry
        JMP shsbuc
shsend: MOV byte ptr usesf,1 ; we can't use SafeBoot: there's entries with passwords.
shaves2:
        PUSH ES
        PUSH DS
        POP ES
        PUSH DX
        MOV CX,3
shabun: PUSH CX
        MOV AX,201h
        MOV CX,0001h
        MOV BX,offset MBR
        INT 13h ; load the Boot Sector (trying three times max)
        JNC shaend
        POP CX
        LOOP shabun
        POP DX
        POP ES
        MOV SI,offset merrls
        CALL merror
        JMP setup
shaend: POP DX ; empty the stack
        MOV word ptr [MBR+510],0AA55h ; saves the boot signature
        MOV byte ptr [MBR],0EBh
        POP DX
        PUSH DX
        MOV DI, offset MBR
        MOV SI,offset load2 ; code for hard disks
        MOV CX,308 ; copies only 308 bytes
shbuc1: MOV AH,[SI]
        MOV [DI],AH
        INC SI
        INC DI
        LOOP shbuc1
        MOV BX,offset MBR
        MOV CX,0001h
        POP DX
        PUSH DX
        MOV AX,0301h ; Save 1 sector
        INT 13h
        JC sherror1
        POP DX
        MOV BX,offset start ; saves the entire program and configuration
        MOV CX,0002h ; starting in the sector 2
        MOV AH,03
        MOV AL,nsect ; saves nsect sectors
        INT 13h
        JC sherror2
        POP ES
        MOV SI,offset msucces
        CALL merror
        JMP setup
sherror1:
        POP DX
sherror2:
        POP ES
        MOV SI,offset mgraba
        CALL merror
        JMP setup


; ******************************************************
; *                    LOAD1                           *
; * this is the code saved in the MBR of the diskettes *
; ******************************************************

load1:  CLD
        XOR AX,AX
        MOV DS,AX
        MOV ES,AX
        MOV SI,7C55h ; start address of the code
        MOV DI,600h ; address 0000:0600h
        MOV CX,100h ; 256 words
        REPZ
        MOVSW ; relocates the code
        DB 0EAh ; Far jump to 0000:0606
        DW 606h
        DW 0h

icopy2: DB "GAG: ",90h ; address 0000:0600h
        MOV DI,600h ; start address of the message 'GAG: '
fbhdb1: MOV AH,0Eh
        MOV BX,7
        MOV AL,[DI]
        CMP AL,90h ; end of the message?
fbe1:   JE fbootn ; if true, continue.
        PUSH DI
        INT 10h ; if false, print the letter
        POP DI
        INC DI
        JMP fbhdb1 ; and close de loop

; loads GAG

fbootn: MOV BX,011Bh ; offset where GAG is loaded
        MOV AX,1000h ; segment where GAG is loaded
        MOV DS,AX
        MOV ES,AX
        MOV AX,0101h ; sector 1, track 1
        MOV CX,3 ; read three tracks
ftrack: PUSH CX
        PUSH BX
        MOV CX,3 ; try 3 times max
fload:  PUSH CX
        PUSH AX
        MOV DX,0h ; Floppy disk 0, head 0
        MOV CX,AX ; sector and track
        MOV AX,212h; 18 sectors, BIOS_load_sector
        INT 13h ; load the MBR
        JNC fco1
        POP AX
        POP CX
        LOOP fload
        MOV AL,49 ; error 1, error reading a sector!
        JMP ferror
fco1:   POP AX
        POP CX
        INC AH ; next track
        POP BX
        ADD BX,9216 ; address where load the next track
        POP CX
        LOOP ftrack ; read next track

frun:   MOV AX,1000h
        MOV ES,AX ; GAG's segment
        CMP word ptr ES:[11Ch],4147h ; tests for GAG signature
        JNE ferr
        CMP word ptr ES:[11Eh],0047h
        JNE ferr
        DB 0EAh
        DW 120h
        DW 1000h ; Jumps to GAG

ferr:   MOV AL,51 ; error 3, GAG is not in the disk!

ferror: MOV BX,7
        MOV AH,0Eh
        INT 10h ; prints the error passed in AL
fbuc:   JMP fbuc ; and locks the machine to allows user to read it.


; *******************************************************
; *                    LOAD2                            *
; * this is the code saved in the MBR of the hard disks *
; *******************************************************

load2:  CLD
        XOR AX,AX
        MOV DS,AX
        MOV ES,AX
        MOV SI,7C00h ; start address of the code
        MOV DI,600h ; address 0000:0600h
        MOV CX,100h ; 256 words
        REPZ
        MOVSW ; relocates the code
        DB 0EAh ; Far jump to 0000:062E
        DW 62Eh
        DW 0h

icopy:  DB "GAG: ",90h ; address 0000:0617h
usesf:  DB 0 ; address 0000:061Dh. If 1, don't allow SafeBoot
        DB 16,0,1,0 ; address 0000:061Eh. LBA table
        DB 0,7Ch,0,0 ; address 0000:0622h. Segment:offset
        DB 0,0,0,0,0,0,0,0 ; address 0000:0626h. Logical sector
        MOV DI,617h ; start address of the message 'GAG: '
mbhdb1: MOV AH,0Eh
        MOV BX,7
        MOV AL,[DI]
        CMP AL,90h ; end of the message?
hbe1:   JE mcont ; if true, continue.
        PUSH DI
        INT 10h ; if false, print the letter
        POP DI
        INC DI
        JMP mbhdb1 ; and close de loop

mcont:  MOV DI,061Dh
        CMP byte ptr [DI],0 ; if is 1, don't allow SafeBoot at this point
        JNE bootn ; to avoid the 'security hole'.
        MOV AH,02h
        INT 16h ; read keyboard flags
        TEST AX,000Fh ; Shift, Ctrl or Alt key pressed?
        JZ bootn ; if none is pressed, loads GAG normally

; SafeBoot

        XOR AX,AX ; if not, enter SafeBoot
        MOV DS,AX
        MOV ES,AX
        MOV SI,7DBEh ; First entry in the partition table
        MOV CX,4 ; max. 4 entries
lobuc:  CMP byte ptr DS:[SI],80h ; active?
        JZ safe ; Boot that partition
        ADD SI,10h ; next entry
        LOOP lobuc
        MOV AL,50 ; error 2, no active partition!
        JMP error
safe:   MOV DL,80h ; here we load the boot sector of the active partition
        MOV AH,41h
        MOV BX,55AAh
        INT 13h ; test for BIOS extensions
        JC safeb
        CMP BX,0AA55h
        JNE safeb
        MOV DI,0626h        ; LBA mode
        MOV CX,DS:[SI+8]
        MOV DS:[DI],CX
        MOV CX,DS:[SI+10]
        MOV DS:[DI+2],CX
        MOV AH,42h
        MOV SI,061Eh
        MOV BX,3 ; try 3 times max
hload3: PUSH AX
        PUSH SI
        PUSH BX
        MOV DL,80h
        INT 13h
        JNC hrun3b
        POP BX
        POP SI
        POP AX
        DEC BX
        JNZ hload3
        MOV AL,49 ; error 1, error reading a sector!
        JMP error

; loads GAG (it's here because in the end is too far for a relative JMP)

bootn:  MOV BX,011Bh ; offset where GAG is loaded
        MOV AX,1000h ; segment where GAG is loaded
        MOV DS,AX
        MOV ES,AX
        MOV CX,3 ; try 3 times max
hload:  PUSH CX
        MOV DX,80h ; Hard disk 0, head 0
        MOV CX,2h ; sector 2, track 0
        MOV AH,2 ; BIOS_load_sector
        MOV AL,nsect ; NSECT sectors to be loaded
        INT 13h ; load the MBR
        JNC hrun
        POP CX
        LOOP hload
        MOV AL,49 ; error 1, error reading a sector!
        JMP error

hrun3b: POP BX
        POP SI
        POP AX
        JMP hrun4
safeb:  MOV DL,80h       ; CHS mode
        MOV DH,DS:[SI+1] ; head
        MOV CX,DS:[SI+2] ; sector and track
        MOV BX,3 ; try 3 times max
hload2: PUSH BX
        PUSH CX
        PUSH DX
        MOV BX,7C00h ; address where we load the boot sector
        MOV AX,0201h ; one sector
        INT 13h
        JNC hrun2
        POP DX
        POP CX
        POP BX
        DEC BX
        JNZ hload2
        MOV AL,49 ; error 1, error reading a sector!
        JMP error
hrun2:  POP DX
        POP CX
        POP BX
hrun4:  CMP word ptr DS:[7DFEh],0AA55h ; MBR signature?
        JZ hrun3
        MOV AL,52 ; error 4, no boot sector found!
        JMP error
hrun3:  DB 0EAh
        DW 7C00h
        DW 0h

hrun:   MOV AX,1000h
        MOV ES,AX ; GAG's segment
        CMP word ptr ES:[11Ch],4147h ; tests for GAG signature
        JNE herr
        CMP word ptr ES:[11Eh],0047h
        JNE herr
        DB 0EAh
        DW 120h
        DW 1000h

herr:   MOV AL,51 ; error 3, GAG is not in the first track!

error:  MOV BX,7
        MOV AH,0Eh
        INT 10h ; prints the error passed in AL
mbuc:   JMP mbuc ; and locks the machine to allows user to read it.


; **************************************************
; *                     INTRO                      *
; * allows to type a string of up to 15 characters *
; * and returns it in mtemp. Prints the string     *
; * pointed by SI in the column DL                 *
; **************************************************

intro:  PUSH SI
        CALL clmsg
        MOV byte ptr [mtemp+15],0
        MOV byte ptr [mtemp+16],0
        MOV byte ptr [mtemp],"_" ; cursor
        PUSH DX
        MOV SI,13603
        MOV BX,72
        MOV AX,56
        CALL window
        POP DX
        POP SI
        MOV DH,0Ah
        MOV BX,1210h ; ink black, background normal white
        CALL prcen
        MOV DI,offset mtemp
        MOV BH,0 ; 0 caracters entered
        CALL refresh
inbuc1: PUSH BX
        CALL waitkey
        POP BX
        CMP AL,0Dh ; CR?
        JNZ insig1
        MOV byte ptr [DI],32
        MOV byte ptr [mtemp+15],0
        RET
insig1: CMP AL,08h ; delete?
        JNZ insig2
        CMP BH,0
        JZ inbuc1
        MOV byte ptr [DI],32
        DEC DI
        DEC BH
        MOV byte ptr [DI],"_"
        JMP inprn
insig2: CMP AL,32 ; ASCII code < 32?
        JL inbuc1
        CMP byte ptr [prpaswd],2 ; if prpaswd=2, the user can type numbers only
        JNE insig3
        CMP AL,48
        JL inbuc1
        CMP AL,57
        JG inbuc1
insig3: CMP BH,15
        JE inbuc1
        MOV [DI],AL
        INC BH
        INC DI
        MOV byte ptr [DI],"_"
inprn:  CALL refresh
        JMP inbuc1
refresh:
        CMP byte ptr [prpaswd],1 ; if prpaswd=1, intro doesn't do echo
        JE reret
        PUSH BX
        PUSH DI
        PUSH DX
        MOV SI,offset mtemp
        MOV BX,1210h ; ink black, background normal white
        MOV DX,0C0Ch
        CALL prstr
        POP DX
        POP DI
        POP BX
reret:  RET


; ************************************************
; *                     MAIN                     *
; * prints the main menu, with the icons and the *
; * presentation text                            *
; ************************************************

main:   CALL palette
        CALL presen
        MOV BX,1016h ; ink green, background black
        MOV SI,offset mnumb
        MOV DH,4h
        CALL prcen
        MOV byte ptr[prstp],0
        JMP puticon ; calls to puticon and returns


; ************************************
; *             CLEAN1               *
; * cleans the screen in two windows *
; ************************************

clean1: MOV AL,15
        CALL setcolor
        MOV SI,0
        MOV CX,38400
cbuc1:  MOV byte ptr ES:[SI],0
        INC SI
        LOOP cbuc1
        MOV SI,10080
        MOV BX,78
        MOV AX,320
        PUSH DX
        PUSH BX
        PUSH AX
        PUSH SI
        MOV DX,AX
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],03Fh
        MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wcbuc1: MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wcbuc1
        MOV byte ptr ES:[SI],0FCh
        MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP SI
        ADD SI,80 ; next line
        PUSH SI
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],03Fh
        MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wcbuc2: MOV AL,11
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wcbuc2
        MOV byte ptr ES:[SI],0F8h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],04h
        MOV AL,10
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP SI
        ADD SI,80 ; next line
        PUSH SI
        MOV CX,DX
wcbuc3: PUSH CX
        MOV AL,04
        CALL setcolor
        MOV byte ptr ES:[SI],30h
        MOV AL,02
        CALL setcolor
        MOV byte ptr ES:[SI],0Fh
        MOV AL,09
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wcbuc4: MOV AL,02h
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        MOV AL,0Dh
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        LOOP wcbuc4
        MOV AL,01h
        CALL setcolor
        MOV byte ptr ES:[SI],0Ch
        MOV AL,02h
        CALL setcolor
        MOV byte ptr ES:[SI],0F0h
        MOV AL,0Ch
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP CX
        POP SI
        ADD SI,80
        PUSH SI
        LOOP wcbuc3
        POP SI
        PUSH SI
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],1Fh
        MOV AL,4
        CALL setcolor
        MOV byte ptr ES:[SI],20h
        MOV AL,10
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wcbuc5: MOV AL,00001110b
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wcbuc5
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],0FCh
        MOV AL,14
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP SI
        ADD SI,80
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],3Fh
        MOV AL,14
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        INC SI
        MOV CX,BX
wcbuc6: MOV AL,14
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        MOV AL,1
        CALL setcolor
        MOV byte ptr ES:[SI],0FFh
        INC SI
        LOOP wcbuc6
        MOV byte ptr ES:[SI],0FCh
        MOV AL,14
        CALL setcolor
        MOV byte ptr ES:[SI],0h
        POP AX
        POP BX
        POP DX
        RET


; ***********************
; *       PALETTE       *
; * changes the palette *
; ***********************

palette:
        PUSH ES
        PUSH DS
        POP ES
        MOV AX,1012h
        MOV BX,0h
        MOV CX,10h
        MOV DX,offset paleta
        INT 10h
        MOV AX,1002h
        MOV DX,offset paleta2
        INT 10h
        POP ES
        RET


; *********************************************************
; *                     PRESEN                            *
; * prints the copyright message and the rest of the info *
; *********************************************************

presen: CALL clean1
        MOV BX,1012h ; ink normal white, background black
        MOV SI,offset mgag
        MOV DH,0h
        CALL prcen
        MOV SI,offset mversn
        MOV DX,124h
        CALL prstr
        MOV SI,offset mcopyr
        MOV DH,1h
        JMP prcen ; calls to prcen and returns


; **********************************************************
; *                         PRCEN                          *
; * prints a string pointed by SI at DH=row, but centered. *
; **********************************************************

prcen:  PUSH SI
        MOV DL,40 ; 40 columns
prcbc:  CMP byte ptr[SI],0
        JZ prcfn
        CMP byte ptr [SI],255 ; Color code?
        JZ prcnt2
        INC SI
        DEC DL
        JMP prcbc
prcnt2: INC SI
        INC SI ; jump the color code and the color
        JMP prcbc
prcfn:  SAR DL,1 ; divide by 2 the conten of DL
        POP SI ; and go to prstr


; *****************************************************************
; *                        PRSTR                                  *
; * prints a string pointed by SI at DH=row DL=column coordinates *
; * BL=text foreground color                                      *
; * BH=text background color. String ended with 0h                *
; *****************************************************************

prstr:  CMP byte ptr [SI],0
        JNE prstr2
        RET
prstr2: CMP BL,10h
        JL prin1
        SUB BL,10h
prin1:  CMP BH,10h
        JL prin2
        SUB BH,10h
prin2:  INC DH
        MOV COORDX,DL
        MOV COORDY,DH ; select the coordinates
prbuc:  MOV AL,[SI]
        CMP AL,0h
        JZ prcnt
        CMP AL,0FFh ; color code?
        JNZ prcn2
        INC SI
        MOV BL,[SI] ; take the color
        INC SI
        JMP prbuc
prcn2:  INC SI
        PUSH SI
        PUSH BX
        CALL prchar
        POP BX
        POP SI
        JMP prbuc
prcnt:  RET


; ***************************************************************
; *                           PRCHAR                            *
; * Prints a single character stored in AL in COORDX and COORDY *
; * with BH as background color and BL as foreground color.     *
; ***************************************************************

prchar: MOV SI,offset character
        CMP AL,31
        JG prcsg1
        MOV AL,32
        JMP prcsg2
prcsg1: CMP AL,127
        JLE prcsg2
        MOV AL,32
prcsg2: SUB AL,32
        MOV AH,32
        MUL AH
        ADD SI,AX
        MOV AL,COORDY
        MOV AH,0
        MOV DX,1280
        MUL DX
        MOV CL,COORDX
        MOV CH,0
        ADD AX,CX
        ADD AX,CX
        MOV DI,AX
        MOV DH,BH
        XOR DH,BL ; mix the two colours (FG and BG)
        MOV DL,1 ; bit 1
        MOV CX,4 ; four bytes
prcbc2: PUSH CX
        PUSH SI
        PUSH DI
        PUSH BX
        MOV AL,DL
        CALL setcolor
        MOV CX,16
        MOV AL,BL
        AND AL,DL
        MOV AH,DH
        AND AH,DL
        CMP AH,0 ; if zero, both planes are equal (0 or 1)
        JNZ prneq
        CMP AL,0
        JNZ pr1
pr0:    MOV word ptr ES:[DI],0 ; both planes are 0
        ADD SI,2
        ADD DI,80
        LOOP pr0
        JMP prfine
pr1:    MOV word ptr ES:[DI],0FFFFh ; both planes are 1
        ADD SI,2
        ADD DI,80
        LOOP pr1
        JMP prfine
prneq:  CMP AL,0 ; if 0, then FG plane is 0 and BG plane is 1
        JZ pr4
pr3:    MOV BX,[SI]
        MOV ES:[DI],BX
        ADD SI,2
        ADD DI,80
        LOOP pr3
        JMP prfine
pr4:    MOV BX,[SI]
        NOT BX
        MOV ES:[DI],BX
        ADD SI,2
        ADD DI,80
        LOOP pr4
prfine: ROL DL,1
        POP BX
        POP DI
        POP SI
        POP CX
        LOOP prcbc2
        MOV AL,COORDX
        INC AL
        MOV COORDX,AL
        RET




; **********************************************************************
; *                             PUTICON                                *
; * puts the OS in the screen. Only if PRSTP=0, prints the SETUP icon. *
; **********************************************************************

puticon:
        MOV byte ptr [prinam],0 ; print icons and names
        MOV DI,offset putemp
        MOV SI,offset msetp2
        MOV CX,10h
putb3:  MOV BH,[SI]
        MOV [DI],BH
        INC SI
        INC DI
        LOOP putb3
        CMP byte ptr [prstp],0
        JNE putnx1
        MOV AX,12h
        MOV byte ptr [thekey],keysetup ; setup key
        CALL picon ; setup icon
putnx1: MOV SI,offset ostabl
        MOV AL,0 ; first position
        MOV BL,49 ; key 1
putb2:  MOV AH,[SI]
        CMP AH,0FFh
        JZ putfin
        MOV DI,offset putemp
        INC SI
        MOV CX,0Fh
putbuc: MOV BH,[SI]
        MOV [DI],BH
        INC SI
        INC DI
        LOOP putbuc
        PUSH SI
        PUSH AX
        PUSH BX
        MOV [thekey],BL
        CALL picon
        POP BX
        POP AX
        POP SI
        ADD SI,24
        INC BL
        INC AX
        INC AX
        JMP putb2
putfin: RET


; **************************************
; *              SHWICN                *
; * prints all the icons in the screen *
; **************************************

shwicn: MOV byte ptr [putemp+1],0
        MOV CL,[icons]
        MOV CH,0
        MOV AX,0
        MOV byte ptr [prinam],1 ; prints only icons
shbuc5: PUSH AX
        PUSH CX
        MOV AH,AL
        INC AH
        MOV BL,AL
        ADD BL,65
        MOV [putemp],BL
        CALL picon
        POP CX
        POP AX
        INC AX
        LOOP shbuc5
        RET


; ***********************************************************
; *                     PICON                               *
; * prints the icon given in AH at the position given by AL *
; * and the name, given in putemp                           *
; * if PRINAM is 1, doesnt print the name                   *
; ***********************************************************

picon:  PUSH AX
        MOV SI,offset icons
        INC SI
        PUSH AX
        PUSH DX
        MOV BX,800 ; size (in bytes) of each icon
        MOV AL,AH
        MOV AH,0
        MUL BX
        ADD SI,AX
        POP DX
        POP AX
        PUSH AX
        MOV DI,10721
        AND AL,1
        JZ sigpic
        ADD DI,20
sigpic: POP AX
        PUSH AX
        AND AL,2
        JZ sigpi2
        ADD DI,40
sigpi2: POP AX
        MOV AH,0
        SHR AX,1
        SHR AX,1
        MOV BX,5120
        MUL BX
        ADD DI,AX
        PUSH SI
        PUSH DI
        POP SI
        PUSH AX
        PUSH BX
        PUSH CX
        PUSH DX
        MOV AX,48
        CMP byte ptr [prinam],0 ; has to print the names?
        JE sigpi3
        MOV BX,5
        JMP sigpi4
sigpi3: MOV BX,36
sigpi4: CALL window
        POP DX ; now we print the icon, starting in the screen position pointed
        POP CX ; by ES:DI. The starting of the icon is pointed by DS:SI
        POP BX
        POP AX
        POP SI
        PUSH DI ; DI points to the start of the window, but we want the icon INTO the
        ADD DI,481 ; window, so we add 4 lines, and one char to the rigth.
        MOV CX,4 ; we have to print four planes for each icon
        MOV AX,8 ; the first plane is the 8
bucle3: CALL setcolor
        CALL prplan
        ROR AX,1 ; next plane
        LOOP bucle3
        POP DI
        POP AX ; print the text
        PUSH AX
        MOV DL,4
        AND AL,1h
        JZ picsg
        ADD DL,10
picsg:  POP AX
        PUSH AX
        AND AL,2h
        JZ picsg2
        ADD DL,20
picsg2: POP AX
        SHR AX,1
        SHR AX,1
        MOV BL,4
        MUL BL
        ADD AL,6
        MOV DH,AL
        MOV byte ptr [putemp+15],0
        MOV SI, offset putemp
        MOV BX,1210h ; ink black, background normal white
picsg3: ADD DH,2
        PUSH DX
        CALL prstr
        POP DX
        CMP byte ptr [prinam],0 ; has to print the names?
        JE picsg4
        RET
picsg4: INC DH
        MOV SI,offset mthek
        JMP prstr ; calls prstr and return


; ***********************************************************
; *                           PRPLAN                        *
; * Prints one plane of an icon in ES:DI. The plane must be *
; * pointed by DS:SI                                        *
; ***********************************************************

prplan: PUSH DI
        PUSH CX
        PUSH AX
        MOV CX,40 ; each icon has 40 lines
prbuc1: PUSH CX
        MOV CX,5 ; and each line has 5 bytes
prbuc2: MOV AL,DS:[SI]
        MOV ES:[DI],AL
        INC SI
        INC DI
        LOOP prbuc2
        ADD DI,75 ; next line in the screen
        POP CX
        LOOP prbuc1
        POP AX
        POP CX
        POP DI
        RET


; **********************************************************************
; *                             WAITKEY                                *
; * waits for a keystroke and return it in AL. Also decrements the     *
; * boot timer counter and returns the ASCII in TOBOOT if it reaches 0 *
; **********************************************************************

waitkey:
        CMP byte ptr [counter],0 ; timer started?
        JE wnotim
        IN AL,42h
        IN AL,42h ; reads the high byte of the timer
        CMP AL,127 ; is greater than 127?
        JL wnotim
        MOV AX,[count1]
        DEC AX
        CMP AX,1
        JE wboot
        MOV [count1],AX
        MOV AL,0B0h
        OUT 43h,AL
        MOV AL,0FFh
        OUT 42h,AL
        MOV AL,0FFh
        OUT 42h,AL
        JMP wnotim2
wboot:  MOV byte ptr [counter],0 ; stops the timer
        MOV AL,[toboot] ; and returns the ASCII of the OS to boot
        RET
wnotim2:
        PUSH AX
        MOV AL,[timertem]
        INC AL
        MOV [timertem],AL
        CMP AL,4
        JNE wtimy
        MOV byte ptr [timertem],0
        MOV AL,15
        CALL setcolor
        MOV SI,[tpos]
        MOV byte ptr ES:[SI],0
        DEC SI
        MOV [tpos],SI
wtimy:  POP AX
wnotim: MOV AH,1
        INT 16h
        JZ waitkey
        MOV byte ptr [counter],0 ; stops the timer
readkey:
        MOV AH,0
        INT 16h

; code for keyboard translation, used to add support for
; AZERTY and QWERTZ keyboards.

        PUSH SI
        CMP byte ptr DS:[11Bh],0 ; QWERTY type?
        JNZ azlpn1
        MOV SI,offset kbtabl0
        JMP azlp
azlpn1: CMP byte ptr DS:[11Bh],1 ; AZERTY type?
        JNZ azlpn2
        MOV SI,offset kbtabl1
        JMP azlp
azlpn2: MOV SI,offset kbtabl2 ; QWERTZ type
azlp:   CMP byte ptr [SI],0 ; end of table?
        JZ azend ; end loop
        CMP AL,DS:[SI] ; key found?
        JZ azfnd
        INC SI
        INC SI ; next key in the table
        JMP azlp ; close loop
azfnd:  INC SI
        MOV AL,DS:[SI] ; change the readed key for the translated key
azend:  POP SI

; end of keyboard translation support code

endkey: RET


; ****************************************
; * Tables for QWERTZ and AZERTY support *
; * Uncoment it to add support.          *
; ****************************************



        ; KBTABL points to the translation tables. Them contains the ASCII
        ; code readed from the keyboard and the ASCII code translated.

; Table for QWERTY keyboards.

kbtabl0:

        DB 0 ; end of table

; Table for AZERTY keyboards.

kbtabl1:
        DB "A","Q" ; 'A' key is translated as 'Q' key
        DB "Q","A" ; 'Q' key is translated as 'A' key
        DB "a","q","q","a","W","Z","Z","W","w","z","z","w"
        DB ":","M","M",":",";","m","m",";"
        DB 0 ; end of table

; Table for QWERTZ keyboards.

kbtabl2:
        DB "Z","Y" ; 'Z' key is translated as 'Y' key
        DB "Y","Z" ; 'Y' key is translated as 'Z' key
        DB "z","y","y","z"
        DB 0 ; end of table



; ***************************************************
; *                     TOASC                       *
; * converts the value of AL into an HEX Ascii code *
; ***************************************************

toasc:  AND AL,0Fh
        CMP AL,9
        JBE toasig
        ADD AL,55
        RET
toasig: ADD AL,48
        RET


; ***************************************************
; *                     LOADMBR                     *
; * loads the sector indicated in exten1 and exten2 *
; * (CHS mode) or in exten5, exten6 and exten7      *
; * (LBA mode).                                     *
; ***************************************************

loadmbr:
        PUSH DS
        PUSH ES
        PUSH BX
        PUSH AX
        PUSH CX
        PUSH DX
        PUSH DI
        PUSH SI
        MOV [lderr],0

        CMP [whatdo],1
        JE ldmbr1
        CMP [whatdo],2
        JE ldmbr3

        MOV DX,[exten1]
        MOV CX,[exten2]
        CMP DH,0FEh
        JE ldmbr2
        CMP DH,0FFh
        JNE ldmbr1
ldmbr2: CMP CX,0FFFFh
        JNE ldmbr1

ldmbr3: MOV BX,offset MBR ; Use LBA mode
        MOV [lbaoff],BX
        PUSH DS
        POP BX
        MOV [lbaseg],BX
        MOV DI,offset lbaadd
        MOV BX,[exten5]
        MOV DS:[DI],BX
        INC DI
        INC DI
        MOV BX,[exten6]
        MOV DS:[DI],BX
        INC DI
        INC DI
        MOV BL,[exten7]
        MOV BH,0
        MOV DS:[DI],BX
        INC DI
        INC DI
        MOV BX,0
        MOV DS:[DI],BX
        MOV SI,offset lbatbl
        MOV DL,[drives]
        MOV AH,42h
        INT 13h
        JNC loexit
        MOV [lderr],1
        JMP loexit


ldmbr1: MOV BX,offset MBR ; Use CSH mode
        PUSH DS
        POP ES
        MOV DX,[exten1]
        MOV CX,[exten2]
        MOV AX,0201h ; 1 sector BIOS_load_sector
        INT 13h ; load the MBR in ES:MBR
        JNC loexit
        MOV [lderr],1
loexit: POP SI
        POP DI
        POP DX
        POP CX
        POP AX
        POP BX
        POP ES
        POP DS
        RET


prlett: MOV SI,offset mlett
        MOV DH,03h
        MOV BX,1016h ; ink green, background black
        JMP prcen


prnum:  MOV SI,offset mnum
        MOV DH,04h
        MOV BX,1016h ; ink green, background black
        JMP prcen


; *******************************************
; *                 CLMSG                   *
; * clears the temporary buffer with spaces *
; *******************************************

clmsg:  MOV SI,offset mtemp
        MOV CX,39
clmbc:  MOV byte ptr [SI],32
        INC SI
        LOOP clmbc
        MOV byte ptr [SI],0
        RET


; ********************************************************************
; * This is the 16 colour palette used by GAG's icons (order: R,G,B) *
; ********************************************************************

paleta  DB 0,0,0        ; 0 Black
        DB 32,32,32     ; 1 gray
        DB 48,48,48     ; 2 White
        DB 48,32,32     ; 3 pink
        DB 63,63,63     ; 4 brigth white
        DB 48,0,0       ; 5 Red
        DB 0,48,0       ; 6 Green
        DB 56,44,2      ; 7 orange
        DB 43,31,8      ; 8 dark orange
        DB 50,50,0      ; 9 Yellow
        DB 0,0,48       ; A Blue
        DB 32,0,0       ; B Dark red
        DB 0,48,48      ; C Cyan
        DB 48,16,16     ; D dark pink
        DB 10,9,28      ; E dark blue
        DB 0,32,0       ; F dark green

paleta2 DB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,0

; *************************************************************************
; * and this is the 10 colour palette used by the text and windows of GAG *
; *************************************************************************

        DB 0,0,0,32,32,32,48,48,48,48,0,0,63,63,63,48,0,48,0,48,0,63,0,0
        DB 63,40,0,48,32,0


; Messages of GAG

INCLUDE MESSAGES.MSG

mversn  DB "v4.0",0
mcopyr  DB "2000 Raster Software Vigo",0
mok     DB "OK",0

tpaswd  DB 15 DUP (32)  ; here we save the password for setup
ispaswd DB 0 ; 0 if there's no password. 1 if is defined


; activ contains the partition types that can be hidden during a boot

activ:  DB 01h,04h,06h,07h,0Bh,0Ch,0Eh,0h


; inactiv contains the partition types that can be unhidden during a boot

inact:  DB 11h,14h,16h,17h,1Bh,1Ch,1Eh,0h


; The character table of GAG. Each character is 16x16 pixels

character:

;   character

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; ! character
DB 1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,0,1,0,1,0,0,0,1,128,1,128,0,0,0,0,0,0

; " character
DB 6,96,6,96,6,96,6,96,2,64,2,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; # character
DB 3,48,3,48,2,32,6,96,15,240,6,96,4,64,12,192,31,224,12,192,8,128,25,128,25,128,0,0,0,0,0,0

; $ character
DB 3,224,6,176,12,144,12,128,6,128,7,128,1,224,0,240,0,184,8,152,12,152,14,176,3,224,0,128,0,128,0,0

; % character
DB 14,16,59,240,50,96,98,64,98,192,100,140,101,178,57,50,3,98,2,98,6,100,4,100,12,56,0,0,0,0,0,0

; & character
DB 7,0,13,128,24,128,24,128,25,128,15,60,30,24,54,16,99,48,97,160,96,224,49,242,31,60,0,0,0,0,0,0

; ' character
DB 1,128,1,128,1,128,1,128,0,128,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; ( character
DB 0,192,1,128,1,0,3,0,2,0,6,0,6,0,6,0,6,0,6,0,6,0,2,0,3,0,1,0,1,128,0,192

; ) character
DB 3,0,1,128,0,128,0,192,0,64,0,96,0,96,0,96,0,96,0,96,0,96,0,64,0,192,0,128,1,128,3,0

; * character
DB 1,192,4,144,6,176,1,192,6,176,4,144,1,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; + character
DB 0,0,0,0,1,0,1,0,1,0,1,0,31,240,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0

; , character
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,3,128,0,128,1,0,2,0

; - character
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,224,7,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; . character
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,128,1,128,0,0,0,0,0,0

; / character
DB 0,96,0,96,0,64,0,192,0,192,0,128,1,128,1,128,1,0,3,0,3,0,2,0,6,0,6,0,0,0,0,0

; 0 character
DB 3,128,12,96,8,32,24,48,24,48,24,48,24,48,24,48,24,48,24,48,8,32,12,96,3,128,0,0,0,0,0,0

; 1 character
DB 0,128,3,128,5,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,7,224,0,0,0,0,0,0

; 2 character
DB 3,192,6,224,8,112,8,48,0,48,0,32,0,96,0,64,0,128,1,0,2,8,7,248,15,240,0,0,0,0,0,0

; 3 character
DB 3,192,6,224,8,96,0,96,0,64,0,192,3,224,0,112,0,48,0,48,0,48,12,96,7,192,0,0,0,0,0,0

; 4 character
DB 0,96,0,224,0,224,1,96,1,96,2,96,2,96,4,96,4,96,15,248,0,96,0,96,0,96,0,0,0,0,0,0

; 5 character
DB 3,240,3,224,4,0,7,0,7,192,4,224,0,96,0,48,0,48,0,48,0,96,12,96,7,128,0,0,0,0,0,0

; 6 character
DB 0,112,1,128,3,0,6,0,7,192,14,112,12,48,12,24,12,24,12,24,4,16,6,48,1,192,0,0,0,0,0,0

; 7 character
DB 7,248,15,248,8,48,0,32,0,32,0,96,0,64,0,64,0,192,0,128,0,128,1,128,1,0,0,0,0,0,0,0

; 8 character
DB 1,192,2,32,6,16,6,16,3,32,3,192,1,224,6,112,12,56,12,24,12,24,6,48,1,192,0,0,0,0,0,0

; 9 character
DB 1,192,6,48,4,16,12,24,12,24,12,24,6,24,7,56,1,240,0,48,0,96,0,192,7,0,0,0,0,0,0,0

; : character
DB 0,0,0,0,0,0,0,0,6,0,6,0,0,0,0,0,0,0,0,0,0,0,6,0,6,0,0,0,0,0,0,0

; ; character
DB 0,0,0,0,0,0,0,0,6,0,6,0,0,0,0,0,0,0,0,0,0,0,6,0,7,0,1,0,2,0,4,0

; < character
DB 0,0,0,0,0,8,0,48,0,192,3,0,12,0,12,0,3,0,0,192,0,48,0,8,0,0,0,0,0,0,0,0

; = character
DB 0,0,0,0,0,0,0,0,15,248,0,0,0,0,0,0,15,248,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; > character
DB 0,0,0,0,8,0,6,0,1,128,0,96,0,24,0,24,0,96,1,128,6,0,8,0,0,0,0,0,0,0,0,0

; ? character
DB 7,192,12,224,8,96,0,96,0,192,1,128,3,0,2,0,2,0,0,0,0,0,3,0,3,0,0,0,0,0,0,0

; @ character
DB 1,248,7,14,12,3,24,1,48,1,48,237,97,157,99,25,99,25,102,49,102,49,102,115,51,222,48,0,24,3,15,254

; A character
DB 1,0,3,0,3,128,7,128,5,192,12,192,8,192,24,96,31,224,48,112,32,48,96,56,240,124,0,0,0,0,0,0

; B character
DB 127,192,24,96,24,48,24,48,24,48,24,96,31,240,24,48,24,24,24,24,24,24,24,48,127,224,0,0,0,0,0,0

; C character
DB 7,144,28,240,48,48,48,16,96,0,96,0,96,0,96,0,96,0,48,16,48,48,28,224,7,128,0,0,0,0,0,0

; D character
DB 127,192,24,112,24,24,24,24,24,12,24,12,24,12,24,12,24,12,24,24,24,24,24,112,127,192,0,0,0,0,0,0

; E character
DB 127,224,24,32,24,32,24,0,24,64,24,64,31,192,24,64,24,64,24,0,24,16,24,48,127,224,0,0,0,0,0,0

; F character
DB 127,224,24,32,24,32,24,0,24,64,24,64,31,192,24,64,24,64,24,0,24,0,24,0,126,0,0,0,0,0,0,0

; G character
DB 3,200,14,120,24,24,24,8,48,0,48,0,48,124,48,24,48,24,24,24,24,24,14,112,3,192,0,0,0,0,0,0

; H character
DB 126,126,24,24,24,24,24,24,24,24,24,24,31,248,24,24,24,24,24,24,24,24,24,24,126,126,0,0,0,0,0,0

; I character
DB 126,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,126,0,0,0,0,0,0,0

; J character
DB 31,128,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,102,0,102,0,60,0,0,0,0,0,0,0

; K character
DB 126,124,24,48,24,96,24,192,25,128,27,0,31,0,27,128,25,192,24,224,24,112,24,56,126,126,0,0,0,0,0,0

; L character
DB 126,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,0,24,16,24,48,127,224,0,0,0,0,0,0

; M character
DB 120,7,24,6,28,14,28,14,30,30,22,22,23,54,19,38,19,102,17,198,17,198,16,134,124,159,0,0,0,0,0,0

; N character
DB 120,62,28,8,30,8,22,8,19,8,19,136,17,200,16,200,16,104,16,120,16,56,16,24,124,8,0,0,0,0,0,0

; O character
DB 3,192,14,112,24,24,24,24,48,12,48,12,48,12,48,12,48,12,24,24,24,24,14,112,3,192,0,0,0,0,0,0

; P character
DB 127,128,24,192,24,96,24,96,24,96,24,192,31,128,24,0,24,0,24,0,24,0,24,0,126,0,0,0,0,0,0,0

; Q character
DB 3,192,14,112,24,24,24,24,48,12,48,12,48,12,48,12,48,12,24,24,24,24,14,112,3,192,1,192,0,120,0,0

; R character
DB 127,128,24,192,24,96,24,96,24,96,24,192,31,128,25,128,24,192,24,192,24,96,24,112,126,56,0,0,0,0,0,0

; S character
DB 30,128,51,128,97,128,96,128,112,0,60,0,15,0,3,128,1,192,64,192,96,192,121,128,79,0,0,0,0,0,0,0

; T character
DB 255,240,198,48,134,16,134,16,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,31,128,0,0,0,0,0,0

; U character
DB 126,62,24,8,24,8,24,8,24,8,24,8,24,8,24,8,24,8,28,8,12,16,15,48,3,192,0,0,0,0,0,0

; V character
DB 248,124,96,24,96,16,48,48,48,32,24,96,24,64,12,192,12,128,7,128,7,0,3,0,2,0,0,0,0,0,0,0

; W character
DB 251,231,97,131,97,131,48,198,48,198,25,198,25,102,25,100,15,60,14,56,6,24,4,16,4,16,0,0,0,0,0,0

; X character
DB 124,124,56,48,24,32,12,64,6,192,7,128,3,128,3,192,6,192,4,96,8,48,24,56,124,124,0,0,0,0,0,0

; Y character
DB 124,60,56,24,24,16,12,32,14,96,6,64,3,192,3,128,1,128,1,128,1,128,1,128,7,224,0,0,0,0,0,0

; Z character
DB 63,240,32,96,32,192,1,192,1,128,3,0,7,0,6,0,12,0,28,0,24,16,48,48,127,224,0,0,0,0,0,0

; [ character
DB 7,192,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,6,0,7,192,0,0

; \ character
DB 6,0,6,0,2,0,3,0,3,0,1,0,1,128,1,128,0,128,0,192,0,192,0,64,0,96,0,96,0,0,0,0

; ] character
DB 7,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,0,192,7,192,0,0

; ^ character
DB 3,128,6,192,12,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; _ character
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,255,255,255,255,0,0

; ` character
DB 6,0,3,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; a character
DB 0,0,0,0,0,0,0,0,3,192,6,96,4,96,1,224,6,96,12,96,12,96,12,224,7,112,0,0,0,0,0,0

; b character
DB 14,0,6,0,6,0,6,0,6,96,6,240,7,56,6,24,6,24,6,24,6,24,7,48,1,224,0,0,0,0,0,0

; c character
DB 0,0,0,0,0,0,0,0,1,224,6,112,4,48,12,0,12,0,12,0,14,16,7,48,3,224,0,0,0,0,0,0

; d character
DB 0,112,0,48,0,48,0,48,3,176,6,112,12,48,12,48,12,48,12,48,14,112,7,184,3,32,0,0,0,0,0,0

; e character
DB 0,0,0,0,0,0,0,0,1,224,6,112,4,48,15,240,12,0,12,0,14,16,7,48,3,224,0,0,0,0,0,0

; f character
DB 3,128,6,64,6,0,6,0,15,128,6,0,6,0,6,0,6,0,6,0,6,0,6,0,15,0,0,0,0,0,0,0

; g character
DB 0,0,0,0,0,0,0,0,3,128,6,240,12,96,12,96,12,96,6,192,3,128,6,0,15,224,7,240,8,48,7,224

; h character
DB 14,0,6,0,6,0,6,0,6,96,6,240,7,48,6,48,6,48,6,48,6,48,6,48,15,120,0,0,0,0,0,0

; i character
DB 6,0,6,0,0,0,0,0,2,0,14,0,6,0,6,0,6,0,6,0,6,0,6,0,15,0,0,0,0,0,0,0

; j character
DB 3,0,3,0,0,0,0,0,1,0,7,0,3,0,3,0,3,0,3,0,3,0,3,0,3,0,3,0,26,0,14,0

; k character
DB 14,0,6,0,6,0,6,0,6,120,6,32,6,64,6,128,7,128,6,192,6,96,6,48,15,120,0,0,0,0,0,0

; l character
DB 3,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,3,192,0,0,0,0,0,0

; m character
DB 0,0,0,0,0,0,0,0,23,56,119,188,57,204,49,140,49,140,49,140,49,140,49,140,121,206,0,0,0,0,0,0

; n character
DB 0,0,0,0,0,0,0,0,2,96,14,240,7,48,6,48,6,48,6,48,6,48,6,48,15,120,0,0,0,0,0,0

; o character
DB 0,0,0,0,0,0,0,0,1,224,6,112,4,56,12,24,12,24,12,24,14,16,7,48,3,192,0,0,0,0,0,0

; p character
DB 0,0,0,0,0,0,0,0,2,96,14,240,7,56,6,24,6,24,6,24,6,24,7,48,6,224,6,0,6,0,15,0

; q character
DB 0,0,0,0,0,0,0,0,3,144,6,112,12,48,12,48,12,48,12,48,14,112,7,176,3,48,0,48,0,48,0,120

; r character
DB 0,0,0,0,0,0,0,0,2,96,14,224,7,32,6,0,6,0,6,0,6,0,6,0,15,0,0,0,0,0,0,0

; s character
DB 0,0,0,0,0,0,0,0,7,64,12,192,12,64,14,0,7,128,1,192,8,192,12,192,11,128,0,0,0,0,0,0

; t character
DB 0,0,2,0,2,0,6,0,15,128,6,0,6,0,6,0,6,0,6,0,6,0,6,128,3,0,0,0,0,0,0,0

; u character
DB 0,0,0,0,0,0,0,0,14,112,6,48,6,48,6,48,6,48,6,48,6,112,7,184,3,32,0,0,0,0,0,0

; v character
DB 0,0,0,0,0,0,0,0,30,56,12,16,6,48,6,32,3,32,3,64,1,192,1,128,0,128,0,0,0,0,0,0

; w character
DB 0,0,0,0,0,0,0,0,123,206,49,132,49,132,24,204,24,200,13,104,13,104,6,48,6,48,0,0,0,0,0,0

; x character
DB 0,0,0,0,0,0,0,0,15,56,6,16,3,32,1,192,1,192,1,192,2,96,4,48,14,120,0,0,0,0,0,0

; y character
DB 0,0,0,0,0,0,0,0,30,56,12,16,6,48,6,32,3,96,3,64,1,192,1,192,0,128,1,128,13,0,6,0

; z character
DB 0,0,0,0,0,0,0,0,15,240,8,96,8,192,0,192,1,128,3,0,3,16,6,16,15,240,0,0,0,0,0,0

; { character
DB 0,96,0,64,0,192,0,192,0,192,0,192,1,128,7,0,1,128,0,192,0,192,0,192,0,192,0,192,0,64,0,96

; | character
DB 1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128,1,128

; } character
DB 3,0,1,0,1,128,1,128,1,128,1,128,0,192,0,112,0,192,1,128,1,128,1,128,1,128,1,128,1,0,3,0

; ~ character
DB 3,160,5,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;  character
DB 1,128,3,192,6,96,12,48,24,24,48,12,96,6,192,3,192,3,192,3,192,3,192,3,255,255,0,0,0,0,0,0

; The icons of GAG

icons   DB 11            ; number of icons (each one of 40x40x16 colours, splited in
                         ; four planes) DON'T INCLUDE THE SETUP ICON!!!!

; setup

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,0,0,0,1,0,0,0,0,2,16,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,32,128,0,0,0,65,0,0,0,0,130,0,0,0,1,4,0,0,0,2,8,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,16,128,0,0,0,1,0,0,0,0,6,0,0,0,0,64,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,240,0,0,0,1,248,0,0,0,3,252,0,0,0,7,252,0,0,0,15,252,0,0,0,31,252,0,0,0,63,248,0,0,0,127,240,0,0,0,255,224,0,0,1,255,192,0,0,3,255,128,0,0,7,255,0,0,0,15,254,0,0,0,31,252,0,0,0,63,248,0,0,1,255,240,0,0,1,255,224,0,0,0,255,192,0,0,0,127,128,0,0,0,127,0,0,0,0,62,0,0,0,0,14,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,63,255,255,255,254,63,255,255,255,252,127,255,255,255,248,255,255,255,255,241,255,255,255,255,227,255,255,255,255,199,255,255,255,255,143,255,255,255,255,31,255,255,255,254,63,255,255,255,240,127,255,255,255,224,255,255,255,255,194,255,255,255,255,194,255,255,255,255,229,255,255,255,255,243,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,192,0,0,0,1,0,0,0,0,2,16,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,32,128,0,0,0,65,0,0,0,0,130,0,0,0,1,4,0,0,0,2,8,0,0,0,4,16,0,0,0,8,32,0,0,0,16,64,0,0,0,16,128,0,0,0,1,0,0,0,0,6,0,0,0,0,64,0,0,0,0,224,0,0,0,1,192,0,0,0,3,128,0,0,0,7,0,0,0,0,14,0,0,0,0,28,0,0,0,0,56,0,0,0,0,112,0,0,0,0,224,0,0,0,1,192,0,0,0,15,128,0,0,0,31,0,0,0,0,61,0,0,0,0,61,0,0,0,0,26,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; diskette

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7,124,0,15,128,7,124,0,31,192,7,124,0,31,224,7,124,3,31,224,7,124,3,31,224,7,124,3,31,224,7,124,3,31,224,7,124,0,31,224,7,254,0,63,224,7,255,255,255,224,7,255,255,255,224,7,255,255,255,224,7,255,255,255,224,7,128,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,0,0,0,224,7,127,255,254,224,7,127,255,254,224,7,0,0,0,224,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,127,255,192,0,3,126,0,15,192,3,126,0,31,224,3,126,3,159,224,3,126,3,159,224,3,126,3,159,224,3,126,3,159,224,3,124,0,31,224,3,128,0,63,224,3,255,255,255,224,3,255,255,255,224,3,255,255,255,224,3,255,255,255,224,3,128,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,0,0,0,96,3,127,255,254,96,3,127,255,254,96,3,127,255,254,96,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,248,0,0,16,127,247,124,0,15,191,247,125,255,223,223,247,125,248,95,239,247,125,251,95,239,247,125,251,95,239,247,125,251,95,239,247,125,251,95,239,247,125,255,223,239,247,254,0,63,239,247,255,255,255,239,247,255,255,255,239,247,255,255,255,239,247,255,255,255,239,247,128,0,0,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,127,255,254,239,247,0,0,0,239,247,0,0,0,239,247,0,0,0,239,248,0,0,0,31,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,127,255,255,0,0,127,255,255,0,0,127,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; OS/2

DB 4,0,0,99,48,16,1,0,243,54,130,8,0,115,247,8,32,129,55,243,64,4,0,56,0,0,16,0,56,0,0,64,0,30,0,0,128,0,29,0,1,0,0,14,128,8,0,0,7,16,2,0,0,3,192,16,0,0,1,232,4,0,0,0,248,0,0,0,0,127,32,0,1,0,31,8,0,0,64,16,0,0,0,0,0,192,0,0,0,0,192,0,2,0,0,200,0,0,128,0,8,0,0,0,0,12,0,0,0,0,14,0,4,0,16,38,0,1,0,4,6,0,0,0,0,7,128,0,0,32,19,128,0,0,8,3,128,0,0,64,249,192,0,0,16,241,192,0,0,128,240,192,0,1,0,48,64,0,2,0,112,16,0,8,0,112,132,0,32,0,240,224,129,4,0,192,200,0,16,0,128,193,0,128,0,129,192,0,0,0,241,192,0,0,0,241,128,0,0,0
DB 3,128,0,0,0,15,1,255,144,0,124,15,255,192,0,240,63,129,206,16,128,124,0,14,0,0,240,0,39,0,1,192,0,7,128,3,128,0,17,192,7,0,0,2,224,14,0,0,1,112,14,0,0,0,240,28,0,0,192,56,28,0,0,192,8,24,241,224,207,64,57,251,241,223,144,57,155,49,217,156,49,155,1,129,140,49,155,129,129,140,49,153,195,131,140,49,152,227,135,12,49,152,115,14,12,49,152,51,28,12,57,155,55,24,28,57,251,247,31,156,24,241,230,31,152,28,0,6,0,56,28,0,6,0,56,14,0,0,0,112,14,0,0,0,112,7,0,0,0,224,3,128,0,1,192,1,192,0,3,128,0,240,0,15,0,0,124,0,62,0,0,63,129,252,0,0,15,255,240,0,0,1,255,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 4,0,0,99,48,16,1,255,243,54,130,15,255,243,247,8,63,129,255,243,64,124,0,62,0,0,240,0,63,0,1,192,0,31,128,3,128,0,29,192,7,0,0,14,224,14,0,0,7,112,14,0,0,3,240,28,0,0,193,248,28,0,0,192,248,24,241,224,207,127,57,251,241,223,159,57,155,49,217,156,49,155,1,129,140,49,155,129,129,140,49,153,195,131,140,49,152,227,135,12,49,152,115,14,12,49,152,51,28,12,57,155,55,24,28,57,251,247,31,156,24,241,230,31,152,28,0,6,0,56,28,0,6,0,56,14,0,0,0,112,14,0,0,0,112,7,0,0,0,224,3,128,0,1,192,1,192,0,3,128,0,240,0,15,0,0,124,0,62,0,0,63,129,252,0,0,15,255,240,0,0,1,255,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 7,128,0,0,0,31,1,0,128,0,254,8,0,0,0,248,32,129,4,0,192,4,0,0,0,0,16,0,0,0,0,64,0,2,0,0,128,0,1,0,1,0,0,0,128,8,0,0,0,16,2,0,0,0,64,16,0,0,0,8,4,0,0,0,0,0,0,0,0,0,32,0,1,0,0,8,0,0,64,16,0,0,0,0,0,128,0,0,0,0,192,0,2,0,0,64,0,0,128,0,8,0,0,0,0,12,0,0,0,0,14,0,4,0,16,38,0,1,0,4,2,0,0,0,0,7,0,0,0,32,17,128,0,0,8,3,128,0,0,64,233,128,0,0,16,241,192,0,0,128,48,192,0,1,0,48,64,0,2,0,48,16,0,8,0,112,132,0,32,0,224,224,129,4,0,192,200,0,16,0,128,193,0,128,0,0,192,0,0,0,241,128,0,0,0,241,128,0,0,0

; Windows

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,2,8,0,0,0,0,2,128,0,0,0,8,0,0,0,4,4,0,2,0,0,16,16,1,128,0,8,16,3,224,8,32,0,1,112,0,32,32,5,244,0,8,0,14,255,31,196,64,7,125,63,208,128,15,215,63,220,0,6,239,63,191,0,3,252,127,127,0,0,251,127,127,0,0,126,127,127,0,0,30,127,127,0,0,0,126,254,0,0,0,0,254,0,0,0,0,60,0,0,0,0,28,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,0,0,0,1,240,0,0,0,3,251,128,0,0,3,243,192,0,0,3,247,224,2,0,7,231,240,1,128,7,239,240,3,224,7,207,224,1,112,15,207,224,5,244,0,15,192,14,255,16,71,192,7,125,32,1,128,15,215,0,64,0,6,239,0,128,0,3,252,64,0,0,0,251,64,0,0,0,126,64,0,0,0,30,1,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,159,255,255,255,255,207,255,255,255,255,227,255,255,255,255,241,255,255,255,255,252,63,255,255,255,254,0,63,255,255,255,128,1,255,255,255,241,0,127,255,255,250,8,31,255,255,240,3,143,252,255,224,11,199,254,127,228,7,227,253,31,224,23,243,254,15,192,15,243,252,3,200,47,231,254,128,0,47,231,250,8,0,15,199,241,0,31,199,207,248,130,63,193,143,240,40,63,192,31,225,16,63,128,31,240,3,127,0,31,248,4,127,0,63,252,0,127,0,63,255,0,127,0,127,255,128,126,0,127,255,224,0,0,127,255,255,128,0,255,255,255,252,0,255,255,255,255,128,255,255,255,255,193,255,255,255,255,225,255,255,255,255,241,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,48,0,0,0,0,92,0,0,0,0,62,0,0,0,0,127,192,0,0,0,47,156,192,0,0,190,240,2,0,1,95,249,224,128,0,239,187,248,32,1,250,243,250,144,3,221,243,248,8,1,255,151,244,0,0,255,103,240,20,0,127,231,232,20,0,31,239,224,0,0,14,207,224,40,0,3,0,8,8,0,0,192,4,80,0,0,128,0,144,0,0,128,28,0,24,0,128,62,32,12,0,0,63,32,7,0,0,63,0,3,129,0,126,64,0,225,0,126,0,0,114,0,126,128,0,24,0,252,128,0,0,96,60,0,0,0,3,13,0,0,0,0,65,0,0,0,0,32,0,0,0,0,16,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; Tux

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63,207,240,0,0,127,207,248,0,0,127,255,248,0,0,63,143,240,0,0,31,23,224,0,0,2,34,0,0,0,2,1,0,0,0,2,1,0,0,0,1,2,0,0,0,0,252,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,192,0,15,224,16,48,0,16,16,32,8,0,32,8,64,36,0,32,4,64,196,0,32,4,64,4,0,32,4,64,4,0,35,4,64,4,0,34,4,32,4,0,32,8,16,4,0,32,16,8,8,0,16,32,7,240,0,15,192
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63,207,240,0,0,127,207,248,0,0,127,141,248,0,0,63,119,240,0,0,30,235,224,0,0,1,220,0,0,0,1,254,0,0,0,1,254,0,0,0,0,252,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,15,192,0,15,224,31,240,0,31,240,63,216,0,31,248,63,56,0,31,248,63,248,0,31,248,63,248,0,28,248,63,248,0,29,248,31,248,0,31,240,15,248,0,31,224,7,240,0,15,192,0,0,0,0,0
DB 255,255,255,255,255,255,255,3,255,255,255,248,0,127,255,255,240,0,63,255,255,224,0,31,255,255,192,0,15,255,255,255,207,255,255,255,255,207,255,255,255,255,143,255,255,255,255,119,255,255,255,254,235,255,255,255,225,220,15,255,255,241,254,15,255,255,241,254,7,255,255,240,252,7,255,255,224,2,3,255,255,226,2,1,255,255,194,7,0,255,255,131,15,128,127,255,3,135,192,63,254,7,135,224,63,252,7,135,224,31,252,15,135,224,31,248,15,3,224,31,248,31,3,224,15,240,31,3,224,15,240,15,3,192,15,224,15,3,192,15,224,15,3,128,7,239,207,3,143,231,223,247,3,31,247,191,219,135,31,251,191,59,134,31,251,191,249,254,31,251,191,249,252,28,251,191,248,248,29,251,223,248,0,31,247,239,248,0,31,239,247,240,0,15,223,248,0,0,0,63
DB 0,0,0,0,0,0,0,132,0,0,0,4,0,128,0,0,0,0,0,0,0,0,0,0,0,0,63,255,240,0,0,0,48,0,0,0,0,48,0,0,0,0,0,0,0,0,0,112,0,0,0,0,248,0,0,0,17,252,16,0,0,1,254,0,0,0,1,254,8,0,0,2,253,128,0,0,19,1,128,0,0,1,1,192,0,0,1,128,192,0,0,0,192,96,0,0,0,72,48,64,0,0,0,0,0,2,0,0,0,32,0,0,0,0,0,4,0,132,0,0,0,0,0,0,16,8,0,0,0,0,0,0,0,32,0,16,0,0,32,0,16,0,0,96,8,15,192,0,111,232,31,240,132,223,240,63,248,0,223,248,63,248,73,223,248,63,248,1,223,248,63,248,3,159,248,63,248,7,31,248,31,248,0,31,240,15,248,0,31,224,7,240,0,15,192,0,0,0,0,0

;DOS

DB 0,0,0,0,0,0,0,0,0,0,0,1,255,128,0,0,15,255,240,0,0,63,129,252,0,0,124,0,62,0,0,240,0,15,0,1,192,0,3,128,3,128,0,1,192,7,0,0,0,224,14,0,0,0,112,14,0,0,0,112,28,0,0,0,56,28,0,62,30,56,24,0,127,63,24,56,57,127,179,28,56,33,115,176,28,48,33,115,184,12,48,33,115,156,12,48,33,115,142,12,48,33,115,135,12,48,33,115,131,12,48,33,115,131,12,56,33,115,179,28,56,1,127,191,28,24,2,63,30,24,28,124,30,0,56,28,0,0,0,56,14,0,0,0,112,14,0,0,0,112,7,0,0,0,224,3,128,0,1,192,1,192,0,3,128,0,240,0,15,0,0,124,0,62,0,0,63,129,252,0,0,15,255,240,0,0,1,255,128,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,1,0,128,0,0,8,0,16,0,0,32,129,4,0,0,4,0,32,0,0,16,0,8,0,0,64,0,2,0,0,128,0,1,0,1,0,0,0,128,8,0,0,0,16,2,0,0,0,64,16,0,0,0,8,4,252,0,30,32,0,254,0,63,0,32,255,0,63,132,8,231,0,51,144,0,231,0,56,0,0,231,0,60,0,0,231,0,30,0,0,231,0,15,0,0,231,0,7,128,0,231,0,3,128,8,231,0,51,144,32,255,0,63,132,0,254,0,31,0,4,124,0,14,32,16,0,0,0,8,2,0,0,0,64,8,0,0,0,16,1,0,0,0,128,0,128,0,1,0,0,64,0,2,0,0,16,0,8,0,0,4,0,32,0,0,32,129,4,0,0,8,0,16,0,0,1,0,128,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,254,255,127,255,255,247,255,239,255,255,223,129,251,255,255,252,0,63,255,255,240,0,15,255,255,192,0,3,255,255,128,0,1,255,255,0,0,0,255,246,0,0,0,111,254,0,0,0,127,236,0,0,0,55,252,252,0,0,63,248,254,0,0,31,216,255,0,0,27,248,231,0,0,31,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,240,231,0,0,15,248,231,0,0,31,216,255,0,0,27,248,254,0,0,31,252,124,0,0,63,236,0,0,0,55,254,0,0,0,127,246,0,0,0,111,255,0,0,0,255,255,128,0,1,255,255,192,0,3,255,255,240,0,15,255,255,252,0,63,255,255,223,129,251,255,255,247,255,239,255,255,254,255,127,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,30,0,0,0,127,63,0,0,57,99,63,128,0,33,99,51,128,0,33,99,56,0,0,33,99,60,0,0,33,99,30,0,0,33,99,15,0,0,33,99,7,128,0,33,99,3,128,0,33,99,51,128,0,1,127,63,128,0,2,62,31,0,0,124,0,14,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;FreeBSD

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1,128,0,4,0,1,192,0,8,0,1,224,0,24,3,251,224,0,24,15,255,240,0,60,31,255,240,0,63,255,204,112,0,63,255,132,48,0,31,255,128,0,0,31,255,0,0,0,15,255,0,0,0,7,255,0,0,0,3,255,0,96,0,1,255,135,240,0,0,255,135,248,0,0,255,131,252,0,0,255,195,252,0,0,255,255,252,0,0,127,255,252,0,0,127,255,248,0,0,63,255,240,0,0,31,255,192,0,0,15,255,0,128,0,7,254,25,0,0,7,252,54,0,0,7,252,60,0,0,7,254,62,0,0,15,255,254,0,0,15,247,254,0,0,15,247,252,0,0,31,241,224,0,0,31,255,192,0,0,31,255,128,0,0,15,255,128,0,0,15,255,128,0,0,15,255,192,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,1,128,0,4,0,1,192,0,8,0,1,224,0,16,3,251,224,0,24,14,219,240,0,52,29,255,240,0,63,241,204,112,0,59,224,132,48,0,29,99,128,0,0,31,55,0,0,0,15,251,0,0,0,7,115,0,0,0,3,51,0,96,0,1,63,135,240,0,0,147,135,120,0,0,187,131,252,0,0,219,195,252,0,0,184,255,252,0,0,92,254,28,0,0,124,253,192,0,0,63,63,176,0,0,31,239,192,0,0,15,255,0,128,0,7,250,25,0,0,5,252,42,0,0,5,220,32,0,0,7,254,38,0,0,15,53,246,0,0,15,143,242,0,0,1,254,252,0,0,27,190,224,0,0,27,223,64,0,0,17,228,0,0,0,12,255,128,0,0,13,255,0,0,0,15,255,192,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,249,255,255,255,255,250,255,255,251,255,253,127,255,243,255,255,63,255,231,255,31,159,255,255,251,195,159,255,211,239,228,143,255,249,223,128,15,255,248,63,35,143,255,252,31,123,207,255,222,156,127,207,255,255,232,255,239,255,239,252,255,239,255,247,252,254,175,255,251,252,248,111,255,255,248,127,247,255,255,252,127,227,255,255,252,123,3,255,255,252,26,1,255,255,255,3,1,63,255,127,3,227,127,255,255,2,58,255,255,191,192,126,239,255,223,240,222,159,255,239,255,126,191,255,247,254,225,27,255,255,253,247,199,255,246,33,253,255,255,244,0,57,255,255,252,203,249,255,255,237,241,253,255,255,239,193,255,255,255,255,193,239,255,255,255,224,255,255,255,255,251,191,255,255,239,255,255,255,255,255,255,191,255,255,255,254,63,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,0,0,5,0,0,6,0,2,128,0,12,0,0,192,0,28,0,224,96,0,12,4,60,112,0,44,17,63,112,0,14,34,127,240,0,7,206,252,240,0,7,255,196,112,0,35,255,130,48,0,0,223,130,48,0,16,7,128,16,0,8,143,129,80,0,4,207,135,144,0,0,199,128,8,0,0,111,128,156,0,0,71,196,254,0,0,39,229,254,0,0,71,252,254,192,0,163,253,254,128,0,3,255,253,0,0,64,255,201,16,0,32,31,33,96,0,16,0,129,64,0,8,5,62,228,0,2,2,28,56,0,11,254,30,0,0,11,255,222,0,0,3,254,14,0,0,18,126,14,0,0,30,63,0,0,0,4,127,24,0,0,4,63,128,0,0,14,31,192,0,0,19,0,64,0,0,2,0,192,0,0,0,1,192,0

;BeOS

DB 0,0,195,0,0,0,4,0,32,0,0,0,0,0,0,0,0,126,0,0,0,1,0,128,0,2,8,0,16,64,4,0,0,0,32,4,0,0,0,0,0,4,0,33,0,0,0,0,0,32,0,0,0,0,64,0,0,0,0,0,0,1,1,1,0,0,0,0,0,128,0,4,0,32,0,0,0,129,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,8,128,0,0,0,17,0,0,0,0,34,0,0,0,0,68,0,0,0,0,136,0,0,0,0,16,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,4,64,0,0,0,4,0,0,0,0,64,0,0,0,2,16,128,0,0,0,0,0,0,0,0,130,0,0,0,0,68,0,0,0,0,0,0,0
DB 255,255,195,255,255,255,252,0,63,255,255,224,0,7,255,255,128,126,1,255,254,1,255,128,127,254,15,255,240,127,252,31,255,248,63,252,127,195,254,31,254,252,0,63,31,255,224,0,0,63,255,192,255,0,127,255,131,255,128,127,255,193,255,1,255,255,224,0,0,255,255,252,0,60,127,255,255,129,252,63,255,255,255,252,63,255,255,255,254,63,255,255,255,254,63,255,255,255,252,63,255,255,255,252,63,255,255,255,252,127,255,255,255,248,255,255,255,255,241,255,255,255,255,227,255,255,255,255,199,255,255,255,255,143,255,255,255,255,31,255,255,255,254,31,255,255,255,254,63,255,255,255,252,63,255,255,255,252,63,255,255,254,252,127,255,255,252,124,127,255,255,252,120,127,255,255,254,16,255,255,255,254,0,255,255,255,255,131,255,255,255,255,199,255,255,255,255,255,255,255
DB 0,0,60,0,0,0,3,255,192,0,0,31,255,248,0,0,127,129,254,0,1,254,0,127,128,1,240,0,15,128,3,224,0,7,192,3,128,60,1,224,1,3,255,192,224,0,31,255,255,192,0,63,0,255,128,0,124,0,127,128,0,62,0,254,0,0,31,255,255,0,0,3,255,195,128,0,0,126,3,192,0,0,0,3,192,0,0,0,1,192,0,0,0,1,192,0,0,0,3,192,0,0,0,3,192,0,0,0,3,128,0,0,0,7,0,0,0,0,14,0,0,0,0,28,0,0,0,0,56,0,0,0,0,112,0,0,0,0,224,0,0,0,1,224,0,0,0,1,192,0,0,0,3,192,0,0,0,3,192,0,0,1,3,128,0,0,3,131,128,0,0,3,135,128,0,0,1,239,0,0,0,1,255,0,0,0,0,124,0,0,0,0,56,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,254,0,127,255,255,240,0,15,255,255,192,255,3,255,255,3,255,192,255,254,31,255,248,127,252,63,255,252,63,252,255,255,255,63,255,254,0,127,63,255,240,0,12,63,255,193,255,128,127,255,199,255,192,255,255,193,255,1,255,255,240,0,8,255,255,254,0,124,127,255,255,255,252,127,255,255,255,254,63,255,255,255,254,63,255,255,255,254,63,255,255,255,254,63,255,255,255,252,127,255,255,255,252,127,255,255,255,248,255,255,255,255,241,255,255,255,255,227,255,255,255,255,199,255,255,255,255,143,255,255,255,255,31,255,255,255,255,31,255,255,255,254,63,255,255,255,254,63,255,255,255,252,127,255,255,255,252,127,255,255,252,124,127,255,255,252,124,127,255,255,254,56,255,255,255,255,1,255,255,255,255,131,255,255,255,255,255,255,255,255,255,255,255,255

; generic icon

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,96,0,0,0,0,120,0,0,0,0,126,0,0,0,0,127,128,0,0,0,127,224,0,0,0,127,240,0,0,0,127,240,0,0,0,127,240,0,0,0,127,240,0,0,0,127,240,0,0,0,31,240,0,0,0,7,240,0,0,0,1,240,0,0,0,0,112,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,8,0,0,0,0,21,0,0,0,0,42,128,0,0,0,213,64,0,0,0,58,144,0,0,0,13,68,0,0,0,3,0,0,0,0,96,192,8,0,0,88,48,48,0,0,126,8,192,0,0,127,131,0,0,0,127,225,0,0,0,127,241,0,0,0,127,241,0,0,0,127,241,0,0,0,95,241,0,0,0,127,241,0,0,0,159,241,0,0,0,39,241,0,0,0,9,241,0,0,0,2,113,0,0,0,0,145,0,0,48,0,33,0,0,12,0,1,0,0,19,0,1,0,0,4,192,1,0,0,0,32,0,0,0,0,12,0,0,0,0,3,0,0,0,0,0,128,0,0,0,0,48,0,0,0,0,12,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,243,207,255,255,255,196,51,255,255,255,42,60,255,255,252,213,79,63,255,253,42,179,207,255,253,197,108,243,255,253,242,187,60,255,253,28,255,207,63,253,103,63,247,207,253,121,207,201,15,253,126,119,58,15,253,127,156,248,15,253,127,230,248,15,253,127,246,248,15,253,127,246,248,15,253,127,246,248,15,253,127,246,248,15,253,127,246,248,15,253,31,246,248,15,252,199,246,248,15,243,49,246,248,63,207,12,118,248,127,63,131,22,248,63,79,204,198,248,207,115,242,62,241,243,124,252,14,195,252,123,63,2,15,252,63,223,192,63,240,207,243,252,255,192,243,204,255,255,0,252,243,127,252,0,255,63,79,240,3,255,207,19,192,15,255,243,132,0,63,255,252,227,0,255,255,255,59,3,255,255,255,207,15,255,255,255,243,63,255,255,255,252,255,255,255,255,255,255,255
DB 0,12,0,0,0,0,48,0,0,0,0,192,192,0,0,2,0,48,0,0,0,0,12,0,0,0,0,3,0,0,0,0,0,192,0,0,160,0,48,0,0,8,0,0,16,0,2,0,6,224,0,32,128,5,224,0,32,32,7,224,0,32,8,7,224,0,32,0,7,224,0,32,0,7,224,0,32,0,7,224,0,0,0,7,224,0,0,0,7,224,0,64,0,7,224,0,16,0,7,208,8,4,0,7,128,32,129,0,6,0,128,48,64,5,0,0,0,0,3,0,0,5,0,2,0,16,0,192,44,0,4,0,48,176,2,0,0,32,192,14,0,0,3,0,62,0,0,0,0,254,0,0,0,3,252,0,0,128,15,240,0,0,32,63,192,0,0,72,255,0,0,0,16,252,0,0,0,4,240,0,0,0,0,192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; Solaris

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,24,2,0,0,0,24,6,0,0,0,24,4,0,0,32,24,12,0,0,24,24,24,0,0,28,28,24,0,0,14,28,120,0,0,15,28,120,4,0,7,156,240,24,0,3,192,112,120,24,3,0,3,224,15,3,0,7,192,3,252,0,7,128,0,252,0,7,0,0,60,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,63,255,255,0,31,255,252,0,7,255,255,128,0,31,255,240,0,0,63,255,192,0,0,255,255,0,0,0,255,255,0,0,0,63,255,192,0,0,31,255,240,0,0,7,255,255,0,0,0,127,255,248,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,24,2,0,0,0,24,6,0,0,0,24,4,0,0,32,24,12,0,0,24,24,24,0,0,28,28,24,0,0,14,28,120,0,0,15,28,120,4,0,7,156,240,24,0,3,192,112,120,24,3,0,3,224,15,3,0,7,192,3,252,0,7,128,0,252,0,7,0,0,60,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,48,0,0,0,16,0,12,0,4,0,0,128,0,0,0,16,0,0,0,0,64,0,0,128,1,0,0,0,128,1,0,0,0,0,0,64,0,0,0,0,16,0,0,4,0,1,0,0,0,64,0,24,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,239,255,255,255,255,231,253,255,255,255,231,249,255,255,255,231,251,255,255,223,231,243,255,255,231,231,231,255,255,227,227,231,255,255,241,227,135,255,255,240,227,135,251,255,248,99,15,231,255,252,63,143,135,231,252,255,252,31,240,252,255,248,63,252,3,255,248,127,255,3,255,248,255,255,195,255,255,255,255,243,255,255,255,255,255,255,255,255,255,255,207,255,255,255,239,255,243,255,251,255,255,127,255,255,255,239,255,255,255,255,191,255,255,127,254,255,255,255,127,254,255,255,255,255,255,191,255,255,255,255,239,255,255,251,255,254,255,255,255,191,255,231,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,24,2,0,0,0,24,6,0,0,64,24,6,0,0,96,24,14,0,0,48,24,28,0,0,56,28,28,0,0,28,60,56,0,0,14,60,120,0,0,15,62,120,6,0,7,254,248,60,0,7,240,240,248,60,3,128,23,240,15,195,0,15,192,3,254,0,15,128,0,254,0,7,0,0,60,0,6,0,0,28,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; SCO unix

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,34,64,0,0,1,127,240,0,0,0,115,128,0,0,0,32,4,0,0,2,60,48,0,0,95,255,252,0,0,15,151,128,0,1,31,255,228,0,8,0,24,0,0,0,240,31,128,0,255,255,214,255,0,0,16,57,224,0,5,255,255,253,0,0,0,51,0,0,0,0,50,0,0,0,0,60,0,0,0,0,60,0,0,0,0,60,0,127,255,255,223,255,127,255,255,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,0,120,1,224,127,1,254,7,248,227,131,207,15,60,192,7,0,28,14,192,7,0,28,14,224,14,0,56,7,127,14,0,56,7,63,142,0,56,7,1,206,0,56,7,0,199,0,28,14,0,199,0,28,14,225,195,207,15,60,127,129,254,7,248,63,0,120,1,224
DB 127,255,255,255,255,127,255,255,255,255,127,255,255,255,255,127,255,255,194,63,127,255,254,127,247,127,255,255,128,127,127,255,240,32,3,127,255,252,60,15,127,255,159,255,252,127,255,240,16,127,127,254,31,255,227,127,240,0,24,0,127,255,0,24,127,127,0,0,16,128,127,255,224,57,159,127,249,255,255,252,127,255,255,254,255,127,255,255,249,255,127,255,255,255,255,127,255,255,251,255,127,255,255,223,255,0,0,0,28,0,127,255,255,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,62,0,120,1,224,127,1,254,7,248,227,131,207,15,60,192,7,0,28,14,192,7,0,28,14,224,14,0,56,7,127,14,0,56,7,63,142,0,56,7,1,206,0,56,7,0,199,0,28,14,0,199,0,28,14,225,195,207,15,60,127,129,254,7,248,63,0,120,1,224
DB 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,194,63,255,255,254,127,247,255,255,255,128,127,255,255,240,32,3,255,255,252,60,15,255,255,159,255,252,255,255,240,16,127,255,254,31,255,227,255,240,0,24,0,255,255,0,24,127,255,0,0,16,128,255,255,224,57,159,255,249,255,255,252,255,255,255,254,255,255,255,255,249,255,255,255,255,255,255,255,255,255,251,255,255,255,255,223,255,128,0,0,28,0,255,255,255,255,255,128,0,0,0,0,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DB 127,255,255,255,255,127,255,255,255,255,127,255,255,255,255,127,255,255,221,191,127,255,254,128,15,127,255,255,140,127,127,255,255,223,251,127,255,253,195,207,127,255,160,0,3,127,255,240,104,127,127,254,224,0,27,127,247,255,231,255,127,255,15,224,127,127,0,0,41,0,127,255,239,198,31,127,250,0,0,2,127,255,255,204,255,127,255,255,205,255,127,255,255,195,255,127,255,255,195,255,127,255,255,195,255,0,0,0,32,0,0,0,0,0,0,127,255,255,255,255,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

; HURD

DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,128,0,4,0,1,128,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,32,0,8,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,16,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,8,0,0,0,0,0,0,0,0,4,16,0,0,0,0,48,0,0,0,0,96,0,0,0,0,0,2,0,0,6,0,0,0,0,0,0,0,0,0,48,0,1,0,0,16,0,0,0,0,0,0,0,128,0,0,0,0,0,0,64,0,0,0,0,192,0,0,0,0,128,0,0,0,4,0,0,0,1,8,0,0,0,0,0,0,0,0,0,0,0,0
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,128,0,4,0,1,128,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,32,0,8,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,16,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,8,0,0,0,0,0,0,0,0,4,16,0,0,0,0,48,0,0,0,0,96,0,0,0,0,0,2,0,0,6,0,0,0,0,0,0,0,0,0,48,0,1,0,0,16,0,0,0,0,0,0,0,128,0,0,0,0,0,0,64,0,0,0,0,192,0,0,0,0,128,0,0,0,4,0,0,0,1,8,0,0,0,0,0,0,0,0,0,0,0,0
DB 255,255,255,255,255,255,255,255,255,255,255,248,3,255,255,255,251,251,255,255,255,251,250,255,255,255,251,248,255,255,255,195,248,135,255,255,155,248,244,255,255,251,250,255,127,253,251,251,255,31,253,248,3,255,223,253,255,191,255,239,240,127,255,255,239,248,255,223,192,23,192,31,223,223,211,223,223,223,95,219,223,223,247,31,219,223,223,248,31,219,223,223,252,31,219,223,223,255,95,219,223,223,255,223,215,192,31,255,192,27,248,255,255,249,247,240,127,255,255,247,253,255,255,251,255,253,248,3,251,255,253,251,251,251,63,255,251,251,246,127,252,251,251,225,255,254,251,248,55,255,255,123,251,247,255,255,123,251,239,255,255,187,251,207,255,255,152,3,223,255,255,207,255,255,255,255,231,254,255,255,255,241,244,255,255,255,253,11,255,255,255,255,255,255,255,255,255,255,255,255
DB 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,8,0,0,32,0,9,0,0,0,0,0,0,2,0,0,0,160,0,0,0,0,0,0,0,64,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,12,0,0,32,0,0,0,0,8,0,0,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,0,4,0,0,0,2,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,8,128,3,0,0,18,0,0,0,0,0,0,0,128,0,8,0,0,0,0,0,0,0,64,0,16,0,0,32,0,32,0,0,16,0,0,0,0,8,0,0,0,0,10,9,0,0,0,2,4,0,0,0,0,0,0,0,0,0,0,0,0


lbatbl  DB 16,0,1,0 ; table to access to the new extended commands
lbaoff  DW 0
lbaseg  DW 0
lbaadd  DW 0,0,0,0

time    DB 0     ; if time=0, GAG doesn't use timer. If not, waits TIME seconds
toboot  DB 49    ; here GAG saves the default OS in ASCII (49 for 1, 50 for 2...)
timertem DB 0   ; here GAG stores the temporal byte to use in the timer

COORDX  DB 0
COORDY  DB 0

MBR     DB 512 DUP(0) ; temporal place to put the partition tables
ptable  DB 200 DUP (0) ; temporal place to put the data boot (20 partitions
                        ; with 10 bytes each one)

prstp   DB 0          ; temporal place to decide if prints the setup icon
prinam  DB 0          ; decides if prints or not the icon name

drives  DB 0        ; temporal place to save the actual drive

exten   DB 0 ; tells if I have to load extended partitions and where
exten1  DW 0
exten2  DW 0
exten3  DW 0 ; LBA data for base sector (to calculate the LBA sector of an
exten4  DW 0 ; extended partition)
extend  DB 0 ; 0 if there's no base sector, 1 if there is.
exten5  DW 0 ; LBA data for the actual extended partition
exten6  DW 0
exten7  DB 0
extens  DB 0

partit  DB 0

mtemp   DB 40 DUP(0) ; here we place temporary strings

putemp  DB 16 DUP(0)

prpaswd DB 0 ; if 1, INTRO don't display the keystrokes.

tpos    DW 0    ; in this variable saves the timer bar position
counter DB 0    ; this variable decides if we count time or not
count1  DW 0    ; here we saves the counter

lderr   DB 0    ; error return for LOADMBR
whatdo  DB 0    ; mode for LOADMBR (0: automatic, 1: CSH, 2: LBA)

tempo   DW 0    ; temporal place
pfoun   DB 0
phidd   DB 0
wtemp   DB 0

nsect   EQU 41 ; Number of sectors to save in hard disk

Prog1   ENDP
Code    ends
        END     Prog1
