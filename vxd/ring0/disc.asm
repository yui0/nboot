;/---------------------------------------------------------
;/	Direct Disc Access
;/
;/		(C)2001-2002 NAKADA
;/---------------------------------------------------------


	.386p

	include vmm.inc
	include vwin32.inc
	include shell.inc


;/---------------------------------------------------------
;/	EQUATES
;/---------------------------------------------------------

VXDName				EQU	<'DISC VXD		  '>	;Must be 16 chars
VXDRev				EQU	00H
VXD_MAJOR_VERSION	EQU	1
VXD_MINOR_VERSION	EQU	0

BUFFER_LEN			EQU	400
ErrorCode			EQU	0FFFFFFFFh


;/---------------------------------------------------------
;/	STRUCTURES
;/---------------------------------------------------------

disc_access struc
	drive		db ?
	buff		dd ?
	sector		dd ?
	n			dd ?
disc_access ends


;/---------------------------------------------------------
;/	VIRTUAL DEVICE DECLARATION
;/---------------------------------------------------------

Declare_Virtual_Device DISC,	\
	VXD_MAJOR_VERSION,			\
	VXD_MINOR_VERSION,			\
	VXD_Control, ,				\
	UNDEFINED_INIT_ORDER


;/---------------------------------------------------------
;/	DATA SEGMENT
;/---------------------------------------------------------

VxD_IDATA_SEG
;	Initialization data here - discarded after Init_Complete
VxD_IDATA_ENDS


VxD_DATA_SEG
;	Normal Data here
VxD_DATA_ENDS


;/---------------------------------------------------------
;/	INITIAL CODE SEGMENT
;/---------------------------------------------------------

VXD_ICODE_SEG

VxdCaption	db	"VxD Extensions",0
VxdMessage	db	"         Loading...",0


;/---------------------------------------------------------
;	VXD_Device_Init
;
;	DESCRIPTION:
;		This is a shell for a routine that is called at system BOOT.
;		Typically, a VxD would do its initialization in this routine.
;
;	ENTRY:
;		EBX = System VM handle
;
;	EXIT:
;		Carry clear to indicate load success
;		Carry set to abort loading this VxD
;
;	USES:
;		flags
;/---------------------------------------------------------

BeginProc VXD_Device_Init
;	Trace_Out "DISC_Device_Init"

	ifdef debug
		; Put up message box indicating we're loading
		VMMcall	Get_Cur_VM_Handle
		mov		eax,MB_OK
		mov		ecx,OFFSET32 VxdMessage
		mov		edi,OFFSET32 VxdCaption
		VxDcall	SHELL_SYSMODAL_Message
	endif

;	mov		eax,68h
;	mov		esi,OFFSET32 Handler_Int_68
;	VMMCall	Hook_V86_Int_Chain
;	jc		error
	clc				;no error - load VxD
	ret

;error:
;	stc
;	ret
EndProc VXD_Device_Init

VXD_ICODE_ENDS


;/---------------------------------------------------------
;/	LOCKED DATA SEGMENT
;/---------------------------------------------------------

VXD_LOCKED_DATA_SEG
;	Pagelocked data here - try to keep this to a minimum.
	FLAGS		dd 0
	SYS_VM		dd 0
	LDT			dd 0
	ptrTextBuff dd 0		;pointer to message text buffer
	ptrTextPos	dd 0		;pointer to current position in text buffer (buffer+offset)
	HexTab		db '0123456789ABCDEFF',0
	szTitle 	db '*** WARNING *** Some process can detect your debugger!',0
	szProcName	db 'Process name: ',0
	szType		db 'Type: ',0
	szInt68 	db 'Int 68, AX=4300',0
	sEAX		db 'EAX='
	sEBX		db 'EBX='
	sECX		db 'ECX='
	sEDX		db 'EDX='
	sESI		db 'ESI='
	sEDI		db 'EDI='
	sEBP		db 'EBP='
	sESP		db 'ESP='
	sEIP		db 'EIP='
	sEOL		db 13,10
	sSpace		db ' '
	szStrHex	db 9 DUP(0)		;hexstring value, 8+1
VXD_LOCKED_DATA_ENDS


;/---------------------------------------------------------
;/	LOCKED CODE SEGMENT (Page Locked Code)
;/---------------------------------------------------------

VXD_LOCKED_CODE_SEG


;/---------------------------------------------------------
;	VXD_Control
;
;	DESCRIPTION:
;		This is a call-back routine to handle the messages that are sent
;
;	ENTRY:
;		EAX = Message number
;		EBX = VM Handle
;/---------------------------------------------------------

public VXD_Control
VXD_Control PROC NEAR
	Control_Dispatch SYS_DYNAMIC_DEVICE_INIT,	VXD_Device_Init
	Control_Dispatch SYS_DYNAMIC_DEVICE_EXIT,	VXD_Device_Exit
	Control_Dispatch W32_DEVICEIOCONTROL,		VXD_ioctl
	clc
	ret
VXD_Control ENDP


;/---------------------------------------------------------
;	VXD_ioctl - Respond to DeviceIOcontrol messages sent by Win32 program.
;
;	Entry: esi -> DIOC block
;	DIOCParams	STRUC
;		Internal1			DD	?
;		VMHandle			DD	?
;		Internal2			DD	?
;		dwIoControlCode		DD	?	; 0=CheckExtensions
;		lpvInBuffer			DD	?
;		cbInBuffer			DD	?
;		lpvOutBuffer		DD	?
;		cbOutBuffer			DD	?
;		lpcbBytesReturned	DD
;		lpoOverlapped		DD	?
;		hDevice				DD	?
;		tagProcess			DD	?
;	DIOCParams	ENDS
;/---------------------------------------------------------

public VXD_ioctl
BeginProc VXD_ioctl
	mov		ecx,[esi].dwIoControlCode	;get ioctl code from your program
	cmp		ecx,1
	je		Function1
	cmp		ecx,2
	je		Function2
	jmp		ioctl_success

Function1:
	;Here everything you want
	;No more Ring3 limitations

	; Floppy only !!
	mov 	edi,[esi].lpvInBuffer
	mov		ah,0ddh				;// R0_READABSOLUTEDISK shr 8
	mov		al,[edi].drive		;// drive (a=0,b=1, ...)
	mov		ecx,[edi].n			;// nsectors
	mov		edx,[edi].sector	;// sector
	mov		esi,[edi].buff		;// buffer
	int		20h
	dd		00400032h			;IFSMgr_Ring0_FileIO
	jc		ioctl_err

;	mov		edi,[esi].lpvOutBuffer
;	mov		[edi].drive,10
;	mov		[esi].cbOutBuffer,1
	jmp		ioctl_success

buff_	db	512 dup (0)

Function2:
	;Here other function
	;and so on.

ioctl_extread:
	jmp		ioctl_success

ioctl_success:
	xor		eax,eax			;return zero = success
	clc
	ret
ioctl_err:
	mov		eax,ErrorCode
	stc
	ret
EndProc VXD_ioctl


;/---------------------------------------------------------
;	VXD_Device_Exit
;	Cleans up any hooks that are still installed before exiting.
;/---------------------------------------------------------

public VXD_Device_Exit
BeginProc VXD_Device_Exit
;	mov	eax, 68h
;	mov	esi, OFFSET32 Handler_Int_68
;	VMMCall UnHook_V86_Int_Chain
	clc
	ret
EndProc VXD_Device_Exit

BeginProc Handler_Int_68		;Int_68 AX = 4300h
	pushad
	mov	eax,[ebp.Client_EAX]
	cmp	ax,4300h
	jnz	Let_DOS_Manage_68
	xor	eax,eax
	mov	FLAGS,eax
	mov eax, OFFSET32 szInt68
	call Display_info
	mov	[ebp.Client_AX],0
	popad
	clc					;consume the interrupt
	ret
Let_DOS_Manage_68:
	popad
	stc					; don't consume the interrupt
	ret
EndProc Handler_Int_68

BeginProc Display_Info
	push eax						;save offset int number
	VMMCall _HeapAllocate,<BUFFER_LEN,HEAPZEROINIT> 	;allocate memory (fill 0)
	mov ptrTextBuff,eax					;save ptr
	mov ptrTextPos,eax

	mov eax,OFFSET32 szProcName
	call PrintStr0
	call Get_Proc_Name					;show process name
	call PrintStr0
	call PrintEol

	mov eax,OFFSET32 szType
	call PrintStr0
	pop eax 						;get offset int from stack
	call PrintStr0
	call PrintEol

	mov eax,OFFSET32 sEAX
	mov ecx,[ebp.Client_EAX]
	call PrintReg
	call PrintSpace
	mov eax,OFFSET32 sEBX
	mov ecx,[ebp.Client_EBX]
	call PrintReg
	call PrintSpace
	mov eax,OFFSET32 sECX
	mov ecx,[ebp.Client_ECX]
	call PrintReg
	call PrintSpace
	mov eax,OFFSET32 sEDX
	mov ecx,[ebp.Client_EDX]
	call PrintReg
	call PrintEol

	mov eax,OFFSET32 sESI
	mov ecx,[ebp.Client_ESI]
	call PrintReg
	call PrintSpace
	mov eax,OFFSET32 sEDI
	mov ecx,[ebp.Client_EDI]
	call PrintReg
	call PrintSpace
	mov eax,OFFSET32 sEBP
	mov ecx,[ebp.Client_EBP]
	call PrintReg

	mov edi,OFFSET32 szTitle				;message title
	mov ecx,ptrTextBuff					;message text
	mov eax,MB_OK						;attributes
	VMMCall Get_Sys_VM_Handle				;
	VxDCall SHELL_sysmodal_Message				;display info
	VMMCall _HeapFree,ptrTextBuff,0
	ret
Get_Proc_Name:							;get caller name (process name)
	VxDCall VWIN32_GetCurrentProcessHandle
	mov eax,[eax+38h]
	or al,7
	mov LDT,eax

	VmmCall Get_Sys_VM_Handle
	mov SYS_VM,ebx

	VmmCall _SelectorMapFlat <SYS_VM,LDT,FLAGS>
	add eax,0F2h						; Now eax points to the caller name

	VMMCall _lstrcpyn,<OFFSET32 szStrHex,eax,8+1>
	mov eax, OFFSET32 szStrHex
	push eax
	VMMCall _lstrlen, <eax> 				;calc len
	pop ebx
	add eax, ebx
	mov byte ptr[eax],0
	mov eax,ebx
	ret
PrintReg:
;in:	eax - offset name reg
;	ecx - register value = [ebp.Client_reg]
	push ecx
	;---------------------------------
	;copy string 'reg=' to text buffer
	;---------------------------------
	VMMCall _lstrcpyn,<ptrTextPos,eax,4+1>
	add ptrTextPos,4
	;-------------------------------------
	;copy [register] from caller to text buffer
	;-------------------------------------
	pop eax
	mov ebx,OFFSET32 szStrHex
	call hex2str
	VMMCall _lstrcpyn,<ptrTextPos,OFFSET32 szStrHex,8+1>
	add ptrTextPos,8
	ret
PrintEol:
	;-------------------------------------
	;copy end of line chars to text buffer
	;-------------------------------------
	VMMCall _lstrcpyn,<ptrTextPos,OFFSET32 sEOL,2+1>
	add ptrTextPos,2
	ret
PrintSpace:
	;-------------------------
	;copy space to text buffer
	;-------------------------
	VMMCall _lstrcpyn,<ptrTextPos,OFFSET32 sSpace,1+1>
	add ptrTextPos,1
	ret
PrintStr0:
;in:	eax - offset string0
	;-------------------------
	;copy string0 to text buffer
	;-------------------------
	push eax						;save offset
	VMMCall _lstrlen, <eax> 				;calc len
	pop ebx
	push eax						;save len
	inc eax
	VMMCall _lstrcpyn,<ptrTextPos,ebx,eax>
	pop eax
	add ptrTextPos,eax
	ret
EndProc Display_Info

Public hex2str
BeginProc hex2str
;in:	eax - liczba w hex
;	ebx - adres poczatku stringu (pointer)
;out:	-
	xor ecx,ecx
	xor edx,edx
next_byte:
	rol eax,4
	mov dl,al
	and dl,0Fh
	mov dl,byte ptr [edx + offset HexTab ]
	mov byte ptr[ebx],dl
	inc cl
	inc ebx
	cmp cl,8
	jnz next_byte
	mov byte ptr [ebx],0
	ret
EndProc hex2str

VXD_LOCKED_CODE_ENDS

	end
