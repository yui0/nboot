;/---------------------------------------------------------
;/	Direct Disc Access
;/
;/		(C)2001-2002 NAKADA
;/---------------------------------------------------------


	.386p

	include vmm.inc
	include vwin32.inc
	include v86mmgr.inc
;	include shell.inc


;/---------------------------------------------------------
;/	EQUATES
;/---------------------------------------------------------

;VXD_Name			EQU	<'DISC VXD'>	;Must be 16 chars
;VXD_Rev				EQU	00H
VXD_MAJOR_VERSION		equ	1
VXD_MINOR_VERSION		equ	0

CARRY_FLAG				equ	1

DISC_CHECKEXTENSIONS	equ	1
DISC_EXTENDEDREAD		equ	2
DISC_EXTENDEDWRITE		equ	3
DISC_GETDRIVEPARAMS		equ	4


;/---------------------------------------------------------
;/	STRUCTURES
;/---------------------------------------------------------

disc_access struc
	drive		db ?	; drive (a=0, c=0x80, ...)
	data		dd ?	; buff address (32bit)
	lba			dd ?	; sector
	blocks		db ?	; n
	verify		db ?	; write with verify ?
disc_access ends

disc_packet struc
	struct_size	db ?
	reserved	db ?
	numblocks	db ?
	reserved2	db ?
	buff		dd ?
	block_lo	dd ?
	block_hi	dd ?
disc_packet ends


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

VXD_LOCKED_DATA_SEG
;	Pagelocked data here - try to keep this to a minimum.
	ioctl_result	dd 0
	dap				disc_packet <?>

	dos_buff		dd ?
	len_buff		dd ?
	dos_dap			dd ?
	len_dap			dd ?
VXD_LOCKED_DATA_ENDS


;/---------------------------------------------------------
;/	INITIAL CODE SEGMENT
;/---------------------------------------------------------

VXD_ICODE_SEG

;VxdCaption	db	"VxD Extensions",0
;VxdMessage	db	"         Loading...",0


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
	clc
	ret
EndProc VXD_Device_Init

VXD_ICODE_ENDS


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
	mov		ecx,[esi].dwIoControlCode		; get ioctl code from your program
	cmp		ecx,DIOC_OPEN					; DIOC_OPEN is sent when VxD is loaded / CreateFile
	je		ioctl_open
	cmp		ecx,DIOC_CLOSEHANDLE			; DIOC_CLOSEHANDLE is sent when VxD is unloaded / CloseHandle
	je		ioctl_close
	cmp		ecx,DISC_CHECKEXTENSIONS
	je		ioctl_chkext
	cmp		ecx,DISC_EXTENDEDREAD
	je		ioctl_extread
	cmp		ecx,DISC_EXTENDEDWRITE
	je		ioctl_extwrite
	cmp		ecx,DISC_GETDRIVEPARAMS
	je		ioctrl_getparams
	jmp		ioctl_success

	; DIOC_OPEN
	; Must return 0 to tell WIN32 that this VxD supports DEVIOCTL
ioctl_open:
	jmp		ioctl_success

	; DIOC_CLOSE
ioctl_close:
	jmp		ioctl_success

	; DISC_CHECKEXTENSIONS
ioctl_chkext:
	mov		edi,[esi].lpvInBuffer
	mov		al,[edi]

	Push_Client_State						; save all registers
	VMMcall	Begin_Nest_V86_Exec				; Enter nested execution in V86-mode (force VM to V86)
	mov		[ebp.Client_AH],41h
	mov		[ebp.Client_BX],55aah
	mov		[ebp.Client_DL],al
	mov		eax,13h
	VMMCall	Exec_Int						; current VM to call BIOS
	test	[ebp.Client_Flags],CARRY_FLAG
	jnz		ioctl_no_ext
	cmp		[ebp.Client_BX],0aa55h
	jne		ioctl_no_ext
	mov		al,1
	jmp		ioctl_chk_done
ioctl_no_ext:
	mov		al,0
ioctl_chk_done:
	VMMcall	End_Nest_Exec					; end of nested exec calls
	Pop_Client_State						; restore all registers when done
	mov		edi,[esi].lpvOutBuffer
	mov		[edi],al
	mov		[esi].cbOutBuffer,1
	jmp		ioctl_success					; exit successfully

	; DISC_EXTENDEDREAD
ioctl_extread:
	mov		edi,[esi].lpvInBuffer
	mov		al,[edi].drive

	mov		ebx,[edi].lba
	mov		dap.block_lo,ebx
	mov		bl,[edi].blocks
	mov		dap.numblocks,bl
	mov		dap.block_hi,0
	mov		dap.reserved,0
	mov		dap.reserved2,0
	mov		dap.struct_size,size disc_packet

	; allocate sector buffer...
	VMMcall	Get_Sys_VM_Handle				; ebx = handle of system VM (address of VM control block)
	mov		ebp,[ebx].CB_Client_Pointer		; ebp = Address of a Client_Reg_Struc structure
	push	eax								; save eax
	xor		ax,ax
	mov		al,[edi].blocks					; blocks * 512 (1 sector)
	mov		cx,512
	mul		cx
	xor		ecx,ecx
	mov		cx,ax							; ecx = NumBytes
	pop		eax								; restore eax
	clc										; carry clear => allocate only
	VxDcall	V86MMGR_Allocate_Buffer			; Result: edi = FarPtrBuffer
	jc		ioctl_error
	mov		dos_buff,edi
	mov		len_buff,ecx
	mov		dap.buff,edi

	; allocate DAP buffer...
	mov		ecx,size disc_packet
	push	esi								; save ESI
	mov		esi,OFFSET32 dap				; extended memory to copy if carry set
	push	ds
	pop		fs
	stc										; set carry flag to copy
	VxDcall	V86MMGR_Allocate_Buffer			; edi = FarPtrBuffer
	pop		esi								; restore ESI
	jc		ioctl_error
	mov		len_dap,ecx
	mov		dos_dap,edi

	; execute INT13...
	Push_Client_State						; save all registers
	VMMcall	Begin_Nest_V86_Exec				; Enter nested execution in V86-mode
	mov		edx,dos_dap
	mov		[ebp.Client_SI],dx
	shr		edx,16
	mov		[ebp.Client_DS],dx				; client ds:si=PtrDAP
	mov		[ebp.Client_AH],42h
	mov		[ebp.Client_DL],al
	mov		eax,13h
	VMMCall	Exec_Int						; current VM to call BIOS
	clc
	test	[ebp.Client_Flags],CARRY_FLAG
	jnz		ioctl_readerr
	cmp		[ebp.Client_AH],0
	jne		ioctl_readerr
;	mov		ioctlerror,INT13EXT_ERROR_SUCCESS
ioctl_readerr:
	VMMcall End_Nest_Exec					; end of nested exec calls
	Pop_Client_State						; restore all registers when done

	; free DAP buffer...
	mov		ecx,len_dap
	clc										; carry cleared=>no copy
	VxDcall	V86MMGR_Free_Buffer

	; copy and free sector buffer...
	mov		ecx,len_buff
	mov		edi,[esi].lpvInBuffer
	push	esi								; save ESI
	mov		esi,[edi].data
	stc										; carry set=>copy
	push	ds
	pop		fs								; fs=ds
	VxDcall	V86MMGR_Free_Buffer
	pop		esi								; restore ESI
	jmp		ioctl_success

	; DISC_EXTENDEDWRITE
ioctl_extwrite:
;	mov		ioctlerror,INT13EXT_ERROR_OUTOFMEMORY
	mov		edi,[esi].lpvInBuffer
	mov		al,[edi].drive

	mov		ebx,[edi].lba
	mov		dap.block_lo,ebx
	mov		dap.numblocks,1
	mov		dap.block_hi,0
	mov		dap.reserved,0
	mov		dap.reserved2,0
	mov		dap.struct_size,size disc_packet

	; allocate sector buffer...
	VMMcall	Get_Sys_VM_Handle				; ebx = handle of system VM (address of VM control block)
	mov		ebp,[ebx].CB_Client_Pointer		; ebp = Address of a Client_Reg_Struc structure
	mov		ecx,512							; ecx = NumBytes
	mov		edi,[esi].lpvInBuffer
	push	esi								; save ESI
	mov		esi,[edi].data
	stc										; carry flag set: copy extended memory from fs:esi
	push	ds
	pop		fs								; fs=ds
	VxDcall	V86MMGR_Allocate_Buffer			; edi = FarPtrBuffer, ecx=BytesCopied
	pop		esi								; restore ESI
	jc		ioctl_error
	mov		dos_buff,edi
	mov		len_buff,ecx
	mov		dap.buff,edi

	; allocate DAP buffer...
	mov		ecx,size disc_packet
	push	esi								; save ESI
	mov		esi,OFFSET32 dap				; extended memory to copy if carry set
	push	ds
	pop		fs
	stc										; set carry flag to copy
	VxDcall	V86MMGR_Allocate_Buffer			; edi = FarPtrBuffer
	pop		esi								; restore ESI
	jc		ioctl_error
	mov		len_dap,ecx
	mov		dos_dap,edi

	; execute INT13...
;	mov		ioctlerror,	INT13EXT_ERROR_FAILED
	Push_Client_State						; save all registers
	VMMcall	Begin_Nest_V86_Exec				; Enter nested execution in V86-mode
	mov		edx,dos_dap
	mov		[ebp.Client_SI],dx
	shr		edx,16
	mov		[ebp.Client_DS],dx				; client ds:si=PtrDAP
	mov		[ebp.Client_AH],43h				; extended write
	mov		[ebp.Client_AL],0				; write with verify off
	mov		edi,[esi].lpvInBuffer
	cmp		[edi].verify,1
	jne		wr_no_verify
	mov		[ebp.Client_AL],2				; write with verify on
wr_no_verify:
	mov		[ebp.Client_DL],al
	mov		eax,13h
	VMMCall	Exec_Int						; current VM to call BIOS
	clc
	test	[ebp.Client_Flags],CARRY_FLAG
	jnz		ioctl_writeerr
	cmp		[ebp.Client_AH],0
	jne		ioctl_writeerr
;	mov		ioctlerror,INT13EXT_ERROR_SUCCESS
ioctl_writeerr:
	VMMcall	End_Nest_Exec					; end of nested exec calls
	Pop_Client_State						; restore all registers when done

	; free DAP buffer...
	mov		ecx,len_dap
	clc
	VxDcall	V86MMGR_Free_Buffer				; ebp=OFFSET ClientRegs, ebx=VMHandle, ecx=NumBytes

	; free sector buffer...
	mov		ecx,len_buff
	clc
	VxDcall	V86MMGR_Free_Buffer
	jmp		ioctl_success


	; DISC_GETDRIVEPARAMS
ioctrl_getparams:
;	mov		ioctlerror,INT13EXT_ERROR_OUTOFMEMORY
	mov		edi,[esi].lpvInBuffer
	mov		al,[edi].drive

	; allocate result buffer...
	VMMcall	Get_Sys_VM_Handle				; ebx = handle of system VM (address of VM control block)
	mov		ebp,[ebx].CB_Client_Pointer		; ebp = Address of a Client_Reg_Struc structure
	mov		ecx,30							; ecx = NumBytes
	mov		edi,[esi].lpvInBuffer
	push	esi								; save ESI
	mov		esi,[edi].data
	stc										; carry flag set: copy extended memory from fs:esi
	push	ds
	pop		fs								; fs=ds
	VxDcall	V86MMGR_Allocate_Buffer			; edi = FarPtrBuffer, ecx=BytesCopied
	pop		esi								; restore ESI
	jc		ioctl_error
	mov		dos_buff,edi
	mov		len_buff,ecx

	; execute INT13...
;	mov		ioctlerror,INT13EXT_ERROR_FAILED
	Push_Client_State						; save all registers
	VMMcall	Begin_Nest_V86_Exec				; Enter nested execution in V86-mode
	mov		edx,dos_buff
	mov		[ebp.Client_SI],dx
	shr		edx,16
	mov		[ebp.Client_DS],dx				; client ds:si=PtrBuf
	mov		[ebp.Client_AH],48h
	mov		[ebp.Client_DL],al
	mov		eax,13h
	VMMCall	Exec_Int						; current VM to call BIOS
	clc
	test	[ebp.Client_Flags],CARRY_FLAG
	jnz		ioctl_getparamerr
	cmp		[ebp.Client_AH],0
	jne		ioctl_getparamerr
;	mov		ioctlerror,INT13EXT_ERROR_SUCCESS
ioctl_getparamerr:
	VMMcall	End_Nest_Exec					; end of nested exec calls
	Pop_Client_State						; restore all registers when done

	; copy and free result buffer...
	push	esi								; save ESI
	mov		esi,[esi].lpvInBuffer
	mov		esi,[esi].data
	stc										; carry set=>copy
	push	ds
	pop		fs								; fs=ds
	VxDcall	V86MMGR_Free_Buffer				; first copy to fs:esi
	pop		esi								; restore ESI
;	jmp		ioctl_success

ioctl_success:
	xor		eax,eax			;return zero = success
	clc
	ret
ioctl_error:
	mov		eax,1
	stc
	ret
EndProc VXD_ioctl


;/---------------------------------------------------------
;	VXD_Device_Exit
;	Cleans up any hooks that are still installed before exiting.
;/---------------------------------------------------------

public VXD_Device_Exit
BeginProc VXD_Device_Exit
	clc
	ret
EndProc VXD_Device_Exit

VXD_LOCKED_CODE_ENDS

	end
