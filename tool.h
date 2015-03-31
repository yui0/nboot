%ifndef _help_tool_inc
%define _help_tool_inc

; this file define the help macros for the nasm assembers. Using those macros
; can moduler program in the assember, do not need to jmp any more.
;						--have fun!!
; 
; Copyright 2000 Christopher Li <chrisl@turbolinux.com.cn>
; 

%assign __cont__ 0
%assign __break__ 0
%assign __return__ 0
%assign __level__ 0
%assign __cndblock__ 0
%macro cat_define 3
%define %{1}%{2} %{3}
%endmacro

%macro l_define 4
%define %{1}%{2} %{3}%{4}
%endmacro

%macro cat_undef 3
%undef %{1}%{2}%{3}
%endmacro

%macro cat_lable 2
%{1}%{2}:
%endmacro

%macro linecat 3
	%1 %{2}%{3}
%endmacro

%macro cat_jc 3
	j%+1 %{2}%{3}
%endmacro

%macro cat_jnc 3
	j%-1 %{2}%{3}
%endmacro

%macro cat_jc_near 3
	j%+1 near %{2}%{3}
%endmacro

%macro cat_jnc_near 3
	j%-1 near %{2}%{3}
%endmacro


%macro cat_jmp 2
	jmp short %{1}%{2}
%endmacro

%macro cat_jmp_near 2
	jmp near %{1}%{2}
%endmacro

%macro jmp_near 2
	%if %1 > 0
		jmp near %2
	%else
		jmp short %2
	%endif
%endmacro

%macro con_push 1
	%assign __lastlevel__ __level__
	%assign __level__ __level__ + 1
	%if __return__ > 0
		%assign con_return 1
	%else
		%assign con_return 0
	%endif

	%if __cont__ > 0
		%assign con_cont 1
	%else
		%assign con_cont 0
	%endif
	%if __break__ > 0
		%assign con_break 1
	%else
		%assign con_break 0
	%endif
; if the contex are going to provide the lable, then we don't provide it twice.
	%ifidn %1,proc
		%assign con_return 0
	%elifidn %1,forcx
		%assign con_break 0
	%elifidn %1,repeat
		%assign con_cont 0
		%assign con_break 0
	%elifidn %1,while
		%assign con_cont 0
		%assign con_break 0
	%endif

	%if __cndblock__ == 1
	    l_define %$ifnot,__lastlevel__,%$ifnot,{}
	%endif

	%push %1
	%if con_return
	    l_define %$return,__level__,%$$return,__lastlevel__
	%endif
	%if con_break
	    l_define %$break,__level__,%$$break,__lastlevel__
	%endif
	%if con_cont
	    l_define %$cont,__level__,%$$cont,__lastlevel__
	%endif
	
	%ifidn %1,andblock
	    l_define %$ifnot,__level__,%$$ifnot,__lastlevel__
	%elifidn %1,orblock
	    l_define %$ifnot,__level__,%$$ifnot,__lastlevel__
	%endif	
%endmacro

%macro con_pop 0
	%pop
	%assign __level__ __level__ - 1
%endmacro

%macro proc 1-*
GLOBAL %{1}
%{1}:
con_push proc
%assign  __return__ __return__ + 1
%assign __have_frame__ 0
%assign %$have_local 0
%assign %$have_save  0
%assign __save_all__ 0
%assign %$local_args 0
%assign %$proc_args 0
%assign __save_args__ 0
%if %0 > 1
	%assign args %0 - 1
	%assign %$bp_off 4
	%assign i 0
	%rotate 1
	%ifidn %1,withlocal
		%assign %$have_local 1
		%assign args args -1
		%rotate 1
	%endif 
	%assign %$proc_args args
	%rep args
		; define the args
		%%inputarg__%{1} EQU %$bp_off
		%undef .%1
		%define .%{1} %%inputarg__%{1}+bp
		cat_define %$proc_argv ,i,%1 
		%assign %$bp_off %$bp_off + 2
		%assign i i+1
		%rotate 1
	%endrep
	%if %$have_local==0
		enter 0,0
		%assign __have_frame__ 1
	%endif
%endif
%endmacro

%macro local 0-*
%ifctx proc
	%if %$have_local == 1
		%assign argc %0
		%assign local_vars 0
		%assign %$have_local 2
		%assign local_off 0
		
		%assign i 0
		%rep argc
			%ifnum %2
				%assign local_off local_off+%2
				%assign token_eat 2
			%else
				%assign local_off local_off+2
				%assign token_eat 1
			%endif

			%%local__%{1} EQU local_off
			%undef .%{1}
			%define .%{1} bp-%%local__%{1}
			cat_define %$proc_argv ,i,%1 
			%assign argc argc-token_eat
			%if argc <= 0
				%exitrep
			%endif
			%assign i i+1
			%rotate token_eat
		%endrep
		%assign %$local_args i
		enter local_off,0
		%assign __have_frame__ 1
	%else
		%error "`proc' not declear `withlocal' as the first argument"
	%endif
%else
	%error "expected `proc' before `local'"
%endif
%endmacro

%macro invoke 1-*
	%if %0 == 2
	    push word %1
    	    %assign rargc 1	 	    
	%else
	%rotate -2
	%assign argc %0-1
	%assign rargc 0
	%rep argc
	    %ifidn %1,byte
	     	push byte %2
		%rotate -1
		%assign argc argc -1
		%assign rargc rargc +1
	    %elifidn %1,word
	    	push word %2
		%rotate -1
		%assign argc argc -1
		%assign rargc rargc +1
	    %elifidn %1,dword
	        push dword %2
		%rotate -1
		%assign argc argc -1
		%assign rargc rargc +2
	    %else
	        push word %2
		%assign rargc rargc +1
    	    %endif
	    %rotate -1
	    %assign argc argc -1
	    %if argc <=0 
	    	%exitrep
	    %endif
	%endrep
	%rotate 1
	%endif
	call %1
	add sp,(rargc)*2
%endmacro

%macro save 1-*
%ifctx proc
	%if %$have_local == 1
		%error "`save' must after `local'"
	%endif

	%ifidn all,%1
		%assign __save_all__ 1
		%if %0 > 1
			%ifidn frame,%2
				%if __have_frame__ == 0
					enter 0,0
					%assign __have_frame__ 1
					%assign local_off 0
				%endif
			%endif
		%endif
		pusha
		%if __have_frame__ == 1
			%undef __AX
			%undef __BX
			%undef __CX
			%undef __DX
			%undef __SI
			%undef __DI
			%idefine __AX bp-local_off - 2
			%idefine __CX bp-local_off - 4
			%idefine __DX bp-local_off - 6
			%idefine __BX bp-local_off - 8
			; %idefine __SP bp-local_off - 10
			; %idefine __BP bp-local_off - 12
			%idefine __SI bp-local_off - 14
			%idefine __DI bp-local_off - 16
		%endif
	%elifidn alld,%1
		%assign __save_all__ 2
		%if %0 > 1
			%ifidn frame,%2
				%if __have_frame__ == 0
					enter 0,0
					%assign __have_frame__ 1
					%assign local_off 0
				%endif
			%endif
		%endif
		pushad
		%if __have_frame__ == 1
			%undef __EAX
			%undef __EBX
			%undef __ECX
			%undef __EDX
			%undef __ESI
			%undef __EDI
			%idefine __EAX bp-local_off - 4
			%idefine __ECX bp-local_off - 8
			%idefine __EDX bp-local_off - 12
			%idefine __EBX bp-local_off - 16
			; %idefine __SP bp-local_off - 10
			; %idefine __BP bp-local_off - 12
			%idefine __ESI bp-local_off - 18
			%idefine __EDI bp-local_off - 32
		%endif

	%else
		%assign __save_args__ %0
		%assign __save_all__ 0
		%assign i %0
		%rep %0
			push %1
			cat_define %$save_argv ,i,%1 
			%rotate 1
			%assign i i - 1
		%endrep
	%endif
%else
	%error "`save' must after `proc'"
%endif
%endmacro

%macro endp 0-1
%ifnctx proc
	%error "expected `proc' before `endp'"
%endif
	cat_lable %$return,__level__
	%if __save_all__ ==1 
		popa
	%elif __save_all__ == 2
		popad
	%elif __save_args__ > 0
		%assign i 1
		%rep __save_args__
			linecat pop, %$save_argv,i
			%assign i i+1
		%endrep
	%endif
	%if __have_frame__ > 0
	    leave
	%endif
	; cleanup proc_args
	%assign i 0
	%rep %$proc_args
;		%undef %$proc_argv1
		%assign i i+1
	%endrep
	; cleanup local_args
	%assign i 0
	%rep %$local_args
;		cat_undef %$local_argv,i
		%assign i i+1
	%endrep
	ret
	con_pop
	%assign  __return__ __return__ - 1
%endmacro

%macro mov_ax 1
	%if %1 == 0
		sub ax,ax
	%else
		mov ax,%1
	%endif
%endmacro

%macro return 0-*
    %if %0 > 0
    	%assign __near__ 0
    	%assign argc %0
	%ifidn %1,near
		%assign __near__ 1
		%assign argc argc-1
		%rotate 1
	%endif
	%if argc > 0
	%ifidn %1,if
	    ; return if, {cmp ax, 0},e 
	    %rotate 1
	    %rep argc-2
		%1
		%rotate 1
	    %endrep
	    %if __near__ == 0
	    	cat_jc %1,%$return,__level__
	    %else
	    	cat_jc_near %1,%$return,__level__
	    %endif	
	%else
	    %error "expect `if' after `return'"
	%endif
	%else
	%if __near__ == 0
		cat_jmp %$return,__level__
	%else
		cat_jmp_near %$return,__level__
	%endif
	%endif
    %else
    	%if __have_frame__ == 0 && __save_all__ == 0 && __save_args__ == 0
		ret
	%else	
    		cat_jmp %$return,__level__
	%endif	
    %endif
%endmacro



%macro forcx 0-1
con_push forcx
%assign  __break__ __break__ + 1
%if %0 > 0
	mov	cx, %1
%endif
%$begin:
%endmacro

%macro endforcx 0-*
    %ifnctx forcx
	%error "expected `forcx' before `endfor'"
    %endif
	%if %0 > 0
	    %ifidn %1,while
	    	%rotate 1
	    	cat_lable %$cont,__level__
	    	%rep %0 - 2
			%1
			%rotate 1
	    	%endrep
	    %else
	    	%error "expected `while' after `endfor'"
	    %endif
            loop%+1 %$begin
	%else
	    loop %$begin
	%endif
	%$end:
	cat_lable %$break,__level__
	
	con_pop
	%assign  __break__ __break__ - 1
%endmacro

; forcx 5, e
;	mov cx,ax
;	cmp ax,bx
; endfor 
;

%macro repeat 0
con_push repeat
%assign  __break__ __break__ + 1
%assign  __cont__ __cont__ + 1
%$begin:
cat_lable %$cont,__level__
%endmacro

%macro until 1-*
    %assign __near__ 0
    %assign __argc__ %0
    %ifidn near,%1
    	%assign __near__ 1
    	%rotate 1
	%assign __argc__ __argc__ -1
    %endif	
    %rep __argc__ -1
	%1
	%rotate 1
    %endrep
    %ifctx repeat
    	%if __near__ >0
		j%-1 near %$begin
	%else
	        j%-1 %$begin
	%endif
	%$end:
	cat_lable %$break,__level__
    %else
	%error "expected `repeat' before `until'"
    %endif
con_pop
%assign  __break__ __break__ - 1
%assign  __cont__ __cont__ - 1
%endmacro

; repeat
;	mov cx,ax
;	cmp ax,bx
; until e
;

%macro while 0-*
con_push while
%assign  __break__ __break__ + 1
%assign  __cont__ __cont__ + 1
%$begin:
cat_lable %$cont,__level__
%if %0 > 0
    %rep %0 -1
	%1
	%rotate 1
    %endrep
    j%-1 %$end
%endif
%endmacro

%macro endwhile 0
    %ifctx while
          jmp %$begin
	  %$end:
	  cat_lable %$break,__level__
    %else
	%error "expected `while' before `endwhile'"
    %endif
con_pop
%assign  __break__ __break__ + 1
%assign  __cont__ __cont__ + 1
%endmacro

; while {cmp ax, bx}, e
;   mov ax, cx
; endwhile

%macro break 0-*
%if %0 > 0
	%ifidn %1,if
	    %rotate 1
	    %rep %0 - 2
	    %1
	    %rotate 1
	    %endrep
	    cat_jc %1,%$break,__level__
	%else
	    %error "expect `if' after `break'"
	%endif
%else
	cat_jmp %$break,__level__
%endif
%endmacro

%macro continue 0-*
%if %0 > 0
	%ifidn %1,if
	    %rotate 1
	    %rep %0 - 2
	    %1
	    %rotate 1
	    %endrep
	    cat_jc %1,%$cont,__level__
	%else
	    %error "expect `if' after `continue'"
	%endif
%else
	cat_jmp %$cont,__level__
%endif
%endmacro

%macro goto 1-*
%if %0 > 1
	%ifidn %2,if
	    %rotate 1
	    %rep %0 - 3
	    	%2
	    	%rotate 1
	    %endrep
	    %rotate 1
	    j%+1 %2
	%else
	    %error "expect `if' after `continue'"
	%endif
%else
	jmp %1
%endif
%endmacro

%macro goton 1-*
%if %0 > 1
	%ifidn %2,if
	    %rotate 1
	    %rep %0 - 3
	    	%2
	    	%rotate 1
	    %endrep
	    %rotate 1
	    j%+1 NEAR %2
	%else
	    %error "expect `if' after `continue'"
	%endif
%else
	jmp %1
%endif
%endmacro

%macro if 0-*
    %rep %0 -1
	%1
	%rotate 1
    %endrep

    con_push if

    %if %0 > 0
    	j%-1 %$ifnot
    %endif
    %assign %$elif_level 0
%endmacro


%macro andblock 0
	%assign __cndblock__ __cndblock__ + 1
	con_push andblock
%endmacro

%macro orblock 0
	%assign __cndblock__ __cndblock__ + 1
	con_push orblock
%endmacro

%macro endblock 0-1
	%ifctx orblock
		cat_jmp %$ifnot,__level__
		%$iftrue:
	%elifctx andblock
	%else
		%error "not in andblock or orblock"
	%endif
	con_pop
	%assign __cndblock__ __cndblock__ - 1
%endmacro

%macro condiction 1-*
	%rep %0 -1
		%1
		%rotate 1
	%endrep
	%ifctx andblock
		cat_jnc %1,%$ifnot,__level__
	%elifctx orblock
		j%+1 %$iftrue
	%else
		%error "not in the andblock or orblock"
	%endif
%endmacro

%macro else 0-1
    %assign __near__ 0
    %if %0 >0
    	%ifidn %1 near
		%assign __near__ 1
	%endif
    %endif
    %ifctx if
        %repl else
        jmp_near __near__,%$ifend
        %$ifnot:
    %elifctx elif
;	%assign elselevel %$elif_level - 1
	jmp_near __near__,%$ifend
	%$ifnot:
	%repl else
	
;	con_pop
;	%rep elselevel
;		%$ifend:
;		con_pop
;	%endrep
    %else
        %error "expected `if' before `else'"
    %endif
%endmacro

%macro elif 1-*
	%ifctx elif
		%assign level %$elif_level + 1
	%else 
		%assign level 1
	%endif
	%repl else
	jmp %$ifend
	%$ifnot:
	%rep %0 -1
		%1
		%rotate 1
	%endrep
	con_push elif
	j%-1 %$ifnot
	%assign %$elif_level level
%endmacro

%macro endif 0
    %assign elselevel 0
    %ifctx if
        %$ifnot:
        con_pop
    %elifctx else
        %assign elselevel %$elif_level
        %$ifend:
        con_pop
    %elifctx elif
	%assign elselevel %$elif_level
	%$ifnot:
	con_pop
    %else
        %error "expected `if' or `else' before `endif'"
    %endif
	%rep elselevel
		%$ifend:
		con_pop
	%endrep
%endmacro
;          cmp ax,bx
;          if ae
;            cmp bx,cx
;            if ae
;              mov ax,cx
;            else
;              mov ax,bx
;            endif
;          else
;            cmp ax,cx
;            if ae
;              mov ax,cx
;            endif
;          endif

%push __base__
%endif

