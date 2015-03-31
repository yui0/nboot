; ---------------------------------------------------------
;	Macro
;
;		(C)2000	NAKADA
; ---------------------------------------------------------

%macro	retz 0
	jnz	%%skip
	ret
%%skip:
%endmacro

%macro	dup	1-2 0
	%rep	%1
		db	%2
	%endrep
%endmacro
