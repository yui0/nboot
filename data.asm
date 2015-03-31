; ---------------------------------------------------------
;	NBoot
;
;		(C)2000	NAKADA
; ---------------------------------------------------------


;==============================================================================
;temp data area for hd_io.asm
;==============================================================================
%ifdef HAVE_HD_IO
%ifndef HD_IO_TEMPDATA
%define HD_IO_TEMPDATA

tmp_cdbc_cmd  resb SIZE_OF_CDBC_CMD
tmp_extparam  resb SIZE_OF_EXTPARAM
tmp_int13ext  resb SIZE_OF_INT13EXT
tmp_driveinfo resb SIZE_OF_DRIVEINFO
disk_errno    resb 1

%endif
%endif

;==============================================================================
;temp data area for knl.asm
;==============================================================================
%ifdef HAVE_KNL
%ifndef KNL_TEMPDATA
%define KNL_TEMPDATA

tmp_good_record_num resb 1
tmp_max_record_num  resb 1
tmp_part_id         resb 1
tmp_logi_father     resd 1

tmp_noflags         resb 1
tmp_floppy_num      resb 1
tmp_cdemu_spec      resb SIZE_OF_CDEMU_SPEC

disk_buf            resb 512
disk_buf1           resb 2048


%endif
%endif

;==============================================================================
;temp data area for ui.asm
;==============================================================================
%ifdef HAVE_UI
%ifndef UI_TEMPDATA
%define UI_TEMPDATA

ui_tmp:
.left_col    resb  1
.top_row     resb  1
.right_col   resb  1
.bottom_row  resb  1
.frame_attr  resb  1
.title_attr  resb  1

ui_input_tmp:
.startp      resw  1
.maxlen      resw  1
.arealen     resw  1
.curp        resw  1
.areapos     resw  1
.tmpbuf      resb  256

tmp_menu_mod resb 1

%endif
%endif

;==============================================================================
;temp data area for utils.asm
;==============================================================================
%ifdef HAVE_UTILS
%ifndef UTILS_TEMPDATA
%define UTILS_TEMPDATA

tmp_kbd_work     resb  1

%endif
%endif

;=============================================================================
;temp data area for main.asm
;=============================================================================
%ifdef HAVE_MAIN_PROG
%ifndef MAIN_TEMPDATA
%define MAIN_TEMPDATA

bootmenu_title_str  resw  1
good_record_number  resb  1
good_record_list    resb MAX_RECORD_NUM

first_visible_rec   resb  1
focus_record        resb  1

time_count          resb  1                       ;
ticks_count         resw  1                       ; used in get_key func
key_pressed         resb  1                       ;

change_occured      resb  1                       ; if change occured.
root_login          resb  1                       ; root login state.
cmd_menu_stat       resb  1                       ; the stat flags of command
                                                ; menus

main_win_width      resb  1
menu_width          resb  1

last_time           resw  1

tmp_buffer          resb 256
tmp_schedule_begin  resw  1
tmp_schedule_end    resw  1
tmp_schedule_day    resw  1
tmp_schedule_input  resb 32
tmp_records_buf     resb MAX_RECORD_NUM * SIZE_OF_BOOTRECORD

tmp_cdimg_menu      resb SIZE_OF_MENU
tmp_cmd_table       resw 8
tmp_hkey_table      resw 8
tmp_string_table    resw 8
tmp_menuitem_str    resb 32*8
cdimg_choice_result  resw 1

%endif
%endif

%ifdef HAVE_MYINT13H
%ifndef MYINT13H_TEMPDATA
%define MYINT13H_TEMPDATA

module_edd30_off  resw 1
module_edd30_seg  resw 1

%endif
%endif
