; ============================================================================
; STNICCC-Archie
; A port of STNICCC-2000 by Oxygene for the Acorn Archimedes series
; ============================================================================

.equ _TESTS, 0
.equ _UNROLL_SPAN, 1
.equ _DRAW_WIREFRAME, 0
.equ _ENABLE_MUSIC, 1
.equ _ALWAYS_CLS, 1

.equ Screen_Banks, 3
.equ Screen_Mode, 9
.equ Screen_Width, 320
.equ Screen_Height, 256
.equ Window_Width, 256
.equ Window_Height, 200
.equ Screen_Stride, Screen_Width/2		; 4bpp
.equ Screen_Bytes, Screen_Stride*Screen_Height
.equ Window_Stride, Screen_Width/2		; 4bpp
.equ Window_Bytes, Window_Stride*Window_Height

.equ Wait_Centisecs_lo, (400) & 0xff
.equ Wait_Centisecs_hi, (400) >> 8

.equ Screen_Offset, (Screen_Stride*(Screen_Height-Window_Height)/2)+((Screen_Width-Window_Width)/4)

.equ Sequence_Total_Frames, 1800

.include "swis.h.asm"

.org 0x8000

; ============================================================================
; Stack
; ============================================================================

Start:
    adrl sp, stack_base
	B main

.skip 1024
stack_base:

; ============================================================================
; Main
; ============================================================================

wtaf:
	.skip 24

main:
	MOV r0,#22	;Set MODE
	SWI OS_WriteC
	MOV r0,#Screen_Mode
	SWI OS_WriteC

	; Set screen size for number of buffers
	MOV r0, #DynArea_Screen
	SWI OS_ReadDynamicArea
	MOV r0, #DynArea_Screen
	MOV r2, #Screen_Bytes * Screen_Banks
	SUBS r1, r2, r1
	SWI OS_ChangeDynamicArea
	MOV r0, #DynArea_Screen
	SWI OS_ReadDynamicArea
	CMP r1, #Screen_Bytes * Screen_Banks
	ADRCC r0, error_noscreenmem
	SWICC OS_GenerateError

	MOV r0,#23	;Disable cursor
	SWI OS_WriteC
	MOV r0,#1
	SWI OS_WriteC
	MOV r0,#0
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC
	SWI OS_WriteC

	; Load scene1.bin
	mov r0, #0xff
	adr r1, scene1_filename
	adr r2, scene1_data_stream
    mov r3, #0
	swi OS_File

.if _ENABLE_MUSIC
	; Load module
	adrl r0, module_filename
	mov r1, #0
	swi QTM_Load

	mov r0, #48
	swi QTM_SetSampleSpeed
.endif

	; Clear all screen buffers
	mov r1, #1
.1:
	str r1, scr_bank

	; CLS bank N
	mov r0, #OSByte_WriteVDUBank
	swi OS_Byte
	mov r0, #12
	SWI OS_WriteC

	ldr r1, scr_bank
	add r1, r1, #1
	cmp r1, #Screen_Banks
	ble .1

	; Start with bank 1
	mov r1, #1
	str r1, scr_bank
	
	; Claim the Error vector
	MOV r0, #ErrorV
	ADR r1, error_handler
	MOV r2, #0
	SWI OS_Claim

	; Claim the Event vector
	mov r0, #EventV
	adr r1, event_handler
	mov r2, #0
	swi OS_AddToVector

	bl initialise_span_buffer

	; Enable Vsync event
	mov r0, #OSByte_EventEnable
	mov r1, #Event_VSync
	SWI OS_Byte

.if _ENABLE_MUSIC
	swi QTM_Start
.endif

events_loop:

	; Block if we've not even had a vsync since last time - we're >50Hz!
	ldr r1, last_vsync
.1:
	ldr r2, vsync_count
	cmp r1, r2
	beq .1
	str r2, last_vsync

	; show debug
	bl debug_write_vsync_count

	; handle any events
	bl events_update

	; do the effect update
	adr lr, .2
	ldr r0, update_fn_id
	adr r1, update_fn_table
	add pc, r1, r0, lsl #2
	.2:

	; exit if SPACE is pressed
	MOV r0, #OSByte_ReadKey
	MOV r1, #IKey_Escape
	MOV r2, #0xff
	SWI OS_Byte
	
	CMP r1, #0xff
	CMPEQ r2, #0xff
	BEQ exit
	
	b events_loop

error_noscreenmem:
	.long 0
	.byte "Cannot allocate screen memory!"
	.align 4
	.long 0

.if 0
wait_pause:
	; Wait 4s
	MOV r0, #OSByte_ReadKey
	MOV r1, #Wait_Centisecs_lo
	MOV r2, #Wait_Centisecs_hi
	SWI OS_Byte
	mov pc, lr
.endif

debug_write_vsync_count:
	mov r0, #30
	swi OS_WriteC

	ldr r0, vsync_count
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertHex4

	adr r0, debug_string
	swi OS_WriteO
	mov pc, r14

debug_string:
	.skip 8

get_screen_addr:
	str lr, [sp, #-4]!
	adrl r0, screen_addr_input
	adrl r1, screen_addr
	swi OS_ReadVduVariables
	ldr pc, [sp], #4
	
screen_addr_input:
	.long VD_ScreenStart, -1
screen_addr:
	.long 0

exit:	
	; wait for vsync (any pending buffers)
	mov r0, #19
	swi OS_Byte

.if _ENABLE_MUSIC
	; disable music
	mov r0, #0
	swi QTM_Stop
.endif

	; disable vsync event
	mov r0, #OSByte_EventDisable
	mov r1, #Event_VSync
	swi OS_Byte

	; release our event handler
	mov r0, #EventV
	adr r1, event_handler
	mov r2, #0
	swi OS_Release

	; release our error handler
	mov r0, #ErrorV
	adr r1, error_handler
	mov r2, #0
	swi OS_Release

	; Display whichever bank we've just written to
	mov r0, #OSByte_WriteDisplayBank
	ldr r1, scr_bank
	swi OS_Byte
	; and write to it
	mov r0, #OSByte_WriteVDUBank
	ldr r1, scr_bank
	swi OS_Byte

	SWI OS_Exit

; R0=event number
event_handler:
	cmp r0, #Event_VSync
	movnes pc, r14

	STMDB sp!, {r0-r1, lr}

	; update the vsync counter
	LDR r0, vsync_count
	ADD r0, r0, #1
	STR r0, vsync_count

	; is there a new screen buffer ready to display?
	LDR r1, buffer_pending
	CMP r1, #0
	LDMEQIA sp!, {r0-r1, pc}

	; set the display buffer
	MOV r0, #0
	STR r0, buffer_pending
	MOV r0, #OSByte_WriteDisplayBank

	; some SVC stuff I don't understand :)
	STMDB sp!, {r2-r12}
	MOV r9, pc     ;Save old mode
	ORR r8, r9, #3 ;SVC mode
	TEQP r8, #0
	MOV r0,r0
	STR lr, [sp, #-4]!
	SWI XOS_Byte

	; set full palette if there is a pending palette block
	ldr r2, palette_pending
	cmp r2, #0
	beq .4

    adr r1, palette_osword_block
    mov r0, #16
    strb r0, [r1, #1]       ; physical colour

    mov r3, #0
    .3:
    strb r3, [r1, #0]       ; logical colour

    ldr r4, [r2], #4        ; rgbx
    and r0, r4, #0xff
    strb r0, [r1, #2]       ; red
    mov r0, r4, lsr #8
    strb r0, [r1, #3]       ; green
    mov r0, r4, lsr #16
    strb r0, [r1, #4]       ; blue
    mov r0, #12
    swi XOS_Word

    add r3, r3, #1
    cmp r3, #16
    blt .3

	mov r0, #0
	str r0, palette_pending
.4:

	LDR lr, [sp], #4
	TEQP r9, #0    ;Restore old mode
	MOV r0, r0
	LDMIA sp!, {r2-r12}
	LDMIA sp!, {r0-r1, pc}

scr_bank:
	.long 0

vsync_count:
	.long 0

last_vsync:
	.long -1

vsync_final:
	.long 0

buffer_pending:
	.long 0

palette_pending:
	.long 0

update_fn_id:
	.long 1

update_fn_table:
	b do_nothing
	b parser_update

error_handler:
	STMDB sp!, {r0-r2, lr}
	MOV r0, #OSByte_EventDisable
	MOV r1, #Event_VSync
	SWI OS_Byte
	MOV r0, #EventV
	ADR r1, event_handler
	mov r2, #0
	SWI OS_Release
	MOV r0, #ErrorV
	ADR r1, error_handler
	MOV r2, #0
	SWI OS_Release
	MOV r0, #OSByte_WriteDisplayBank
	LDR r1, scr_bank
	SWI OS_Byte
	LDMIA sp!, {r0-r2, lr}
	MOVS pc, lr

show_screen_at_vsync:
	; Show current bank at next vsync
	ldr r1, scr_bank
	str r1, buffer_pending
	; Including its associated palette
	ldr r1, palette_block_addr
	str r1, palette_pending
	mov pc, lr

get_next_screen_for_writing:
	; Increment to next bank for writing
	ldr r1, scr_bank
	add r1, r1, #1
	cmp r1, #Screen_Banks
	movgt r1, #1
	str r1, scr_bank

	; Now set the screen bank to write to
	mov r0, #OSByte_WriteVDUBank
	swi OS_Byte

	; Back buffer address for writing bank stored at screen_addr
	b get_screen_addr

; R12=screen_addr, trashes r7, r8, r9
window_cls:
	ldr r8, screen_addr
	add r8, r8, #Screen_Offset
	add r9, r8, #Window_Bytes
	mov r0, #0
	mov r1, #0
	mov r2, #0
	mov r3, #0
	mov r4, #0
	mov r5, #0
	mov r6, #0
	mov r7, #0
.1:
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	add r8, r8, #32
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	add r8, r8, #32
	cmp r8, r9
	blt .1
	mov pc, lr

; trashes r0-r9
screen_cls:
	add r9, r8, #Screen_Bytes
	mov r0, #0
	mov r1, #0
	mov r2, #0
	mov r3, #0
	mov r4, #0
	mov r5, #0
	mov r6, #0
	mov r7, #0
.1:
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	stmia r8!, {r0-r7}
	cmp r8, r9
	blt .1
	mov pc, lr

; ============================================================================
; Additional code modules
; ============================================================================

.include "events.asm"
.include "parser.asm"
.include "palette.asm"
.include "image.asm"
.include "plot.asm"
.include "lz4-decode.asm"

; ============================================================================
; Assets and data
; ============================================================================

images_table:
    .long slide_01_lz4-images_table, slide_01_pal_block-images_table
    .long slide_02_lz4-images_table, slide_02_pal_block-images_table
    .long slide_03_lz4-images_table, slide_03_pal_block-images_table
    .long slide_04_lz4-images_table, slide_04_pal_block-images_table
    .long slide_05_lz4-images_table, slide_05_pal_block-images_table
    .long patarty_lz4-images_table,	 patarty_pal_block-images_table
    .long logo_lz4-images_table, 	 logo_pal_block-images_table

title_pal_block:
.incbin "build/title.pal"

slide_01_pal_block:
.incbin "build/slide_01.pal"

slide_02_pal_block:
.incbin "build/slide_02.pal"

slide_03_pal_block:
.incbin "build/slide_03.pal"

slide_04_pal_block:
.incbin "build/slide_04.pal"

slide_05_pal_block:
.incbin "build/slide_05.pal"

patarty_pal_block:
.incbin "build/patarty.pal"

logo_pal_block:
.incbin "build/logo.pal"

text_blocks_table:
	.long text_01_string-text_blocks_table
	.long text_02_string-text_blocks_table

text_01_string:
	.byte 31,15,15,17,15,"Not again?",0
	.align 4

text_02_string:
	.byte 31,14,15,17,15,"A demo by...",0
	.align 4

scene1_filename:
	.byte "<Demo$Dir>.Scene1",0
	.align 4

module_filename:
	.byte "<Demo$Dir>.Music",0
	.align 4

; ============================================================================
; BSS Segment
; ============================================================================

.p2align 8
span_buffer_start:
	.skip 1024, 0

.p2align 8
span_buffer_end:
	.skip 1024,0 

; ============================================================================
; Data Segment
; ============================================================================

scene1_data_index:
.incbin "data/stniccc/index.bin"

scene1_colours_index:
.incbin "data/stniccc/colours.bin"

.equ scene1_colours_array, scene1_colours_index + 1800

.align 4
slide_01_lz4:
.incbin "build/slide_01.lz4"

.align 4
slide_02_lz4:
.incbin "build/slide_02.lz4"

.align 4
slide_03_lz4:
.incbin "build/slide_03.lz4"

.align 4
slide_04_lz4:
.incbin "build/slide_04.lz4"

.align 4
slide_05_lz4:
.incbin "build/slide_05.lz4"

.align 4
patarty_lz4:
.incbin "build/patarty.lz4"

.align 4
logo_lz4:
.incbin "build/logo.lz4"

; ============================================================================
; Scene1.bin data stream
; ============================================================================

.p2align 8
scene1_data_stream:
