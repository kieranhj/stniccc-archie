; ============================================================================
; STNICCC-Archie
; A port of STNICCC-2000 by Oxygene for the Acorn Archimedes series
; ============================================================================

.equ _TESTS, 0
.equ _UNROLL_SPAN, 1
.equ _DRAW_WIREFRAME, 0
.equ _ENABLE_MUSIC, 1
.equ _ALWAYS_CLS, 1
.equ _INDEX_PALETTE, 1

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

.if _ENABLE_MUSIC
	; Load module
	mov r0, #0
	adrl r1, module_data
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

main_loop:   
	; debug
	bl debug_write_vsync_count

	; Block if we've not even had a vsync since last time - we're >50Hz!
	ldr r1, last_vsync
.1:
	ldr r2, vsync_count
	cmp r1, r2
	beq .1
	str r2, last_vsync

	; Swap banks
	; Display whichever bank we've just written to
	ldr r1, scr_bank			; bank we want to display next
	str r1, buffer_pending		; we might overwrite a bank if too fast (drop a frame?)
	; If we have more than 3 banks then this needs to be a queue
	; This now happens in vsync event handler
	;	mov r0, #OSByte_WriteDisplayBank
	;	swi OS_Byte

	.if _INDEX_PALETTE
	ldr r9, palette_count
	str r9, palette_pending
	.else
	; Set the palette from the previous frame if there is one
	ldr r9, palette_count
	cmp r9, #0
	beq .2

	mov r0, #12
	adrl r1, palette_block
.3:
	swi OS_Word
	add r1, r1, #8
	subs r9, r9, #1
	bne .3

	str r9, palette_count
.2:
	.endif

	; Increment to next bank for writing
	ldr r1, scr_bank
	add r1, r1, #1
	cmp r1, #Screen_Banks
	movgt r1, #1
	str r1, scr_bank

	; Now set the screen bank to write to
	mov r0, #OSByte_WriteVDUBank
	swi OS_Byte

	; Wait for vsync if double buffering
	.if Screen_Banks <= 2
	mov r0, #OSByte_Vsync
	swi OS_Byte
	.endif

	; Back buffer address for writing bank stored at screen_addr
	bl get_screen_addr

	; Do stuff here!
	ldr r0, frame_number

	adrl r4, scene1_colours_index
	ldrb r5, [r4, r0]				; colour index for this frame
	strb r5, palette_count

	adrl r2, scene1_data_index
	ldr r3, [r2, r0, lsl #2]		; offset for this frame number

	adrl r1, scene1_data_stream
	add r11, r3, r1					; pointer to data for frame

	;ldr r11, parse_frame_ptr
	bl parse_frame
	;str r11, parse_frame_ptr

	ldr r11, frame_number
	subs r11, r11, #1
	bmi exit
	str r11, frame_number

	;cmp r0, #POLY_DESC_END_OF_STREAM
	;beq exit

	;Exit if SPACE is pressed
	MOV r0, #OSByte_ReadKey
	MOV r1, #IKey_Space
	MOV r2, #0xff
	SWI OS_Byte
	
	CMP r1, #0xff
	CMPEQ r2, #0xff
	BEQ exit
	
	B main_loop

error_noscreenmem:
	.long 0
	.byte "Cannot allocate screen memory!"
	.align 4
	.long 0

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

	; Display whichever bank we've just written to
	mov r0, #OSByte_WriteDisplayBank
	ldr r1, scr_bank
	swi OS_Byte
	; and write to it
	mov r0, #OSByte_WriteVDUBank
	ldr r1, scr_bank
	swi OS_Byte

	; Show our final frame count
	bl debug_write_vsync_count

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

	.if _INDEX_PALETTE
	; Palette for frame should be set in event handler.
	ldr r2, palette_pending
	adrl r3, scene1_colours_array
	add r1, r3, r2, lsl #7			; each block is 16 * 8 bytes = 128
	mov r0, #12						; OS_Word 12 = redefine palette
	mov r2, #16						; do all 16 colours
.3:
	swi XOS_Word
	add r1, r1, #8
	subs r2, r2, #1
	bne .3
	.endif

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

buffer_pending:
	.long 0

palette_pending:
	.long 0

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

; R12=screen_addr, trashes r7, r8, r9
window_cls:
	ldr r8, screen_addr
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

; ============================================================================
; Parsing the scene1.bin data file
; ============================================================================

.equ FLAG_CLEAR_SCREEN, 0x01
.equ FLAG_CONTAINS_PALETTE, 0x02
.equ FLAG_INDEXED_DATA, 0x04

.equ POLY_DESC_END_OF_STREAM, 0xfd
.equ POLY_DESC_SKIP_TO_64K, 0xfe
.equ POLY_DESC_END_OF_FRAME, 0xff

; R11=ptr to frame
; R12=screen_addr
parse_frame:
	stmfd sp!, {lr}

	; get_byte
	ldrb r10, [r11], #1			; r10=frame_flags

	tst r10, #FLAG_CLEAR_SCREEN
	.if _DRAW_WIREFRAME | _ALWAYS_CLS
	bl window_cls
	.else
	blne window_cls
	.endif

	tst r10, #FLAG_CONTAINS_PALETTE
	beq .1						; no_palette

	; get_byte
	ldrb r1, [r11], #1			; r1=palette_mask HI
	; get_byte
	ldrb r0, [r11], #1			; r0=palette_mask LO
	mov r2, r1, lsl #24
	orr r2, r2, r0, lsl #16		; r2 = r1 << 24 | r0 << 16

	.if _INDEX_PALETTE
	.else
	mov r6, #0					; r6 = palette_count
	adrl r7, palette_block		; r7 = &palette_block
	.endif

	; read palette words
	mov r5, #0					; r1 = palette loop counter
.2:
	movs r2, r2, asl #1
	bcc .3

	; get_byte
	ldrb r3, [r11], #1			; r3 = xxxxrrrr
	; get_byte
	ldrb r4, [r11], #1			; r4 = ggggbbbb

	.if _INDEX_PALETTE
	.else
	strb r5, [r7], #1			; logical colour
	mov r0, #16
	strb r0, [r7], #1			; physical colour

	mov r0, r3, lsl #5
	orr r0, r0, #0x10
	strb r0, [r7], #1			; red component

	and r0, r4, #0x70
	mov r0, r0, lsl #1
	orr r0, r0, #0x10
	strb r0, [r7], #1			; green component

	mov r0, r4, lsl #5
	orr r0, r0, #0x10
	strb r0, [r7], #1+3			; blue component

	add r6, r6, #1				; palette_count++
	.endif

.3:
	add r5, r5, #1
	cmp r5, #16
	blt .2

	.if _INDEX_PALETTE
	.else
	str r6, palette_count
	.endif

.1:

	tst r10, #FLAG_INDEXED_DATA
	beq parse_frame_read_poly_data

	; get_byte
	ldrb r1, [r11], #1			; r0=num_verts

	; store ptr to literal vertex data
	mov r8, r11
	add r9, r8, #1

	; next is an array of (x,y) bytes

	add r11, r11, r1, lsl #1	; skip num_verts*2 bytes

parse_frame_read_poly_data:
	; get_byte
	ldrb r0, [r11], #1			; r0=poly_descriptor

	; end of frame marker?
	cmp r0, #POLY_DESC_END_OF_STREAM
	bge parse_end_of_frame

	; low nibble = num verts
	and r1, r0, #0x0F			; r1=num_verts

	; high nibble = palette
	mov r4, r0, lsr #4			; r4=palette

	adrl r6, polygon_list		; r6=polygon_list array

	; is the data indexed?
	tst r10, #FLAG_INDEXED_DATA
	beq non_indexed_data

	; indexed
	mov r0, r1
.5:
	; read index
	; get_byte
	ldrb r5, [r11], #1			; r5=index

	; lookup verts
	ldrb r2, [r8, r5, lsl #1]	; r2=vertices_x[i]
	ldrb r3, [r9, r5, lsl #1]	; r3=vertices_y[i]

	; store into a temp array for now
	stmia r6!, {r2, r3}			; *temp_poly_data++ = x

	subs r1, r1, #1
	bne .5
	b parse_plot_poly

non_indexed_data:
	; non-indexed
	mov r0, r1
.6:
	; copy (x,y) bytes directly to temp array
	; get_byte
	ldrb r2, [r11], #1			; r2=x
	; get_byte
	ldrb r3, [r11], #1			; r3=y

	; store into a temp array for now
	stmia r6!, {r2, r3}			; *temp_poly_data++ = x, y

	subs r1, r1, #1
	bne .6

parse_plot_poly:
	; store off any registers we need here!
	stmfd sp!, {r8-r11}

	; plot the polygon!
	adrl r1, polygon_list
	; r4=palette
	.if _DRAW_WIREFRAME
	bl plot_polygon_line
	.else
	bl plot_polygon_span
	.endif
	
	; pull any registers we need here!
	ldmfd sp!, {r8-r11}

	b parse_frame_read_poly_data

parse_end_of_frame:
	; parse EOF flag
	cmp r0, #POLY_DESC_SKIP_TO_64K
	bne .1

	; make ptr relative to 0
	adr r1, scene1_data_stream
	sub r11, r11, r1

	; align ptr to 64K
	; ptr += 0xffff
	add r11, r11, #0xff
	add r11, r11, #0xff00

	; ptr AND= 0xffff0000
	bic r11, r11, #0x000000ff
	bic r11, r11, #0x0000ff00

	; add base back to ptr
	add r11, r11, r1
.1:

	ldmfd sp!, {lr}
	mov pc, lr

parse_frame_ptr:
	.long scene1_data_stream

frame_number:
	.long 1799

polygon_list:
	.long 32, 32
	.long 160, 32
	.long 160, 101
	.long 32, 111
	.long 0, 64
	.long 0, 0
	.long 0, 0
	.long 0, 0

palette_count:
	.long 0

palette_block:
	.skip 8*16

; ============================================================================
; Additional code modules
; ============================================================================

.include "plot.asm"
.include "palette.asm"

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
.incbin "data/index.bin"

scene1_colours_index:
.incbin "data/colours.bin"

.equ scene1_colours_array, scene1_colours_index + 1800

scene1_data_stream:
.incbin "data/scene1.bin"

; ============================================================================
; Music
; ============================================================================

.if _ENABLE_MUSIC
.p2align 8
module_data:
.incbin "data/checknobankh.mod"
.endif
