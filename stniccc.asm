; STNICCC-Archie
; A port of STNICCC-2000 by Oxygene for the Acorn Archimedes series
; 

.equ _TESTS, 0
.equ _TEST_EXO, 0
.equ _UNROLL_SPAN, 1
.equ _DRAW_WIREFRAME, 0
.equ _ENABLE_MUSIC, 1
.equ _DEBUG, 1

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

Start:
    adr sp, stack_base
	B main

.skip 1024
stack_base:

scr_bank:
	.long 0

main:
	mov r0, #16
	adr r1, data_filename
	adr r2, scene1_data_stream
	mov r3, #0
	swi OS_File
	; R4=file length

	adr r12, scene1_data_stream
.if _TEST_EXO
	; R4=file length
	add r10, r12, r4
	add r10, r10, #0xff
	bic r10, r10, #0xff
	str r10, top_addr
.endif

	; init decruncher
	bl exo_decrunch_new

	; fill exo window
	mov r8, #WINDOW_LENGTH
	bl exo_read_decrunched_byte

.if _TEST_EXO
	bl test_exo
	swi OS_Exit
.endif

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
	adr r1, module_data
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

	; Set the palette from the previous frame if there is one
	ldr r9, palette_count
	cmp r9, #0
	beq .2

	mov r0, #12
	adr r1, palette_block
.3:
	swi OS_Word
	add r1, r1, #8
	subs r9, r9, #1
	bne .3

	str r9, palette_count
.2:

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

	;Do stuff here!

.if _TESTS
	ldr r12, screen_addr			; R12=generic screen_addr ptr
	add r9, r12, #Screen_Bytes

	mov r2, #0
	mov r8, #121
	mov r7, #319
	mov r4, #0x11
	orr r4, r4, r4, lsl #8
	orr r4, r4, r4, lsl #16

	mov r1, r4
	mov r2, r4
	mov r6, r4

my_test_loop:

	cmp r8, r7
	movle r0, r8
	movle r3, r7
	movgt r0, r7
	movgt r3, r8

	; preserve r7, r8, r9, r12
	bl plot_span

;	add r6, r6, #1
	sub r7, r7, #1

	add r12, r12, #160
	cmp r12, r9
	bne my_test_loop

	ldr r12, screen_addr			; R12=generic screen_addr ptr

	mov r0, #0
	mov r1, #128
my_test_points:
	mov r4, #2
	bl plot_pixel
	add r0, r0, #3
	eor r1, r1, #1
	cmp r0, #320
	blt my_test_points

	bl initialise_span_buffer

	adr r1, poly_buffer
	mov r0, #5
	mov r4, #6
	bl plot_polygon_span

	b exit
.endif

	; Store current ptr
	ldr r0, parse_frame_ptr
	str r0, parse_frame_prev

	bl parse_frame
	cmp r0, #POLY_DESC_END_OF_STREAM
	beq exit

	; Catch up the decompressor
	ldr r8, parse_frame_ptr
	ldr r0, parse_frame_prev
	subs r8, r8, r0
	addlt r8, r8, #WINDOW_LENGTH
	bl exo_read_decrunched_byte

	;Exit if SPACE is pressed
	MOV r0, #OSByte_ReadKey
	MOV r1, #IKey_Space
	MOV r2, #0xff
	SWI OS_Byte
	
	CMP r1, #0xff
	CMPEQ r2, #0xff
	BEQ exit
	
	B main_loop

wtaf_pad:
	.skip 32

error_noscreenmem:
	.long 0
	.byte "Cannot allocate screen memory!"
	.p2align 2
	.long 0

.if _DEBUG
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

debug_write_r0:
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertHex2
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, r14

debug_write_16:
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertHex4
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, r14

debug_write_32:
	adr r1, debug_string
	mov r2, #12
	swi OS_ConvertHex8
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #32
	swi OS_WriteC
	mov pc, r14

debug_string:
	.skip 12
.endif

.if _TEST_EXO
top_addr:
	.long 0

test_exo:
	str lr, [sp, #-4]!
	ldr r8, top_addr

	.1:
	bl exo_read_decrunched_byte
	cmp r0, #-1
	beq .2

	strb r0, [r8], #1
	b .1

	.2:
	mov r0, #0
	adr r1, save_filename
	mov r2, #0x8000
	mov r3, #0x8000
	ldr r4, top_addr	; start address
	mov r5, r8			; end address
	swi OS_File

	ldr pc, [sp], #4

save_filename:
	.byte "test/bin"
	.byte 0
	.p2align 2
.endif

get_screen_addr:
	str lr, [sp, #-4]!
	adr r0, screen_addr_input
	adr r1, screen_addr
	swi OS_ReadVduVariables
	ldr pc, [sp], #4
	
screen_addr_input:
	.long VD_ScreenStart, -1
screen_addr:
	.long 0

exit:	
.if _ENABLE_MUSIC
	mov r0, #0
	swi QTM_Stop
.endif

	; Display whichever bank we've just written to
	ldr r1, scr_bank
	mov r0, #OSByte_WriteDisplayBank
	swi OS_Byte

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

.if 1
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
	LDR lr, [sp], #4
	TEQP r9, #0    ;Restore old mode
	MOV r0, r0
	LDMIA sp!, {r2-r12}
.endif

	LDMIA sp!, {r0-r1, pc}

vsync_count:
	.long 0

last_vsync:
	.long -1

buffer_pending:
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

.equ FLAG_CLEAR_SCREEN, 0x01
.equ FLAG_CONTAINS_PALETTE, 0x02
.equ FLAG_INDEXED_DATA, 0x04

.equ POLY_DESC_END_OF_STREAM, 0xfd
.equ POLY_DESC_SKIP_TO_64K, 0xfe
.equ POLY_DESC_END_OF_FRAME, 0xff

.macro GET_BYTE reg
.if 0
; Read byte from uncompressed stream
;ldrb \reg, [r11], #1

; Decompress each byte on demand
stmfd sp!, {r1-r12}
bl exo_read_decrunched_byte
ldmfd sp!, {r1-r12}
add r11, r11, #1
mov \reg, r0
.else
; Read byte from decompressed window
ldrb \reg, [r11], #1
cmp r11, r7
subge r11, r11, #WINDOW_LENGTH
.endif
.endm

parse_frame:
	stmfd sp!, {lr}

	ldr r11, parse_frame_ptr
	adr r7, exo_window_end

	; get_byte
	GET_BYTE r10				; r10=frame_flags

	tst r10, #FLAG_CLEAR_SCREEN
	.if _DRAW_WIREFRAME
	bl window_cls
	.else
	blne window_cls
	.endif
	adr r7, exo_window_end

	tst r10, #FLAG_CONTAINS_PALETTE
	beq .1						; no_palette

	; get_byte
	GET_BYTE r1					; r1=palette_mask HI
	; get_byte
	GET_BYTE r0					; r0=palette_mask LO
	mov r2, r1, lsl #24
	orr r2, r2, r0, lsl #16		; r2 = r1 << 24 | r0 << 16

	mov r6, #0					; r6 = palette_count
	adr r9, palette_block		; r9 = &palette_block

	; read palette words
	mov r5, #0					; r5 = palette loop counter
.2:
	movs r2, r2, asl #1
	bcc .3

	; get_byte
	GET_BYTE r3					; r3 = xxxxrrrr
	; get_byte
	GET_BYTE r4					; r4 = ggggbbbb

	strb r5, [r9], #1			; logical colour
	mov r0, #16
	strb r0, [r9], #1			; physical colour

	mov r0, r3, lsl #5
	orr r0, r0, #0x10
	strb r0, [r9], #1			; red component

	and r0, r4, #0x70
	mov r0, r0, lsl #1
	orr r0, r0, #0x10
	strb r0, [r9], #1			; green component

	mov r0, r4, lsl #5
	orr r0, r0, #0x10
	strb r0, [r9], #1+3			; blue component

	add r6, r6, #1				; palette_count++

.3:
	add r5, r5, #1
	cmp r5, #16
	blt .2

	str r6, palette_count

.1:

	tst r10, #FLAG_INDEXED_DATA
	beq parse_frame_read_poly_data

	; get_byte
	GET_BYTE r1					; r1 = num_verts

.if 0
	; store ptr to literal vertex data
	mov r8, r11
	add r9, r8, #1

	; next is an array of (x,y) bytes

	add r11, r11, r1, lsl #1	; skip num_verts*2 bytes
.else
	adr r8, vertex_buffer
	mov r9, r1
	; Could copy N*2 bytes directly out of buffer?
	; Data only guaranteed to be 64K aligned so could
	; wrap around our 8K buffer during a frame, so no!
	.4:
		GET_BYTE r0				; x
		strb r0, [r8], #1
		GET_BYTE r0				; y
		strb r0, [r8], #1
		subs r9, r9, #1
		bne .4

	adr r8, vertex_buffer
	add r9, r8, #1
.endif

parse_frame_read_poly_data:

	; get_byte
	GET_BYTE r0					; r0 = poly_descriptor

	; end of frame marker?
	cmp r0, #POLY_DESC_END_OF_STREAM
	bge parse_end_of_frame

	; low nibble = num verts
	and r1, r0, #0x0F			; r1=num_verts

	; high nibble = palette
	mov r4, r0, lsr #4			; r4=palette

	adr r6, poly_buffer		; r6=poly_buffer array

	; is the data indexed?
	tst r10, #FLAG_INDEXED_DATA
	beq non_indexed_data

	; indexed
	mov r12, r1
.5:
	; read index
	; get_byte
	GET_BYTE r5					; r5 = index

	; lookup verts
	ldrb r2, [r8, r5, lsl #1]	; r2=vertices_x[i]
	ldrb r3, [r9, r5, lsl #1]	; r3=vertices_y[i]

	; store into a temp array for now
	stmia r6!, {r2, r3}			; *temp_poly_data++ = x

	subs r12, r12, #1
	bne .5
	b parse_plot_poly

non_indexed_data:

	; non-indexed
	mov r12, r1
	.6:
		; copy (x,y) bytes directly to temp array
		; get_byte
		GET_BYTE r2					; r2 = x
		; get_byte
		GET_BYTE r3					; r3 = y

		; store into a temp array for now
		stmia r6!, {r2, r3}			; *poly_buffer++ = x, y
		subs r12, r12, #1
		bne .6

parse_plot_poly:

	; store off any registers we need here!
	stmfd sp!, {r7-r11}

	mov r0, r1
	; plot the polygon!
	adr r1, poly_buffer
	; r4=palette
	.if _DRAW_WIREFRAME
	bl plot_polygon_line
	.else
	bl plot_polygon_span
	.endif
	
	; pull any registers we need here!
	ldmfd sp!, {r7-r11}

	b parse_frame_read_poly_data

parse_end_of_frame:

	; parse EOF flag
	cmp r0, #POLY_DESC_SKIP_TO_64K
	bne .1

	.if 0
	; align ptr to 64K
	; ptr += 0xffff
	.2:
		GET_BYTE r0
		bic r0, r11, #0xff000000
		bics r0, r0,  #0x00ff0000
		bne .2
	.else
	adr r11, exo_window
	.endif

.1:
	str r11, parse_frame_ptr

	ldmfd sp!, {lr}
	mov pc, lr

parse_frame_ptr:
	.long exo_window	;0	;scene1_data_stream

parse_frame_prev:
	.long 0

; reserved r15, r14, r13
; preserve r7, r8, r9, r12
; passed in r0, r3, r4
; R0=x_start, R3=x_end/width, r4=colour, R12=screen_addr for line

; free r1, r2, r5, r6, r10, r11
; r10=writeptr, r5=readptr, r11=temp
; would like 4x colour registers! r4 + r1, r2, r6

; no longer handles x_end < x_start!!
plot_span:

	; ptr = screen_addr + y * screen_stride + x_start DIV 2
	add r10, r12, r0, lsr #1	; r10 += startx DIV 2

	sub r3, r3, r0				; r3 = x_end - x_start = x_width
	add r3, r3, #1				; always plot at least one pixel

	cmp r3, #9
	ble plot_short_span

	ands r0, r0, #7				; r0=pixel offset [0-7]
	beq .1						; already aligned

	; align to word
	bic r10, r10, #3			; nearest word
	rsb r0, r0, #8				; number of pixels to mask in
	sub r3, r3, r0				; width -= number pixels plotted in start word

	; find table
	adr r5, start_word_pixel_masks - 4

	; read mask word 0
	ldr r11, [r5, r0, lsl #2]	; r11 = start_word_pixel_masks[pixels to plot * 4]
	ldr r5, [r10]				; r5 = screen word
	bic r5, r5, r11				; mask screen word
	and r11, r4, r11			; mask colour word
	orr r5, r5, r11				; mask together
	str r5, [r10], #4			; store back to screen

.1:
	; plot word at a time
	movs r5, r3, lsr #3			; each word = 8 pixels so word count = width/8
	beq plot_span_last_word		; if width = 10, pixel offset = 1, then will get here with 3 pixels left

	sub r3, r3, r5, lsl #3		; width -= words * 8

.if _UNROLL_SPAN
	adr r11, span_jump_table - 4
	add r11, r11, r5, lsl #2
	mov pc, r11
	return_here_from_jump:
.else
	; plot words
	; max 40 words
.4:
	str r4, [r10], #4			; write 8 pixels (one word) to screen, post index
	subs r5, r5, #1				; decrement word count
	bne .4
.endif

	; handle remaining word
plot_span_last_word:
	cmp r3, #0
	moveq pc, lr				; rts

	; find table
	adr r5, short_pixel_1 - 64

	; read mask word 0
	ldr r11, [r5, r3, lsl #6]	; r11 = start_word_pixel_masks[pixels to plot * 4]
	ldr r5, [r10]				; r5 = screen word
	bic r5, r5, r11				; mask screen word
	and r11, r4, r11			; mask colour word
	orr r5, r5, r11				; mask together
	str r5, [r10], #4			; store back to screen

	; return
	mov pc, lr					; rts

initialise_span_buffer:
	mov r0, #0
	str r0, span_buffer_max_y
	mov r1, #256
	str r1, span_buffer_min_y

	mov r2, #0
	adr r3, span_buffer_start
	adr r4, span_buffer_end
.1:
	str r1, [r3, r2, lsl #2]			; span_buffer_start[y] = 256
	str r0, [r4, r2, lsl #2]			; span_buffer_end[y] = 0

	add r2, r2, #1
	cmp r2, #256
	blt .1
	mov pc, lr

.if 1
; passed in R0=startx, R1=starty, R2=endx, R3=endy
; used R5=dx, R6=dy, R7=sx, R8=sy, R9=err, R10=e2/addr/temp
; preserve: R11=span_buffer_start, R12=span_buffer_end, R4=poly ptr
drawline_into_span_buffer:

	ldr r5, span_buffer_min_y
	cmp r1, r5					; starty < min_y?
	strlt r1, span_buffer_min_y	; min_y = starty

	ldr r5, span_buffer_max_y
	cmp r1, r5					; starty > max_y?
	strgt r1, span_buffer_max_y	; max_y = starty

	subs r5, r2, r0				; r5 = dx = endx - startx
	rsblt r5, r5, #0			; r5 = abs(dx)
	movge r7, #1				; r8 = sy = 1
	movlt r7, #-1				; r8 = sy = -1

	subs r6, r3, r1				; r6 = dy = endy - starty
	rsbgt r6, r6, #0			; r6 = -abs(dy)
	movge r8, #1				; r8 = sy = 1
	movlt r8, #-1				; r8 = sy = -1

	add r9, r5, r6				; r9 = dx + dy = err

.1:
	; Only really need to update the span extents when changing to a new line
	ldr r10, [r11, r1, lsl #2]	; span_buffer_start[y]
	cmp r0, r10					; x < span_buffer_start[y]?
	strlt r0, [r11, r1, lsl #2]	; span_buffer_start[y] = x

	ldr r10, [r12, r1, lsl #2]	; span_buffer_end[y]
	cmp r0, r10					; x > span_buffer_start[y]?
	strgt r0, [r12, r1, lsl #2]	; span_buffer_end[y] = x

	cmp r0, r2					; x0 == x1?
	cmpeq r1, r3				; y0 == y1?
	moveq pc, lr				; rts

	mov r10, r9, lsl #1			; r10 = err * 2
	cmp r10, r6					; e2 >= dy?
	addge r9, r9, r6			; err += dy
	addge r0, r0, r7			; x0 += sx

	cmp r10, r5					; e2 <= dx?
	addle r9, r9, r5			; err += dx
	addle r1, r1, r8			; y0 += sy

	b .1
.else
; passed in R0=startx, R1=starty, R2=endx, R3=endy
; used R5=dx, R6=dy, R7=sx, R8=sy, R9=err, R10=e2/addr/temp
; preserve: R11=span_buffer_start, R12=span_buffer_end, R4=poly ptr
drawline_into_span_buffer:
	ldr r5, span_buffer_min_y
	cmp r1, r5					; starty < min_y?
	strlt r1, span_buffer_min_y	; min_y = starty

	ldr r5, span_buffer_max_y
	cmp r1, r5					; starty > max_y?
	strgt r1, span_buffer_max_y	; max_y = starty

	sub r5, r2, r0				; r5 = dx = endx - startx
	subs r6, r3, r1				; r6 = dy = endy - starty
	; need to deal with sign of dy somehow?
	rsblt r6, r6, #0			; r6 = abs(dy)
	; table is inclusive [1-256]

	adr r10, division_table
	ldr r7, [r10, r6, lsl #2]	; r7 = division_table[dy * 4]
	mul r8, r5, r7				; r8 = dx * 1/dy => result << 16

	; foreach y step by r8
	mov r0, r0, lsl #16
.1:
	mov r2, r0, lsr #16

	; plot_pixel HERE!
	ldr r10, [r11, r1, lsl #2]	; span_buffer_start[y]
	cmp r2, r10					; x < span_buffer_start[y]?
	strlt r2, [r11, r1, lsl #2]	; span_buffer_start[y] = x

	ldr r10, [r12, r1, lsl #2]	; span_buffer_end[y]
	cmp r0, r10					; x > span_buffer_start[y]?
	strgt r2, [r12, r1, lsl #2]	; span_buffer_end[y] = x

	; check for end y
	cmp r1, r3
	moveq pc, lr

	; increment by dx/dy
	add r0, r0, r8
	add r1, r1, #1
	b .1

division_table:
.set divisor, 1
.rept 256
	.long 65536 / divisor
	.set divisor, divisor + 1
.endr
.endif

; R0=num verts, R1=buffer of vertices (x,y) as words, R4=colour
plot_polygon_span:
	str lr, [sp, #-4]!			; push lr on stack
	str r4, plot_polygon_colour
	mov r4, r1					; TODO

	; Set up pointers to span buffers for line draw
	adr r11, span_buffer_start
	adr r12, span_buffer_end

	; Store first vertex for reuse as last vertex
	ldmia r4, {r2-r3}
	str r2, plot_polygon_x0
	str r3, plot_polygon_y0

	sub r0, r0, #1
.1:
	str r0, plot_polygon_num_verts

	ldmia r4, {r0-r3}			; load 4 registers x0, y0, x1, y1 but don't update ptr
	add r4, r4, #8				; update pointer to x1

	bl drawline_into_span_buffer

	ldr r0, plot_polygon_num_verts
	subs r0, r0, #1
	bne .1

	; Double up the first/last vertex for plotting
	ldmia r4, {r0-r1}			; load 4 registers x0, y0
	ldr r2, plot_polygon_x0
	ldr r3, plot_polygon_y0

	bl drawline_into_span_buffer

	; Set up our span buffer pointers
	ldr r2, span_buffer_min_y	; r2 = span_buffer_min_y
	ldr r7, span_buffer_max_y	; r7 = span_buffer_max_y

	; r11=span_buffer_start, r12=span_buffer_end
	add r7, r11, r7, lsl #2		; r7 = &span_buffer_start[span_buffer_max_y]
	add r9, r11, r2, lsl #2		; r9 = &span_buffer_start[span_buffer_min_y]
	add r8, r12, r2, lsl #2		; r8 = &span_buffer_end[span_buffer_min_y]

	; Set up our screen buffer pointer
	ldr r12, screen_addr		; R12=generic screen_addr ptr
	add r12, r12, r2, lsl #7	; r10 = screen_addr + starty * 128
	add r12, r12, r2, lsl #5	; r10 += starty * 32 = starty * 160

	; Turn our polygon colour value into 4x words
	ldr r4, plot_polygon_colour
	orr r4, r4, r4, lsl #4		; r4 = colour | colour << 4
	orr r4, r4, r4, lsl #8		; r4 = 2 bytes
	orr r4, r4, r4, lsl #16		; r4 = 4 bytes

	mov r1, r4
	mov r2, r4
	mov r6, r4

.span_loop:
	ldr r0, [r9]				; r0 = span_buffer_start[y]
	ldr r3, [r8]				; r1 = span_buffer_end[y]

	; reserved r15, r14, r13
	; preserve r7, r8, r9, r12
	; passed in r0, r3, r4
	; free r1, r2, r5, r6, r10, r11
	bl plot_span
	add r12, r12, #Screen_Stride	; move ptr to start address of next line

	; reset the span buffer
	mov r5, #256
	str r5, [r9], #4			; span_buffer_start[y] = 256
	mov r11, #0
	str r11, [r8], #4			; span_buffer_end[y] = 0

	cmp r9, r7					; y <= max_y?
	ble .span_loop

	; reset the span limits
	str r5, span_buffer_min_y
	str r11, span_buffer_max_y

	ldr pc, [sp], #4			; rts

; R0=num verts, R1=buffer of vertices (x,y) as words, R4=colour, R12=screen_addr
plot_polygon_line:

	str lr, [sp, #-4]!			; push lr on stack
	str r1, plot_polygon_ptr

	ldr r12, screen_addr

	ldmia r1, {r2-r3}
	str r2, plot_polygon_x0
	str r3, plot_polygon_y0

	sub r0, r0, #1
.1:
	str r0, plot_polygon_num_verts

	ldr r10, plot_polygon_ptr
	ldmia r10, {r0-r3}			; load 4 registers x0, y0, x1, y1 but don't update ptr
	add r10, r10, #8			; update pointer to x1
	str r10, plot_polygon_ptr

	bl drawline

	ldr r0, plot_polygon_num_verts
	subs r0, r0, #1
	bne .1

	; Double up the first/last vertex for plotting
	ldr r10, plot_polygon_ptr
	ldmia r10, {r0-r1}			; load 4 registers x0, y0
	ldr r2, plot_polygon_x0
	ldr r3, plot_polygon_y0

	bl drawline

	ldr pc, [sp], #4			; rts

plot_polygon_ptr:
	.long 0
plot_polygon_num_verts:
	.long 0
plot_polygon_x0:
	.long 0
plot_polygon_y0:
	.long 0
plot_polygon_colour:
	.long 0

; R0=startx, R1=starty, R2=endx, R3=endy, R4=colour, R12=screen_addr
drawline:

	str lr, [sp, #-4]!			; push lr on stack

	subs r5, r2, r0				; r5 = dx = endx - startx
	rsblt r5, r5, #0			; r5 = abs(dx)

	cmp r0,r2					; startx < endx?
	movlt r7, #1				; r7 = sx = 1
	movge r7, #-1				; r7 = sx = -1

	subs r6, r3, r1				; r6 = dy = endy - starty
	rsblt r6, r6, #0			; r6 = abs(dy)
	rsb r6, r6, #0				; r6 = -abs(dy)

	cmp r1, r3					; starty < endy?
	movlt r8, #1				; r8 = sy = 1
	movge r8, #-1				; r8 = sy = -1

	add r9, r5, r6				; r9 = dx + dy = err

.1:

	cmp r0, r2					; x0 == x1?
	cmpeq r1, r3				; y0 == y1?
	ldreq pc, [sp], #4			; rts

	; there will be faster line plot algorithms by keeping track of
	; screen pointer then flushing a byte or word when moving to next row
	bl plot_pixel

	mov r10, r9, lsl #1			; r10 = err * 2
	cmp r10, r6					; e2 >= dy?
	addge r9, r9, r6			; err += dy
	addge r0, r0, r7			; x0 += sx

	cmp r10, r5					; e2 <= dx?
	addle r9, r9, r5			; err += dx
	addle r1, r1, r8			; y0 += sy

	b .1

; R0=x, R1=y, R4=colour, R12=screen_addr, trashes r10, r11
plot_pixel:
	; ptr = screen_addr + starty * screen_stride + startx DIV 2
	add r10, r12, r1, lsl #7	; r10 = screen_addr + starty * 128
	add r10, r10, r1, lsl #5	; r10 += starty * 32 = starty * 160
	add r10, r10, r0, lsr #1	; r10 += startx DIV 2

	ldrb r11, [r10]				; load screen byte

	tst r0, #1					; odd or even pixel?
	andeq r11, r11, #0xF0		; mask out left hand pixel
	orreq r11, r11, r4			; mask in colour as left hand pixel

	andne r11, r11, #0x0F		; mask out right hand pixel
	orrne r11, r11, r4, lsl #4	; mask in colour as right hand pixel

	strb r11, [r10]				; store screen byte
	mov pc, lr

; One pixel:
short_pixel_1:
; Offset                      0                       1                       2                       3
.long    0x0000000F, 0x00000000, 0x000000F0, 0x00000000, 0x00000F00, 0x00000000, 0x0000F000, 0x00000000
;                             4                       5                       6                       7
.long	 0x000F0000, 0x00000000, 0x00F00000, 0x00000000, 0x0F000000, 0x00000000, 0xF0000000, 0x00000000

short_pixel_2:
; Offset                      0                       1                       2                       3
.long    0x000000FF, 0x00000000, 0x00000FF0, 0x00000000, 0x0000FF00, 0x00000000, 0x000FF000, 0x00000000
;                             4                       5                       6                       7
.long	 0x00FF0000, 0x00000000, 0x0FF00000, 0x00000000, 0xFF000000, 0x00000000, 0xF0000000, 0x0000000F

short_pixel_3:
; Offset                      0                       1                       2                       3
.long    0x00000FFF, 0x00000000, 0x0000FFF0, 0x00000000, 0x000FFF00, 0x00000000, 0x00FFF000, 0x00000000
;                             4                       5                       6                       7
.long	 0x0FFF0000, 0x00000000, 0xFFF00000, 0x00000000, 0xFF000000, 0x0000000F, 0xF0000000, 0x000000FF

short_pixel_4:
; Offset                      0                       1                       2                       3
.long    0x0000FFFF, 0x00000000, 0x000FFFF0, 0x00000000, 0x00FFFF00, 0x00000000, 0x0FFFF000, 0x00000000
;                             4                       5                       6                       7
.long	 0xFFFF0000, 0x00000000, 0xFFF00000, 0x0000000F, 0xFF000000, 0x000000FF, 0xF0000000, 0x00000FFF

short_pixel_5:
; Offset                      0                       1                       2                       3
.long    0x000FFFFF, 0x00000000, 0x00FFFFF0, 0x00000000, 0x0FFFFF00, 0x00000000, 0xFFFFF000, 0x00000000
;                             4                       5                       6                       7
.long	 0xFFFF0000, 0x0000000F, 0xFFF00000, 0x000000FF, 0xFF000000, 0x00000FFF, 0xF0000000, 0x0000FFFF

short_pixel_6:
; Offset                      0                       1                       2                       3
.long    0x00FFFFFF, 0x00000000, 0x0FFFFFF0, 0x00000000, 0xFFFFFF00, 0x00000000, 0xFFFFF000, 0x0000000F
;                             4                       5                       6                       7
.long	 0xFFFF0000, 0x000000FF, 0xFFF00000, 0x00000FFF, 0xFF000000, 0x0000FFFF, 0xF0000000, 0x000FFFFF

short_pixel_7:
; Offset                      0                       1                       2                       3
.long    0x0FFFFFFF, 0x00000000, 0xFFFFFFF0, 0x00000000, 0xFFFFFF00, 0x0000000F, 0xFFFFF000, 0x000000FF
;                             4                       5                       6                       7
.long	 0xFFFF0000, 0x00000FFF, 0xFFF00000, 0x0000FFFF, 0xFF000000, 0x000FFFFF, 0xF0000000, 0x00FFFFFF

short_pixel_8:
; Offset                      0                       1                       2                       3
.long    0xFFFFFFFF, 0x00000000, 0xFFFFFFF0, 0x0000000F, 0xFFFFFF00, 0x000000FF, 0xFFFFF000, 0x00000FFF
;                             4                       5                       6                       7
.long	 0xFFFF0000, 0x0000FFFF, 0xFFF00000, 0x000FFFFF, 0xFF000000, 0x00FFFFFF, 0xF0000000, 0x0FFFFFFF

short_pixel_9:
; Offset                      0                       1                       2                       3
.long    0xFFFFFFFF, 0x0000000F, 0xFFFFFFF0, 0x000000FF, 0xFFFFFF00, 0x00000FFF, 0xFFFFF000, 0x0000FFFF
;                             4                       5                       6                       7
.long	 0xFFFF0000, 0x000FFFFF, 0xFFF00000, 0x00FFFFFF, 0xFF000000, 0x0FFFFFFF, 0xF0000000, 0xFFFFFFFF

start_word_pixel_masks:
; Offset 0 display last N pixels of word
.long 	 0xF0000000, 0xFF000000, 0xFFF00000, 0xFFFF0000, 0xFFFFF000, 0xFFFFFF00, 0xFFFFFFF0

; In two words we can plot up to 9 pixels w/ shift of 7 pixels
; R0=xstart, R3=width, R4=colour, R12=screen line address
; R5=temp, R11=temp, R10=writeptr
; preserves r7, r8, r9
; colour r1, r2, r4, r6
plot_short_span:

	bic r10, r10, #3			; nearest word
	and r0, r0, #7				; r0=pixel offset [0-7]

	; find table
	adr r5, short_pixel_1 - 64
	add r5, r5, r3, lsl #6		; r5 = short_pixel_1 + width * 16 * 4
	add r5, r5, r0, lsl #3		; r5 = short_pixel_W + pixel_offset * 8

	; read mask word 0
	ldr r11, [r5], #4			; r11 = *short_pixel_W_offset++
	ldr r3, [r10]				; r3 = screen word
	bic r3, r3, r11				; mask screen word
	and r0, r4, r11				; mask colour word
	orr r3, r3, r0				; mask together
	str r3, [r10], #4			; store back to screen

	; read mask word 1
	ldr r11, [r5], #4			; r11 = *short_pixel_W_offset++
	cmp r11, #0					; early out for blank mask
	moveq pc, lr
	ldr r3, [r10]				; r3 = screen word
	bic r3, r3, r11				; mask screen word
	and r0, r4, r11				; mask colour word
	orr r3, r3, r0				; mask together
	str r3, [r10], #4			; store back to screen

	mov pc, lr

poly_buffer:
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

span_buffer_min_y:
	.long 0
span_buffer_max_y:
	.long 0

module_filename:
	.byte "checknobankh"
	.byte 0

.p2align 2
data_filename:
	.byte "scene1_8k/exo"
	.byte 0

; colour stored in r1, r2, r4, r6
.macro plot_span_X num_pixels

plot_span_\num_pixels:

	.set fours, (\num_pixels / 4)
	.rept fours
	stmia r10!, {r1, r2, r4, r6}		; 4x words
	.endr

	.set words, (\num_pixels - fours * 4)
	.if words == 1
	str r4, [r10], #4					; 1x word
	.endif

	.if words == 2
	stmia r10!, {r1, r4}				; 2x words
	.endif

	.if words == 3
	stmia r10!, {r1, r2, r4}			; 3x words
	.endif

	b return_here_from_jump
.endm

.if _UNROLL_SPAN

.irp my_width, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
.p2align 4
plot_span_X \my_width
.endr
.irp my_width, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
.p2align 4
plot_span_X \my_width
.endr
.irp my_width, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
.p2align 4
plot_span_X \my_width
.endr
.irp my_width, 31, 32
.p2align 4
plot_span_X \my_width
.endr

;.irp my_width, 33, 34, 35, 36, 37, 38, 39, 40
;plot_span_X \my_width
;.endr

b return_here_from_jump	; in case this gets called with 0
; This is relocatable but could be changed to .long plot_span_\my_width
span_jump_table:
	.irp my_width, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
	b plot_span_\my_width
	.endr
	.irp my_width, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
	b plot_span_\my_width
	.endr
	.irp my_width, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
	b plot_span_\my_width
	.endr
	.irp my_width, 31, 32
	b plot_span_\my_width
	.endr
;	.irp my_width, 33, 34, 35, 36, 37, 38, 39, 40
;	b plot_span_\my_width
;	.endr
.endif

.p2align 8
span_buffer_start:
	.skip 1024, 0

.p2align 8
span_buffer_end:
	.skip 1024,0 

.p2align 8
vertex_buffer:
	.skip 256*2,0

.p2align 8
exo_window:
    .skip WINDOW_LENGTH
exo_window_end:

.include "exodecrunch.asm"

.if _ENABLE_MUSIC
.p2align 8
module_data:
.incbin "data/checknobankh.mod"
.endif

.p2align 8
scene1_data_stream:
;.incbin "data/scene2.exo"
