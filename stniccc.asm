; TARGET ARM2
; AREA   ASM$$Code,Code,ReadOnly
; ENTRY Start

.equ Screen_Mode, 9
.equ Screen_Width, 320
.equ Screen_Height, 256
.equ Screen_Stride, Screen_Width/2		; 4bpp
.equ Screen_Bytes, Screen_Stride*Screen_Height

.include "swis.h.asm"

.org 0x8000

Start:
    adrl sp, stack_base
	B main

.skip 1024
stack_base:

scr_bank:
	.byte 0
	.align 4

main:
	MOV r0,#22	;Set MODE
	SWI OS_WriteC
	MOV r0,#Screen_Mode
	SWI OS_WriteC

	; Set screen size to 160k (double buffer)
	MOV r0, #DynArea_Screen
	SWI OS_ReadDynamicArea
	MOV r0, #DynArea_Screen
	MOV r2, #2*Screen_Bytes
	SUBS r1, r2, r1
	SWI OS_ChangeDynamicArea
	MOV r0, #DynArea_Screen
	SWI OS_ReadDynamicArea
	CMP r1, #2*Screen_Bytes
	ADRCC r0, error_noscreenmem
	SWICC OS_GenerateError

	MOV r0,#22	;Set MODE
	SWI OS_WriteC
	MOV r0,#128+Screen_Mode
	SWI OS_WriteC
	
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

	adrl r0, scr_bank
	MOV r1, #1
	STRB r1, [r0]

	; Claim the Event vector
	mov r0, #EventV
	adr r1, event_handler
	mov r2, #0
	swi OS_Claim

	bl initialise_span_buffer

	; Enable Vsync event
	mov r0, #14
	mov r1, #Event_VSync
	SWI OS_Byte

loop:   
	; debug
	bl debug_write_vsync_count

	;Swap banks
	adrl r0, scr_bank
	LDRB r1, [r0]
	ADD r1, r1, #1
	MOV r0, #OSByte_WriteDisplayBank
	SWI OS_Byte
	adrl r0, scr_bank
	LDRB r1, [r0]
	EOR r1, r1, #1
	STRB r1, [r0]
	ADD r1, r1, #1
	MOV r0, #OSByte_WriteVDUBank
	SWI OS_Byte

	MOV r0, #OSByte_Vsync
	SWI OS_Byte

	BL get_screen_addr
	;Back buffer address stored at screen_addr

	;Do stuff here!

.if 0
	ldr r12, screen_addr			; R12=generic screen_addr ptr

	mov r2, #0
	mov r0, #4
	mov r1, #319
	mov r4, #0x11
	orr r4, r4, r4, lsl #8
	orr r4, r4, r4, lsl #16

my_test_loop:

	bl plot_span

	add r0, r0, #1
	sub r1, r1, #1
	add r2, r2, #1
	cmp r2, #256
	bne my_test_loop

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

	adrl r1, test_poly_data
	mov r0, #5
	mov r4, #6
	bl plot_polygon_span

	b exit
.endif

	bl parse_frame
	cmp r0, #POLY_DESC_END_OF_STREAM
	beq exit

	;Exit if SPACE is pressed
	MOV r0, #OSByte_ReadKey
	MOV r1, #IKey_Space
	MOV r2, #0xff
	SWI OS_Byte
	
	CMP r1, #0xff
	CMPEQ r2, #0xff
	BEQ exit
	
	B loop

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
	STR lr, [sp, #-4]!
	adrl r0, screen_addr_input
	adrl r1, screen_addr
	SWI OS_ReadVduVariables
	LDR pc, [sp], #4
	
screen_addr_input:
	.long VD_ScreenStart, -1
screen_addr:
	.long 0

exit:	
	adrl r1, scr_bank
	LDRB r1, [r1]
	ADD r1, r1, #1
	MOV r0, #OSByte_WriteDisplayBank
	SWI OS_Byte

	; disable vsync event
	mov r0, #13
	mov r1, #Event_VSync
	swi OS_Byte

	; release our event handler
	mov r0, #EventV
	adr r1, event_handler
	mov r2, #0
	swi OS_Release

	bl debug_write_vsync_count

	SWI OS_Exit

; R12=screen_addr, trashes r7, r8, r9
screen_cls:
	ldr r8, screen_addr
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
	mov r0,r0				; WTF?
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

.equ FLAG_CLEAR_SCREEN, 0x01
.equ FLAG_CONTAINS_PALETTE, 0x02
.equ FLAG_INDEXED_DATA, 0x04

.equ POLY_DESC_END_OF_STREAM, 0xfd
.equ POLY_DESC_SKIP_TO_64K, 0xfe
.equ POLY_DESC_END_OF_FRAME, 0xff

; R12=screen_addr
parse_frame:
	stmfd sp!, {lr}

	ldr r11, parse_frame_ptr

	; get_byte
	ldrb r10, [r11], #1			; r10=frame_flags

	tst r10, #FLAG_CLEAR_SCREEN
	blne screen_cls

	tst r10, #FLAG_CONTAINS_PALETTE
	beq .1						; no_palette

	; get_byte
	ldrb r1, [r11], #1			; r1=palette_mask HI
	; get_byte
	ldrb r0, [r11], #1			; r0=palette_mask LO
	mov r2, r1, lsl #24
	orr r2, r2, r0, lsl #16		; r2 = r1 << 24 | r0 << 16

	; read palette words
	mov r1, #0					; r1=palette loop counter
.2:
	movs r2, r2, asl #1
	bcc .3

	; get_byte
	ldrb r3, [r11], #1			; r3=xxxxrrrr
	; get_byte
	ldrb r4, [r11], #1			; r4=ggggbbbb

	; VDU 19,logical colour,16,red,green,blue
	mov r0, #19
	SWI OS_WriteC
	mov r0, r1
	SWI OS_WriteC
	mov r0, #16
	SWI OS_WriteC
	mov r0, r3, lsl #5
	and r0, r0, #0xF0
	SWI OS_WriteC
	and r0, r4, #0x70
	mov r0, r0, lsl #1
	SWI OS_WriteC
	mov r0, r4, lsl #5
	and r0, r0, #0xF0
	SWI OS_WriteC

.3:
	add r1, r1, #1
	cmp r1, #16
	blt .2

.1:

	tst r10, #FLAG_INDEXED_DATA
	beq parse_frame_read_poly_data

	; get_byte
	ldrb r1, [r11], #1			; r0=num_verts

	adrl r8, vertices_x			; r8=vertices_x
	adrl r9, vertices_y			; r9=vertices_y

	; next is an array of (x,y) bytes

	mov r0, #0
.4:
	; get_byte
	ldrb r2, [r11], #1			; r2=x
	; get_byte
	ldrb r3, [r11], #1			; r3=y

	str r2, [r8, r0, lsl #2]	; vertices_x[i] = x
	str r3, [r9, r0, lsl #2]	; vertices_y[i] = y

	add r0, r0, #1
	cmp r0, r1
	blt .4

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

	adrl r6, test_poly_data		; r6=test_poly_data array

	; is the data indexed?
	tst r10, #FLAG_INDEXED_DATA
	beq non_indexed_data

	; indexed
	mov r0, #0
.5:
	; read index
	; get_byte
	ldrb r5, [r11], #1			; r5=index

	; lookup verts
	ldr r2, [r8, r5, lsl #2]	; r2=vertices_x[i]
	ldr r3, [r9, r5, lsl #2]	; r3=vertices_y[i]

	; store into a temp array for now
	str r2, [r6], #4			; *temp_poly_data++ = x
	str r3, [r6], #4			; *temp_poly_data++ = y

	add r0, r0, #1				; n++
	cmp r0, r1					; n == num_verts?
	blt .5
	b parse_plot_poly

non_indexed_data:

	; non-indexed
	mov r0, #0
.6:
	; copy (x,y) bytes directly to temp array
	; get_byte
	ldrb r2, [r11], #1			; r2=x
	; get_byte
	ldrb r3, [r11], #1			; r3=y

	; store into a temp array for now
	str r2, [r6], #4			; *temp_poly_data++ = x
	str r3, [r6], #4			; *temp_poly_data++ = y

	add r0, r0, #1
	cmp r0, r1
	blt .6

parse_plot_poly:

	; store off any registers we need here!
	stmfd sp!, {r8-r11}

	; plot the polygon!
	mov r0, r1
	adrl r1, test_poly_data
	; r4=palette
	bl plot_polygon_span
;	bl plot_polygon_line
	
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
	str r11, parse_frame_ptr

	ldmfd sp!, {lr}
	mov pc, lr

parse_frame_ptr:
	.long scene1_data_stream

; R0=x_start, R1=x_end, R2=y, r4=colour, R12=screen_addr, trashes r4, r5, r6, r10, r11
plot_span:
	cmp r1, r0
	movlt r6, r1				; r6 = x_start
	sublt r3, r0, r1			; r4 = x_start - x_end = x_width when x_start > x_end
	movge r6, r0				; r6 = x_start
	subge r3, r1, r0			; r3 = x_end - x_start = x_width

	add r3, r3, #1				; always plot at least one pixel

	; ptr = screen_addr + y * screen_stride + x_start DIV 2
	add r10, r12, r2, lsl #7	; r10 = screen_addr + starty * 128
	add r10, r10, r2, lsl #5	; r10 += starty * 32 = starty * 160
	add r10, r10, r6, lsr #1	; r10 += startx DIV 2

	; do all this in bytes for now - this needs to be made 32-bit words for speed!

	; handle first odd pixel
	ands r5, r6, #1				; is x_start odd?
	beq .1

	ldrb r5, [r10]				; load screen byte
	and r5, r5, #0x0F			; mask out right hand pixel
	and r11, r4, #0xF0
	orr r5, r5, r11				; mask in colour
	strb r5, [r10], #1			; store screen byte
	subs r3, r3, #1				; decrement width by 1
	moveq pc, lr				; exit if x_width == 0

.1:

	; now we need to align r10 to next word
	ands r5, r10, #3			; r5=number of bytes
	beq .3

	; plot one byte (2 pixels) at a time until we reach word alignment...
	rsb r5, r5, #4				; count = 4 - bytes
.2:
	cmp r3, #2					; continue until x_width < 2
	blt .3

	strb r4, [r10], #1			; write two pixels (one byte) to screen, post index
	subs r3, r3, #2				; decrement x_width by 2
	moveq pc, lr				; exit if x_width == 0

	subs r5, r5, #1				; decement byte count
	bne .2

.3:
	; plot word at a time
	movs r5, r3, lsr #3			; each word = 8 pixels so word count = width/8
	beq .5

	sub r3, r3, r5, lsl #3		; width -= words * 8

	; plot words
.4:
	str r4, [r10], #4			; write 8 pixels (one word) to screen, post index
	subs r5, r5, #1				; decrement word count
	bne .4

	; handle remaining bytes
.5:
	cmp r3, #2					; continue until x_width < 2
	blt .6

	strb r4, [r10], #1			; write two pixels (one byte) to screen, post index
	subs r3, r3, #2				; decrement x_width by 2
	moveq pc, lr				; exit if x_width == 0
	b .5

.6:
	cmp r3, #0
	moveq pc, lr

	; handle final odd pixel
	ldrb r5, [r10]				; load screen byte
	and r5, r5, #0xF0			; mask out left hand pixel
	and r11, r4, #0x0F
	orr r5, r5, r11				; mask in colour << 4
	strb r5, [r10]				; store screen byte

	; return
	mov pc, lr

initialise_span_buffer:
	mov r0, #0
	str r0, span_buffer_max_y
	mov r1, #256
	str r1, span_buffer_min_y

	mov r2, #0
	adrl r3, span_buffer_start
	adrl r4, span_buffer_end
.1:
	str r1, [r3, r2, lsl #2]			; span_buffer_start[y] = 256
	str r0, [r4, r2, lsl #2]			; span_buffer_end[y] = 0

	add r2, r2, #1
	cmp r2, #256
	blt .1
	mov pc, lr

; R0=startx, R1=starty, R2=endx, R3=endy, R4=colour (preserve)
; R5=dx, R6=dy, R7=sx, R8=sy, R9=err, R10=e2/addr/temp
; R11=span_buffer_start, R12=span_buffer_end
drawline_into_span_buffer:

	ldr r5, span_buffer_min_y
	cmp r1, r5					; starty < min_y?
	strlt r1, span_buffer_min_y	; min_y = starty

	ldr r5, span_buffer_max_y
	cmp r1, r5					; starty > max_y?
	strgt r1, span_buffer_max_y	; max_y = starty

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

	adrl r11, span_buffer_start
	adrl r12, span_buffer_end

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

; R0=num verts, R1=buffer of vertices (x,y) as words, R4=colour, R12=screen_addr
plot_polygon_span:
	str lr, [sp, #-4]!			; push lr on stack
	str r1, plot_polygon_ptr

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

	bl drawline_into_span_buffer

	ldr r0, plot_polygon_num_verts
	subs r0, r0, #1
	bne .1

	; Double up the first/last vertex for plotting
	ldr r10, plot_polygon_ptr
	ldmia r10, {r0-r1}			; load 4 registers x0, y0
	ldr r2, plot_polygon_x0
	ldr r3, plot_polygon_y0

	bl drawline_into_span_buffer

	ldr r2, span_buffer_min_y	; r2 = span_buffer_min_y
	ldr r7, span_buffer_max_y	; r7 = span_buffer_max_y
	adrl r9, span_buffer_start
	adrl r8, span_buffer_end

	orr r4, r4, r4, lsl #4		; r4 = colour | colour << 4
	orr r4, r4, r4, lsl #8		; r4 = 2 bytes
	orr r4, r4, r4, lsl #16		; r4 = 4 bytes

	ldr r12, screen_addr		; R12=generic screen_addr ptr

.span_loop:
	ldr r0, [r9, r2, lsl #2]	; r0 = span_buffer_start[y]
	ldr r1, [r8, r2, lsl #2]	; r1 = span_buffer_end[y]

	bl plot_span

	; reset the span buffer
	mov r0, #256
	str r0, [r9, r2, lsl #2]	; span_buffer_start[y] = 256
	mov r1, #0
	str r1, [r8, r2, lsl #2]	; span_buffer_end[y] = 0

	add r2, r2, #1				; y += 1
	cmp r2, r7					; y <= max_y?
	ble .span_loop

	; reset the span limits
	str r0, span_buffer_min_y
	str r1, span_buffer_max_y

	ldr pc, [sp], #4			; rts

; R0=num verts, R1=buffer of vertices (x,y) as words, R4=colour, R12=screen_addr
plot_polygon_line:

	str lr, [sp, #-4]!			; push lr on stack
	str r1, plot_polygon_ptr

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

; R0=x, R1=y, trashes r10, r11
plot_pixel_into_span_buffer:
	adrl r10, span_buffer_start
	ldr r11, [r10, r1, lsl #2]	; span_buffer_start[y]
	cmp r0, r11					; x < span_buffer_start[y]?
	strlt r0, [r10, r1, lsl #2]	; span_buffer_start[y] = x

	adrl r10, span_buffer_end
	ldr r11, [r10, r1, lsl #2]	; span_buffer_end[y]
	cmp r0, r11					; x > span_buffer_start[y]?
	strgt r0, [r10, r1, lsl #2]	; span_buffer_end[y] = x
	mov pc, lr

; R0=event number
event_handler:
	cmp r0, #Event_VSync
	movnes pc, r14

	ldr r12, vsync_count
	add r12, r12, #1
	str r12, vsync_count

	movs pc, r14

vsync_count:
	.long 0

test_poly_data:

	.long 32, 32
	.long 160, 32
	.long 160, 160
	.long 32, 160
	.long 0, 64
	.long 0, 0
	.long 0, 0
	.long 0, 0

span_buffer_min_y:
	.long 0
span_buffer_max_y:
	.long 0

.p2align 8
span_buffer_start:
	.skip 1024, 0

.p2align 8
span_buffer_end:
	.skip 1024,0 

.p2align 8
vertices_x:
	.skip 1024,0

.p2align 8
vertices_y:
	.skip 1024,0

scene1_data_stream:
.incbin "data/scene1.bin"
