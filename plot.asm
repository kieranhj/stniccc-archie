; ============================================================================
; Plot routines
; Spans, lines, polygons
; ============================================================================

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
	adrl r5, start_word_pixel_masks - 4

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
	adrl r11, span_jump_table - 4
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
	adrl r5, short_pixel_1 - 64

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
	adrl r3, span_buffer_start
	adrl r4, span_buffer_end
.1:
	str r1, [r3, r2, lsl #2]			; span_buffer_start[y] = 256
	str r0, [r4, r2, lsl #2]			; span_buffer_end[y] = 0

	add r2, r2, #1
	cmp r2, #256
	blt .1
	mov pc, lr

span_buffer_min_y:
	.long 0
span_buffer_max_y:
	.long 0

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

; R0=num verts, R1=buffer of vertices (x,y) as words, R4=colour
plot_polygon_span:
	str lr, [sp, #-4]!			; push lr on stack
	str r4, plot_polygon_colour
	mov r4, r1					; TODO

	; Set up pointers to span buffers for line draw
	adrl r11, span_buffer_start
	adrl r12, span_buffer_end

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
	add r12, r12, #Screen_Offset
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
	adrl r5, short_pixel_1 - 64
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
plot_span_X \my_width
.endr
.irp my_width, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20
plot_span_X \my_width
.endr
.irp my_width, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
plot_span_X \my_width
.endr
.irp my_width, 31, 32
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
