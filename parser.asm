; ============================================================================
; STNICCC Parser
; ============================================================================

; R0 = clear screen flag
show_parser:
	str lr, [sp, #-4]!

.if _NO_WINDOW_CLS
.else
    cmp r0, #0
    beq .1

	; Wipe current (visible) screen 
	ldr r8, screen_addr
	bl screen_cls
    .1:
.endif

    mov r0, #1                      ; parser_update
    str r0, update_fn_id
	ldr pc, [sp], #4                ; rts

.if 1
parser_sync:
	str lr, [sp, #-4]!

    ; Swap to next screen buffer
	bl get_next_screen_for_writing

	mov r0, #0		; stniccc_frame track
	bl rocket_sync_get_val_hi
	mov r0, r1

	adrl r4, scene1_colours_index
	ldrb r5, [r4, r0]				; colour index for this frame
	adrl r3, scene1_colours_array
	add r1, r3, r5, lsl #6			; each block is 16 * 4 bytes =64
	str r1, palette_block_addr

	adrl r2, scene1_data_index
	ldr r3, [r2, r0, lsl #2]		; offset for this frame number

	adrl r1, scene1_data_stream
	add r11, r3, r1					; pointer to data for frame

	; PALETTE STUFF HERE!
	mov r0, #2		; grey_scale track
	bl rocket_sync_get_val_hi
	cmp r1, #0
	beq .1
	; b&w
	ldr r2, palette_block_addr
   	bl palette_make_greyscale
	adr r2, palette_interp_block
	str r2, palette_block_addr
	.1:

	mov r0, #3		; fade_to_white track
	bl rocket_sync_get_val_lo
	cmp r1, #0
	beq .2
	; white out
    ldr r2, palette_block_addr
	mov r0, r1, lsr #12
   	bl palette_make_fade_to_white
	adr r2, palette_interp_block
	str r2, palette_block_addr
	.2:

    ; r11 contains pointer to STNICCC frame data
	bl parse_frame

	; Display whichever bank we've just written to
    bl show_screen_at_vsync

    ; return
	ldr pc, [sp], #4

.else
parser_set_speed:
    str r0, forwards_speed
    mov pc, lr

parser_set_frame:
    str r0, frame_number
    mov pc, lr

parser_set_filter:
    str r0, palette_filter
	mov r0, #15
	str r0, fade_value
	mov r0, #1
	str r0, fade_speed
    mov pc, lr

parser_update:
	str lr, [sp, #-4]!

    ; Swap to next screen buffer
	bl get_next_screen_for_writing

	; Do the STNICCC stuff here!
	ldr r0, frame_number

	adrl r4, scene1_colours_index
	ldrb r5, [r4, r0]				; colour index for this frame
	adrl r3, scene1_colours_array
	add r1, r3, r5, lsl #6			; each block is 16 * 4 bytes =64
	str r1, palette_block_addr

	adrl r2, scene1_data_index
	ldr r3, [r2, r0, lsl #2]		; offset for this frame number

	adrl r1, scene1_data_stream
	add r11, r3, r1					; pointer to data for frame

    ; Do palette stuff here!
    ldr r0, palette_filter
    cmp r0, #0
    beq .1
	cmp r0, #2
	beq .2
	cmp r0, #3
	beq .3
	; b&w
    ldr r2, palette_block_addr
   	bl palette_make_greyscale
	adr r2, palette_interp_block
	str r2, palette_block_addr
	b .1
	.2:
	; white out
    ldr r2, palette_block_addr
	ldr r0, fade_value
   	bl palette_make_fade_to_white
	adr r2, palette_interp_block
	str r2, palette_block_addr
	
	ldr r1, fade_speed
	add r0, r0, r1
	cmp r0, #16
	moveq r1, #-2
	streq r1, fade_speed
	str r0, fade_value
	cmp r0, #0
	streq r0, palette_filter
	b .1
	.3:
	; white out w/ b&w
    ldr r2, palette_block_addr
   	bl palette_make_greyscale
	adr r2, palette_interp_block
	ldr r0, fade_value
   	bl palette_make_fade_to_white
	adr r2, palette_interp_block
	str r2, palette_block_addr
	
	ldr r1, fade_speed
	add r0, r0, r1
	cmp r0, #16
	moveq r1, #-2
	streq r1, fade_speed
	str r0, fade_value
	cmp r0, #0
	moveq r0, #1
	streq r0, palette_filter
    .1:

    ; r11 contains pointer to STNICCC frame data
	bl parse_frame

    ; Update STNICCC frame
	ldr r1, forwards_speed
	ldr r11, frame_number
	adds r11, r11, r1
    movmi r11, #0                   ; clamp to 0
	ldr r1, max_frames
	cmp r11, r1
    movgt r11, r1                   ; clamp to max frame
	str r11, frame_number

	; Display whichever bank we've just written to
    bl show_screen_at_vsync

    ; return
	ldr pc, [sp], #4
.endif

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
		.if _NO_WINDOW_CLS
		ldr r8, screen_addr
		bl screen_cls
		.else
		bl window_cls
		.endif
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

	; read palette words
	mov r5, #0					; r1 = palette loop counter
.2:
	movs r2, r2, asl #1
	bcc .3
	; just consume the palette data here.
	; get_byte get_byte
	ldrb r3, [r11], #2
.3:
	add r5, r5, #1
	cmp r5, #16
	blt .2

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
	.long 0 ;Sequence_Total_Frames-1

max_frames:
	.long Sequence_Total_Frames-1

forwards_speed:
	.long 1

palette_filter:
    .long 0

polygon_list:
	.long 32, 32
	.long 160, 32
	.long 160, 101
	.long 32, 111
	.long 0, 64
	.long 0, 0
	.long 0, 0
	.long 0, 0
