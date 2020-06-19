; ============================================================================
; Static images
; ============================================================================

; R0 = image no.
show_image:
	str lr, [sp, #-4]!			; push lr on stack
    mov r11, r0

	ldr r8, screen_addr			; current screen

    ; get a fresh screen bank
    bl get_next_screen_for_writing

	ldr r0, update_fn_id
	cmp r0, #0					; hackz0r - assume static image
	bne .1

	; wipe prev screen first
	bl screen_cls

	.1:
	; Load image
	adr r4, images_table
    add r3, r4, r11, lsl #3     ; 8 byte stride
    ldmia r3, {r0, r2}
    add r0, r0, r4
    add r2, r2, r4
    str r2, palette_block_addr

    ; r1 = filename
	bl decompress_to_screen
	bl show_screen_at_vsync

    mov r0, #0                  ; do_nothing
    str r0, update_fn_id
	ldr pc, [sp], #4			; rts

; R0 = pointer to filename string
load_image_to_screen:
    mov r1, r0
    mov r0, #0xff
	ldr r2, screen_addr
    mov r3, #0
    swi OS_File
    mov pc, lr

; R0 = address of compressed data
decompress_to_screen:
    ldr r1, screen_addr
    b unlz4

; R0 = text block no.
show_text_block:
	str lr, [sp, #-4]!			; push lr on stack

    mov r11, r0

    ; get a fresh screen bank
    bl get_next_screen_for_writing

    ; clear screen
	ldr r8, screen_addr
	bl screen_cls

	; Write string
	adr r4, text_blocks_table
    ldr r0, [r4, r11, lsl #2]    ; 4 byte stride
    add r0, r0, r4
	swi OS_WriteO

    ; set text colour
    adr r0, title_pal_block
    str r0, palette_block_addr

	; Show screen
    bl show_screen_at_vsync

    mov r0, #0
    str r0, update_fn_id        ; do_nothing
	ldr pc, [sp], #4			; rts

fade_to_black:
	str r0, fade_speed
	str r0, fade_count

	mov r0, #16
	str r0, fade_value

	ldr r0, palette_block_addr
	str r0, fade_from_pal_block

    mov r0, #2
    str r0, update_fn_id
    mov pc, lr

update_fade_to_black:
	str lr, [sp, #-4]!			; push lr on stack

	ldr r0, fade_value
	ldr r2, fade_from_pal_block
	bl palette_make_fade_to_black
	adr r2, palette_interp_block
	str r2, palette_block_addr
	bl show_screen_at_vsync

	cmp r0, #0
	beq .1

	ldr r1, fade_count
	subs r1, r1, #1
	ldreq r1, fade_speed
	subeq r0, r0, #1
	str r1, fade_count
	str r0, fade_value

	.1:
	ldr pc, [sp], #4			; rts

fade_value:
	.long 0

fade_speed:
	.long 1

fade_count:
	.long 0

fade_from_pal_block:
	.long 0

.if 0
clock_minutes:
	.long 6000

clock_string:
	.byte 31,16,15,17,15,0
	.align 4

show_clock:
	ldr r0, vsync_count
	str r0, vsync_final

	; Write string
	bl get_next_screen_for_writing
	bl window_cls
	; Set default palette
	mov r3, #15
	mov r4, #0x00ffffff
	bl palette_set_colour
	adr r0, clock_string
	swi OS_WriteO

	ldr r6, clock_minutes
	ldr r5, vsync_final
	mov r5, r5, lsl #1			; vsyncs * 2 = 100 ticks per second
	mov r3, #0
	mov r4, #0
.1:
	cmp r5, r6				; minutes
	blt .2
	sub r5, r5, r6
	add r3, r3, #1
	b .1
.2:
	cmp r5, #100				; seconds
	blt .3
	sub r5, r5, #100
	add r4, r4, #1
	b .2
.3:

	mov r0, r3
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertCardinal1
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #58					; ':'
	swi OS_WriteC
	mov r0, r4
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertCardinal1
	adr r0, debug_string
	swi OS_WriteO
	mov r0, #46					; '.'
	swi OS_WriteC
	mov r0, r5
	adr r1, debug_string
	mov r2, #8
	swi OS_ConvertCardinal1
	adr r0, debug_string
	swi OS_WriteO
.endif
