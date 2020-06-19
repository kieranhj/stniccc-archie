; ============================================================================
; Palette Utils
; ============================================================================

; R3 = index
; R4 = RGBx word
; Uses R0,R1 
palette_set_colour:
    adrl r1, palette_osword_block
    strb r3, [r1, #0]       ; logical colour
    mov r0, #16
    strb r0, [r1, #1]       ; physical colour
    and r0, r4, #0xff
    strb r0, [r1, #2]       ; red
    mov r0, r4, lsr #8
    strb r0, [r1, #3]       ; green
    mov r0, r4, lsr #16
    strb r0, [r1, #4]       ; blue
    mov r0, #12
    swi OS_Word
    mov pc,lr

; R2 = palette block ptr
palette_set_block:
	str lr, [sp, #-4]!			; push lr on stack
    mov r3, #0
    .1:
    ldr r4, [r2], #4            ; rgbx
    bl palette_set_colour
    add r3, r3, #1
    cmp r3, #16
    blt .1
	ldr pc, [sp], #4			; rts

; R2 = palette block ptr
palette_make_greyscale:
	str lr, [sp, #-4]!			; push lr on stack
    adr r1, palette_interp_block

    mov r3, #16
    .1:
    ldr r4, [r2], #4            ; rgbx

    mov r5, r4, lsr #2          ; red * 0.25
    and r5, r5, #0xff
    mov r6, r4, lsr #9          ; green * 0.5
    and r6, r6, #0xff
    mov r7, r4, lsr #18         ; blue * 0.25
    and r7, r7, #0xff
    
    add r0, r5, r6
    add r0, r0, r7
    orr r0, r0, r0, lsl #8
    orr r0, r0, r0, lsl #8

    str r0, [r1], #4
    
    subs r3, r3, #1
    bne .1
	ldr pc, [sp], #4			; rts

palette_osword_block:
    .skip 8
    ; logical colour
    ; physical colour (16)
    ; red
    ; green
    ; blue
    ; (pad)

palette_interp_block:
    .skip 64
