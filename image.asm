; ============================================================================
; Static images
; ============================================================================

; R1 = pointer to filename string
load_image_to_screen:
    mov r0, #0xff
	ldr r2, screen_addr
    mov r3, #0
    swi OS_File
    mov pc, lr
