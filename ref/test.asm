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

	adrl r1, test_poly_data
	mov r0, #5
	mov r4, #6
	bl plot_polygon_span

	b exit
.endif
