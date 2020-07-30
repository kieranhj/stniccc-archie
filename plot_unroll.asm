
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
