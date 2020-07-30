; ============================================================================
; Events system
; ============================================================================

;.equ TRACK_SPEED, 4

.macro do_event pattern, temp, func, data
;    .long \temp*TRACK_SPEED+\pattern*64*TRACK_SPEED
    .long \temp+\pattern*256
    .long \data
    b \func
.endm

.if 1
events_update:
	str lr, [sp, #-4]!			; push lr on stack
  	mov r0, #1				    ; show_image track
	bl rocket_sync_get_val_hi
	ldr r0, last_show_image
	cmp r0, r1
	beq .6
	; display changed
	mov r0, r1
	str r1, last_show_image
    cmp r0, #255
    bne .7
    ; hacky end of demo event
    mov r0, #0
    bl rocket_set_audio_playing ; pause playback
    b .6
    .7:
	cmp r0, #100
	blge show_parser
	bllt show_image
	.6:
    ldr lr, [sp], #4			; pull lr from stack
    mov pc, lr
    
.else

events_update:
    ; read current tracker position
    mov r0, #-1
    mov r1, #-1
    swi QTM_Pos

    ldr r2, events_ptr

    ; load pattern/line for the next event
    ldr r3, [r2], #4
    mov r4, r3, lsr #8          ; pattern
    cmp r0, r4
    bne do_nothing

    and r4, r3, #0xff           ; line
    cmp r1, r4
    blt do_nothing              ; if we miss a line still process this event

    ; call event function
    ldr r0, [r2], #4
    add r3, r2, #4
    str r3, events_ptr

	str lr, [sp, #-4]!			; push lr on stack
    adr lr, .2
    mov pc, r2                  ; call fn.
    .2:
    ldr lr, [sp], #4			; pull lr from stack
    b events_update             ; repeat until all events processed
.endif

do_nothing:
    mov pc, lr

show_pause:
    mov r0, #0
    str r0, update_fn_id
    mov pc, lr

.if 0
events_ptr:
    .long events_data

; ============================================================================
; Events data
;
; Available functions:
;   show_image, data = image no.
;   show_parser = STNICC sequence, clear screen if data = 1 (needed for restarting after image)
;   parser_set_frame, data = STNICCC frame no. [0-1799]
;   parser_set_speed, data = frame step (negative for backwards)
;   parser_set_filter, colour if data = 0, b&w if data = 1 (STNICCC only), white flas = 2 (STNICCC only)
;   show_text_block, data = text block no.
; ============================================================================

events_data:
do_event 0,  0,  show_image, 0              ; slide 1 'back by popular demand'
do_event 0,  0x30, fade_to_black, 3         ; fade to black, speed = 2

do_event 1,  0,  parser_set_frame, 1799
do_event 1,  0,  parser_set_speed, -1
do_event 1,  0,  show_parser, 1             ; STNICCC forwards

do_event 1,  0x30, parser_set_speed, 1      ; forwards
do_event 1,  0x34, parser_set_speed, -1     ; backwards
do_event 1,  0x36, parser_set_speed, 1      ; forwards
do_event 1,  0x38, parser_set_speed, -2     ; backwards

do_event 2,  0,  show_image, 1              ; slide 2 'recognise this?'
do_event 2,  0x20,  parser_set_frame, 1700
do_event 2,  0x20, show_parser, 1           ; STNICCC

do_event 3,  0,  show_image, 2              ; slide 3 'yeah?'
do_event 3,  0x20,  parser_set_frame, 1650
do_event 3,  0x20, show_parser, 1           ; STNICCC
;do_event 3,  0x2e, parser_set_speed, -1     ; backwards
do_event 3,  0x30, parser_set_speed, 2      ; forwards
do_event 3,  0x32, parser_set_speed, -2     ; backwards
do_event 3,  0x34, parser_set_speed, 2      ; forwards
do_event 3,  0x38, parser_set_speed, -2     ; backwards 

do_event 4,  0,  show_image, 3              ; slide 4 'it's called'
do_event 4,  0x20, show_parser, 1           ; STNICCC

do_event 5,  0,  show_image, 4              ; slide 5 'compo filler'
;do_event 5,  0x20, parser_set_filter, 1     ; b&w
do_event 5,  0x20, parser_set_speed, -1     ; backwards
do_event 5,  0x20,  parser_set_frame, 1375
do_event 5,  0x20, show_parser, 1           ; STNICCC

do_event 6,  0,  show_image, 5              ; slide 6 'Remain calm'
;do_event 6,  32, parser_set_filter, 0       ; colour
;do_event 6,  32, parser_set_speed, 1        ; forwards
do_event 6,  32, show_parser, 1             ; STNICCC

do_event 7,  0,  show_image, 6              ; slide 7 'This demo is short'
;do_event 7,  32, parser_set_filter, 0       ; colour
;do_event 7,  32, parser_set_speed, 1        ; forwards
do_event 7,  0x20, show_parser, 1             ; STNICCC
do_event 7,  0x30, parser_set_speed, 2      ; forwards
do_event 7,  0x32, parser_set_speed, -2     ; backwards
do_event 7,  0x34, parser_set_speed, 2      ; forwards
do_event 7,  0x38, parser_set_speed, -2     ; backwards
do_event 7,  0x3a, parser_set_speed, 2      ; forwards
do_event 7,  0x3c, parser_set_speed, -2     ; backwards 

do_event 8,  0,  show_image, 7              ; slide 8 'The End'
do_event 8,  0x18,  show_image, 8           ; slide 9 'Only Joking' 

do_event 9,  0, show_parser, 1              ; STNICCC
;do_event 9,  32, parser_set_filter, 1       ; b&w
;do_event 9,  32, parser_set_speed, -3       ; back
;do_event 9,  32, show_parser, 1             ; STNICCC

do_event 9,  0x8,  parser_set_filter, 2       ; flash white
do_event 9,  0x18, parser_set_filter, 2       ; flash white
do_event 9,  0x28, parser_set_filter, 2       ; flash white
do_event 9,  0x38, parser_set_filter, 2       ; flash white

do_event 0xa,  0,  show_image, 10              ; slide 11 'Well this is fun'
do_event 0xa,  0x20, show_parser, 1              ; STNICCC
do_event 0xa,  0x28, parser_set_filter, 2       ; flash white
do_event 0xa,  0x34, parser_set_filter, 2       ; flash white
do_event 0xa,  0x3a, parser_set_filter, 2       ; flash white

;do_event 0xb, 0,  show_image, 20             ; bitshifters logo
do_event 0xb, 0,  show_image, 11             ; slide 12 'black and white?'
do_event 0xb, 0x20, parser_set_filter, 1       ; b@w
do_event 0xb, 0x20,  parser_set_frame, 300
do_event 0xb, 0x20, parser_set_speed, 2       ; forwards
do_event 0xb, 0x20, show_parser, 1             ; STNICCC
do_event 0xb, 0x28, parser_set_filter, 3       ; flash white to b&w
do_event 0xb, 0x38, parser_set_filter, 3       ; flash white to b&w

do_event 0xc, 0,  show_image, 21             ; patarty
do_event 0xc, 0x20, parser_set_filter, 0       ; colour
do_event 0xc, 0x20, parser_set_speed, 4        ; forwards
do_event 0xc, 0x20, show_parser, 1             ; STNICCC
do_event 0xc, 0x28, parser_set_filter, 2       ; flash white
do_event 0xc, 0x34, parser_set_filter, 2       ; flash white
do_event 0xc, 0x3a, parser_set_filter, 2       ; flash white

do_event 0xd, 0,  show_image, 22             ; pic gangster
do_event 0xd, 0x20, parser_set_filter, 0       ; colour
do_event 0xd, 0x20, parser_set_speed, 4        ; forwards
do_event 0xd, 0x20, show_parser, 1             ; STNICCC
do_event 0xd, 0x28, parser_set_filter, 2       ; flash white
do_event 0xd, 0x38, parser_set_filter, 2       ; flash white

do_event 0xe, 0,  show_image, 9             ; slide 10 'avon'
do_event 0xe, 0x20, parser_set_filter, 0       ; colour
do_event 0xe, 0x20, parser_set_speed, 4        ; forwards
do_event 0xe, 0x20, show_parser, 1             ; STNICCC
do_event 0xe, 0x32, parser_set_filter, 2       ; flash white
;do_event 0xe, 0x35, parser_set_filter, 2       ; flash white
do_event 0xe, 0x38, parser_set_filter, 2       ; flash white
;do_event 0xe, 0x3b, parser_set_filter, 2       ; flash white
do_event 0xe, 0x3e, parser_set_filter, 2       ; flash white

do_event 0xf, 0,  show_image, 12             ; slide 13 'nearly done'

do_event 0x10,  0,  parser_set_frame, 0
do_event 0x10, 0, parser_set_filter, 0       ; colour
do_event 0x10, 0, parser_set_speed, 4        ; forwards
do_event 0x10, 0, show_parser, 1             ; STNICCC
do_event 0x10, 0x8, parser_set_filter, 2       ; flash white
do_event 0x10, 0x18, parser_set_filter, 2       ; flash white
do_event 0x10, 0x20,  show_image, 15             ; slide 16 'greets 1'

do_event 0x11,  0,  parser_set_frame, 450
do_event 0x11, 0, parser_set_filter, 0       ; colour
do_event 0x11, 0, parser_set_speed, 4        ; forwards
do_event 0x11, 0, show_parser, 1             ; STNICCC
do_event 0x11, 0x8, parser_set_filter, 2       ; flash white
do_event 0x11, 0x18, parser_set_filter, 2       ; flash white
do_event 0x11, 0x20,  show_image, 16             ; slide 17 'greets 2'

do_event 0x12,  0,  parser_set_frame, 900
do_event 0x12, 0, parser_set_filter, 0       ; colour
do_event 0x12, 0, parser_set_speed, 4        ; forwards
do_event 0x12, 0, show_parser, 1             ; STNICCC
do_event 0x12, 0x8, parser_set_filter, 2       ; flash white
do_event 0x12, 0x18, parser_set_filter, 2       ; flash white
do_event 0x12, 0x20,  show_image, 17             ; slide 18 'greets 3'

do_event 0x13,  0,  parser_set_frame, 1350
do_event 0x13, 0, parser_set_filter, 0       ; colour
do_event 0x13, 0, parser_set_speed, 4        ; forwards
do_event 0x13, 0, show_parser, 1             ; STNICCC
do_event 0x13, 0x8, parser_set_filter, 2       ; flash white
do_event 0x13, 0x18, parser_set_filter, 2       ; flash white
do_event 0x13, 0x20,  show_image, 23             ; credits

do_event 0x14, 0,  show_image, 13             ; slide 14 'the end again'
do_event 0x14, 0x18, fade_to_black, 3         ; fade to black, speed = 2

do_event 0x15, 0,  show_image, 14            ; slide 15 'no, really'
do_event 0x15, 0x18, fade_to_black, 3         ; fade to black, speed = 2

do_event 0x16, 0,  show_image, 20             ; bitshifters logo'
do_event 0x16, 0x28, fade_to_black, 2         ; fade to black, speed = 2


;do_event 16, 0,  show_image, 23             ; Credits
;do_event 16, 32,  show_parser, 1             ; STNICCC

; When Tracker module loops around we'll get back to pattern 0
do_event 0,  0,  exit, 0                    ; end
.endif
