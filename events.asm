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

do_nothing:
    mov pc, lr

show_pause:
    mov r0, #0
    str r0, update_fn_id
    mov pc, lr

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

do_event 1,  0,  parser_set_frame, 1799
do_event 1,  0,  parser_set_speed, -1
do_event 1,  0,  show_parser, 1             ; STNICCC forwards

do_event 1,  0x30, parser_set_speed, 1      ; forwards
do_event 1,  0x34, parser_set_speed, -1     ; backwards
do_event 1,  0x36, parser_set_speed, 1      ; forwards
do_event 1,  0x38, parser_set_speed, -2     ; backwards

do_event 2,  0,  show_image, 1              ; slide 2 'recognise this?'
do_event 2,  0x20, show_parser, 1           ; STNICCC

do_event 3,  0,  show_image, 2              ; slide 3 'yeah?'
do_event 3,  0x20, show_parser, 1           ; STNICCC
do_event 3,  0x2e, parser_set_speed, -1     ; backwards
do_event 3,  0x30, parser_set_speed, 1      ; forwards
do_event 3,  0x32, parser_set_speed, -1     ; backwards
do_event 3,  0x34, parser_set_speed, 1      ; forwards
do_event 3,  0x38, parser_set_speed, -1     ; backwards 

do_event 4,  0,  show_image, 3              ; slide 4 'it's called'
do_event 4,  0x20, show_parser, 1           ; STNICCC

do_event 5,  0,  show_image, 4              ; slide 5 'compo filler'
;do_event 5,  0x20, parser_set_filter, 1     ; b&w
;do_event 5,  0x20, parser_set_speed, -1     ; backwards
do_event 5,  0x20,  parser_set_frame, 1350
do_event 5,  0x20, show_parser, 1           ; STNICCC

do_event 6,  0,  show_image, 5              ; slide 6 'Remain calm'
;do_event 6,  32, parser_set_filter, 0       ; colour
;do_event 6,  32, parser_set_speed, 1        ; forwards
do_event 6,  32, show_parser, 1             ; STNICCC

do_event 7,  0,  show_image, 6              ; slide 7 'This demo is short'
;do_event 7,  32, parser_set_filter, 0       ; colour
;do_event 7,  32, parser_set_speed, 1        ; forwards
do_event 7,  32, show_parser, 1             ; STNICCC

do_event 8,  0,  show_image, 7              ; slide 7 'The End'
do_event 8,  0x18,  show_image, 8           ; slide 8 'Only Joking' 

do_event 9,  0, show_parser, 1              ; STNICCC
;do_event 9,  32, parser_set_filter, 1       ; b&w
;do_event 9,  32, parser_set_speed, -3       ; back
;do_event 9,  32, show_parser, 1             ; STNICCC

do_event 9,  0x8,  parser_set_filter, 2       ; flash white
do_event 9,  0x18, parser_set_filter, 2       ; flash white
do_event 9,  0x28, parser_set_filter, 2       ; flash white
do_event 9,  0x38, parser_set_filter, 2       ; flash white

do_event 10,  0x8,  parser_set_filter, 2       ; flash white
do_event 10,  0x18, parser_set_filter, 2       ; flash white
do_event 10,  0x28, parser_set_filter, 2       ; flash white
do_event 10,  0x38, parser_set_filter, 2       ; flash white

do_event 11, 0,  show_image, 20             ; bitshifters logo
do_event 11, 32, parser_set_filter, 0       ; colour
do_event 11, 32, parser_set_speed, 1        ; forwards
do_event 11, 32, show_parser, 1             ; STNICCC

do_event 13, 0,  show_image, 21             ; patarty
do_event 13, 32, parser_set_filter, 0       ; colour
do_event 13, 32, parser_set_speed, 4        ; forwards
do_event 13, 32, show_parser, 1             ; STNICCC

do_event 14, 0,  show_image, 22             ; gangster
do_event 14, 32, parser_set_filter, 0       ; colour
do_event 14, 32, parser_set_speed, 1        ; forwards
do_event 14, 32, show_parser, 1             ; STNICCC

do_event 16, 0,  show_image, 23             ; Credits
do_event 16, 32,  show_parser, 1             ; STNICCC

; When Tracker module loops around we'll get back to pattern 0
do_event 0,  0,  exit, 0                    ; end

; ============================================================================
; IF YOU GET THE FOLLOWING ERROR MESSAGE WHEN BUILDING:
; fatal error 8: cannot resolve section <seg8000>, maximum number of passes reached
; Just keep adding 4 to the number below until it works! (Stupid assembler bug.)
; ============================================================================

.skip 4