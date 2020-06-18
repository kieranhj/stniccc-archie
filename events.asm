.equ TRACK_SPEED, 3

; Or could use swi "QTM_Pos" (&47E46) to read current tracker pos.

.macro do_event pattern, temp, func, data
    .long \temp*TRACK_SPEED+\pattern*64*TRACK_SPEED
    .long \data
    b \func
.endm

events_update:
    ldr r0, vsync_count
    ldr r1, events_ptr

    ; load vsync time for the next event
    ldr r2, [r1], #4
    cmp r2, r0
    bgt do_nothing

    ; call event function
    ldr r0, [r1], #4
    add r2, r1, #4
    str r2, events_ptr

	str lr, [sp, #-4]!			; push lr on stack
    adr lr, .2
    mov pc, r1                  ; call fn.
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

events_data:
do_event 0,  0,  show_image, 0
do_event 1,  0,  parser_set_frame, 1799
do_event 1,  0,  parser_set_speed, -2
do_event 1,  0,  show_parser, 1
do_event 1,  8,  show_pause, 0
do_event 1,  16, show_parser, 0
do_event 1,  24, parser_set_speed, 1
do_event 1,  32, parser_set_speed, -1
do_event 1,  48, parser_set_speed, 1
do_event 1,  56, parser_set_speed, -1

do_event 2,  0,  show_text_block, 0
do_event 3,  0,  show_parser, 1
do_event 3,  16, parser_set_filter, 1
do_event 3,  32, parser_set_filter, 0
do_event 3,  48, parser_set_filter, 1
do_event 4,  0,  parser_set_speed, -2
do_event 5,  0,  show_image, 1
do_event 5,  64, show_parser, 1
do_event 6,  0,  parser_set_speed, -4
do_event 7,  0,  show_text_block, 1
do_event 7,  64, parser_set_filter, 0
do_event 7,  64, show_parser, 1
do_event 8,  0,  parser_set_speed, -6

do_event 256, 0, do_nothing, 0      ; end
