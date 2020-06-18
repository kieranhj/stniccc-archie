.equ TRACK_SPEED, 3

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

events_ptr:
    .long events_data

.long 1 + 2 * 3

events_data:
do_event 1,  8,  update_set_fn_id, 0
do_event 1,  16, update_set_fn_id, 1
do_event 1,  24, parser_set_speed, -1
do_event 1,  32, parser_set_speed, 1
do_event 1,  48, parser_set_speed, -1
do_event 1,  56, parser_set_speed, 1
do_event 256, 0, do_nothing, 0      ; end
