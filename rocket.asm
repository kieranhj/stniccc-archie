; ============================================================================
; Rocket sync
; ============================================================================

.equ Pattern_Max, 23

.if _SYNC_EDITOR
podule3_base:
    .long 0x300C000

podule3_audio_is_playing:
    .long 0x300FFFC

podule3_vsync_count:
    .long 0x300FFF8

; R0 = track no.
; Returns R1 = 16.16 value
; Trashes R2, R3
rocket_sync_get_val:
    ldr r2, podule3_base
    ldr r1, [r2, r0, lsl #3]        ; r3 = podule3_base[track_no * 8]
    add r2, r2, #4
    ldr r3, [r2, r0, lsl #3]        ; r1 = podule3_base[track_no * 8 + 4]
    orr r1, r1, r3, lsl #16         ; val = r3 << 16 | r1
    mov pc, lr

; R0 = track no.
; Returns R1 = integer part only
; Trashes R2
rocket_sync_get_val_hi:
    ldr r2, podule3_base
    add r2, r2, #4
    ldr r1, [r2, r0, lsl #3]        ; r1 = podule3_base[track_no * 8 + 4]
    mov pc, lr

; R0 = track no.
; Returns R1 = fractional part only
; Trashes R2
rocket_sync_get_val_lo:
    ldr r2, podule3_base
    ldr r1, [r2, r0, lsl #3]        ; r1 = podule3_base[track_no * 8]
    mov pc, lr

rocket_get_audio_is_playing:
    ldr r2, podule3_audio_is_playing
    ldr r0, [r2]
    mov pc, lr

rocket_set_audio_playing:
    ldr r2, podule3_audio_is_playing
    mov r1, r0, lsl #16             ; podule write to upper 16 bits
    str r1, [r2]
    mov pc, lr

rocket_get_vsync_count:
    ldr r2, podule3_vsync_count
    ldr r0, [r2]
    mov pc, lr

rocket_set_vsync_count:
    ldr r2, podule3_vsync_count
    mov r1, r0, lsl #16             ; podule write to upper 16 bits
    str r1, [r2]
    mov pc, lr

; R0 = vsync count
; Returns R0 = pattern, R1 = line
; Trashes R2, R3
rocket_vsync_to_pos:
	mov r1, r0, lsr #2		; row = vsyncs / vpr; fixed speed = 4
;	mov r0, r1, lsr #6		; pattern = row DIV 64
;	and r1, r1, #63			; line = row MOD 64

    adr r2, rocket_pattern_starts_table
    mov r0, #0
    .1:
    ldr r3, [r2, r0, lsl #3]
    cmp r3, r1
    bge .2
    add r0, r0, #1
    b .1
    .2:

    ; R0 = pattern
    subgt r0, r0, #1

    ; clamp to end of song.
    cmp r0, #Pattern_Max
    movge r0, #Pattern_Max-1
    movge r1, #63
    movge pc, lr

    sub r1, r1, r3          ; remove pattern start
    add r2, r2, r0, lsl #3
    ldr r3, [r2, #4]
    and r1, r1, r3          ; line in pattern
    mov pc, lr

.macro pat_len start, len
    .long \start
    .long \len-1
    .set \start, \start + \len
.endm

; BBPD MOD has short (32 line) patterns at 8, 15, 20, 21.
rocket_pattern_starts_table:
    .set ps, 0
    pat_len ps, 64   ; 0
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 32   ; 8
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 32   ; 15
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 64
    pat_len ps, 32   ; 20
    pat_len ps, 32   ; 21
    pat_len ps, 64   ; 22
.else
; TODO - get values without editor!
.endif
