; exodecrunch.c
; 

.equ STATE_IMPLICIT_FIRST_LITERAL_BYTE, 0
.equ STATE_NEXT_BYTE, 1
.equ STATE_NEXT_LITERAL_BYTE, 2
.equ STATE_NEXT_SEQUENCE_BYTE, 3
.equ STATE_EOF, 4

.equ WINDOW_LENGTH, 8192;   65535
.equ TABLE_ENTRY_SIZE, 8

;struct exo_table_entry
;{
;    unsigned short int base;       ; 4 byte aligned
;    unsigned char bits;            ; 4 byte aligned
;};

.if _DEBUG
ctx_debug:
	str lr, [sp, #-4]!

    ldr r0, ctx_state
    bl debug_write_r0

    ldr r0, ctx_length
    bl debug_write_16

    ldr r0, ctx_offset
    bl debug_write_16

    ldr r0, ctx_window_pos
    bl debug_write_16

    ldr r0, ctx_bit_buffer
    bl debug_write_r0

    ldr r0, ctx_read_data
    bl debug_write_32

	ldr pc, [sp], #4        ; return bits;
.endif

ctx_block:
ctx_state:
    .long 0                 ; enum exo_state state;
ctx_length:
    .long 0                 ; unsigned short int length;
ctx_offset:
    .long 0                 ; unsigned short int offset;
ctx_bit_buffer:
    .long 0                 ; unsigned char bit_buffer;
ctx_read_data:
    .long 0                 ; void *read_data;
ctx_window_pos:
    .long 0                 ; unsigned short int window_pos;

; R11 = ctx->bit_buffer
; R0 = carry_in
; R0 = carry_out
;bitbuffer_rotate:
;    mov r11, r11, lsl #1    ; bit_buffer <<= 1;
;    orr r11, r11, r0        ; if (carry) bit_buffer |= 0x01;
;    mov r0, r11, lsr #8     ; C = carry_out
;    and r11, r11, #0xff     ; unsigned char
;    mov pc, lr


; R12 = ctx->read_data
; R0 = returned byte
.macro EXO_READ_BYTE reg
    ldrb \reg, [r12], #1      ; return *read_data++;
.endm

; R11 = ctx->bit_buffer
; R12 = ctx->read_data
; Returns R2 = bits
; Uses R0 as scratch
; R1 = bit_count
; R3 = byte_copy
read_bits:
	str lr, [sp, #-4]!
    mov r2, #0              ; R2 = int bits = 0;
    and r3, r1, #8          ; R3 = int byte_copy = bit_count & 8;

    ands r1, r1, #7         ; bit_count &= 7;
    beq read_bits_0

    bic r11, r11, #0xff00   ; clear HI byte (unsigned char bit_buffer)

    adr r0, read_bits_jump_table
    add r0, r0, r1, lsl #2
    mov pc, r0

read_bits_7:
    .rept 7
    and r0, r11, #0x7f
    rsbs r0, r0, #0         ; 0 - bit_buffer -> sets carry_in iff bit_buffer & 0x7f == 0
    ; EXO_READ_BYTE r0
    ldrcsb r0, [r12], #1    ; bit_buffer = read_byte(inp);
    biccs r11, r11, #0xff   ; replace bottom byte - bit 7 can still be set :\
    orrcs r11, r11, r0      ; replace bottom byte
    adc r11, r11, r11       ; bit_buffer <<= 1; bit_buffer |= carry_in
    .endr

    mov r2, r11, lsr #8     ; but only want N bits of HI byte

read_bits_0:
    cmp r3, #0              ; if (byte_copy != 0)
    ldreq pc, [sp], #4      ; return bits;

    mov r2, r2, lsl #8      ; bits <<= 8;
    EXO_READ_BYTE r0
    orr r2, r2, r0          ; bits |= read_byte(inp);
	ldr pc, [sp], #4        ; return bits;

read_bits_jump_table:
    b read_bits_7 + 7*24
    b read_bits_7 + 6*24
    b read_bits_7 + 5*24
    b read_bits_7 + 4*24
    b read_bits_7 + 3*24
    b read_bits_7 + 2*24
    b read_bits_7 + 1*24
    b read_bits_7

; Inline version of above when we know the bit_count at assemble time.
.macro READ_BITS_NO_COPY bit_count
    bic r11, r11, #0xff00   ; clear HI byte (unsigned char bit_buffer)
.rept \bit_count
    and r0, r11, #0x7f
    rsbs r0, r0, #0         ; 0 - bit_buffer -> sets carry_in iff bit_buffer & 0x7f == 0
    ldrcsb r0, [r12], #1    ; bit_buffer = read_byte(inp);
    biccs r11, r11, #0xff   ; replace bottom byte - bit 7 can still be set :\
    orrcs r11, r11, r0      ; replace bottom byte
    adc r11, r11, r11       ; bit_buffer <<= 1; bit_buffer |= carry_in
.endr
    movs r2, r11, lsr #8     ; but only want N bits of HI byte
.endm

; R11 = ctx->bit_buffer
; R12 = ctx->read_data
; R4 = size
; R6 = table
; Uses R0, R1, R2, R3, R4, R8
; R7 = base
generate_table:
	str lr, [sp, #-4]!

    mov r7, #1              ; int base = 1;
.1:                         ; for(i = 0; i < size; ++i)
    str r7, [r6], #4        ; table[i].base = base; *table++ = base;
    READ_BITS_NO_COPY 3

    mov r8, r2              ; r8 = (unsigned char)read_bits(ctx, 3);
    READ_BITS_NO_COPY 1
    orr r8, r8, r2, lsl #3  ; r8 |= (unsigned char)read_bits(ctx, 1) << 3;
    str r8, [r6], #4        ; table[i].bits = r8

    mov r0, #1
    add r7, r7, r0, lsl r8  ; base += 1 << table[i].bits;

    subs r4, r4, #1
    bne .1                  ; for(i = 0; i < size; ++i)

	ldr pc, [sp], #4        ; return table

; R12 = ctx->read_data
; Trashes R0-R8, R11
exo_decrunch_new:
	str lr, [sp, #-4]! 

    mov r0, #STATE_IMPLICIT_FIRST_LITERAL_BYTE
    str r0, ctx_state       ; ctx->state = STATE_IMPLICIT_FIRST_LITERAL_BYTE;

    mov r0, #0
    str r0, ctx_window_pos  ; ctx->window_pos = 0;

    EXO_READ_BYTE r0
    str r0, ctx_bit_buffer  ; ctx->bit_buffer = read_byte(read_data);
    mov r11, r0             ; r11 = ctx->bit_buffer

    adr r6, exo_table_lengths
    mov r4, #16
    bl generate_table       ; ctx->lengths = generate_table(ctx, 16);

    adr r6, exo_table_offsets3
    mov r4, #16
    bl generate_table       ; ctx->offsets3 = generate_table(ctx, 16);

    adr r6, exo_table_offsets2
    mov r4, #16
    bl generate_table       ; ctx->offsets2 = generate_table(ctx, 16);

    adr r6, exo_table_offsets1
    mov r4, #4
    bl generate_table       ; ctx->offsets1 = generate_table(ctx, 4);
    
    ; store context
    str r11, ctx_bit_buffer
    str r12, ctx_read_data
	ldr pc, [sp], #4        ; return

; R11 = ctx->bit_buffer
; R12 = ctx->read_data
; R4 = ctx->state
; R5 = gamma
; Uses R0, R1, R2, R3
;get_gamma_code:
;	str lr, [sp, #-4]!
;    mov r5, #0              ; r5 = int gamma = 0;
;.1:
;    READ_BITS_NO_COPY 1
;    cmp r2, #0
;    bne .2                  ; while(read_bits(ctx, 1) == 0)
;    add r5, r5, #1          ;  ++gamma;
;    b .1
;.2:
;	ldr pc, [sp], #4        ; return gamma

; R12 = ctx->read_data
; R11 = ctx->bit_buffer
; R10 = ctx->window
; R9 = ctx->offset
; R6 = ctx->length
; R4 = ctx->state
; R5 = length_index
; R7 = table_entry
; Returns R0
exo_read_decrunched_byte:
	str lr, [sp, #-4]!

    ; Load the context
    adr r10, exo_window     ; r10 = ctx->window
    adr r1, ctx_block
    ldmia r1, {r4, r6, r9, r11, r12}

exo_read_single_byte:
    adr r0, exo_decrunch_state_jump
    add r0, r0, r4, lsl #2
    mov pc, r0              ; switch(ctx->state)

state_implicit_first_literal:

    ; literal byte
    EXO_READ_BYTE r0
    mov r4, #STATE_NEXT_BYTE
    b store_byte_in_window_and_return

state_eof_default:

    mov r4, #STATE_EOF      ; if(length_index == 16)
    str r4, ctx_state       ; ctx->state = STATE_EOF;
    mov r0, #-1
	ldr pc, [sp], #4        ; return -1

state_next_byte:

    READ_BITS_NO_COPY 1

    cmp r2, #1
    bne .1
    ; literal byte
    EXO_READ_BYTE r0            ; c = ctx->read_byte(ctx->read_data);
    b store_byte_in_window_and_return    

.1:
    ; sequence
    ;bl get_gamma_code          ; length_index = get_gamma_code(ctx);
    mov r5, #0                  ; r5 = int gamma = 0;
    .3:
        READ_BITS_NO_COPY 1
        bne .2                  ; while(read_bits(ctx, 1) == 0)
        add r5, r5, #1          ; ++gamma;
        b .3
    .2:

    ; end of data marker, we're done
    cmp r5, #16
    beq state_eof_default   ; return -1

    cmp r5, #17             ; if(length_index == 17)
    bne state_next_byte_cont

    ; literal data block
    EXO_READ_BYTE r0
    mov r6, r0, lsl #8      ; ctx->length = ctx->read_byte(ctx->read_data) << 8;
    EXO_READ_BYTE r0
    orr r6, r6, r0          ; ctx->length |= ctx->read_byte(ctx->read_data);
    mov r4, #STATE_NEXT_LITERAL_BYTE    ; ctx->state = STATE_NEXT_LITERAL_BYTE;
    ; fall through!

state_next_literal_byte:

    subs r6, r6, #1                 ; if(--ctx->length == 0)
    moveq r4, #STATE_NEXT_BYTE      ; ctx->state = STATE_NEXT_BYTE;

    EXO_READ_BYTE r0
    b store_byte_in_window_and_return

state_next_byte_cont:
    ; sequence
    adr r7, exo_table_lengths   
    add r7, r7, r5, lsl #3      ; table_entry = ctx->lengths + length_index;
    ldr r1, [r7, #4]            ; table_entry->bits
    bl read_bits

    ldr r6, [r7]                ; table_entry->base
    add r6, r6, r2              ; ctx->length = table_entry->base + read_bits(ctx, table_entry->bits);

    ;switch(ctx->length)
.1:
    cmp r6, #1
    bne .2
    ; case 1:
    adr r7, exo_table_offsets1  ; table_entry = ctx->offsets1 + read_bits(ctx, 2);
    READ_BITS_NO_COPY 2
    b .4

.2:
    ; case 2:
    cmp r6, #2
    bne .3
    adr r7, exo_table_offsets2  ; table_entry = ctx->offsets2 + read_bits(ctx, 4);
    READ_BITS_NO_COPY 4
    b .4

.3:
    ; default:
    adr r7, exo_table_offsets3  ; table_entry = ctx->offsets2 + read_bits(ctx, 4);
    READ_BITS_NO_COPY 4
    ; drop through

.4:
    add r7, r7, r2, lsl #3      ; table_entry += read_bits(ctx, n)

    ldr r1, [r7, #4]            ; table_entry->bits
    bl read_bits                ; read_bits(ctx, table_entry->bits);

    ldr r9, [r7]                ; table_entry->base
    add r9, r9, r2              ; r9 = ctx->offset = table_entry->base + read_bits(ctx, table_entry->bits);

    mov r4, #STATE_NEXT_SEQUENCE_BYTE   ; ctx->state = STATE_NEXT_SEQUENCE_BYTE;
    ; fall through!

state_next_sequence_byte:

    subs r6, r6, #1             ; --ctx->length
    moveq r4, #STATE_NEXT_BYTE  ; if(--ctx->length == 0) ctx->state = STATE_NEXT_BYTE;

    ; c = read_byte_from_window(ctx, ctx->offset);
    ldr r1, ctx_window_pos
    subs r1, r1, r9             ; read_pos = ctx->window_pos - offset;
                                ; if(read_pos < 0)
    addlt r1, r1, #WINDOW_LENGTH; read_pos += ctx->window_length;

    ldrb r0, [r10, r1]          ; return ctx->window[read_pos];
    ; fall through
    
store_byte_in_window_and_return:

    ; Store byte in the window
    ldr r1, ctx_window_pos
    strb r0, [r10, r1]      ; ctx->window[ctx->window_pos++] = (unsigned char)c;
    add r1, r1, #1
    cmp r1, #WINDOW_LENGTH  ; if(ctx->window_pos == ctx->window_length)
    moveq r1, #0            ; ctx->window_pos = 0;
    str r1, ctx_window_pos
    ; I guess this can be read directly from the preceding window of data

    subs r8, r8, #1
    bne exo_read_single_byte

    ; Store the context
    adr r1, ctx_block
    stmia r1, {r4, r6, r9, r11, r12}
    ldr pc, [sp], #4        ; return r0

exo_decrunch_state_jump:
    b state_implicit_first_literal
    b state_next_byte
    b state_next_literal_byte
    b state_next_sequence_byte
    b state_eof_default

exo_table_lengths:
    .skip TABLE_ENTRY_SIZE * 16

exo_table_offsets3:
    .skip TABLE_ENTRY_SIZE * 16

exo_table_offsets2:
    .skip TABLE_ENTRY_SIZE * 16

exo_table_offsets1:
    .skip TABLE_ENTRY_SIZE * 4
