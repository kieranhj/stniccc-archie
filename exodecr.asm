; exodecr.c
; Ported to ARM
; BUSTED! THIS CODE DOESN'T ACTUALLY WORK
; SEE EXDECRUNCH.ASM INSTEAD

.org 0x8000

;static unsigned short int base[52];
exodecr_base:
    .skip 52

;static char bits[52];
exodecr_bits:
    .skip 52

;static unsigned char bit_buffer;
exodecr_bit_buffer:
    .long 0

; R11 = exodecr_bit_buffer
; R0 = carry_in
; R0 = carry_out
exodecr_bitbuffer_rotate:
    mov r11, r11, asl #1    ; bit_buffer <<= 1;
    orr r11, r11, r0        ; if (carry) bit_buffer |= 0x01;
    mov r0, r11, lsr #8     ; C = carry_out
    and r11, r11, #0xff     ; unsigned char
    mov pc, lr

; R12 = inp
; R0 = returned byte
exodecr_read_byte:
    ldrb r0, [r12, #-1]!    ; unsigned char val = *--(*inp) & 0xff;
    mov pc, lr

; R11 = exodecr_bit_buffer
; R12 = inp
; R1 = bit_count
; Returns R2
; Uses R0, R3
exodecr_read_bits:
	str lr, [sp, #-4]!
    mov r2, #0              ; R2 = unsigned short int bits = 0;
    and r3, r1, #8          ; R3 = int byte_copy = bit_count & 8;

    and r1, r1, #7          ; bit_count &= 7;

.1:
    cmp r1, #0
    beq .2
    subs r1, r1, #1         ; while(bit_count-- > 0)

    mov r0, #0              
    bl exodecr_bitbuffer_rotate
                            ; R0 = int carry = bitbuffer_rotate(0);

    cmp r11, #0             ; if (bit_buffer == 0)
    bne .3

    bl exodecr_read_byte    ; 
    mov r11, r0             ; bit_buffer = read_byte(inp);
    mov r0, #1
    bl exodecr_bitbuffer_rotate
                            ; R0 = carry = bitbuffer_rotate(1);
.3:
    mov r2, r2, asl #1      ; bits <<= 1;
    orr r2, r2, r0          ; bits |= carry;
    bic r2, r2, #0xff0000   ; unsigned short int

    b .1                    ; while(bit_count-- > 0)
    
.2:
    cmp r3, #0              ; if (byte_copy != 0)
    beq .5

    mov r2, r2, lsl #8      ; bits <<= 8;
    bic r2, r2, #0xff0000   ; unsigned short int bits
    bl exodecr_read_byte
    orr r2, r2, r0          ; bits |= read_byte(inp);

.5:
	ldr pc, [sp], #4        ; return bits;

; R11 = exodecr_bit_buffer
; R12 = inp
; R6 = exodecr_base
; R7 = exodecr_bits
; Uses R0, R1, R2, R3, R4, R5, R8
exodecr_init_table:
	str lr, [sp, #-4]!

    mov r4, #0              ; r4 = int i
.1:
    tst r4, #15             ; if((i & 15) == 0)
    moveq r5, #1            ; r5 = b2 = 1;

    strb r5, [r6, r4]       ; base[i] = b2;

    mov r1, #3
    bl exodecr_read_bits
    mov r8, r2              ; r8 = b1 = read_bits(inp, 3);

    mov r1, #1
    bl exodecr_read_bits
    orr r8, r8, r2, lsl #3  ; b1 |= read_bits(inp, 1) << 3;
    bic r8, r8, #0xff0000   ; unsigned short int b1

    strb r8, [r7, r4]       ; bits[i] = b1;

    mov r0, #1
    add r5, r5, r0, lsl r8  ; b2 += 1 << b1;
    bic r5, r5, #0x00ff0000 ; unsigned short int b2
    bic r5, r5, #0xff000000 ; unsigned short int b2

    add r4, r4, #1
    cmp r4, #52
    blt .1                  ; for(i = 0; i < 52; ++i)

  	ldr pc, [sp], #4

; R12 = in (end)
; R10 = out (end)
exo_decrunch:
    stmfd sp!, {r0-r9, r11, lr}

    bl exodecr_read_byte
    mov r11, r0             ; r11 = bit_buffer = read_byte(&in);

    adrl r6, exodecr_base   ; r6 = &exodecr_base
    adrl r7, exodecr_bits   ; r7 = &exodecr_bits
    bl exodecr_init_table   ; init_table(&in);

    mov r5, #1              ; char literal; <= unassigned variable!
    b implicit_literal_byte ; goto implicit_literal_byte;

; R4 = index
; R5 = literal
; R8 = length
; R9 = offset
; R0 = c
exo_decrunch_loop:          ; for(;;)

    mov r1, #1
    bl exodecr_read_bits
    mov r5, r2              ; r5 = literal = read_bits(&in, 1);
    cmp r5, #1              ; if(literal == 1)
    bne not_literal_byte

implicit_literal_byte:

    mov r8, #1              ; r8 = length = 1
    b exo_decrunch_copy

not_literal_byte:

    mov r4, #0              ; r4 = index = 0;

.1:
    mov r1, #1
    bl exodecr_read_bits
    cmp r2, #0              ; while(read_bits(&in, 1) == 0)
    addeq r4, r4, #1        ; ++index
    beq .1

    cmp r4, #16             ; if(index == 16)
    beq break_loop          ; break;

    cmp r4, #17
    bne not_new_literal     ; if(index == 17)

    mov r5, #1              ; r5 = literal = 1;
    bl exodecr_read_byte
    mov r8, r0, lsl #8      ; r8 = length = read_byte(&in) << 8;
    bl exodecr_read_byte
    orr r8, r8, r0          ; length |= read_byte(&in);
    b exo_decrunch_copy

not_new_literal:

    ldrb r8, [r6, r4]       ; length = base[index];

    ldrb r1, [r7, r4]       ; bits[index]
    bl exodecr_read_bits
    add r8, r8, r2          ; length += read_bits(&in, bits[index]);
    bic r8, r8, #0xff0000   ; unsigned short int length

    cmp r8, #1              ; switch(length)
    bne .2
    ; case 1:
    mov r1, #2
    bl exodecr_read_bits    ; index = read_bits(&in, 2);
    add r4, r2, #48         ; index += 48;
    b .4

.2:
    cmp r6, #2
    bne .3
    ; case 2:
    mov r1, #4
    bl exodecr_read_bits    ; index = read_bits(&in, 4);
    add r4, r2, #32         ; index += 32;
    b .4
    
.3: 
    ; default:
    mov r1, #4
    bl exodecr_read_bits    ; index = read_bits(&in, 4);
    add r4, r2, #16         ; index += 16;

.4:
    ldrb r9, [r6, r4]       ; r9 = offset = base[index];
    ldrb r1, [r7, r4]       ; bits[index]
    bl exodecr_read_bits
    add r9, r9, r2          ; offset += read_bits(&in, bits[index]);
    bic r9, r9, #0xff0000   ; unsigned short int offset

exo_decrunch_copy:

    sub r10, r10, #1        ; --out

    cmp r5, #0              ; if(literal)
    bne .1

    ldrb r0, [r10, r9]      ; r0 = c = out[offset];
    b .2

    .1:
    bl exodecr_read_byte    ; r0 = c = read_byte(&in);

    .2:
    strb r0, [r10]          ; *out = c;
    subs r8, r8, #1
    bne exo_decrunch_copy   ; while(--length > 0);
    b exo_decrunch_loop     ; for(;;)

break_loop:                 ; return out;
    ldmfd sp!, {r0-r9, r11, pc}
