
//jmp endintro

intro:
lda #$0e
sta brdColor
lda #$06
sta bgColor
	          ///SKIP THIS PART

jsr clearscreen
jsr setc64screen

ldx #4
jsr blink1

jsr copy_text01     //unity

ldx #2
jsr blink2

jsr errortext

ldx #2
jsr blink3

jsr copy_text02     //unreal

ldx #2
jsr blink4

jsr errortext2

ldx #2
jsr blink5

jsr copy_textpro

ldx #2
jsr blink6

jsr copy_text03

ldx #2
jsr blink7

jsr loading
jsr loading
jsr loading
jsr loading
jsr loading 

jsr clearscreen
jsr cleartextline

jmp endintro
//jmp intro

copy_text01:
    ldx #0
cl1:
    lda text01,x
    sta $4400+6*40,x
    cmp #$20
    beq skpsnd1
    ldy $ff
    jsr global_delay
    jsr sound
skpsnd1:
    inx
    cpx #40
    bne cl1
    rts   


copy_text02:
    ldx #0
cl2:
    lda text02,x
    sta $4400+11*40,x
    cmp #$20
    beq skpsnd2
    ldy $ff
    jsr global_delay
    jsr sound
skpsnd2:
    inx
    cpx #40
    bne cl2
    rts   

copy_text03:
    ldx #0
cl3:
    lda text03,x
    sta $4400+19*40,x
    cmp #$20
    beq skpsnd3
    ldy $ff
    jsr global_delay
    jsr sound
skpsnd3:
    inx
    cpx #40
    bne cl3
    rts   

copy_textpro:
    ldx #0
clp:
    lda textpro,x
    sta $4400+16*40,x
    cmp #$20
    beq skpsndp
    ldy $ff
    jsr global_delay
    jsr sound
skpsndp:
    inx
    cpx #80
    bne clp
    rts   

errortext:
    ldx #0
cle1:
    lda text04,x
    sta $4400+9*40,x
    inx
    cpx #80
    bne cle1
    rts

errortext2:
    ldx #0
cle2:
    lda text04,x
    sta $4400+14*40,x
    inx
    cpx #80
    bne cle2
    rts  

blink1:

    lda #$e0
    sta $4400+6*40
ldy #72
jsr global_delay

    lda #$20
    sta $4400+6*40
ldy #72
jsr global_delay
dex
bne blink1
rts

blink2:
    lda #$e0
    sta $4409+6*40
ldy #72
jsr global_delay

    lda #$20
    sta $4409+6*40
ldy #72
jsr global_delay
dex
bne blink2
rts 

blink3:
    lda #$e0
    sta $4400+11*40
ldy #72
jsr global_delay

    lda #$20
    sta $4400+11*40
ldy #72
jsr global_delay
dex
bne blink3
rts

blink4:
    lda #$e0
    sta $441a+11*40
ldy #72
jsr global_delay

    lda #$20
    sta $441a+11*40
ldy #72
jsr global_delay
dex
bne blink4
rts

blink5:
    lda #$e0
    sta $4400+16*40
ldy #72
jsr global_delay

    lda #$20
    sta $4400+16*40
ldy #72
jsr global_delay
dex
bne blink5
rts

blink6:
    lda #$e0
    sta $4400+19*40
ldy #72
jsr global_delay

    lda #$20
    sta $4400+19*40
ldy #72
jsr global_delay
dex
bne blink6
rts

blink7:
    lda #$e0
    sta $4400+20*40
ldy #72
jsr global_delay

    lda #$20
    sta $4400+20*40
ldy #72
jsr global_delay
dex
bne blink7
rts






cur_delay:
            ldx #$ff
cd_loop:    dex
            cpx #0
            bne cd_loop
            dey
            cpy #0
            bne cur_delay
            rts



setc64screen:
            ldx #0
c64t_loop:    
            lda c64text,x     
            sta screen,x
            inx
            cpx #206        /// Character count.
            bne c64t_loop
            rts

curpos:
.byte 0


loading:
		ldy #0
		ldx #0
loadloop:
		dex
		bne loadloop
		sty brdColor
		dey
		bne loadloop
		rts

cleartextline:
        ldx #0
        lda #$00
cltxtloop: sta $4400+19*40,x
        inx
        bne cltxtloop
		rts

endintro:
