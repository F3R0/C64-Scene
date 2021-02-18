

lda #$00
sta brdColor
lda #$00
sta bgColor	

initSpritesIntro:
	.for (var i = 0; i <= 7; i++)  // don't forget to change!!!!
	{	
	lda #337+i
	sta sprtPointer+i
	lda #$0
	sta $d027+i
	}

	lda #%11111111
	sta $d015

	lda #%00000000
	sta $d01b

	lda #%00000000
	sta $d01c	

    lda #$89
sta $d000
    sta $d004
    sta $d008
        adc #47
    sta $d002
    sta $d006
    sta $d00a

        lda #158
        sta $d00c
        adc #24
        sta $d00e               /// 6-7 x,y coords

	lda #$ff
	sta $d00d
        sta $d00f

    lda #82
    sta $d001
    sta $d003
    adc #42
    sta $d005
    sta $d007
    adc #42
    sta $d009
    sta $d00b

        lda #%00111111
	sta $d01d
	sta $d017

                lda #$01
	        sta $d02d       ///retrojen logo color
                sta $d02e


///-----------------------------------------------------------///
///            	Prepare Raster IRQs          		      ///
///-----------------------------------------------------------///

      sei               /// disable interrupts
      
   	lda #$7f		/// turn off the cia interrupts
	sta $dc0d
	sta $dd0d
	and $d011
	sta $d011
	ldx #$01
	stx $d01a
      

        
        lda #%00110101		/// no basic or kernal
	sta $01

	lda #rasterPos0
	sta $d012

   	lda #<irq00
	sta $fffe
	lda #>irq00
	sta $ffff

    cli         /// re-enable interrupts

//jmp skipbigr  
                          ///SKIP THIS PART
jsr cleartoptxt
jsr paintlogo
  
son:

jsr showcredits
jsr counttoleave
jsr showcredits
jsr counttoleave
jsr mirrorlogo

jsr showcredits
jsr counttoleave
jsr showcredits
jsr counttoleave
jsr mirrorlogo

jsr showcredits
jsr counttoleave
jsr showcredits
jsr counttoleave
jsr mirrorlogo

jmp son
  
irq00:

        sta irq00a
		stx irq00x
		sty irq00y



		ldx #$05
	rl1: dex
		bne rl1

		lda #%00010011 // Text Mode // 24 Rows
		sta $d011

		ldx #$0e
	rl2: dex
		bne rl2

        	lda #%11001000 // 40 Columns // Multicolor
		sta $d016

	 	lda #%00011011 // Text Mode // 25 Rows
		sta $d011

	jsr musicPlay
	
                lsr $d019
		lda #<irq04		/// prepare irq vector
		sta $fffe
		lda #>irq04
		sta $ffff

		lda #rasterPos4 /// set next irq raster position
		sta $d012

     	irq00a_p: lda #$00
		.label irq00a = irq00a_p+1

		irq00x_p: ldx #$00
		.label irq00x = irq00x_p+1

		irq00y_p: ldy #$00
		.label irq00y = irq00y_p+1

		rti

irq04:

        sta irq04a
		stx irq04x
		sty irq04y


jsr colcyc
jsr upscroll


                lsr $d019

		lda #<irq00		/// prepare irq vector
		sta $fffe
		lda #>irq00
		sta $ffff

                lda #rasterPos0	/// set next irq raster position
		sta $d012

     	irq04a_p: lda #$00
		.label irq04a = irq04a_p+1

		irq04x_p: ldx #$00
		.label irq04x = irq04x_p+1

		irq04y_p: ldy #$00
		.label irq04y = irq04y_p+1

		rti

upscroll:

        lda ydelay    //set out a delay for the upscroll
        cmp #$3		//delayed enough? because upscroll is faster
        beq ydelayok    //compared to side scroll.
        inc ydelay    //increment delay by 1 byte until 8
        rts
ydelayok:
              // are you ready to change the text?
        lda #$00      //reset delay
        sta ydelay
        lda ypos      //set y-position for upwards scroll
        cmp #$10
        bpl doscroll  //if scroll value is under $10 ... call doscroll
        sec           //else, subtract value by 1
        sbc #$01
        sta ypos //for smooth scroll.
        rts
doscroll:
        
        lda #$17 //reset value of y pos for scroll
        sta ypos

//move screen data upwards, one row after another
	
        ldx #$00
puttxt:	
        .for(var i=4; i<19; i++) {
        lda $440e+i*40,x //take 40 chars from last row
        sta $440e+(i-1)*40,x
        }
                inx
                cpx #$b
                bne puttxt

                ldx rtextcount
                ldy #0
scrread:
        lda message,x    //self modifying message ...
        sta $440e+18*40,y
        inx
        iny
        cpy #$b
        bne scrread
        stx rtextcount
        rts

showcredits:
        ldy #0
        ldx #0
cycletext:
        lda creditstxt,x
        sta $440a+20*40,y
        iny
                cmp #$00
                bne dontreset
                lda #<creditstxt
                sta cycletext+1
                lda #>creditstxt
                sta cycletext+2
                jmp showcredits
                

dontreset:
        inx
        cpx #20
        bne cycletext
        lda cycletext+1
        clc                       
        adc #20
        sta cycletext+1
        lda cycletext+2
        adc #$00
        sta cycletext+2
        inc creditscount

waitafewsec:
        ldy #210     //default 210
waitloop:
        lda $d012
        cmp #$80
        bne waitloop
        dey
        bne waitloop
        
        
colcyc:
            lda colortable3
            sta colortable3+20
            ldx #$0
coloop:     lda colortable3+1,x 
            sta colortable3,x  

            sta $d80a+20*40,x
            
            inx
            cpx #20
            bne coloop

            rts  


paintlogo:
        ldx #0
        ldy #0
        .for(var i=4; i<19; i++) {
ploop:  lda colortable4,y
        sta $d80e+i*40,x
        inx
        cpx #$b
        bne ploop
        iny
        }
rts


mirrorlogo:
    lda toggle
    eor #%01000001
    sta toggle
    cmp #%11111111
    bne mirror

.for (var i = 0; i <= 5; i++)  // don't forget to change!!!!
{	
lda #345+i
sta sprtPointer+i
}
rts

mirror:
.for (var i = 0; i <= 5; i++)  // don't forget to change!!!!
{	
lda #337+i
sta sprtPointer+i
}
rts

cleartoptxt:
ldx #0
lda #0
clrtxtloop:
sta $d800,x
inx
cpx #160
bne clrtxtloop
rts

counttoleave:
lda creditscount
cmp #70
beq bigrend
rts

toggle:
.byte $ff

rtextcount:
.byte 0

creditscount:
.byte 0

ydelay: 
.byte 0
ypos: 
.byte $17 //always init with $17 for scroller

bigrend:
lda #$0
sta $d02d       ///retrojen logo color
sta $d02e
jsr showcredits

ldx #0
waittogo:
lda $d012
cmp #$80
bne waittogo

lda #0
clearscrloop:
sta $d80e+4*40,x
inx
cpx #255
bne waittogo
 
ldx #0
waittogo2:
lda $d012
cmp #$80
bne waittogo2

lda #0
clearscrloop2:
sta $d90d+4*40,x
inx
cpx #255
bne waittogo2

ldx #0
waittogo3:
lda $d012
cmp #$80
bne waittogo3

lda #0
clearscrloop3:
sta $da0c+4*40,x
inx
cpx #160
bne waittogo3

jsr showcredits
ldy #200
jsr global_delay
jsr showcredits
ldy #200
jsr global_delay
jsr showcredits

skipbigr: