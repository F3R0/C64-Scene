///---------------------------------------///
///         Constants & Variables         ///
///---------------------------------------///

.const bitmap 		= $6000
.const bitmapScr	= $4900 //1000
.const bitmapCol	= $4ce8 //1000
.const screen 		= $4400	//1000
.const sprites		= $5100
.const sprtPointer  = screen+$03f8
.const charset		= $4000
.const charsetm		= $5800
.const datablocks	= $9000
.const music 		= $1000
.const musicPlay 	= music+3
.const color 		= $d800

.const rasterPos0	= 250
.const rasterPos4	= 0

.const rasterPos1	= 0
.const rasterPos2	= 216
.const rasterPos3	= 250


.const brdColor 	= $d020
.const bgColor 		= $d021

.const Start 		= $8000

.label firstrow = screen + (22 * 40)
.label secondrow = screen + (23 * 40)

///---------------------------------------///
///              Code Start               ///
///---------------------------------------///

*= charset "Character Set"
.import binary "resources/ferofont.bin"

* = music "Music"
.import binary "resources/1980.dat",2

*= bitmap "Bitmap"
.import binary "resources/bit.map"

*= bitmapCol "BitmapColors"
.import binary "resources/bit.col"

*= bitmapScr "Bitmap Screen"
.import binary "resources/bit.scr"

*= sprites "Sprites"
.import binary "resources/spritechar.dat"

*= charsetm "CharSetMixed"
.import binary "resources/map.bin"


* = $0801 "Basic"
	BasicUpstart(Start)

* = Start "Code"
 
		lda $dd00
        and #$FC	 ///	11111100
        ora #2       ///	00000010 - Bank1: $4000-$7fff
        sta $dd00
		lda #%00010110	//Bitmap: $6000, Screen: $4400
		sta $d018

///-----------------------------------------------------------///
///           		   Initialize Assets     	    		  ///
///-----------------------------------------------------------///
		
	lda #$0
	sta $3fff	/// Clear ghost bytes


	jsr init_sound
 	jsr music


	#import "type.asm"
	#import "bigr.asm"



///-----------------------------------------------------------///
///						Prepare Raster IRQs					  ///
///-----------------------------------------------------------///


		//				 Chrmem: $5800 */
		


///-----------------------------------------------------------///
///           			Prepare Raster IRQs          		  ///
///-----------------------------------------------------------///


	sei


	
	lda #rasterPos1
	sta $d012
	
	lda #<irq01
	sta $fffe
	lda #>irq01
	sta $ffff

	cli	

	jsr clearscreen
	jsr image
	jsr initSprites

mainLoop:	
	
    jsr toggletext
    jsr counttext

	jmp mainLoop


irq01:
    	sta irq01a
		stx irq01x
		sty irq01y

		lda #%00111000		//25 Rows
		sta $d011
/* 		lda #%11011000		//40 Columns // Multicolor Mode
		sta $d016 */

		lsr $d019		/// ack RASTER IRQ

		jsr spritemovey
		jsr musicPlay

lda #$00
sta $d021

		lda #<irq02		/// prepare irq vector
		sta $fffe
		lda #>irq02
		sta $ffff

		lda #rasterPos2	/// set next irq raster position
		sta $d012


		irq01a_p: lda #$00
		.label irq01a = irq01a_p+1

		irq01x_p: ldx #$00
		.label irq01x = irq01x_p+1

		irq01y_p: ldy #$00
		.label irq01y = irq01y_p+1

		rti



irq02:

    	sta irq02a
		stx irq02x
		sty irq02y      


		lda #%00010000 // Screen 0400 // Bitmap 2000 // Chars 1800
		sta $d018

/* 		lda #%11001000		//40 Columns // Multicolor Mode
		sta $d016 */

		lda #%00011000 // 25 Rows
		sta $d011 

		lsr $d019		/// ack RASTER IRQ
		lda #<irq03		/// prepare irq vector
		sta $fffe
		lda #>irq03
		sta $ffff

		lda #rasterPos3	/// set next irq raster position
		sta $d012


       jsr scrollpixel


		irq02a_p: lda #$00

		.label irq02a = irq02a_p+1

		irq02x_p: ldx #$00
		.label irq02x = irq02x_p+1

		irq02y_p: ldy #$00
		.label irq02y = irq02y_p+1

		rti


irq03:

    	sta irq03a
		stx irq03x
		sty irq03y
	

		ldx #$02
	l1: dex
		bne l1

		lda #%00010011 // Text Mode // 24 Rows
		sta $d011

		ldx #$0b
	l2: dex
		bne l2


		lda #%00011110
		sta $d018		 /// Bitmap #6000 // Charmem #7800 // OMASA NE OLUR??


		lda #%11011000 // 40 Columns // Multicolor
		sta $d016

	/*  	lda #%00011011 // Text Mode // 25 Rows
		sta $d011 */
	
        
		lsr $d019		/// ack RASTER IRQ
		lda #<irq01		/// prepare irq vector
		sta $fffe
		lda #>irq01
		sta $ffff

         jsr colorCycle
        jsr scrollchr

		lda #rasterPos1	/// set next irq raster position
		sta $d012


		irq03a_p: lda #$00
		.label irq03a = irq03a_p+1

		irq03x_p: ldx #$00
		.label irq03x = irq03x_p+1

		irq03y_p: ldy #$00
		.label irq03y = irq03y_p+1

		rti


initSprites:

	
	.for (var i = 0; i <= 7; i++)  // don't forget to change!!!!
	{	
	lda #324+i
	sta sprtPointer+i
	lda #$06
	sta $d027+i
	}

	lda #$0e
	sta $d025
	lda #$06
	sta $d026

	lda #%11111111
	sta $d01c

    lda #$80
	sta $d000
	adc #$16
    sta $d002
	adc #$15
    sta $d004
    adc #$15
    sta $d006
    adc #$17
    sta $d008

	lda #%00000000
	sta $d01d
	sta $d017

    /// still sprites
	lda #26
	sta $d00b
	sta $d00d
	sta $d00f
	lda #62
	sta $d00a
	lda #$ce
	sta $d00c
	lda #226
	sta $d00e
	lda #11
	sta $d02d
	sta $d02e

rts



image:

	lda #$00
    ldx #$00

	
	copy:   lda bitmapScr ,x
    		sta screen ,x
    		lda bitmapScr+$100,x
    		sta screen +$100,x
    		lda bitmapScr+$200,x
    		sta screen +$200,x
			lda bitmapScr+$250,x
    		sta screen +$250,x


    		lda bitmapCol ,x
    		sta color ,x
    		lda bitmapCol+$100,x
    		sta color +$100,x
    		lda bitmapCol+$200,x
    		sta color +$200,x
			lda bitmapCol+$250,x
    		sta color +$250,x
    		dex
    		bne copy
    		rts



colorCycle:
	lda colorTable2
	sta colorTable2+40
	ldx #$00
cl:
	lda colorTable2+1,x
	sta colorTable2,x
	sta $db98,x   /// color + 23*40
	sta $db70,x   /// color + 22*40
	lda #02
	sta $dbc0,x   /// color + 23*40
	sta $db48,x   /// color + 22*40
	inx
	cpx #40
	bne cl
	rts

stableSprites:
			lda #26
			sta $d00b
			sta $d00d
			sta $d00f

			lda #62
			sta $d00a
			lda #$ce
			sta $d00c

			lda #226
			sta $d00e

			lda #11
			sta $d02d
			sta $d02e
spritemovey:
/* 
			lda framecountery
			cmp #255
			bne movey

			lda #0
			sta framecountery */
	movey:	ldx framecountery
			.for(var i=0; i<=8; i+=2) {
			lda sinustable,x
			adc #247
			sta $d001+i
			inx
			}
	        inc framecountery
			lda colortable,x
	       	sta $d020
			sta $d027
			sta $d028
			sta $d029
			sta $d02a
			sta $d02b

/* 	lda sinustable
	sta sinustable+60

	ldx #$00
    clo:    
	lda sinustable+1,x
	sta sinustable,x

	inx
	cpx #60
	bne clo */
	rts


toggletext:

    lda textno
    cmp #1
    beq raat
    .for (var i = 0; i <= 4; i++)  // don't forget to change!!!!
	{	
	lda #332+i
	sta sprtPointer+i
	}

    lda #128
	sta $d000

	adc #27
    sta $d002

	adc #25
    sta $d004

    adc #22
    sta $d006

    adc #23
    sta $d008

/* 	lda #%0011111		//sprite x/y stretch
	sta $d01d
	sta $d017 */

    inc textno
    rts

raat:
    .for (var i = 0; i <= 4; i++)  // don't forget to change!!!!
	{	
	lda #324+i
	sta sprtPointer+i
	}

    lda #$80
	sta $d000
	adc #$16
    sta $d002
	adc #$15
    sta $d004
    adc #$15
    sta $d006
    adc #$17
    sta $d008

    dec textno
    rts


    
    
    
    


scrollpixel:
	lda stored016
	sec
	sbc #%00000001
	and #%00000111
    sta stored016
    ora #%00001000  
    sta $d016
	rts

scrollchr:
	lda stored016
	beq scrollchars
	rts

scrollchars:
	ldx #0
scrloop:
	lda firstrow+1,x
	sta firstrow,x
	lda secondrow+1,x
	sta secondrow,x
	inx
	cpx #40
	bne scrloop
	
getchr: 
	lda text
	cmp #$ff
	bne ct

	lda #<text
	sta getchr+1
	lda #>text
	sta getchr+2
	jmp getchr

ct:	sta firstrow+39
	ora #$40
	sta secondrow+39

	inc getchr+1
	bne skip
	inc getchr+2

skip:

	rts

counttext:  
        lda textcount
        ldy #$ff
        ldx #0
delaym: dex
        bne delaym
        dey
        bne delaym
        inc textcount
        cmp #4
        bne counttext
        lda #0
        sta textcount
        rts

global_delay:
        lda $d012
        cmp #$80
        bne global_delay
        dey
        bne global_delay
        rts


init_sound:
lda #$ff
sta $d400+24 //volume max
lda #54
sta $d400+1 //hi byte freq voice 1
lda #$05 //instant attack, a little decay
sta $d400+5

sound:

lda #%10000000 // noise, gate close
sta $d400+4 //voice 1 control register
lda #%10000001 //noise, gate open
sta $d400+4

rts


		

clearscreen:  ldx #0
        lda #$20
clearloop:   sta $4400,x
        sta $4500,x
        sta $4600,x
        inx
        bne clearloop
		rts

*= datablocks "Datablocks"

	sinustable:
		.byte 4,3,2,1,1,1,1,1,2,2,3,4,5,6,7,7
		.byte 7,7,7,6,5,4,3,2,1,1,1,1,1,1,2,3
		.byte 4,5,6,7,7,7,7,7,6,6,5,3,2,2,1,1
		.byte 1,1,1,2,3,4,5,6,7,7,7,7,7,7,6,5
		.byte 4,3,2,1,1,1,1,1,2,3,4,5,6,6,7,7
		.byte 7,7,7,6,5,4,3,2,1,1,1,1,1,2,2,3
		.byte 4,5,6,7,7,7,7,7,6,5,4,3,2,1,1,1
		.byte 1,1,1,2,3,4,5,6,7,7,7,7,7,6,6,5
		.byte 3,2,2,1,1,1,1,1,2,3,4,5,6,7,7,7
		.byte 7,7,7,6,5,4,3,2,1,1,1,1,1,2,3,4
		.byte 5,6,6,7,7,7,7,7,6,5,4,3,2,1,1,1
		.byte 1,1,2,2,3,4,5,6,7,7,7,7,7,6,5,4
		.byte 3,2,1,1,1,1,1,1,2,3,4,5,6,7,7,7
		.byte 7,7,6,6,5,3,2,2,1,1,1,1,1,2,3,4
		.byte 5,6,7,7,7,7,7,7,6,5,4,3,2,1,1,1
		.byte 1,1,2,3,4,5,6,6,7,7,7,7,7,6,5,4

	sinx:
.byte 10,10,9,9,8,8,8,7,7,6,6,6,6,5,5,5
.byte 5,5,4,4,4,4,4,4,4,5,5,5,5,5,5,6
.byte 6,6,7,7,7,8,8,9,9,9,10,10,11,11,11,12
.byte 12,13,13,13,14,14,14,15,15,15,15,15,15,16,16,16
.byte 16,16,16,16,15,15,15,15,15,14,14,14,14,13,13,12
.byte 12,12,11,11,10,10,10,9,9,8,8,8,7,7,6,6
.byte 6,6,5,5,5,5,5,4,4,4,4,4,4,4,5,5
.byte 5,5,5,5,6,6,6,7,7,7,8,8,9,9,9,10
.byte 10,11,11,11,12,12,13,13,13,14,14,14,15,15,15,15
.byte 15,15,16,16,16,16,16,16,16,15,15,15,15,15,14,14
.byte 14,14,13,13,12,12,12,11,11,10,10,10,9,9,8,8
.byte 8,7,7,6,6,6,6,5,5,5,5,5,4,4,4,4
.byte 4,4,4,5,5,5,5,5,5,6,6,6,7,7,7,8
.byte 8,9,9,9,10,10,11,11,11,12,12,13,13,13,14,14
.byte 14,15,15,15,15,15,15,16,16,16,16,16,16,16,15,15
.byte 15,15,15,14,14,14,14,13,13,12,12,12,11,11,10,10

cosx:
.byte 10,10,9,9,8,8,8,7,7,7,7,7,7,7,7,7
.byte 7,7,7,8,8,8,9,9,9,10,10,11,11,11,12,12
.byte 12,13,13,13,13,13,13,13,13,13,13,13,13,12,12,12
.byte 11,11,10,10,10,9,9,8,8,8,7,7,7,7,7,7
.byte 7,7,7,7,7,7,8,8,8,9,9,9,10,10,11,11
.byte 11,12,12,12,13,13,13,13,13,13,13,13,13,13,13,13
.byte 12,12,12,11,11,10,10,10,9,9,8,8,8,7,7,7
.byte 7,7,7,7,7,7,7,7,7,8,8,8,9,9,9,10
.byte 10,11,11,11,12,12,12,13,13,13,13,13,13,13,13,13
.byte 13,13,13,12,12,12,11,11,10,10,10,9,9,8,8,8
.byte 7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,9
.byte 9,9,10,10,11,11,11,12,12,12,13,13,13,13,13,13
.byte 13,13,13,13,13,12,12,12,12,11,11,10,10,10,9,9
.byte 8,8,8,7,7,7,7,7,7,7,7,7,7,7,7,8
.byte 8,8,9,9,9,10,10,11,11,11,12,12,12,13,13,13
.byte 13,13,13,13,13,13,13,13,12,12,12,12,11,11,10,10


	colortable:

		.byte 1,1,1,1,1,1,1,1,1
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 1,1,1,1,1,1,1
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

		.byte 5,5,5,5,5,5,5,5,5
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 5,5,5,5,5,5,5
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

		.byte 3,3,3,3,3,3,3,3,3
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 3,3,3,3,3,3,3
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

		.byte 7,7,7,7,7,7,7,7,7
		.byte 7,7,15,15,12,12,11,11,11,1,1,2,2,2,9,9
		.byte 8,8,10,10,7,7,13,13,15,15
		.byte 7,7,7,7,7,7,7
		.byte 7,7,15,15,12,12
		.byte 11,11,11,1,1,6,6,6,4,4,14,14,3,3,15,15

	colorTable2:

		.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b
		.byte $0b,$0b,$0c,$0c,$0c,$0c,$0c,$0f
		.byte $0f,$03,$03,$07,$01,$01,$01,$01
		.byte $01,$07,$03,$03,$0c,$0c,$0b,$0b
		.byte $0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b

colortable3:
        
		.byte 2,2,2,2
        .byte 8,8,8,8
		.byte $a,$a,$a,$a
        .byte 7,7,7,7
        .byte 7,7,7,7
		.byte $a,$a,$a,$a
        .byte 8,8,8,8
        .byte 2,2,2,2

      
colortable3end:
               

colortable4:  
		.byte $b,$b
		.byte $c,$c
		.byte $f,$f,$f
		.byte 1,1
		.byte $f,$f,$f
		.byte $c,$c
		.byte $b,$b

logocolortab:
		.byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
		.byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f
		.byte $c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c
		.byte $b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b
		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

/* 
		.byte 6,6
		.byte $e,$e
		.byte 3,3,3
		.byte 1
		.byte 1
		.byte 1
		.byte 3,3,3
		.byte $e,$e
		.byte 6,6
		.byte 6,6 */

colortable4end:
       

	framecounterx:
		.byte 0

	framecounterx2:
		.byte 0

	framecountery:
		.byte 0

	spriteoffsettable:
		.byte 0,20,40,60,80

	spritecounter:
		.byte 0
        
stored016:
	.byte 0	

textno:	
	.byte 0

textcount:	
	.byte 0

text:
	.text "     $% retrojen presents $%     raat#06: 'the four horsemen of the apocalypse'     "
	.text "28/29 december 2019 - retrojen headquarters / karakoy / istanbul / turkey     "
	.text "the last meeting of 2019. organization: $% alcofribas $%     "
	.text "alcofribas on the keys... prepare yourself for quick seminars, demo talks, "
	.text "8 bit computers and consoles, surprize game contests, basic, assembler, c "
	.text "and even fpga workshops!     last, but not least... batman demo, vespertino "
	.text "teaser, pinball dreams, more batman demo. as you know, there is no such "
	.text "thing as an ugly girl, just not enough batman demo!      we are always seeking for fresh "
	.text "stuff. don't forget to bring your code, pixelart, demo, intro, $%robe$%, retro "
	.text "gear, joysticks, dinner, drinks and bed with you!      thanks for watching and " 
	.text "see you at the meeting! "
	.text "so... what about raat? what is raat???      people are shocked to discover that "
	.text "this kind of incredibly mysterious question has a non-mysterious answer. "
	.text "as you may know, mystery is a property of questions, not answers.     "
	.text "raat is an abbreviation of ''retrojen akil adamlar toplantisi''' and means "
	.text "''retrojen wise men meeting''. yes, it's that simple.      but... oh my dear! you "
	.text "come across a new, mysterious question now. what could it be?      yes, you're "
	.text "right! what's retrojen? does this ring any bells for anyone? the scenerman "
	.text "always rings twice :) :) :)     retrojen is a retro computer focused "
	.text "group making annual meetings, forum & hardcopy fanzin. and now... the youtube "
	.text "channel is on the way! stay tuned for details!"
	.text "by the way, it called me awake to organise raat, to give thanks to our "
	.text "predecessor 7dx parties for their presence. without them we wouldn't have "
	.text "raat here today.     last, but not least :) thanks to our precious 3d & pixel-art "
	.text "guru f3r0 for this nice invitro, which is also his first scene release.      "
	.text "are you still watching?      what are you waiting for???      $% the four horsemen are "
	.text "waiting for you at retrojen hq $%     take your backpack and come to visit us!"

	.fill 40,$20
	.byte $ff


text01:
.text "UNITY.EXE"
.fill 31,$20    ////f = $06 - screen koda cevirdi.
.byte 0

text02:
.text "UNREAL ENGINE LAUNCHER.EXE"
.fill 14,$20
.byte 0

text03:
.text "LOAD"
.text @"\"SINUSTABLES.BIN\""
.fill 3,$20
.text ":P"
.fill 24,$20
.byte 0

text04:
.text "?SYNTAX  ERROR"
.fill 26,$20
.text "READY."
.fill 34,$20
.byte 0

textpro:
.text "OK... NWM..."
.fill 28,$20
.text "LET'S DO IT LIKE A PRO..."
.fill 15,$20
.byte 0

c64text:
.fill 44,$20
.text "**** RAAT #06 : 28.12.2019 ****"
.fill 46,$20
.text "RAATHQ - MAKEREVI, ISTANBUL / BEYOGLU  "
.fill 40,$20
.text "READY."
.byte 0



message:

.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen 11111111 Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN"
.text "Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen RETROJEN Retrojen asdasdas"


//*=$c000
creditstxt:
.text @"  YOU ARE INVITED!  "
.text @"  YOU ARE INVITED!  "
.text @"-====AdaminBiri====-"
.text @"-=======AGY========-"
.text @"-=====Akermen======-"
.text @"-====Alcofribas====-"
.text @"-======Alpyre======-"
.text @"-======AmonR=======-"
.text @"-======Arcane======-"
.text @"-=======Ari========-"
.text @"-======Astron======-"
.text @"-=====Attilan======-"
.text @"-=======Axon=======-"
.text @"-=======Bager======-"
.text @"-=======Beast======-"
.text @"-=====Blockmind====-"
.text @"-======Caisson=====-"
.text @"-====Cengizermis===-"
.text @"-====codewarrior===-"
.text @"-=======Coze=======-"
.text @"-=======Curt=======-"
.text @"-======Datura======-"
.text @"-======doMiNO======-"
.text @"-=======Drey=======-"
.text @"-=======eins=======-"
.text @"-=======Endo=======-"
.text @"-======ExtMode=====-"
.text @"-=======F3R0=======-"
.text @"-=====fullgrim=====-"
.text @"-=====function=====-"
.text @"-======Gaddar======-"
.text @"-=======Geos=======-"
.text @"-=====gibraltar====-"
.text @"-=======Hades======-"
.text @"-=====Hydrogen=====-"
.text @"-=====Impetigo=====-"
.text @"-======i.r.on======-"
.text @"-======IlkerG======-"
.text @"-=======ilky=======-"
.text @"-=IllcareBarrelers=-"
.text @"-=======joker======-"
.text @"-======Madcat======-"
.text @"-======matahari====-"
.text @"-======Memrah======-"
.text @"-=MineCrafter6860==-"
.text @"-=====modelist=====-"
.text @"-====Nightlord=====-"
.text @"-======Norvax======-"
.text @"-=====overkill=====-"
.text @"-======ozkano======-"
.text @"-======Peacer======-"
.text @"-=====Perpetual====-"
.text @"-======Ragnor======-"
.text @"-========Ref=======-"
.text @"-====Retromaster===-"
.text @"-=====Savagery=====-"
.text @"-=======Shax=======-"
.text @"-=======Skate======-"
.text @"-======spritus=====-"
.text @"-========SSG=======-"
.text @"-=======Senol======-"
.text @"-=====TTalayman====-"
.text @"-=======Vigo=======-"
.text @"-======Wisdom======-"
.text @"-====Witchdoktor===-"
.text @"-====Wizardofwar===-"
.text @"-======Wizofwor====-"
.text @"-======Wolfiem=====-"
.text @"-======YavuzG======-"
.text @"-=======Zer0=======-"
.text @"                    "
.text @"   ARE YOU READY?   "
.text @"HERE COMES THE SOLO!"
.text @"\$00"
.byte 0
.align $ff
