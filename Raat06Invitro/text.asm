
.const Start 		= $080d


* = $0801 "Basic"
	BasicUpstart(Start)

* = Start "Code"



cloopreset: ldx #0
creditsloop:    
            lda credits,x
            sta $040e+20*40,x
            inx
            cpx #17
            bne creditsloop

colcyc:
            lda colortable3
            sta colortable3+128
            ldx #$0
coloop:     lda colortable3+1,x 
            sta colortable3,x      
            sta $d80e+20*40,x
            inx
            cpx #128
            bne coloop
            jmp colcyc



credits:
.text "illcare barrelers"
.text "1sdasdas asd asd "
.text "2llcare asrrelers"
.text "3llcfaasdasrrelrs"
.text "4llcarefffffffers"

	colortable3:


		.byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
        .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
        .byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
        .byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
        .byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
        .byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8
        .byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
        .byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
         