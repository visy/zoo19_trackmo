
screen     = $5800
bitmap     = $6000

memcpySrc = $f4
memcpyDst = $f6
memcpyLen = $f8


.include "../../build/loadersymbols-c16.inc"

.include "standard.inc"
.include "ted.inc"

one_bits   = COLOUR_DARKGREY
zero_bits  = COLOUR_MEDIUMGREY

            .org $100d

			sta $ff3e

			lda #0
			sta $ff15
			sta $ff19 ; border

		    lda #%00100000 ; screen off
			sta $ff06

			lda #%00000000 ; screen at 4*$400
			sta $ff13

			lda #%00001000 ; vidram at $0800
			sta $ff14

		    lda #%00010000 ; ted stop
			sta $ff07 ; ted stop

	        jsr install

	        sei

	        sta $ff3f


			ldx #<quadco
			ldy #>quadco
			jsr loadcompd

			ldx #<quadsc
			ldy #>quadsc
			jsr loadcompd

			ldx #5
			lda tedvidoffs,x
			clc
			sta $ff12

		    lda #%00001000 ; hires ted on
			sta $ff07

		    lda #%00110000 ; no blank, bitmap
			sta $ff06

			ldx #<screen2
			ldy #>screen2
			jsr loadcompd

			ldx #<screen3
			ldy #>screen3
			jsr loadcompd

			ldx #<screen4
			ldy #>screen4
			jsr loadcompd

		    lda #%00100000 ; screen off
			sta $ff06

		    lda #<irq_vector    ; Set IRQ vector to be called
		    sta $FFFE           ; Once per screen refresh
		    lda #>irq_vector
		    sta $FFFF           ; The $ 314 / $ 315 vector points to the IRQ raster routine

		    lda #$00
		    jsr $1600           ; Initialize sid to play song 0

		    cli                 ; Enable interrupts again





mainloop:

			lda demopart
			asl
			tax
			lda demoparts+1,x ; high byte first
			pha
			lda demoparts,x
			pha
			php ; RTI expects processor flags on top.
			rti

            jmp mainloop

error:      ldx #COLOUR_BLACK
:           sta BORDERCOLOUR
            stx BORDERCOLOUR
            jmp :-

logoinit: .byte 0

dologo:
	lda logoinit
	cmp #1
	beq dologo2


    lda #%00100000 ; screen off
	sta $ff06

	lda #$0
	sta $ff19 ; border
 	sta $ff15 ; bgcolor

    lda #%00010000 ; ted stop
	sta $ff07 ; ted stop

	ldx #<logoco
	ldy #>logoco
	jsr loadcompd

	ldx #<logosc
	ldy #>logosc
	jsr loadcompd


	lda #$d0
	sta $ff12

    lda #8 ; mc
	sta $ff07
    lda #$3b ; no blank, bitmap
	sta $ff06

	lda #1
	sta logoinit

dologo2:

	lda logoinit
	cmp #2
	beq nologofade

	ldy #200
yloop:
	lda #11
	clc
    sbc partframes2
    ror
    ror
    ror
    sta x1

	lda $ff1e
	and #7
	and x1
	sta x2
	lda #0
	clc
	adc x2
	sta $ff07

logowiper:

donelogoy:

	dey
	cpy #0
	bne yloop

nologofade:

	lda partpatts
	cmp #1
	bne nologowipe


	inc logowipe2
	lda logowipe2
	cmp #48
	bne nologowipe

	inc logowipe

	lda #0
	sta logowipe2

	ldx #00
nologowipe2:
	txa
	tay
	ldx logowipe
	lda fadetab,x
	cmp #0
	bne nologoblank

    lda #%00100000 ; screen off
	sta $ff06

nologoblank:


	sta x1
	tya
	tax
	lda x1
	sta $0800,x
	sta $0900,x
	sta $0a00,x
	sta $0b00,x

	inx
	cpx #0
	bne nologowipe2

nologowipe:

	jmp mainloop

fadetab: .byte $ef,$ee,$dd,$cc,$bb,$aa,$99,$88,$00,$00,$00,$00,$00,$00

logowipe: .byte 0
logowipe2: .byte 0


patientinit: .byte 0

dopatient:
	lda patientinit
	cmp #1
	beq dopatient2


    lda #%00100000 ; screen off
	sta $ff06

	lda #$f1
	sta $ff19 ; border
 	sta $ff15 ; bgcolor

    lda #%00010000 ; ted stop
	sta $ff07 ; ted stop

	ldx #<halpco
	ldy #>halpco
	jsr loadcompd

	ldx #<halpsc
	ldy #>halpsc
	jsr loadcompd


	lda #$d0
	sta $ff12

    lda #8 ; mc
	sta $ff07
    lda #$3b ; no blank, bitmap
	sta $ff06


	lda #1
	sta patientinit

dopatient2:
	ldy #200
yloop2:
	lda #2
    sta x1

	lda $ff1e
	and #7
	and x1
	sta x2
	lda #0
	clc
	adc x2
	sta $ff07

	dey
	cpy #0
	bne yloop2


	jmp mainloop

signcolors: .byte $5c,$1b,$7a,$d3,0
signcolor: .byte 0

dosign:
	lda #0
	sta talkinit

	lda signinit
	cmp #1
	beq signdone

    lda #%00100000 ; screen off
	sta $ff06

    ldx #<filename2
    ldy #>filename2
    jsr loadcompd

    ldx #<filename1
    ldy #>filename1
    jsr loadcompd

	ldx #1
	lda tedvidoffs,x
	clc
	sta $ff12

    lda #%00011000 ; mc
	sta $ff07
    lda #%00110000 ; no blank, bitmap
	sta $ff06

	lda #1
	sta signinit

signdone:

	lda #<pixbuf
	sta memcpyLen
	lda #>pixbuf
	sta memcpyLen+1

	ldy #0
signyloop:
	ldx #0
signxloop:
	stx xt
	tya
	tax
	lda xl1,x
	sta x1
	ldx xt
	clc
	cpx x1
	sty x1
	bcs nocross3

	tya
	tax
	lda xl2,x
	sta x2
	ldx xt
	clc
	cpx x2
	bcc nocross3

	jsr next_rnd

	ldy #0
	lda (memcpyLen),y
	ldx xt

	; kierto
	clc
	adc frame
	and frame3

signadd:
	sta $0878,x
	ldy partpatts
	lda signcolors,y
signadd2:
	sta $0c78,x
nocross:

nocross3:
	inc memcpyLen
	lda memcpyLen
	cmp #0
	bne nocross2
	inc memcpyLen+1
nocross2:

	ldy x1

	inx
	cpx #40
	bne signxloop

add:
	clc		
	lda wordval+0
	adc #40
	sta wordval+0
	bcc ok             
	inc wordval+1
ok:
	lda wordval+0
	sta signadd+1
	lda wordval+1
	sta signadd+2

add2:
	clc		
	lda wordval2+0
	adc #40
	sta wordval2+0
	bcc ok2
	inc wordval2+1
ok2:
	lda wordval2+0
	sta signadd2+1
	lda wordval2+1
	sta signadd2+2

	iny
	cpy #18
	bne signyloop22

	jmp signyexit

signyloop22:
	jmp signyloop

signyexit:

	lda #$08
	sta signadd+2
	sta wordval+1
	lda #$78
	sta signadd+1
	sta wordval+0

	lda #$0c
	sta signadd2+2
	sta wordval2+1
	lda #$78
	sta signadd2+1
	sta wordval2+0

	jsr next_rnd

	jmp mainloop



talkinit: .byte 0

dotalk:
	lda #0
	sta signinit
;;;;;;;;;;;;;;; talk anim
	jsr next_rnd

	lda talkinit
	cmp #1
	beq dotalk3
	jmp talkinitor


dotalk3:
	jmp dotalk2

talkinitor:

    lda #%00100000 ; screen off
	sta $ff06

	lda #$0
 	sta $ff15 ; bgcolor
	sta $ff19 ; border

	ldx #<color4
	ldy #>color4
	jsr loadcompd

	lda #$00
	sta memcpyDst
	lda #$08
	sta memcpyDst+1

	lda #$0
	sta memcpyLen
	lda #$08
	sta memcpyLen+1
	lda #0
	sta memcpySrc
	lda #$d8
	sta memcpySrc+1
	jsr memcpy

	ldx #4
	lda tedvidoffs,x
	clc
	sta $ff12

    lda #%00011000 ; mc
	sta $ff07
    lda #%00110000 ; no blank, bitmap
	sta $ff06

	ldx #<color1
	ldy #>color1
	jsr loadcompd

	ldx #<color2
	ldy #>color2
	jsr loadcompd

	ldx #<color3
	ldy #>color3
	jsr loadcompd

	ldx #<screen1
	ldy #>screen1
	jsr loadcompd

	lda #0
	sta frame

	lda #1
	sta talkinit

	jmp mainloop

dotalk2:
	lda frame
	cmp #6
	bne no_switch3
	jmp check
no_switch3:
	jmp no_switch
check:
	lda #0
	sta frame

	inc animframe
	lda animframe
	cmp #2
	bne no_switch
	lda #0
	sta animframe

no_switch2:

	lda #$00
	sta memcpyDst
	lda #$08
	sta memcpyDst+1

	lda #$0
	sta memcpyLen
	lda #$08
	sta memcpyLen+1

	lda rnd
	and #3
	clc
	adc #1
	tax
	lda tedvidoffs,x
	clc
	sta $ff12

	dex

	cpx #0
	beq tf1
	cpx #1
	beq tf2
	cpx #2
	beq tf3
	cpx #3
	beq tf4
	jmp no_switch

tf1:
	lda #0
	sta memcpySrc
	lda #$c0
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch
tf2:
	lda #0
	sta memcpySrc
	lda #$c8
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch
tf3:
	lda #0
	sta memcpySrc
	lda #$d0
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch
tf4:
	lda #0
	sta memcpySrc
	lda #$d8
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch

no_switch:

	lda $ff1d ; scanline num
	;sta $ff19 ; border

;;;;;;;;;;;;;;; talk anim end
	jmp mainloop


irq_vector:
	pha
	txa
	pha
	tya
	pha

	asl $ff09

	inc frame

	inc frame2
	lda frame2
	cmp #128
	bne notframe2
	lda frame3
	clc
	adc #8
	sta frame3
	lda #0
	sta frame2
notframe2:

	inc partframes3
	lda partframes3
	cmp #8
	bne nopf3
	lda #0
	sta partframes3
	inc partframes2
nopf3:

	inc partframes

	lda partframes
	cmp #0
	bne no_pattinc
	inc partpatts

no_pattinc:
	ldx demopart
	lda partpattlen,x
	cmp partpatts
	bne nopartadd

	inc extracount

	lda partpattextra,x
	cmp extracount
	bne nopartadd

	inc demopart
	lda #0
	sta partframes
	sta partpatts
	sta extracount
nopartadd:


	; tick and output to ted
    jsr $1603
	jsr $1606

    pla
    tay
    pla
    tax
    pla
    rti

rnd:  .byte 0

next_rnd:
	lda rnd
	rol
	rol
	clc
	adc rnd      ; A = RND * 5
	clc
	adc #17      ; A = RND * 5 + 17
	sta rnd
	rts

; Copy operation is divided in two parts. N is the total length
; * memcpyLong for N / 256 blocks
; * memcpyShort for N % 256 remaining bytes
memcpy:
memcpy2:
	ldy #0
	lda #0
	ldx memcpyLen+1
	beq memcpyShort ; We need only the short version for <1 pages
memcpyLoopLong: ; Copy X pages
	lda (memcpySrc),y ; Loop unrolling can be done with confidence here
	sta (memcpyDst),y ; any power of 2 will work
	iny
	bne memcpyLoopLong
	dex
	beq memcpyShort
	inc memcpySrc+1 ; Go to the next page
	inc memcpyDst+1
	jmp memcpyLoopLong
memcpyShort: ; Copy remaining bytes
	ldx memcpyLen
	beq memcpyEnd
memcpyLoopShort: ; Copy X bytes
	lda (memcpySrc),y
	sta (memcpyDst),y
	iny
	dex
	bne memcpyLoopShort
memcpyEnd:
	rts

frame: .byte 0
frame2: .byte 0
frame3: .byte 0
animframe: .byte 0
tedvidoffs: .byte 8,16,24,32,40,48

.res $1600 - *
.incbin "music.bin"


.res $2000 - *
.incbin "../../build/loader-c16.prg", 2

.res $2800 - *
.incbin "../../build/install-c16.prg", 2


filename1:  .asciiz "signcol"
filename2:  .asciiz "sign"

screen1: .asciiz  "screen1"
screen2: .asciiz  "screen2"
screen3: .asciiz  "screen3"
screen4: .asciiz  "screen4"

color1: .asciiz  "color1"
color2: .asciiz  "color2"
color3: .asciiz  "color3"
color4: .asciiz  "color4"

quadsc: .asciiz "quadsc"
quadco: .asciiz "quadco"

halpsc: .asciiz "halpsc"
halpco: .asciiz "halpco"

logosc: .asciiz "logosc"
logoco: .asciiz "logoco"


ptr: .word 0

partpattlen: .byte 2,4,3,4,4
partpattextra: .byte 1,1,1,1,32
demoparts: .word dologo, dosign, dopatient, dotalk, dosign

extracount: .byte 0

signinit: .byte 0

partframes: .byte 0
partframes2: .byte 0
partframes3: .byte 0

partpatts: .byte 0
demopart: .byte 0

wordval: .word $0878
wordval2: .word $0c78

xl1: .byte 23,23,23,23,23,23,29,29,29,29,29,29,23,23,23,23,23,23

xl2: .byte 17,17,17,17,17,17,11,11,11,11,11,11,17,17,17,17,17,17

x1: .byte 0
x2: .byte 0 
xt: .byte 0

lumavals: .byte 32,64,128,82,14,55,191

pixbuf: ; gradient
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,42,42,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,110,0,48,48,0,110,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,0,0,65,65,65,65,0,0,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,40,42,48,65,123,123,65,48,42,40,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,40,42,48,65,123,123,65,48,42,40,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,0,0,65,65,65,65,0,0,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,110,0,48,48,0,110,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,42,42,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

