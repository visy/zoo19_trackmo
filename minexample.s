
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

	        jsr install

			ldx #<screen1
			ldy #>screen1
			jsr loadraw

			ldx #<screen2
			ldy #>screen2
			jsr loadraw

			ldx #<screen3
			ldy #>screen3
			jsr loadraw

		    lda #<irq_vector    ; Set IRQ vector to be called
		    sta $0314           ; Once per screen refresh
		    lda #>irq_vector
		    sta $0315           ; The $ 314 / $ 315 vector points to the IRQ raster routine

		    lda #$00
		    jsr $1600           ; Initialize sid to play song 0

		    cli                 ; Enable interrupts again

			lda #0
			sta $ff19 ; border


		    lda #%00110000 ; mc, bitmap
			sta $ff06

		    lda #%00011000
			sta $ff07

			lda #%00001000
			sta $ff12

			lda #%00000000 ; screen at 4*$400
			sta $ff13

			lda #%00001000 ; vidram at $0800
			sta $ff14

		 	lda #0
		 	sta $ff15 ; bgcolor
			sta $ff3e


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

            

dosign:
	lda #0
	sta $ff19 ; border
	sta talkinit

	lda signinit
	cmp #1
	beq signdone
	ldx #5
	lda tedvidoffs,x
	clc
	sta $ff12

    ldx #<filename2
    ldy #>filename2
    jsr loadraw

    ldx #<filename1
    ldy #>filename1
    jsr loadraw

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
	and #64

signadd:
	sta $0878,x
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

	iny
	cpy #18
	bne signyloop

	lda #$08
	sta signadd+2
	sta wordval+1
	lda #$78
	sta signadd+1
	sta wordval+0

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
	beq dotalk2

	lda #1
	sta talkinit

		    lda #%00100000 ; screen off
			sta $ff06


	ldx #<color1
	ldy #>color1
	jsr loadraw


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
	lda #$c0
	sta memcpySrc+1
	jsr memcpy


	ldx #1
	lda tedvidoffs,x
	clc
	sta $ff12

		    lda #%00110000 ; mc, bitmap
			sta $ff06

	ldx #<screen4
	ldy #>screen4
	jsr loadraw

	ldx #<color2
	ldy #>color2
	jsr loadraw

	ldx #<color3
	ldy #>color3
	jsr loadraw

	ldx #<color4
	ldy #>color4
	jsr loadraw

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
	asl $ff09

	inc frame

	inc partframes

	lda partframes
	cmp #0
	bne no_pattinc
	inc partpatts

	ldx demopart
	lda partpattlen,x
	cmp partpatts
	bne nopartadd
	inc demopart
	lda #0
	sta partframes
	sta partpatts

nopartadd:
no_pattinc:

	; tick and output to ted
    jsr $1603
	jsr $1606

    jmp $ce0e           ; Exit interrupt


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
	sei
	sta $ff3f

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
	sta $ff3e
	cli
	rts

frame: .byte 0
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

ptr: .word 0

partpattlen: .byte 5,7,5,7 
partpattextra: .byte 240,255,128,255
demoparts: .word dosign, dotalk, dosign, dotalk

signinit: .byte 0

partframes: .byte 0
partpatts: .byte 0
demopart: .byte 0

wordval: .word $0878

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
