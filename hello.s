	processor 6502

memcpySrc = $f4
memcpyDst = $f6
memcpyLen = $f8

	org $0800
	incbin "color1.bin"

	org $1000

	sta $ff3e

    sei                 ; Disable interrupts

    lda #<irq_vector    ; Set IRQ vector to be called
    sta $0314           ; Once per screen refresh
    lda #>irq_vector
    sta $0315           ; The $ 314 / $ 315 vector points to the IRQ raster routine

    lda #$00
    jsr $1600           ; Initialize sid to play song 0

    cli                 ; Enable interrupts again

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

ptr: word 0

partpattlen: byte 64,1,1,1 
partpattextra: byte 240,255,128,255
demoparts: word dosign, dotalk, dosign, dotalk

signinit: byte 0

partframes: byte 0
partpatts: byte 0
demopart: byte 0

wordval: word $0878

xl1: byte 23,23,23,23,23,23,29,29,29,29,29,29,23,23,23,23,23,23

xl2: byte 17,17,17,17,17,17,11,11,11,11,11,11,17,17,17,17,17,17

x1: byte 0
x2: byte 0 
xt: byte 0

lumavals: byte 32,64,128,82,14,55,191
dosign:
	lda #0
	sta $ff19 ; border

	lda signinit
	cmp #1
	beq signdone
	ldx #5
	lda tedvidoffs,x
	clc
	sta $ff12

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
	lda #$e0
	sta memcpySrc+1
	jsr memcpy
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

pixbuf: ; gradient
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,42,42,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,110,0,48,48,0,110,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,0,0,65,65,65,65,0,0,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,40,42,48,65,123,123,65,48,42,40,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,40,42,48,65,123,123,65,48,42,40,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,0,0,65,65,65,65,0,0,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,110,0,48,48,0,110,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,42,42,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,40,40,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,0,0,0,0,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0


dotalk:
;;;;;;;;;;;;;;; talk anim
	jsr next_rnd

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
	cmp #4
	bne no_switch2
	lda #0
	sta animframe

	jmp no_switch
no_switch2:

	lda rnd
	and #3
	clc
	tax
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #$00
	sta memcpyDst
	lda #$08
	sta memcpyDst+1

	lda #$0
	sta memcpyLen
	lda #$08
	sta memcpyLen+1

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
	lda #$a0
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch
tf2:
	lda #0
	sta memcpySrc
	lda #$a8
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch
tf3:
	lda #0
	sta memcpySrc
	lda #$b0
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch
tf4:
	lda #0
	sta memcpySrc
	lda #$b8
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


printstring:
	ldx #$00
printstrlp:
	lda message,x
	cmp #0
	beq quitstr
	cmp #32         ;' ' character
	beq noconv
	cmp #33         ;! character
	beq noconv
	cmp #42         ;* character
	beq noconv
	cmp #48         ;numbers 0-9
	bcs numconv
conv:
	sec
	sbc #$40
noconv: 
	sta $0c00,x
	inx         
	bne printstrlp
quitstr:
	rts
numconv:
	cmp #58
	bcc noconv
	jmp conv

rnd:  byte 0

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
memcpyLoopLong ; Copy X pages
	lda (memcpySrc),y ; Loop unrolling can be done with confidence here
	sta (memcpyDst),y ; any power of 2 will work
	iny
	bne memcpyLoopLong
	dex
	beq memcpyShort
	inc memcpySrc+1 ; Go to the next page
	inc memcpyDst+1
	jmp memcpyLoopLong
memcpyShort ; Copy remaining bytes
	ldx memcpyLen
	beq memcpyEnd
memcpyLoopShort ; Copy X bytes
	lda (memcpySrc),y
	sta (memcpyDst),y
	iny
	dex
	bne memcpyLoopShort
memcpyEnd
	sta $ff3e
	cli
	rts


frame byte 0
animframe byte 0
tedvidoffs byte 8,16,24,32,40,48

message dc "** HELLO C16 WORLD! **",0

	org $1600
	incbin "music.bin"
	org $2000
	incbin "screen1.bin"
	org $4000
	incbin "screen2.bin"
	org $6000
	incbin "screen3.bin"
	org $8000
	incbin "screen4.bin"

	org $a000
	incbin "color1.bin"
	org $a800
	incbin "color2.bin"
	org $b000
	incbin "color3.bin"
	org $b800
	incbin "color4.bin"

	org $c000
	incbin "sign.bin"
	incbin "signcol.bin"
