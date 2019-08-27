
memcpySrc = $f4
memcpyDst = $f6
memcpyLen = $f8

.include "../../build/loadersymbols-c16.inc"

    .macro MEMDECOMP_TO source_lo, source_hi, dest_lo, dest_hi
            sec
            lda dest_lo
            sta decdestlo
            lda dest_hi
            sta decdesthi
            ldx source_lo
            ldy source_hi
            jsr memdecomp
    .endmacro

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

			lda #%00001000 ; color at $0800
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

		    lda #<irq_vector    ; Set IRQ vector to be called
		    sta $FFFE           ; Once per screen refresh
		    lda #>irq_vector
		    sta $FFFF           ; The $ 314 / $ 315 vector points to the IRQ raster routine

		    lda #$00
		    jsr $1600           ; Initialize sid to play song 0


		    cli                 ; Enable interrupts again

do_yclr:
	ldx #0
yclr:
	lda #$e1
	sta $0800,x
	sta $0900,x
	sta $0a00,x
	cpx #112
	bcs nobig2
	sta $0b00,x
nobig2:
	sta $0c00,x
	sta $0d00,x
	sta $0e00,x
	cpx #112
	bcs nobig3
	sta $0f00,x
nobig3:
	lda #$ff
	and $4001,x
	sta $4000,x

	lda #$ff
	and $4101,x
	sta $4100,x
	lda #$ff
	and $4201,x
	sta $4200,x
	lda #$ff
	and $4301,x
	sta $4300,x
	lda #$ff
	and $4401,x
	sta $4400,x

	sta $4500,x
	sta $4600,x
	sta $4700,x
	sta $4800,x
	sta $4900,x
	sta $4a00,x
	sta $4b00,x
	sta $4c00,x
	sta $4d00,x
	sta $4e00,x
	sta $4f00,x
	sta $5000,x
	sta $5100,x
	sta $5200,x
	sta $5300,x
	sta $5400,x
	sta $5500,x
	sta $5600,x
	sta $5700,x
	sta $5800,x
	sta $5900,x
	sta $5a00,x
	cpx #128
	bcs nobig
	sta $5b00,x
nobig:
	inx
	cpx #0
	bne yclr2

	jmp mainloop

yclr2:
	jmp yclr

mainloop:

			lda changesong
			cmp #1
			bne nochangesong

			sei
			lda #0
			sta changesong

			lda #0
			sta $ff11

			ldx #<music2
			ldy #>music2
			jsr loadcompd

		    lda #$00
		    jsr $1600           ; Initialize sid to play song 0

			cli

nochangesong:

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



pointX =*                   ;0-319
.word 0

pointY =*                   ;0-199
.byte 100

drawmode =*                 ;0 = erase point, 1 =set point
.byte 1

screen = $4000              ;for example
dest = $eb

;--------------

plot:

;-------------------------
;calc Y-cell, divide by 8
;y/8 is y-cell table index
;-------------------------
lda pointY
lsr                         ;/ 2
lsr                         ;/ 4
lsr                         ;/ 8
tay                         ;tbl_8,y index

;------------------------
;calc X-cell, divide by 8
;divide 2-byte pointX / 8
;------------------------
ror pointX+1                ;rotate the high byte into carry flag
lda pointX
ror                         ;lo byte / 2 (rotate C into low byte)
lsr                         ;lo byte / 4
lsr                         ;lo byte / 8
tax                         ;tbl_8,x index

;----------------------------------
;add x & y to calc cell point is in
;----------------------------------
clc

lda tbl_vbaseLo,y           ;table of screen row base addresses
adc tbl_8Lo,x               ;+ (8 * Xcell)
sta dest                    ;= cell address

lda tbl_vbaseHi,y           ;do the high byte
adc tbl_8Hi,x
sta dest+1

;---------------------------------
;get in-cell offset to point (0-7)
;---------------------------------
lda pointX                  ;get pointX offset from cell topleft
and #%00000111              ;3 lowest bits = (0-7)
tax                         ;put into index register

lda pointY                  ;get pointY offset from cell topleft
and #%00000111              ;3 lowest bits = (0-7)
tay                         ;put into index register

;----------------------------------------------
;depending on drawmode, routine draws or erases
;----------------------------------------------

lda drawmode                ;(0 = erase, 1 = set)
beq erase                   ;if = 0 then branch to clear the point

;---------
;set point
;---------
lda (dest),y                ;get row with point in it
ora tbl_orbit,x             ;isolate and set the point
sta (dest),y                ;write back to screen
jmp past                    ;skip the erase-point section

;-----------
;erase point
;-----------
erase:                    ;handled same way as setting a point
lda (dest),y                ;just with opposite bit-mask
and tbl_andbit,x            ;isolate and erase the point
sta (dest),y                ;write back to screen

past:
rts


meminit:
	.byte 0


domem:

	lda meminit
	cmp #1
	beq nomeminit


	ldx #<tekstico
	ldy #>tekstico
	jsr loadcompd

	ldx #<tekstisc
	ldy #>tekstisc
	jsr loadcompd

    lda #8 ; mc
	sta $ff07

	ldx #1
	lda tedvidoffs,x
	clc
	sta $ff12

    lda #$3b ; no blank, bitmap
	sta $ff06

	lda #1
	sta meminit

nomeminit:
	ldy #0
do_memxl:
	ldx #0
memxl:

	stx x2
	sty x1

	cpx #64
	bne noexit
	cpy #1
	bne noexit
	jmp exitmem
noexit:
	stx pointX
	sty pointX+1

	lda $ff1d
	adc pointX
	and #127
	sta pointY

	jsr plot

	ldx x2
	ldy x1

	inx
	cpx #0
	bne memxl2


	iny
	cpy #2
	bne do_memxl

@nonewframe:
	lda $ff1d
	cmp #0
	bne @nonewframe      

exitmem:
	jmp do_yclr

memxl2:
	jmp memxl


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


	ldx #<logosc
	ldy #>logosc
	jsr loadcompd


	ldx #<logoco
	ldy #>logoco
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

    lda #%00011000 ; ted stop
	sta $ff07 ; ted stop

	;; extra 2 = show pill
	ldx demopart
	lda partpattextra,x
	cmp #2
	beq patientspecialinit

	jmp patient0

patientspecialinit:

	lda #$00
	sta $ff19 ; border
	lda #$80
 	sta $ff15 ; bgcolor

	ldx #<pillco
	ldy #>pillco
	jsr loadcompd


	ldx #<pillsc
	ldy #>pillsc
	jsr loadcompd

    lda #%00011000 ; mc
	sta $ff07


	jmp patmutual

patient0:

	lda #$f1
	sta $ff19 ; border
 	sta $ff15 ; bgcolor

	ldx #<halpco
	ldy #>halpco
	jsr loadcompd

	ldx #<halpsc
	ldy #>halpsc
	jsr loadcompd

    lda #8 ; mc
	sta $ff07

patmutual:

	ldx #1
	lda tedvidoffs,x
	clc
	sta $ff12

    lda #$3b ; no blank, bitmap
	sta $ff06


	lda #1
	sta patientinit

dopatient2:

	ldx demopart
	lda partpattextra,x
	cmp #2
	beq patientspecial


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

patientspecial:
	jmp mainloop

signcolors: .byte $5c,$1b,$7a,$d3,$5c,$1b,$7a,$d3,0
signcolor: .byte 0

dosign:
	lda signinit
	cmp #1
	beq signdone

	lda #0
	sta talkinit
    lda #%00100000 ; screen off
	sta $ff06

	lda #0
	sta $ff19
	sta frame
	lda #1
	sta frame3

    ldx #<filename2
    ldy #>filename2
    jsr loadcompd

    ldx #<filename1
    ldy #>filename1
    jsr loadcompd

    lda #0
    sta partpatts


	ldx #1
	lda tedvidoffs,x
	clc
	sta $ff12


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

	lda signinit
	cmp #1
	beq nosignon

    lda #%00011000 ; mc
	sta $ff07
    lda #%00110000 ; no blank, bitmap
	sta $ff06

	lda #1
	sta signinit
nosignon:

	jmp mainloop



talkinit: .byte 0


dotalkandrun:
	lda #0
	sta signinit
	sta patientinit
;;;;;;;;;;;;;;; talk anim
	lda talkinit
	cmp #1
	beq dotalk3
	jmp talkinitor

dotalk3:
	jsr next_rnd

	jmp dotalk2

talkinitor:

    lda #%00100000 ; screen off
	sta $ff06

	ldx demopart
	lda partpattextra,x
	cmp #2
	beq initrun


	lda #0
	sta frame

	inc talkinit

	jmp runinitdone

initrun:

	lda #$f1
	sta $ff19 ; border
	lda #$0
 	sta $ff15 ; bgcolor

	ldx #<runco
	ldy #>runco
	jsr loadcompd

	ldx #<runsc
	ldy #>runsc
	jsr loadcompd


	inc talkinit

	lda #0
	sta frame

	jmp runinitdone

norun:



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
	bne no_switch3
	lda #0
	sta animframe

no_switch2:

runinitdone:

	lda #$00
	sta memcpyDst
	lda #$08
	sta memcpyDst+1

	lda #$0
	sta memcpyLen
	lda #$08
	sta memcpyLen+1

	lda talkinit
	cmp #1
	beq runlogic

	lda rnd
	and #3
	clc
	adc #2
	tax

	jmp talklogicdone

runlogic:

	lda partframes2
	and #3
	clc
	adc #2
	tax

talklogicdone:

	lda tedvidoffs,x
	clc
	sta $ff12

	dex
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
	lda #$40
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch4
tf2:
	lda #0
	sta memcpySrc
	lda #$48
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch4
tf3:
	lda #0
	sta memcpySrc
	lda #$50
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch4
tf4:
	lda #0
	sta memcpySrc
	lda #$58
	sta memcpySrc+1
	jsr memcpy
	jmp no_switch4

no_switch4:

	ldx demopart
	lda partpattextra,x
	cmp #1
	beq mctalk

	lda #%00001000 ; hires ted on
	sta $ff07

	jmp no_switch
mctalk:
    lda #%00011000 ; mc
	sta $ff07

no_switch:
    lda #%00110000 ; no blank, bitmap
	sta $ff06

;;;;;;;;;;;;;;; talk anim end
	jmp mainloop



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

animframe: .byte 0
tedvidoffs: .byte 8,16,24,32,40,48,56

.res $1600 - *
.incbin "music.bin"

.res $2300 - *
.incbin "../../build/install-c16.prg", 2

.res $2f00 - *
.incbin "../../build/loader-c16.prg", 2



filename1:  .asciiz "signcol"
filename2:  .asciiz "sign"

cpack: .asciiz  "cpack"

runsc: .asciiz  "runsc"
runco: .asciiz  "runco"

quadsc: .asciiz "quadsc"
quadco: .asciiz "quadco"

halpsc: .asciiz "halpsc"
halpco: .asciiz "halpco"

logosc: .asciiz "logosc"
logoco: .asciiz "logoco"

pillsc: .asciiz "pillsc"
pillco: .asciiz "pillco"

music2: .asciiz "music2"

tekstisc: .asciiz "tekstisc"
tekstico: .asciiz "tekstico"


signinit: .byte 0

changesong: .byte 0

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
	ldx demopart
	lda partpattextra,x
	cmp #254
	bne nomusicchangetoggle

	lda #1
	sta changesong

nomusicchangetoggle:	


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

ptr: .word 0

frame: .byte 0
frame2: .byte 0
frame3: .byte 0
partpattlen: .byte 2,2,2,3,6,4,4
partpattextra: .byte 1,1,1,2,254,1,2
demoparts: .word  dologo, domem, dopatient, dotalkandrun, dosign, dorun, dopatient

extracount: .byte 0
partframes: .byte 0
partframes2: .byte 0
partframes3: .byte 0

partpatts: .byte 0
demopart: .byte 0

;----------------------------------------------------------------

tbl_vbaseLo:
.byte <(screen+(0*320)),<(screen+(1*320)),<(screen+(2*320)),<(screen+(3*320))
.byte <(screen+(4*320)),<(screen+(5*320)),<(screen+(6*320)),<(screen+(7*320))
.byte <(screen+(8*320)),<(screen+(9*320)),<(screen+(10*320)),<(screen+(11*320))
.byte <(screen+(12*320)),<(screen+(13*320)),<(screen+(14*320)),<(screen+(15*320))
.byte <(screen+(16*320)),<(screen+(17*320)),<(screen+(18*320)),<(screen+(19*320))
.byte <(screen+(20*320)),<(screen+(21*320)),<(screen+(22*320)),<(screen+(23*320))
.byte <(screen+(24*320))

tbl_vbaseHi:
.byte >(screen+(0*320)),>(screen+(1*320)),>(screen+(2*320)),>(screen+(3*320))
.byte >(screen+(4*320)),>(screen+(5*320)),>(screen+(6*320)),>(screen+(7*320))
.byte >(screen+(8*320)),>(screen+(9*320)),>(screen+(10*320)),>(screen+(11*320))
.byte >(screen+(12*320)),>(screen+(13*320)),>(screen+(14*320)),>(screen+(15*320))
.byte >(screen+(16*320)),>(screen+(17*320)),>(screen+(18*320)),>(screen+(19*320))
.byte >(screen+(20*320)),>(screen+(21*320)),>(screen+(22*320)),>(screen+(23*320))
.byte >(screen+(24*320))

tbl_8Lo:
.byte <(0*8),<(1*8),<(2*8),<(3*8),<(4*8),<(5*8),<(6*8),<(7*8),<(8*8),<(9*8)
.byte <(10*8),<(11*8),<(12*8),<(13*8),<(14*8),<(15*8),<(16*8),<(17*8),<(18*8),<(19*8)
.byte <(20*8),<(21*8),<(22*8),<(23*8),<(24*8),<(25*8),<(26*8),<(27*8),<(28*8),<(29*8)
.byte <(30*8),<(31*8),<(32*8),<(33*8),<(34*8),<(35*8),<(36*8),<(37*8),<(38*8),<(39*8)

tbl_8Hi:
.byte >(0*8),>(1*8),>(2*8),>(3*8),>(4*8),>(5*8),>(6*8),>(7*8),>(8*8),>(9*8)
.byte >(10*8),>(11*8),>(12*8),>(13*8),>(14*8),>(15*8),>(16*8),>(17*8),>(18*8),>(19*8)
.byte >(20*8),>(21*8),>(22*8),>(23*8),>(24*8),>(25*8),>(26*8),>(27*8),>(28*8),>(29*8)
.byte >(30*8),>(31*8),>(32*8),>(33*8),>(34*8),>(35*8),>(36*8),>(37*8),>(38*8),>(39*8)

tbl_orbit:
.byte %10000000
.byte %01000000
.byte %00100000
.byte %00010000
.byte %00001000
.byte %00000100
.byte %00000010
.byte %00000001

tbl_andbit:
.byte %01111111
.byte %10111111
.byte %11011111
.byte %11101111
.byte %11110111
.byte %11111011
.byte %11111101
.byte %11111110 

runinit:
	.byte 0

runindex:
	.byte 0

compdataoffsets:
	.word $6000, $6735, $69BB, $712D, $73E8, $7B65, $7E28, $859D

decdestoffsets:
	.word $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000

	;; alt to 4000 and 0800 / A000 and c000

runtimes:
	.byte 0

screenflip:
	.byte 0

dorun:

	lda runinit
	cmp #0
	bne rundo_start

    lda #%00100000 ; screen off
	sta $ff06

	ldx #<cpack
	ldy #>cpack
	jsr loadraw

	lda #1
	sta runinit

	lda #0
	sta runindex

    lda #%00011000 ; mc
	sta $ff07

	jmp mainloop

rundo_start:
rundo:

	ldx runindex

	lda decdestoffsets+1,x
	sta decdesthi

	lda decdestoffsets,x
	sta decdestlo

	lda compdataoffsets+1,x
	tay
	lda compdataoffsets,x
	tax

	stx loadaddrlo
	sty loadaddrhi

	sec
    jsr memdecomp ;; decomp to memory based on offset tables

	inc runindex
	inc runindex

	inc runtimes
	lda runtimes
	cmp #2
	bne rundo

	inc screenflip
	lda screenflip
	cmp #2
	beq flip2
flip1:
	lda #%00001000 ; color at $0800
	sta $ff14

	ldx #1 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	jmp flipdone
flip2:
	lda #%11000000 ; color at $c000
	sta $ff14

	ldx #4 ;bitmap at $a000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #0
	sta screenflip

flipdone:

	lda #$3b ; no blank, bitmap
	sta $ff06

	lda #0
	sta runtimes

	lda runindex
	cmp #16
	bne runexit

	lda #0
	sta runindex

runexit:


	jmp mainloop





