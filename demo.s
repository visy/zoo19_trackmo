
memcpySrc = $f4
memcpyDst = $f6
memcpyLen = $f8

.include "../../build/loadersymbols-c16.inc"

.include "standard.inc"
.include "ted.inc"

one_bits   = COLOUR_DARKGREY
zero_bits  = COLOUR_MEDIUMGREY

.org $100d

;;;; INIT HARDWARE LOADER ETC.

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





lda #<irq_vector    ; Set IRQ vector to be called
sta $FFFE           ; Once per screen refresh
lda #>irq_vector
sta $FFFF           ; The $ 314 / $ 315 vector points to the IRQ raster routine

lda #%00000010  ;; use interrupt source: raster counter
sta $ff0a
lda #1
sta $ff0b ;; raster counter val

ldx #<logosc
ldy #>logosc
jsr loadcompdd


ldx #<logoco
ldy #>logoco
jsr loadcompdd


lda #$00
jsr $1600           ; Initialize sid to play song 0
lda #$00
jsr $2400           ; Initialize sid to play song 0


cli                 ; Enable interrupts again

jmp nocl

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START MAINLOOP
nocl:
mainloop:

;;;; SONG CHANGE LOGIC

lda changesong
cmp #1
bne nochangesong

sei
lda #0
sta changesong

lda #0
sta $ff11

lda #%00100000 ; screen off
sta $ff06

lda #%00010000 ; ted stop
sta $ff07 ; ted stop


inc biisi

cli

nochangesong:

;;;;; JUMP TO DEMOPART CODE

nop
nop
nop
nop
nop
nop
nop
nop
nop
nop

lda demopart
asl
tax
lda demoparts+1,x ; high byte first
pha
lda demoparts,x
pha
php ; RTI expects processor flags on top.
rti

;;;; MAINLOOP END

jmp mainloop

error:      ldx #COLOUR_BLACK
:           sta BORDERCOLOUR
stx BORDERCOLOUR
jmp :-


.res $1600 - *
.incbin "music3.bin"

.res $2400 - *
.incbin "music22.bin"

.res $3800 - *
pixbuf: ; gradient
.incbin "sief_sid.bin",2
.res $3b00 - *
.incbin "sief_gra.bin",2

.res $4000 - *
.incbin "../../build/install-c16.prg", 2

.res $e000 -*
logoinit: .byte 0

;;;;;;;;;;; GET/PUTPIXEL

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

;;;;;;;;;;;;;; demopart ;;;;;;;;;;;;;   MEM

meminit:
.byte 0

domem:

lda meminit
cmp #1
beq nomeminit


ldx #<tekstico
ldy #>tekstico
jsr loadcompdd

ldx #<tekstisc
ldy #>tekstisc
jsr loadcompdd

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

;;;;;;;;;;;;;; demopart ;;;;;;;;;;;;;   LOGO

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


pic3init:
	.byte 0

dopic3:
	lda pic3init
	cmp #1
	beq pic3inited

	ldx #<cred4sc
	ldy #>cred4sc
	jsr loadcompdd


	ldx #<cred4co
	ldy #>cred4co
	jsr loadcompdd

	ldx #0
copyloopx:
	lda $4800,x
	sta $0800,x
	lda $4900,x
	sta $0900,x
	lda $4a00,x
	sta $0a00,x
	lda $4b00,x
	sta $0b00,x
	lda $4c00,x
	sta $0c00,x
	lda $4d00,x
	sta $0d00,x
	lda $4e00,x
	sta $0e00,x
	lda $4f00,x
	sta $0f00,x
	inx
	cpx #0
	bne copyloopx

	lda #%00011000 ; mc
	sta $ff07

	ldx #2 ;bitmap at $6000
	lda tedvidoffs,x
	clc
	sta $ff12

	inc pic3init

pic3inited:
	jmp mainloop

pic2init:
	.byte 0

dopic2:
	lda pic2init
	cmp #1
	beq endada
	jmp noendad
endada:
	jmp endad
noendad:
	inc pic2init

	ldx demopart
	lda partpattdata,x
	cmp #5
	bne no_kupla


	ldx #<kuplasc
	ldy #>kuplasc
	jsr loadcompdd


	lda #%00100000 ; screen off
	sta $ff06

	lda #$C1
	sta $ff16 ; extra color

	lda #%00001000 ; color at $0800
	sta $ff14

	ldx #<kuplaa
	ldy #>kuplaa
	stx loadaddrlo
	sty loadaddrhi

	lda #$08
	sta decdesthi
	lda #0
	sta decdestlo

	sec
	jsr memdecomp ;; decomp to memory based on offset tables


	ldx #2 ;bitmap at $6000
	lda tedvidoffs,x
	clc
	sta $ff12


	lda #$3b ; no blank, bitmap
	sta $ff06

	jmp pic2done
no_kupla:
	ldx demopart
	lda partpattdata,x
	cmp #6
	beq quadlogo

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<quadco
	ldy #>quadco
	jsr loadcompdd

	ldx #<quadsc
	ldy #>quadsc
	jsr loadcompdd

	ldx #5 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #%00001000 ; hires
	sta $ff07

	lda #$3b ; no blank, bitmap
	sta $ff06
	jmp pic2done

quadlogo:


	lda #%00011000 ; mc
	sta $ff07


	ldx #2 ;bitmap at $6000
	lda tedvidoffs,x
	clc
	sta $ff12

	ldx #0
copyloopz:
	lda $8800,x
	sta $0800,x
	lda $8900,x
	sta $0900,x
	lda $8a00,x
	sta $0a00,x
	lda $8b00,x
	sta $0b00,x
	lda $8c00,x
	sta $0c00,x
	lda $8d00,x
	sta $0d00,x
	lda $8e00,x
	sta $0e00,x
	lda $8f00,x
	sta $0f00,x
	inx
	cpx #0
	bne copyloopz

	jmp pic2done

pic2done:

endad:
	jmp mainloop


dopic:

	lda picinit
	cmp #0
	bne picdone_go
	jmp doinitpic
picdone_go:
	jmp picdone
doinitpic:
	inc picinit

	ldx demopart
	lda partpattdata,x
	cmp #1
	beq showpic1
	cmp #2
	beq showpic2
	cmp #3
	beq showpic3

showpic1:

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<halpco
	ldy #>halpco
	jsr loadcompdd

	ldx #<halpsc
	ldy #>halpsc
	jsr loadcompdd

	lda #%00001000 ; color at $0800
	sta $ff14

	lda #$f1
	sta $ff19 ; border
	sta $ff15 ; bgcolor

	ldx #1 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #8 
	sta $ff07

	lda #$3b ; no blank, bitmap
	sta $ff06

	jmp picdone

showpic2:

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<pillco
	ldy #>pillco
	jsr loadraw

	ldx #<pillsc
	ldy #>pillsc
	jsr loadraw

	ldx #1 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #%00011000 ; mc
	sta $ff07

	lda #$11
	sta $ff19

	lda #$3b ; no blank, bitmap
	sta $ff06

	jmp picdone

showpic3:

	lda #$1c
	sta $ff19

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<pharco
	ldy #>pharco
	jsr loadcompdd

	ldx #<pharsc
	ldy #>pharsc
	jsr loadcompdd

	ldx #<hurtco
	ldy #>hurtco
	jsr loadcompdd

	ldx #<hurtsc
	ldy #>hurtsc
	jsr loadcompdd

	ldx #1 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #%00001000 ; hires
	sta $ff07

	lda #$3b ; no blank, bitmap
	sta $ff06

	jmp picdone


picdone:

	ldx demopart
	lda partpattdata,x
	cmp #1
	bne no_wobble

ldy #50
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
and #7
sta $FA
lda #%00001000 ; hires + scroll 7
clc
sbc $FA
sta $ff07

dey
cpy #0
bne yloop2

no_wobble:

	jmp mainloop



;;;;;;;;;;;;;; demopart ;;;;;;;;;;;;;  SIGN

signcolors: .byte $5c,$1b,$7a,$d3,$5c,$1b,$7a,$d3,0
signcolor: .byte 0

signindex: .byte 0

dosign:
lda signinit
cmp #1
beq signdone2
jmp singasasas
signdone2:
	jmp signdone
singasasas:
lda #%00100000 ; screen off
sta $ff06

lda #0
sta $ff19
sta frame

lda #1
sta frame3

ldx #<filename2
ldy #>filename2
jsr loadcompdd

ldx #<filename1
ldy #>filename1
jsr loadcompdd



ldx #120
colortext2:
lda #64
sta $0f70,x
sta $0b70,x
dex
cpx #255
bne colortext2

lda #$0
sta $ff15 ; bgcolor

lda #%00001000 ; color at $0800
sta $ff14


ldx #2
lda tedvidoffs,x
clc
sta $ff12

signdone:


lda signindex
cmp #0
beq nobottomcolor

ldx #120
ldy frame

lda fadetab,y
and #$0f
sta $d1
colortext:
lda $d1
sta $0b70,x
dex
cpx #255
bne colortext

nobottomcolor:

lda partpatts
cmp #4

bne sign_nochange

lda signindex
cmp #0
bne sign_nochange

lda #33
sta frame3
lda #0
sta frame

lda #$3b ;siefgra at 3b00
sta signaddy+1

lda #%00100000 ; screen off
sta $ff06

ldx #<sgtex
ldy #>sgtex
jsr loadraw

lda #%00110000 ; no blank, bitmap
sta $ff06

inc signindex

sign_nochange:

lda #<pixbuf
sta memcpyLen
signaddy:
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

lda #%00110000 ; no blank, bitmap
sta $ff06

lda #1
sta signinit
nosignon:

jmp mainloop

;;;;;; PRNG ;;;;;;

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

creditspart:
	.byte 0

creditsfades:
	.byte 0

docredits:
	lda creditspart
	cmp #3
	bne no_pic3fade

	inc creditsfades
	lda creditsfades
	cmp #2
	bne no_pic3fade
	lda #0
	sta creditsfades

	ldx #0
lumafade:	
	clc

	lda $0800,x
	clc
	cmp $4800,x
	bcs now1
	inc $0800,x
now1:

	lda $0900,x
	clc
	cmp $4900,x
	bcs now2
	inc $0900,x
now2:

	lda $0a00,x
	clc
	cmp $4a00,x
	bcs now3
	inc $0a00,x
now3:

	lda $0b00,x
	clc
	cmp $4b00,x
	bcs now4
	inc $0b00,x
now4:
	inx
	cpx #0
	bne lumafade

no_pic3fade:

	lda partpatts
	cmp #3
	bne no_pic3

	lda creditspart
	cmp #2
	bne no_pic3

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<cred3co
	ldy #>cred3co
	jsr loadcompdd

	ldx #0
clearluma:
	lda #0
	sta $0800,x
	sta $0900,x
	sta $0a00,x
	sta $0b00,x
	inx
	cpx #0
	bne clearluma

	ldx #0
copycolor:
	lda $4c00,x
	sta $0c00,x
	lda $4d00,x
	sta $0d00,x
	lda $4e00,x
	sta $0e00,x
	lda $4f00,x
	sta $0f00,x
	inx
	cpx #0
	bne copycolor

	lda #$0
	sta $ff19 ; border

	ldx #3 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #%00011000 ; mc
	sta $ff07

	lda #$3b ; no blank, bitmap
	sta $ff06

	inc creditspart

no_pic3:

	lda partpatts
	cmp #2
	bne no_pic2

	lda creditspart
	cmp #1
	bne no_pic2

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<cred2co
	ldy #>cred2co
	jsr loadcompdd

	lda #$83
	sta $ff19 ; border

	ldx #2 ;bitmap at $6000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #$3b ; no blank, bitmap
	sta $ff06

	inc creditspart

no_pic2:

	lda creditspart
	cmp #0
	bne no_pic1

	lda #%00100000 ; screen off
	sta $ff06

	ldx #<cred1co
	ldy #>cred1co
	jsr loadcompdd

	ldx #<cred1sc
	ldy #>cred1sc
	jsr loadcompdd

	ldx #1 ;bitmap at $4000
	lda tedvidoffs,x
	clc
	sta $ff12

	lda #%00001000 ; hires
	sta $ff07

	lda #$3b ; no blank, bitmap
	sta $ff06

	ldx #<cred2sc
	ldy #>cred2sc
	jsr loadcompdd

	ldx #<cred3sc
	ldy #>cred3sc
	jsr loadcompdd


	inc creditspart
no_pic1:



	jmp mainloop

picinit:
	.byte 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MUSIC AND LOADER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

tedvidoffs: .byte 8,16,24,32,40,48,56


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

filename1:  .asciiz "signcol"
filename2:  .asciiz "sign"

cpack: .asciiz  "cpack"

runpack: .asciiz  "runpack"

sgcol: .asciiz  "sgcol"
sgtex: .asciiz  "sgtex"

quadsc: .asciiz "quadsc"
quadco: .asciiz "quadco"

halpsc: .asciiz "halpsc"
halpco: .asciiz "halpco"

logosc: .asciiz "logosc"
logoco: .asciiz "logoco"

pillsc: .asciiz "pillsc"
pillco: .asciiz "pillco"

hurtsc: .asciiz "hurtsc"
hurtco: .asciiz "hurtco"

music2: .asciiz "music2"

tekstisc: .asciiz "tekstisc"
tekstico: .asciiz "tekstico"

cred1sc: .asciiz "cred1sc"
cred1co: .asciiz "cred1co"

cred2sc: .asciiz "cred2sc"
cred2co: .asciiz "cred2co"

cred3sc: .asciiz "cred3sc"
cred3co: .asciiz "cred3co"

cred4sc: .asciiz "cred4sc"
cred4co: .asciiz "cred4co"

pharsc: .asciiz "pharsc"
pharco: .asciiz "pharco"

kuplasc: .asciiz "kuplasc"
kuplaco: .asciiz "kuplaco"

signinit: .byte 0

changesong: .byte 0

wordval: .word $0878
wordval2: .word $0c78

xl1: .byte 23,23,23,23,23,23,29,29,29,29,29,29,23,23,23,23,23,23

xl2: .byte 17,17,17,17,17,17,11,11,11,11,11,11,17,17,17,17,17,17

x1: .byte 0
x2: .byte 0 
xt: .byte 0

rastercnt: .byte 0

lumavals: .byte 32,64,128,82,14,55,191

;;;;;;;;;;;;;;;;;;;;;   main IRQ, start of frame
;;;;;;;;;;;;;;;;;;;;;

irq_vector: ;;; start of frame

;; count 57 cycles to next scanline
pha ;3
txa ;2
pha ;3
tya ;2
pha ;3

asl $ff09 ;6

;19

lda demopart
cmp #0
bne gotoplayer2

lda $ff1d
adc frame
adc frame
tax
lda sintab,x
ror 
ror 
ror 
ror 
ror 
and #7
clc
adc #%00001000 
sta $ff07       

clc
lda $ff1d
cmp #$c9
bcc do_screen_irq
gotoplayer2:
lda demopart ;4
cmp #2 ;2
bne gotoplayer ;2+1

lda runinit
cmp #2
bne gotoplayer

nop 
nop 
nop 

lda #$f1 ;2
sta $ff19 ;; border 4
now:
clc
lda $ff1d
cmp #$c9
bcc do_screen_irq
gotoplayer:

lda demopart
cmp #4
bne gotoplayer0

nop 
nop 
nop 


clc
lda $ff1d
cmp #176
bcc nohires

lda #%00001000 ; hires
sta $ff07

jmp mcmzcz

nohires:
lda #%00011000 ; mc
sta $ff07

mcmzcz:

lda $ff1d
cmp #$c9
bcc do_screen_irq

gotoplayer0:

lda #<irq_vector2    ; Set IRQ vector to be called
sta $FFFE           ; Once per screen refresh
lda #>irq_vector2
sta $FFFF           ; The $ 314 / $ 315 vector points to the IRQ raster routine

lda demopart
cmp #0
bne screen_irq_done
beq screen_irq_done2

do_screen_irq:

lda #<irq_vector    ; Set IRQ vector to be called
sta $FFFE           ; Once per screen refresh
lda #>irq_vector
sta $FFFF           ; The $ 314 / $ 315 vector points to the IRQ raster routine


screen_irq_done:

inc rastercnt
inc rastercnt
inc rastercnt

jmp ohihihi

screen_irq_done2:
inc rastercnt
inc rastercnt

ohihihi:

lda #%00000010  ;; use interrupt source: raster counter
sta $ff0a
lda rastercnt
sta $ff0b ;; raster counter val
pla
tay
pla
tax
pla
rti    

;;;;;;;;;;;;;;;;;;;;;   part-specific irq logic
;;;;;;;;;;;;;;;;;;;;;

irq_vector2: ;;; timers, partlogic, music player
pha
txa
pha
tya
pha

asl $ff09


lda demopart
cmp #2
bne noblack
lda runinit
cmp #2
bne noblack

ldx #0
asl $1000,x
asl $1000,x
asl $1000,x
asl $1000,x
asl $1000,x
asl $1000,x
asl $1000,x

lda #0
sta $ff19 ;; border
noblack:



lda #0
sta $ff15

lda #0
sta rastercnt

lda #<irq_vector    ; Set IRQ vector to be called
sta $FFFE           ; Once per screen refresh
lda #>irq_vector
sta $FFFF           ; The $ 314 / $ 315 vector points to the IRQ raster routine

lda #%00000010  ;; use interrupt source: raster counter
sta $ff0a
lda #1
sta $ff0b ;; raster counter val

inc frame

inc frame2
lda frame2
cmp #64
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

lda loading
cmp #1
beq nopartadd

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
cmp #129
bne nomusicchangetoggle

lda #1
sta changesong

nomusicchangetoggle:	


lda #0
sta partframes
sta partpatts
sta extracount
sta runinit
sta picinit
sta pic2init
sta kerrat
nopartadd:

; tick and output to ted
lda biisi
cmp #1
beq tokabis

jsr $1603
jsr $1606
jmp exx

tokabis:
jsr $2403
jsr $2406

exx:

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

biisi:
	.byte 0

;;;;;;;;;;;;;;;;;;;; demopart lengths, extra databyte, pointer to function


partpattextra: .byte 65,     1,     64,       128,   1,    129,     64,     1,       90,     64,   164,        1,     1

partpattdata: .byte  0,      0,     0,        1,     0,      3,     6,      0,        5,      2,     0,        7,     4

partpattlen: .byte   1,      2,     2,        1,     6,      1,     1,      2,        1,      1,     3,        1,     64
demoparts: .word     dologo, domem, dorunner, dopic, dosign, dopic, dopic2, dotalker, dopic2, dopic, docredits,dopic3,dopic2

extracount: .byte 0
partframes: .byte 0
partframes2: .byte 0
partframes3: .byte 0

partpatts: .byte 0
demopart: .byte 0

;----------------------------------------------------------------
; lookup data for fast pixel access
;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;; part data


runinit:
.byte 0

runindex:
.byte 0

;; for talk anim

compdataoffsets:


;;;;;;4      4
.word $7E28, $859D
;;;;;;3      3
.word $73E8, $7B65
;;;;;;3      3
.word $73E8, $7B65
;;;;;;4      4
.word $7E28, $859D
;;;;;;2      2      
.word $69BB, $712D 
;;;;;;1      1      
.word $6000, $6735
;;;;;;4      4
.word $7E28, $859D
;;;;;;4      4
.word $7E28, $859D
;;;;;;3      3
.word $73E8, $7B65
;;;;;;4      4
.word $7E28, $859D
;;;;;;2      2      
.word $69BB, $712D 
;;;;;;4      4
.word $7E28, $859D
;;;;;;3      3
.word $73E8, $7B65
;;;;;;2      2      
.word $69BB, $712D 
;;;;;;1      1      
.word $6000, $6735
;;;;;;4      4
.word $7E28, $859D

decdestoffsets:

.word $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000,$4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000

;; alt to 4000 and 0800 / A000 and c000

;; for run anim

compdataoffsets2:
.word $6200, $64F1, $6625, $6923, $6A56, $6D7A, $6EB3, $71B2, $72E2, $75D5, $7707, $7A05, $7B35, $7E53, $7F8E, $829F, $83D0, $86AB, $87D5, $8AE0, $8C08, $8F01, $9028, $92F5

decdestoffsets2:
.word $4F00, $09E0, $AF00, $C1E0, $4F00, $09E0, $AF00, $C1E0, $4F00, $09E0, $AF00, $C1E0, $4F00, $09E0, $AF00, $C1E0, $4F00, $09E0, $AF00, $C1E0, $4F00, $09E0, $AF00, $C1E0

runtimes:
.byte 0

screenflip:
.byte 0

runscroll:
.byte 0

runscrolltimes:
.byte 8

;;;;;;;;;;;;;;;;;;;;;;;; demopart runner

dorunner:

lda kerrat
cmp #3
beq norunmo
jmp yesrunmo
norunmo:
	jmp mainloop
yesrunmo:
lda runinit
cmp #0
bne rundo_start2

jmp dorunneriniter

rundo_start2:
jmp rundo_start

dorunneriniter:

lda #%00100000 ; screen off
sta $ff06

ldx #<runpack
ldy #>runpack
jsr loadraw

lda #0
ldx #0

runclrloop:

lda #0
sta $4000,x
sta $4100,x
sta $4200,x
sta $4300,x
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

sta $a000,x
sta $a100,x
sta $a200,x
sta $a300,x
sta $a400,x
sta $a500,x
sta $a600,x
sta $a700,x
sta $a800,x
sta $a900,x
sta $aa00,x
sta $ab00,x
sta $ac00,x
sta $ad00,x
sta $ae00,x

inx
cpx #0
bne runclrloop 

lda #0
ldx #$df

runclrloop2:

lda #$70
sta $0900,x
sta $C100,x
dex
cpx #255
bne runclrloop2

lda #0
ldx #0

runclrloop3:

lda #$70
sta $0800,x
sta $C000,x
inx
cpx #0
bne runclrloop3

lda #2
sta runinit

lda #0
sta runindex

lda #%00001000 ; hires
sta $ff07

jmp mainloop

dotalker:

lda runinit
cmp #0
bne rundo_start

lda #0
sta $ff19 ; border

lda #%00100000 ; screen off
sta $ff06



ldx #<cpack
ldy #>cpack
jsr loadraw


lda #1
sta runinit

lda #0
sta runindex

lda #$0
sta $ff15 ; bgcolor
sta $ff19 ; border

lda #%00011000 ; mc
sta $ff07

jmp mainloop

rundo_start:
rundo:

ldx runindex

lda runinit
cmp #2
beq runnerlogic1

talkerlogic1:

lda decdestoffsets+1,x
sta decdesthi

lda decdestoffsets,x
sta decdestlo

lda compdataoffsets+1,x
tay
lda compdataoffsets,x
tax

jmp commonlogic

runnerlogic1:

inc runscroll

lda runscroll
cmp #8
bne no_runscrollreset
lda #0
sta runscroll
no_runscrollreset:
lda runscroll
cmp #7
bne no_scrolloffsetting
dec runscrolltimes

lda runscrolltimes
cmp #0
bne no_scrolloffsetting
lda #7
sta runscrolltimes

no_scrolloffsetting:

lda decdestoffsets2+1,x
sta decdesthi

lda decdestoffsets2,x

ldy runtimes
cpy #0
bne nosp
clc
adc runscrolltimes
adc runscrolltimes
adc runscrolltimes
adc runscrolltimes
adc runscrolltimes
adc runscrolltimes
adc runscrolltimes
adc runscrolltimes
jmp ddddd
nosp:	
clc
adc runscrolltimes
ddddd:

sta decdestlo

lda compdataoffsets2+1,x
tay
lda compdataoffsets2,x
tax

commonlogic:
stx loadaddrlo
sty loadaddrhi

sec
jsr memdecomp ;; decomp to memory based on offset tables

lda demopart
cmp #2
bne noscroll33

lda #%00001000 ; hires + scroll 7
clc
sbc runscroll
sta $ff07

noscroll33:

inc runindex
inc runindex

inc runtimes
lda runtimes
cmp #2
bne rundo22
jmp flipper
rundo22:
jmp rundo

flipper:
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

lda runinit
cmp #2
beq longerlogic

lda runindex
cmp #64
bne runexit

lda #0
sta runindex

jmp runexit

longerlogic:

ldx #7
colclr:
lda #$71
sta $0de0,x
sta $09e0,x
sta $C1e0,x
sta $c5e0,x
sta $0C00,x
sta $0800,x
sta $C000,x
sta $c400,x
dex
cpx #255
bne colclr

ldx #0
cleartop:
lda #0
sta $4f00,x
sta $Af00,x
sta $4000,x
sta $A000,x
inx
cpx #64
bne cleartop


lda runindex
matti:
cmp #32
bne runexit

lda #0
sta runindex

inc kerrat
lda kerrat
cmp #2
bne nofall
lda #48
sta matti+1


nofall:

runexit:

jmp mainloop

kerrat:
	.byte 0

;;;;;;;;;;;;; more data

sintab:
.byte 128, 131, 134, 137, 140, 143, 146, 149, 152, 155, 158, 162, 165, 167, 170, 173
.byte 176, 179, 182, 185, 188, 190, 193, 196, 198, 201, 203, 206, 208, 211, 213, 21
.byte 218, 220, 222, 224, 226, 228, 230, 232, 234, 235, 237, 238, 240, 241, 243, 244
.byte 245, 246, 248, 249, 250, 250, 251, 252, 253, 253, 254, 254, 254, 255, 255, 255
.byte 255, 255, 255, 255, 254, 254, 254, 253, 253, 252, 251, 250, 250, 249, 248, 246
.byte 245, 244, 243, 241, 240, 238, 237, 235, 234, 232, 230, 228, 226, 224, 222, 220
.byte 218, 215, 213, 211, 208, 206, 203, 201, 198, 196, 193, 190, 188, 185, 182, 179
.byte 176, 173, 170, 167, 165, 162, 158, 155, 152, 149, 146, 143, 140, 137, 134, 131
.byte 128, 124, 121, 118, 115, 112, 109, 106, 103, 100, 97, 93, 90, 88, 85, 82, 79
.byte 76, 73, 70, 67, 65, 62, 59, 57, 54, 52, 49, 47, 44, 42, 40, 37, 35, 33, 31, 29
.byte 27, 25, 23, 21, 20, 18, 17, 15, 14, 12, 11, 10, 9, 7, 6, 5, 5, 4, 3, 2, 2, 1, 1
.byte 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 7, 9, 10, 11, 12, 14, 15, 17
.byte 18, 20, 21, 23, 25, 27, 29, 31, 33, 35, 37, 40, 42, 44, 47, 49, 52, 54, 57, 59, 62
.byte 65, 67, 70, 73, 76, 79, 82, 85, 88, 90, 93, 97, 100, 103, 106, 109, 112, 115, 118, 121, 124

kuplaa:
.incbin "kuplaco.tc",2

loading:
.byte 0

loadcompdd:
	lda #1
	sta loading
	jsr loadcompd
	lda #0
	sta loading
	rts

;;;; all the way to the end of memory!

.res $fa00 - *
.incbin "../../build/loader-c16.prg", 2
