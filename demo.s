
memcpySrc = $f4
memcpyDst = $f6
memcpyLen = $f8

.include "loadersymbols-c16.inc"

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

lda #%00000010  ;; use interrupt source: raster counter
sta $ff0a
lda #1
sta $ff0b ;; raster counter val

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START MAINLOOP

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

ldx #<music2
ldy #>music2
jsr loadcompd


lda #$00
jsr $1600           ; Initialize sid to play song 0

cli

nochangesong:

;;;;; JUMP TO DEMOPART CODE

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

;;;;;;;;;;;;;; demopart ;;;;;;;;;;;;;  PATIENT

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

lda #%00001000 ; color at $0800
sta $ff14

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

lda #%00001000 ; color at $0800
sta $ff14

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

;;;;;;;;;;;;;; demopart ;;;;;;;;;;;;;  SIGN

signcolors: .byte $5c,$1b,$7a,$d3,$5c,$1b,$7a,$d3,0
signcolor: .byte 0

signindex: .byte 0

dosign:
lda signinit
cmp #1
beq signdone

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

ldx #<siefsid
ldy #>siefsid
jsr loadcompd

ldx #<siefgra
ldy #>siefgra
jsr loadcompd


lda #$0
sta $ff15 ; bgcolor

lda #%00001000 ; color at $0800
sta $ff14


ldx #2
lda tedvidoffs,x
clc
sta $ff12


signdone:

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

lda #$48 ;siefgra at 4800
sta signaddy+1

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

lda #%00011000 ; mc
sta $ff07
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MUSIC AND LOADER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

tedvidoffs: .byte 8,16,24,32,40,48,56

.res $1600 - *
.incbin "music.bin"

.res $2300 - *
.incbin "install-c16.prg", 2

.res $2f00 - *
.incbin "loader-c16.prg", 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

filename1:  .asciiz "signcol"
filename2:  .asciiz "sign"

cpack: .asciiz  "cpack"

runpack: .asciiz  "runpack"

siefgra: .asciiz  "siefgra"
siefsid: .asciiz  "siefsid"

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
cmp #3 ;2
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
cmp #3
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
sta runinit
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

;;;;;;;;;;;;;;;;;;;; demopart lengths, extra databyte, pointer to function

partpattlen: .byte 2,2,2,2,6,4,4
partpattextra: .byte 1,1,1,202,254,1,2
demoparts: .word  dologo, domem, dopatient, dorunner, dosign, dotalker, dopatient

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
.word $6000, $6735, $69BB, $712D, $73E8, $7B65, $7E28, $859D

decdestoffsets:
.word $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000

;; alt to 4000 and 0800 / A000 and c000

;; for run anim

compdataoffsets2:
.word $6200, $66A7, $680A, $6CBC, $6E23, $72FC, $746A, $7924, $7A85, $7F31, $8092, $8548, $86AD, $8B7C, $8CE6, $91AA

decdestoffsets2:
.word $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000, $4000, $0800, $A000, $C000

runtimes:
.byte 0

screenflip:
.byte 0

runscroll:
.byte 0

runscrolltimes:
.byte 6

;;;;;;;;;;;;;;;;;;;;;;;; demopart runner

dorunner:

lda runinit
cmp #0
bne rundo_start

lda #%00100000 ; screen off
sta $ff06

ldx #<runpack
ldy #>runpack
jsr loadraw

lda #2
sta runinit

lda #0
sta runindex

lda #%00001000 ; hires
sta $ff07

jmp mainloop

dotalker:

lda #0
sta patientinit

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

lda #$0
sta $ff15 ; bgcolor

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


lda #%00001000 ; hires + scroll 7
clc
sbc runscroll
sta $ff07

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
cmp #16
bne runexit

lda #0
sta runindex

jmp runexit

longerlogic:

lda #$71
sta $0C00
sta $0C01
sta $0C02
sta $0C03
sta $0C04
sta $0C05
sta $0C06
sta $0800
sta $0801
sta $0802
sta $0803
sta $0804
sta $0805
sta $0806
sta $C000
sta $c001
sta $c002
sta $c003
sta $c004
sta $c005
sta $c006
sta $c400
sta $c401
sta $c402
sta $c403
sta $c404
sta $c405
sta $c406

ldx #0
cleartop:
lda #0
sta $4000,x
sta $A000,x
inx
cpx #64
bne cleartop


lda runindex
cmp #32
bne runexit

lda #0
sta runindex
runexit:

jmp mainloop

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

.res $4000 - *

pixbuf: ; gradient

.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,8,8,8,8,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,8,8,8,8,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,16,16,16,16,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,16,16,16,16,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,32,32,32,32,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,110,32,32,32,32,110,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,110,8,8,16,16,24,24,32,32,40,40,48,48,56,56,64,64,110,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,110,8,8,16,16,24,24,32,32,40,40,48,48,56,56,64,64,110,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,110,8,8,16,16,24,24,32,32,40,40,48,48,56,56,64,64,110,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,110,8,8,16,16,24,24,32,32,40,40,48,48,56,56,64,64,110,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,110,40,40,40,40,110,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,40,40,40,40,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,48,48,48,48,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,48,48,48,48,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,56,56,56,56,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,56,56,56,56,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,110,110,110,110,110,110,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0



;;;; all the way to the end of memory!