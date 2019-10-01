; built on Tue, 01 Oct 2019 14:20:50 +0200 using
; make PLATFORM=c16 prg INSTALL=4000 RESIDENT=fa00 ZP=02

; configuration
config_ALLOW_2_MHZ_ON_C128      = $00
config_DECOMPRESSOR             = $08
config_DIRTRACK                 = $12
config_DIRTRACK81               = $28
config_DISABLE_WATCHDOG         = $00
config_END_ADDRESS_API          = $00
config_FILENAME_MAXLENGTH       = $10
config_IDLE_BUS_LOCK            = $00
config_LC_SPEED                 = $06
config_LOAD_COMPD_API           = $01
config_LOAD_RAW_API             = $01
config_LOAD_TO_API              = $00
config_LOAD_UNDER_D000_DFFF     = $00
config_LOAD_VIA_KERNAL_FALLBACK = $00
config_MEM_DECOMP_API           = $01
config_MEM_DECOMP_TO_API        = $01
config_NTSC_COMPATIBILITY       = $00
config_ONLY_1541_AND_COMPATIBLE = $01
config_UNINSTALL_API            = $00

; status codes
status_OK                       = $00
status_DEVICE_INCOMPATIBLE      = $fb
status_TOO_MANY_DEVICES         = $fc
status_GENERIC_KERNAL_ERROR     = $fd
status_DEVICE_NOT_PRESENT       = $fe
status_FILE_NOT_FOUND           = $ff

; zeropage
loader_zp_first                 = $02
loadaddrlo                      = $02
loadaddrhi                      = $03
decdestlo                       = $04
decdesthi                       = $05
loader_zp_last                  = $12

; install
install                         = $47ea

; resident
loadraw                         = $fa00
loadcompd                       = $fa0b
memdecomp                       = $fbf0
