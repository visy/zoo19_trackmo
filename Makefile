
ifeq ($(PLATFORM),)
_PLATFORM_   = c16
else ifeq ($(PLATFORM),c116)
_PLATFORM_   = c16
else ifeq ($(PLATFORM),plus4)
_PLATFORM_   = c16
else
_PLATFORM_   = $(PLATFORM)
endif

ifeq ($(NO_VICE),)
NO_VICE      = 0
endif


ifneq ($(_PLATFORM_),c64)
ifneq ($(_PLATFORM_),c16)
$(error invalid platform $(_PLATFORM_) specified)
endif
endif


ARCH         = $(shell uname | tr "[a-z]" "[A-Z]" | tr -c -d "[A-Z]")

ifneq ($(findstring CYGWINNT,$(ARCH)),)
    ifeq (CYGWINNT,$(ARCH))
ARCH         = WIN32
    else
ARCH         = WIN64
    endif    
endif
ifneq ($(findstring DARWIN,$(ARCH)),)
ARCH         = MACOSX
endif


ifeq ($(_PLATFORM_),c16)
    ifneq ($(NO_VICE),0)
        ifneq ($(findstring WIN,$(ARCH)),)
USE_PLUS4EMU = 0
USE_YAPE     = 1
        else
USE_PLUS4EMU = 1
USE_YAPE     = 0
        endif
    else
USE_PLUS4EMU = 0
USE_YAPE     = 0
    endif
else
USE_PLUS4EMU = 0
USE_YAPE     = 0
endif


ifeq ($(_PLATFORM_),c16)
    ifeq ($(ARCH),MACOSX)
    # MacOSX, these programs must be installed as applications
VICE         = /Applications/VICE/xplus4.app/Contents/MacOS/xplus4
PLUS4EMU     = /Applications/plus4emu.app/Contents/MacOS/plus4emu
    else    
VICE         = xplus4
PLUS4EMU     = plus4emu
        ifeq ($(ARCH),WIN64)
YAPE         = YapeWin64
        else
YAPE         = Yape
        endif
    endif
else ifeq ($(ARCH),MACOSX)
    # MacOSX, these programs must be installed as applications
VICE         = /Applications/VICE/x64.app/Contents/MacOS/x64
else
VICE         = x64
endif

ifneq ($(USE_PLUS4EMU),0)
EMU          = $(PLUS4EMU) -disk
else
EMU          = $(VICE) -drive8type 1541 -drive9type 0 -autostart
endif

ECHO         = echo
PRINTF       = printf

AS           = cl65
LD           = ld65
C1541        = c1541

MKDIR        = mkdir -p
RM           = rm -f
ifeq ($(ARCH),MACOSX)
RMDIR        = rmdir # XXX TODO xargs to remove .DS_Store
else
RMDIR        = rmdir
endif
CAT          = cat


.PHONY: default loader assemble diskimage run clean distclean wipe
.PHONY: tellarch


BUILDDIR     = ../../build
INTERMDIR    = ../../build/interm
LOADER_SRC   = ../../src
LOADER       = $(BUILDDIR)/loader-$(_PLATFORM_).prg

RESOURCESDIR = ../resources
SIGNC        = signcol.tc
SIGN         = sign.tc

SCREEN1		 = screen1.tc
SCREEN2		 = screen2.bin
SCREEN3		 = screen3.bin
SCREEN4		 = screen4.bin

SCREEN1C	= color1.bin
SCREEN2C	= color2.bin
SCREEN3C	= color3.bin
SCREEN4C	= color4.bin

HALPSC		 = halpsc.tc
HALPCO		 = halpco.tc

QUADSC		= quadsc.tc
QUADCO		= quadco.tc

NAME         = minexample

SOURCE       = $(NAME).s
LOADERCFG    = loaderconfig.inc
ASSEMBLE     = $(INTERMDIR)/$(NAME)-uncompressed-$(_PLATFORM_).prg
DISKIMAGE    = $(BUILDDIR)/$(NAME)-$(_PLATFORM_).d64

AS_FLAGS     = -Wa -I../../../shared -I ../../include --cpu 6502X 


default: diskimage


tellarch:
	@$(ECHO) $(ARCH)


loader: $(LOADER)

$(LOADER): $(LOADERCFG)
	make -C $(LOADER_SRC) EXTCONFIGPATH=../samples/$(NAME) INSTALL=2800 RESIDENT=2000 ZP=02 prg


$(ASSEMBLE): $(SOURCE) $(LOADER) $(LOADERCFG)
	$(MKDIR) $(BUILDDIR)
	$(MKDIR) $(INTERMDIR)
ifeq ($(_PLATFORM_),c64)
	$(AS) $(AS_FLAGS) -C c64-asm.cfg -Wa -DPLATFORM=64 -o $@ $<
else
	$(AS) $(AS_FLAGS) -t c16 -Wa -DPLATFORM=16 -o $@ $<
endif


diskimage: $(DISKIMAGE)

$(DISKIMAGE): $(ASSEMBLE) $(PIC1) $(PIC2)
	$(C1541) -format "pretending to be,qt" d64 $@
	$(C1541) -attach $@ \
	 -write $(ASSEMBLE) "$(NAME)" \
	 -write $(SIGNC) "signcol" \
	 -write $(SIGN) "sign" \
	 -write $(SCREEN1) "screen1" \
	 -write $(SCREEN1C) "color1" \
	 -write $(SCREEN2) "screen2" \
	 -write $(SCREEN2C) "color2" \
	 -write $(SCREEN3) "screen3" \
	 -write $(SCREEN3C) "color3" \
	 -write $(SCREEN4) "screen4" \
	 -write $(SCREEN4C) "color4" \
	 -write $(QUADSC) "quadsc" \
	 -write $(QUADCO) "quadco" \
	 -write $(HALPSC) "halpsc" \
	 -write $(HALPCO) "halpco" \
	 -write "logosc.tc" "logosc" \
	 -write "logoco.tc" "logoco"
	 


ifneq ($(USE_YAPE),0)
run: $(DISKIMAGE)
	$(YAPE) "..\..\build\$^"
else
run: $(DISKIMAGE)
	$(EMU) $^
endif

clean:
	-$(RM) *.o $(ASSEMBLE) $(DISKIMAGE)
	-$(RM) -rf $(INTERMDIR)
	-$(RM) $(BUILDDIR)/loader-c64.prg
	-$(RM) $(BUILDDIR)/loader-c16.prg
	-$(RMDIR) $(BUILDDIR)

distclean:
	-$(MAKE) -C $(LOADER_SRC) clean

wipe: distclean clean
