.segment "HEADER"

INES_MAPPER = 0 ; NROM
INES_MIRROR = 0 ;0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM = 0 ; 1 = battery-backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; nes ID
.byte $02 ; 16k PRG bang count
.byte $01 ; 8k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; import background and sprite character sets
.segment "TILES"
.incbin "megablast.chr"

; interupt handlers
.segment "VECTORS" 
.word nmi
.word reset
.word irq

; variable storage (in zero-page)
.segment "ZEROPAGE"

; sprite oam data
.segment "OAM"
oam: .res 256

.include "neslib.s"

; larger memory allocations
.segment "BSS"
palette: .res 32 ; current palette buffer


.segment "CODE"

; allow for irq clock interupt to call
irq:
	rti

; default palette for tiles and sprites
.segment "RODATA"
default_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$19,$29,$39 ; bg1 green
.byte $0F,$11,$21,$31 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$28,$21,$11 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

