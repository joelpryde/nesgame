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

; main application entry point for startup/reset
.segment "CODE"
.proc reset
	sei 								; masks interupt
	lda #0
	sta PPU_CONTROL 		; disable nmi
	sta PPU_MASK 				; disable rendering
	sta APU_DM_CONTROL
	lda #$40
	sta JOYPAD2					; disable apu frame irq

	cld									; disable decimal mode
	ldx #$FF
	txs									; initialize the stack

	bit PPU_STATUS			; wait for first vblank
wait_vblank:
	bit PPU_STATUS
	bpl wait_vblank

	lda #0							; clear all ram to 0
	ldx #0
clear_ram:
	sta $0000,x
	sta $0100,x
	sta $0200,x
	sta $0300,x
	sta $0400,x
	sta $0500,x
	sta $0600,x
	sta $0700,x
	inx
	bne clear_ram			; branch when x overflows at FF (zero-flag set)

	lda #255					; place all sprites off-screen at y = 255
	ldx #0
clear_oam:
	sta oam,x
	inx
	inx
	inx
	inx
	bne clear_oam

wait_vblank2:				;wait for second vblank
	bit PPU_STATUS
	bpl wait_vblank2

	; nes initialized and ready to begin

	lda #%10001000		;enables nmi for graphical update and jump to main
	sta PPU_CONTROL
	jmp main
.endproc

.proc nmi
	pha								; save registers
	txa
	pha
	tya
	pha

	bit PPU_STATUS
	lda #>oam					; transfer sprite oam using dma
	sta SPRITE_DMA

	; transfer current palette to vram
	vram_set_address $3F00 ; set ppu address
	lda #0						; transfer 32 bytes to vram
@loop:
	lda palette, x
	sta PPU_VRAM_IO
	inx
	cpx #32
	bcc @loop

	; write the current scroll and control regsiter settings to ppu
	lda #0
	sta PPU_VRAM_ADDRESS1
	sta PPU_VRAM_ADDRESS1
	lda ppu_ct10
	sta PPU_CONTROL
	lda ppu_ct11
	sta PPU_MASK

	; flag that the ppu has update is complete
	ldx #0
	stx nmi_ready
	pla								; restor registers and returns
	tay
	pla
	tax
	pla
	rti
.endproc

.proc main
	;
	ldx #0
paletteloop:				; intitialize palette table
	lda default_palette, x
	sta palette, x
	inx
	cpx #32
	bcc paletteloop

	lda #VBLANK_NMI|BG_0000|OBJ_1000	; set our game settings
	sta ppu_ct10
	lda #BG_ON|OBJ_ON
	sta ppu_ct11

	jsr ppu_update

mainloop:
	jmp mainloop
.endproc