; Define PPU Registers
PPU_CONTROL = $2000
PPU_MASK = $2001
PPU_STATUS = $2002
PPU_SPRRAM_ADDRESS = $2003
PPU_SPRRAM_IO = $2004
PPU_VRAM_ADDRESS1 = $2005
PPU_VRAM_ADDRESS2 = $2006
PPU_VRAM_IO = $2007
SPRITE_DMA = $4014
  
; Define APU Registers
APU_DM_CONTROL = $4010
APU_CLOCK = $4015
  
; Joystick/Controller values
JOYPAD1 = $4016
JOYPAD2 = $4017
  
; Gamepad bit values
PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "HEADER"
INES_MAPPER = 0
INES_MIRROR = 0
INES_SRAM = 0
.byte 'N', 'E', 'S', $1A
.byte $02
.byte $01
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0

.segment "VECTORS" ; interupt handlers
.addr nmi_handler, reset_handler, irq_handler

.segment "ZEROPAGE"
nmi_ready: .res 1
gamepad: .res 1
d_x: .res 1
d_y: .res 1

.segment "OAM"
oam: .res 256

; Our default palette table has 16 entries for tiles
; and 16 entries for sprites
.segment "RODATA"
default_palette:
.byte $0F,$15,$26,$37
.byte $0F,$09,$19,$29
.byte $0F,$01,$11,$21
.byte $0F,$00,$10,$30
.byte $0F,$18,$28,$38
.byte $0F,$14,$24,$34
.byte $0F,$1B,$2B,$3B
.byte $0F,$12,$22,$32  
welcome_txt:
.byte 'W','E','L','C', 'O', 'M', 'E', 0
string: .asciiz "Hello, World!" ; null-terminated string

.segment "TILES"
.incbin "src/font.chr" ; include the binary file created with NEXXT
;.incbin "example.chr"

.segment "BSS"
palette: .res 32

.segment "CODE"
irq_handler:
  RTI
nmi_handler:
  RTI

.proc reset_handler
  SEI ; Deactivate IRQ (non-NMI interrupts)
  CLD ; Deactivate non-existing decimal mode
  LDX #%00000000
  STX PPU_CONTROL ; PPU is unstable on boot, ignore NMI for now
  STX PPU_MASK ; Deactivate PPU drawing, so CPU can safely write to PPU's VRAM
  BIT PPU_STATUS ; Clear the vblank flag; its value on boot cannot be trusted
  vblankwait1: ; PPU unstable on boot, wait for vertical blanking
	BIT PPU_STATUS ; Clear the vblank flag;
	; and store its value into bit 7 of CPU status register
	BPL vblankwait1 ; repeat until bit 7 of CPU status register is set (1)
  vblankwait2: ; PPU still unstable, wait for another vertical blanking
	BIT PPU_STATUS
	BPL vblankwait2
  ; PPU should be stable enough now

  ; RAM contents on boot cannot be trusted (visual artifacts)
  ; Clear nametable 0; It is at PPU VRAM's address $2000
  ; CPU registers size is 1 byte, but addresses size is 2 bytes
  LDA PPU_STATUS ; Clear w register,
  ; so the next write to PPU_VRAM_ADDRESS2 is taken as the VRAM's address high byte.
  ; First, we need the high byte of $2000
  ;                                  ^^
  LDA #$20
  STA PPU_VRAM_ADDRESS2 ; (this also sets the w register,
  ; so the next write to PPU_VRAM_ADDRESS2 is taken as the VRAM's address low byte)
  ; Then, the low byte of  $2000
  ;                           ^^
  LDA #$00
  STA PPU_VRAM_ADDRESS2 ; (this also clears the w register)
  LDX #0 ; index for inner loop; overflows after 256
  LDY #4 ; index for outer loop; repeat overflow 4 times
  empty_background:
	; The size of a nametable is 1024 bytes
	; 256 bytes * 4 = 1024 bytes
	; A register already contains 0
	STA PPU_VRAM_IO ; After writing, PPU_VRAM_ADDRESS2 is automatically increased by 1
	INX
	BNE empty_background ; repeat until overflow
	DEY
	BNE empty_background ; repeat 4 times

  ; Background color (index 0 of first color palette)
  ; is at PPU's VRAM address 3f00
  LDX #$3f ; 3f00
  ;          ^^
  STX PPU_VRAM_ADDRESS2
  LDX #$00 ; 3f00
  ;            ^^
  STX PPU_VRAM_ADDRESS2
  ; Finally, we need indexes of two PPU's internal color
  LDA #$0F ; black for the transparency color (palette 0 color 0)
  STA PPU_VRAM_IO
  LDA #$30 ; white for the first background color (palette 0 color 1)
  STA PPU_VRAM_IO

  ; Nametable 0
  ; I have chosen a position near the center of the screen
  ; Address 218c
  LDX #$21 ; 218c
  ;          ^^
  STX PPU_VRAM_ADDRESS2
  LDX #$8c ; 218c
  ;            ^^
  STX PPU_VRAM_ADDRESS2
  LDX #0
  LDA string,X ; load first character of the string
  .scope
	print:
	  STA PPU_VRAM_IO ; write its ASCII code, which coincides with its tile index
	  INX
	  LDA string,X ; load next character of the string
	  BNE print ; repeat until null character is loaded
  .endscope

  ; center viewer to nametable 0
  LDA #0
  STA PPU_VRAM_ADDRESS1 ; X position (this also sets the w register)
  STA PPU_VRAM_ADDRESS1 ; Y position (this also clears the w register)

  ;     BGRsbMmG
  LDA #%00001010
  STA PPU_MASK ; Enable background drawing and leftmost 8 pixels of screen

	lda #05
	forever:
		jsr somefunction
	JMP forever ; Make CPU wait forever, while PPU keeps drawing frames forever

	somefunction:
		lda $20
		pha
		lda #10
		adc $80
		sta $81
		pla
		rts
.endproc