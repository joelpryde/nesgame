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
.incbin "src/example.chr" ; include the binary file created with NEXXT
;.incbin "example.chr"

.segment "BSS"
palette: .res 32

.segment "CODE"

; wait until next NMI and then turn rendering on
.proc ppu_update
	lda #1
	sta nmi_ready
	loop:
		lda nmi_ready
		bne loop
	rts
.endproc

; turn ppu rendering off (for transfering large data to ppu)
.proc ppu_off
	lda #2
	sta nmi_ready
	loop:
		lda nmi_ready
		bne loop
	rts
.endproc

; clear name table
.proc clear_nametable
	lda PPU_STATUS
	lda #$20
	sta PPU_VRAM_ADDRESS2
	lda #$00
	sta PPU_VRAM_ADDRESS2

	lda #0
	ldy #30
	rowloop:
		ldx #32
		columnloop:
			sta PPU_VRAM_IO
			dex
			bne columnloop
		dey
		bne rowloop
	
	ldx #64
	loop:
		sta PPU_VRAM_IO
		dex
		bne loop
	rts
.endproc

; poll gamepad into byte varaible (8 buttons)
.proc gamepad_poll
	; strobe gamepad to latch the current button state
	lda #1
	sta JOYPAD1
	lda #0
	sta JOYPAD1

	; read 8 butes from interface
	ldx #8
	loop:
		pha
		lda JOYPAD1

		;comines 2 low bits and store them in carry
		and #$00000011
		cmp #%00000001
		pla

		; rotate carry into the gamepad variable
		ror
		dex
		bne loop
		sta gamepad
		rts
.endproc

; irq handler - not used
irq_handler:
  rti

; handle nmi
.proc nmi_handler
	; save registers to stack
  pha
	txa
	pha
	tya
	pha

	; skip rendering if nmi_ready isn't set
	lda nmi_ready
	bne :+
		jmp ppu_update_end
	:
	cmp #2
	bne cont_render
		lda #%00000000
		sta PPU_MASK
		ldx #0
		stx nmi_ready
		jmp ppu_update_end
	cont_render:

	; transfer sprite table to video memory
	ldx #0
	stx PPU_SPRRAM_ADDRESS
	lda #>oam
	sta SPRITE_DMA

	; transfer palette table to video memory
	lda #%10001000
	sta PPU_CONTROL
	lda PPU_STATUS
	lda #$3F
	sta PPU_VRAM_ADDRESS2
	stx PPU_VRAM_ADDRESS2
	ldx #0
	loop:
		lda palette, x
		sta PPU_VRAM_IO
		inx
		cpx #32
		bcc loop

	; enable rendering
	lda #%00011110
	sta PPU_MASK
	ldx #0
	stx nmi_ready
	ppu_update_end:

	; restor registers and return
	pla
	tya
	pla
	tax
	pla
	rti
.endproc

.proc reset_handler
	; Clear registers and setup joypad2
	sei
	lda #0
	sta PPU_CONTROL
	sta PPU_MASK
	sta APU_DM_CONTROL
	lda #$40
	sta JOYPAD2

	; Disable decimal mode and transfer FF to the stack register (no items)
	cld
	ldx #$FF
	txs

	; Wait for first vblank (bit 7 - negative flag is clear)
	bit PPU_STATUS
	wait_vblank:
		bit PPU_STATUS
		bpl wait_vblank

	; Clear all ram
	lda #0
	ldx #0
	clear_ram:
		sta $0100,x
    sta $0200,x
    sta $0300,x
    sta $0400,x
    sta $0500,x
    sta $0600,x
    sta $0700,x
    inx
    bne clear_ram

	; Place all sprites offscreen (byte 1 of 4 bytes is y position)
	lda #255
	ldx #0
	clear_oam:
		sta oam,x
		inx
		inx
		inx
		inx
		bne clear_oam

	; Wait for second vBlank
	wait_vblank2:
		bit PPU_STATUS
		bpl wait_vblank2

	; Tell PPU to use second pattern table for sprites and start nmi interupts
	lda #%10001000
	sta PPU_CONTROL
	jmp main
.endproc




main:
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