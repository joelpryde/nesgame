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

; Name table locations
NT_2000 = $00
NT_2400 = $01
NT_2800 = $02
NT_2C00 = $03
 
; Increment the vram pointer by row
VRAM_DOWN = $04
 
OBJ_0000 = $00 
OBJ_1000 = $08
OBJ_8X16 = $20
 
BG_0000 = $00
BG_1000 = $10
 
; Enable NMI
VBLANK_NMI = $80
 
BG_OFF = $00 ; Turn background off
BG_CLIP = $08 ; Clip the background
BG_ON = $0A ; Turn background on
 
OBJ_OFF = $00 ; Turn objects off
OBJ_CLIP = $10 ; Clip objects
OBJ_ON = $14 ; Turn objects on
  
; Define APU Registers
APU_DM_CONTROL = $4010 ; API delta modulation control register (write)
APU_CLOCK = $4015 ; API sound/vertical clock signal register (read/write)
  
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

; Useful PPU memory addresses
NAME_TABLE_0_ADDRESS = $2000
ATTRIBUTE_TABLE_O_ADDRESS = $23C0
NAME_TABLE_1_ADDRESS = $2400
ATTRIBUTE_TABLE_1_ADDRESS = $27C0

.segment "ZEROPAGE"

nmi_ready: .res 1	; Sets to 1 to push a PPU update, 2 to turn rendering off next NMI

.include "macro.s"

ppu_ct10: .res 1	; PPU control register 1 value 
ppu_ct11: .res 1	; PPU control register 2 value

gamepad:	.res 1 	; current gamepad value

.segment "CODE"

; Wait for nmi_ready
.proc wait_frame
	inc nmi_ready
@loop:
	lda nmi_ready
	bne @loop
	rts
.endproc

; Wait until next NMI and turn rendering on
.proc ppu_update
	lda ppu_ct10
	ora #VBLANK_NMI
	sta ppu_ct10
	sta PPU_CONTROL
	lda ppu_ct11
	ora #OBJ_ON|BG_ON
	sta ppu_ct11
	jsr wait_frame
	rts
.endproc

; Wait for screen to be rendered and then turn rendering off so we can write to PPU without corruption
.proc ppu_off
	jsr wait_frame
	lda ppu_ct10
	and #%0111111
	sta ppu_ct10
	sta PPU_CONTROL
	lda ppu_ct11
	and #%11100001
	sta ppu_ct11
	sta PPU_MASK
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




