;
; Define PPU Registers
;

; Controls basic PPU operation: base nametable address, VRAM increment direction, sprite/background pattern table addresses, 
; sprite size (8x8 or 8x16), and whether to generate NMI on VBlank.
PPU_CONTROL = $2000

; Controls rendering options: grayscale mode, background/sprite clipping in leftmost 8 pixels, background/sprite enable/disable, 
; and color emphasis (tinting).
PPU_MASK = $2001

; Read-only register that reports PPU status: VBlank flag, sprite 0 hit flag, sprite overflow flag. 
; Reading this register clears the VBlank flag and resets the write toggle for $2005/$2006.
PPU_STATUS = $2002

; Sets the address in Object Attribute Memory (sprite memory) for accessing via $2004.
OAMADDR = $2003

; Read/write sprite data. Writing increments the OAM address. Used for updating sprite attributes (Y position, tile number, attributes, X position).
OAMDATA = $2004

; Write-only register for setting scroll position. Takes two writes - first for horizontal scroll, second for vertical scroll. 
; The write toggle is shared with $2006.
PPUSCROLL = $2005

; Write-only register for setting VRAM address. Takes two writes (high byte, then low byte) to set the full 16-bit address for accessing VRAM via $2007.
PPUADDR = $2006

; Read/write VRAM data at the address set by $2006. After each access, the VRAM address increments by either 1 or 32 (controlled by bit 2 of PPUCTRL).
PPUDATA = $2007

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
VBLANK_NMI = $80 ; %10000000 
 
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

SEED0: 	.res 2		; First 16-bit seed value
SEED2: 	.res 2		; Second 16-bit seed value

cx1:		.res 1 		; obj 1 x pos
cy1:		.res 1		; obj 1 y pos
cw1:		.res 1		; obj 1 height
ch1:		.res 1		; obj 1 width

cx2:		.res 1		; obj 2 x pos
cy2:		.res 1		; obj 2 y pos
cw2:		.res 1		; obj 2 height
ch2:		.res 1		; obj 2 width

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

	; turn off nmi (why)
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
	sta PPUADDR
	lda #$00
	sta PPUADDR

	lda #0
	ldy #30
	rowloop:
		ldx #32
		columnloop:
			sta PPUDATA
			dex
			bne columnloop
		dey
		bne rowloop
	
	ldx #64
	loop:
		sta PPUDATA
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

	; read 8 butes from interface at $4016
	ldx #8
	loop:
		pha
		lda JOYPAD1

		;comines 2 low bits and store them in carry
		and #$00000011
		cmp #%00000001
		pla

		; rotate carry into the gamepad variable
		ror a
		dex
		bne loop
		sta gamepad
		rts
.endproc

.proc randomize
	lda SEED0
	lsr
	rol SEED0 + 1
	bcc @noeor
	eor #$B4
@noeor:
	sta SEED0
	eor SEED0 + 1
	rts
.endproc

.proc rand
	jsr rand64k
	jsr rand32k
	lda SEED0 + 1
	eor SEED2 + 1
	tay
	lda SEED0
	eor SEED2
	rts
.endproc

.proc rand64k
	lda SEED0 + 1
	asl
	asl
	eor SEED0 + 1
	asl
	asl
	eor SEED0 + 1
	asl
	rol SEED0
	rol SEED0 + 1
	rts
.endproc

.proc rand32k
	lda SEED2 + 1
	asl
	eor SEED2 + 1
	asl
	asl
	ror SEED2
	rol SEED2 + 1
	rts
.endproc

.proc collision_test
	clc
	lda cx1				; get obj 1 x pos
	adc cw1				; add obj 1 width
	cmp cx2				; is obj 2 to the right of obj 1 + width
	bcc @exit
	clc
	lda cx2				; get obj 2 x pos
	adc cw2				; add object 2 width
	cmp cx1				; is obj 2 to the left of obj 1
	bcc	@exit

	lda cy1				; get obj 1 y
	adc ch1				; add obj 1 height
	cmp cy2				; is obj 2 below obj 1 + height
	bcc @exit
	clc
	lda cy2				; get obj 2 y
	adc ch2				; add obj 2 height
	cmp cy1				; is obj 2 above obj 1
	bcc @exit

	sec						; we have a hit, set carry flag and return
	rts

@exit:
	clc						; no hit, clear carry flag and exit
	rts
.endproc

.proc dec99_to_bytes
	ldx #0
	cmp #50						; check if over 50, otherwise try subtracting 20s
	bcc try20
	sbc #50						
	ldx #5						; subtract 50 and set decimal digit to 5
	bne try20

div20:
	inx
	inx
	sbc #20					; add 2 to decimal digit and subtract 20
try20:
	cmp #20
	bcs div20				; still more than 20, repeat

try10:
	cmp #10
	bcc @finished
	sbc #10
	inx							; add 1 to decimal digit and subtract 10

@finished:

	rts
.endproc

.proc clear_sprites
	;
	lda #255							; place all sprites offscreen at y pos = 255
	ldx #0
clear_oam:
	sta oam, x
	inx
	inx
	inx
	inx
	bne clear_oam

	rts
.endproc