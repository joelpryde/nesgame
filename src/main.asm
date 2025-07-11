.segment "VECTORS" ; interupt handlers
.addr nmi_handler, reset_handler, irq_handler

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
welcome_text:
.byte 'W','E','L','C', 'O', 'M', 'E', 0
string: .asciiz "Hello, World!" ; null-terminated string

.segment "TILES"
.incbin "src/example.chr" ; include the binary file created with NEXXT
;.incbin "example.chr"

.segment "BSS"
palette: .res 32

.segment "CODE"

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

; main application and game loop
.proc main
	; initialize palette table
	ldx #0
	paletteloop:
		lda default_palette, x
		sta palette, x
		inx
		cpx #32
		bcc paletteloop

	; clear name table
	jsr clear_nametable

	; draw some text (loop until we find 0 in welcome message)
	lda PPU_STATUS
	lda #$20
	sta PPU_VRAM_ADDRESS2
	lda #$8A
	sta PPU_VRAM_ADDRESS2
	ldx #0
	textloop:
		lda welcome_text, x
		sta PPU_VRAM_IO
		inx
		cmp #0
		beq endtextloop
		jmp textloop
	endtextloop:

	; place bat on screen (120, 180)
	lda #180
	sta oam	; y position
	lda #120
	sta oam + 3 ; x position
	lda #1
	sta oam + 1 ; 0 pattern
	lda #0
	sta oam + 2 ; 0 attributes

	; place ball on screen (124, 124)
	lda #124
	sta oam + (1 * 4) ; y position
	sta oam + (1 * 4) + 3 ; x position
	lda #2 
	sta oam + (1 * 4) + 1 ; 1 pattern
	lda #0
	sta oam + (1 * 4) + 2 ; 0 attributes

	; set ball initial velocity (1, 1)
	lda #1
	sta d_x
	sta d_y

	; setup screen to render
	jsr ppu_update

	mainloop:
		; skips reading constrols if change has not been drawn
		lda nmi_ready
		cmp #0
		bne mainloop

		; read gamepad
		jsr gamepad_poll

		;;; move bat
		lda gamepad
		and #PAD_L
		beq NOT_GAMEPAD_LEFT
			lda oam + 3 ; load current x position
			cmp #0
			beq NOT_GAMEPAD_LEFT ; don't move, at left edge of screen
			sec
			sbc #1
			sta oam + 3 ; move left

NOT_GAMEPAD_LEFT:

		; move bat right if pressed
		lda gamepad
		and #PAD_R
		beq NOT_GAMEPAD_RIGHT
			lda oam + 3 ; load current x position
			cmp #248
			beq NOT_GAMEPAD_RIGHT ; don't move, at left edge of screen
			clc
			adc #1
			sta oam + 3 ; move right

NOT_GAMEPAD_RIGHT:

		;;; move ball
		lda oam + (1 * 4) + 0 ; get current y
		clc
		adc d_y
		sta oam + (1 * 4) + 0 ; set updated y
		cmp #0
		bne NOT_HITTOP ; we hit the top, reverse direction
			lda #1
			sta d_y

NOT_HITTOP:
	
		lda oam + (1 * 4) + 0 ; get current y
		cmp #210
		bne NOT_HITBOTTOM
			lda #$FF
			sta d_y ; hit bottom, reverse direction (-1)

NOT_HITBOTTOM:

		lda oam + (1 * 4) + 3
		clc
		adc d_x
		sta oam + (1 * 4) + 3
		cmp #0
		bne NOT_HITLEFT
			lda #1
			sta d_x

NOT_HITLEFT:

		lda oam + (1 * 4) + 3
		cmp #248
		bne NOT_HITRIGHT
			lda #$FF
			sta d_x

NOT_HITRIGHT:

		; ensure changes are rendered
		lda #1
		sta nmi_ready
		jmp mainloop

.endproc