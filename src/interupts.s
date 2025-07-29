; interupt handlers
.segment "VECTORS" 
.word nmi
.word reset
.word irq

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

	inc time					; increment lower byte of time counter
	bne :+						; increment upper byte after 255 times
		inc time + 1
	:

	; only update PPU if rendering is enabled
	lda ppu_ct11
	and #BG_ON|OBJ_ON
	bne :+
		jmp @skip_ppu_update
	:

	bit PPU_STATUS
	lda #>oam					; transfer sprite oam using dma
	sta SPRITE_DMA

	; transfer current palette to vram
	vram_set_address $3F00 ; set ppu address
	ldx #0						; transfer 32 bytes to vram
@loop:
	lda palette, x
	sta PPUDATA
	inx
	cpx #32
bcc @loop

	; display score if update flag is set
	lda #%00000001			; check update flag
	bit update
	beq @skipscore
		jsr display_score	; display score
		lda #%11111110		; reset update flag
		and update
		sta update
	@skipscore:

	; display player lives if update flag is set
@skiphighscore:
	lda #%00000100
	bit update
	beq @skiplives
		jsr display_lives
		lda #%11111011
		and update
		sta update
@skiplives:

	; display game over message if update flag is set
	lda #%00001000			; does game-over message need to be displayed
	bit update
	beq @skipgameover
		vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 7)
		assign_16i text_address, gameovertext
		jsr write_text
		lda #%11110111		; reset game-over update flag
		and update
		sta update
@skipgameover:

	; display and remove the level display message
	lda #%00010000			; check if level message needs to be displayed
	bit update
	beq @skipdisplaylevel
		vram_set_address (NAME_TABLE_0_ADDRESS + 14 *32 + 9)
		assign_16i text_address, leveltext
		jsr write_text
		lda level					; transform each decimal digit of level
		jsr dec99_to_bytes
		stx temp
		sta temp + 1
		lda temp
		clc
		adc #48
		sta PPUDATA
		lda temp + 1
		clc
		adc #48
		sta PPUDATA
		lda #%11101111		; reset the level message update flag
		and update
		sta update
@skipdisplaylevel:
	
	; remove the level display
	lda #%00100000			; does level message need to be removed
	bit update
	beq @skipremovedisplaylevel
		vram_set_address (NAME_TABLE_0_ADDRESS + 14 * 32 + 9)
		ldx #0
		lda #0
		:
			sta PPUDATA
			inx
			cpx #18
			bne :-
		lda #%11011111		; reset the level message update flag
		and update
		ora update
@skipremovedisplaylevel:		

	jsr animate_stars

	; shake screen using scroll
	lda shake
	beq :+
		dec shake
		and #%11
		asl a
		asl a
	:

	; write current screen settings (for shake scroll)
	sta PPUSCROLL
	sta PPUSCROLL

	lda ppu_ct10
	sta PPU_CONTROL
	lda ppu_ct11
	sta PPU_MASK

@skip_ppu_update:

	; call famistudio play update
	jsr famistudio_update

	; flag that the ppu has update is complete
	ldx #0
	stx nmi_ready

	; restore registers and returns
	pla
	tay
	pla
	tax
	pla
	rti
.endproc