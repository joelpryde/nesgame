.segment "CODE"

title_text:
.byte "M E G A  B L A S T",0

press_play_text:
.byte "PRESS FIRE TO BEGIN",0
 
title_attributes:
.byte %00000101,%00000101,%00000101,%00000101
.byte %00000101,%00000101,%00000101,%00000101

.proc display_title_screen
	jsr ppu_off						; wait for screen to be drawn and then turn off
	jsr clear_nametable		; write title text
	
	; write out press play text (on 5 line of screen and 7 tiles in)
	vram_set_address (NAME_TABLE_0_ADDRESS + 4 * 32 + 6)
	assign_16i text_address, title_text
	jsr write_text

	; write out press play text (on 21 line of screen and 7 tiles in)
	vram_set_address (NAME_TABLE_0_ADDRESS + 20 * 32 + 6)
	assign_16i text_address, press_play_text
	jsr write_text

	; set the title text to use the second palette table
	vram_set_address (ATTRIBUTE_TABLE_O_ADDRESS + 8)
	assign_16i paddr, title_attributes
	ldy #0
loop:
	lda (paddr),y
	sta PPUDATA
	iny
	cpy #8
	bne loop

	; clear star locations
	lda #0
	ldx #0
@loop:

	sta starlocations, x
	inx
	cpx #20
	bne @loop

	jsr ppu_update
	
	rts
.endproc