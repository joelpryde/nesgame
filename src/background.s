.segment "CODE"

.proc display_lives

	; write top row of tiles
	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 14)
	ldx lives
	beq @skip					; no lives to display
	and #%00000111		; limit to max of 8
@loop:
	lda #5
	sta PPUDATA
	lda #6
	sta PPUDATA
	dex
	bne @loop
@skip:

	; and bottom row of blank tiles to remove any previous tiles
	lda #8						; blank out the remainder of the row
	sec
	sbc lives
	bcc @skip2
	tax
	lda #0
@loop2:
	sta PPUDATA
	sta PPUDATA
	dex
	bne @loop2
@skip2:

	; repeat above code with two different tiles for the base
	vram_set_address (NAME_TABLE_0_ADDRESS + 28 * 32 + 14)
	ldx lives
	beq @skip3				; no lives to display
	and #%00000111		; limit to max of 8
@loop3:
	lda #7
	sta PPUDATA
	lda #8
	sta PPUDATA
	dex
	bne @loop3
@skip3:

	lda #8						; blank out the remainder of the row
	sec
	sbc lives
	bcc @skip4
	tax
	lda #0
@loop4:
	sta PPUDATA
	sta PPUDATA
	dex
	bne @loop4
@skip4:

	rts
.endproc

.proc place_stars

	; clear star locations
	lda #0
	ldx #0
@loop:
	sta starlocations, x
	inx
	cpx #20
	bne @loop

	; loop and set random row (rlower nibble + 1)
	ldx #0
@loop2:
	jsr rand
	pha								; save a
	and #%1111				; get the lower nibble
	clc
	adc #1						; skip 0
	sta temp 					; this is row of star
	pla								; get a back

	; set random column (upper 4 bits, multiplied by 2 = 0-31)
	lsr
	lsr
	lsr
	sta temp + 1

	; add 32 until we get to our chosen row
	assign_16i paddr, NAME_TABLE_0_ADDRESS + 64	; start from second row
@loop3:
	add_16_8 paddr, #32	; add 32 for each row
	dec temp
	bne @loop3

	; then add column value for final screen location
	add_16_8 paddr, temp + 1

	; write pattern of star to screen
	vram_set_address_i paddr
	lda #12
	sta PPUDATA

	; save our calculated address so we can change star pattern later
	lda paddr
	sta starlocations, x
	lda paddr + 1
	sta starlocations + 1, x

	; increment x to point to our next start location entry and iterate if not done
	inx
	inx
	cpx #20
	beq :+
		jmp @loop2
	:

	rts
.endproc



; animate stars by changing their pattern
.proc animate_stars

	; apply change every 4 ticks
	lda time
	and #%11
	beq :+
		rts
	:

	; check to see if star locations have been set
	ldx #0
	lda starlocations, x
	bne @loop
		rts 			; skip processing if no stars
@loop:

	; for each location, use stored value to set vram address
	lda PPU_STATUS
	lda starlocations + 1, x
	sta PPUADDR
	lda starlocations, x
	sta PPUADDR

	; using time counter, grab third bit to determine whether we show first or second star pattern
	lda time
	lsr
	lsr
	and #%1
	clc
	adc #12
	sta PPUDATA

	; udpate x to point to next location and check to see if we have processed all
	inx
	inx
	cpx #20
	bne @loop
	rts
.endproc