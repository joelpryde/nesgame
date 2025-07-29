.segment "CODE"

.proc add_score
	clc
	adc score					; add the value in a to the first byte of score
	sta score
	cmp #99
	bcc @skip

	sec								; first byte has exceeded 99, handle overflow
	sbc #100
	sta score
	inc score + 1
	lda score + 1
	cmp #99
	bcc @skip

	sec								; second byte has exceeded 99, handle overflow
	sbc #100
	sta score + 1
	inc score + 2
	lda score + 2
	cmp #99

	bcc @skip
	sec								; third byte has exceeded 99, handle overflow
	sbc #100
	sta score + 2

@skip:
	lda #$00000001		; set the flag to write score to screen
	ora update
	sta update
	rts
.endproc
	
.proc subtract_score
	sta temp					; save A value
	sec
	lda score
	sbc temp					; subtract A value from the first byte of score
	sta score
	bcs @skip

	clc
	adc #100					; current value in A is negative, add to 100 so we are 99 or less
	sta score
	dec score + 1			; decrement second score byte
	bcs @skip

	clc								; add 100 to ensure byte 2 is 99 or less
	lda score + 1
	adc #100
	sta score + 1
	dec score + 2			; decrement our third score byte
	bcs @skip

	lda #0						; ensure score can't be less than zero
	sta score + 2
	sta score + 1
	sta score

@skip:
	lda #%00000001		; set flag to write score to screen
	ora update
	sta update
	rts
.endproc

.proc display_score
	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 6)

	lda score + 2					; transform first two digits
	jsr dec99_to_bytes
	stx temp
	sta temp + 1

	lda score + 1					; next two digits
	jsr dec99_to_bytes
	stx temp + 2
	sta temp + 3

	lda score							; final two digits
	jsr dec99_to_bytes
	stx temp + 4
	sta temp + 5

	ldx #0								; write the six characters to the screen
@loop:
	lda temp, x
	clc
	adc #48
	sta PPUDATA
	inx
	cpx #6
	bne @loop
	lda #48								; write trailing 0
	sta PPUDATA

	vram_clear_address
	rts
.endproc