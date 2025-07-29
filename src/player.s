.segment "CODE"

.proc player_actions
	lda player_dead
	beq @continue

	cmp #1							; player flagged dead, set intitial shape
	bne @notstep1

	ldx #8							; set the fire explosion pattern
	jsr set_player_shape
	lda #$00000001			; select the second palette
	sta oam + 2
	sta oam + 6
	sta oam + 10
	sta oam + 14
	jmp @nextstep

	; wait four frames and then change player shape to next explosion pattern
@notstep1:
	cmp #5							; ready to change to next explosion shape
	bne @notstep2
	ldx #12							; set the second explosion pattern
	jsr set_player_shape
	jmp @nextstep

	; after five more steps, next explosion pattern
@notstep2:
	cmp #10							; ready to change to next explosion shape
	bne @notstep3
	ldx #16							; set third explosion pattern
	jsr set_player_shape
	jmp @nextstep

	; after five more frames, next explosion pattern
@notstep3:
	cmp #15							; read to change to next explosion shape
	bne @notstep4
	ldx #20							; set the fourth explosion pattern
	jsr set_player_shape
	jmp @nextstep

	; after five more frames, check lives for game over
@notstep4:
	cmp #20							; explosion is finished, reset player
	bne @nextstep
	lda lives
	cmp #0							; check for game over
	bne @notgameover
	rts

;.endproc 

@notgameover:
	jsr setup_level			; reset all enemy objects
	jsr display_player	; display the player at the starting position
	lda #0							; clear the player dead flag
	sta player_dead
	rts
@nextstep:
	inc player_dead
	rts
@continue:

	jsr gamepad_poll		; check gamepad to see what is selected
	lda gamepad
	and #PAD_L					; see if left is selected
	beq not_gamepad_left
		lda oam + 3				; get the x pos of ship
		cmp #0						; check if at left side of screen
		beq not_gamepad_left

		sec
		sbc #2						; subtract 2 from x pos

		sta oam + 3				; update sprite positions
		sta oam + 11
		clc
		
		adc #8						; adjust to right side of ship
		sta oam + 7
		sta oam + 15

not_gamepad_left:

	lda gamepad
	and #PAD_R					; check right bit
	beq not_gamepad_right
		lda oam + 3				; get x pos of ship
		clc
		adc #12						; allow for width of ship when checking right side
		cmp #254
		beq not_gamepad_right
		lda oam + 3				; get x pos of ship
		clc
		adc #2						; add 2 to  x pos

		sta oam + 3				; update sprite positions
		sta oam + 11
		clc
		adc #8						; adjust to right side of ship
		sta oam + 7
		sta oam + 15

not_gamepad_right:

	lda gamepad
	and #PAD_A					; check a button
	beq not_gamepad_a
		lda oam + 16			; get y position of player bullet
		cmp #$FF					; see if sprite not in use
		bne not_gamepad_a

			lda #192				; place the bullet
			sta oam + 16		; set y position
			lda #4
			sta oam + 17		; set bullet sprite
			lda #0
			sta oam + 18		; set bullet attribute
			lda oam + 3			; get x position of ship
			clc
			adc #6					; adjust the c position to center the bullen on the player hsip
			sta oam + 19		; set bullet x pos

			lda #FAMISTUDIO_SFX_CH1	; play zap sound affect
			sta sfx_channel
			lda #0
			jsr play_sfx

not_gamepad_a:

	rts
.endproc

.proc place_ship
	; set y positions of player ship sprites
	lda #192
	sta oam
	sta oam + 4
	lda #200
	sta oam + 8
	sta oam + 12

	; set index number of player sprite
	ldx #0
	stx oam + 1
	inx
	stx oam + 5
	inx
	stx oam + 9
	inx
	stx oam + 13

	; set the sprite attributes
	lda #%00000000
	sta oam + 2
	sta oam + 6
	sta oam + 10
	sta oam + 14

	; set x positions of player ship sprites
	lda #120
	sta oam + 3
	sta oam + 11
	lda #128
	sta oam + 7
	sta oam + 15

	rts
.endproc

.proc move_player_bullet
	lda oam + 16			; get current y pos of bullet
	cmp #$FF					; see if bullet is on screen
	beq @exit
		sec
		sbc #4					; move upwards by 4
		sta oam + 16		; store new y pos
		bcs @exit
			lda #$FF			; value is carried, so we are off screen, hide it
			sta oam + 16

@exit:
	rts
.endproc

.proc set_player_shape
	stx oam + 1
	inx
	stx oam + 5
	inx
	stx oam + 9
	inx
	stx oam + 13
	rts
.endproc

.proc display_player
	lda #196					; set y pos of all four parts of player ship
	sta oam
	sta oam + 4
	lda #204
	sta oam + 8
	sta oam + 12

	ldx #0						; set the index number of spprite pattern
	stx oam + 1
	inx
	stx oam + 5
	inx
	stx oam + 9
	inx
	stx oam + 13

	lda #%00000000		; set sprite attributes
	sta oam + 2
	sta oam + 6
	sta oam + 10
	sta oam + 14

	lda #120					; set the x pos of all four parts of player ship
	sta oam + 3
	sta oam + 11
	lda #128
	sta oam + 7
	sta oam + 15
	rts
.endproc