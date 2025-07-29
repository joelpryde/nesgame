; larger memory allocations
.segment "BSS"
enemydata: .res 100 ; enemy tracking data (10 * 10 bytes)

.segment "CODE"


.proc spawn_enemies

	; decrement enemy cooldown and return if not 0
	ldx enemycooldown		
	dex
	stx enemycooldown
	cpx #0
	beq :+
	rts
:

	; set a short cooldown
	ldx #1
	stx enemycooldown

	; use level and random value to see if we should spawn
	lda level 				; get the current level
	clc
	adc #1						; increment by 1
	asl
	asl								; multiply by 4 (shift left twice)
	sta temp					; save our value
	jsr rand					; get next random value
	tay								; transfer value to y register
	cpy temp
bcc :+						; continue if random value is less than calculated value
	rts
:

	; set new cooldown period
	ldx #20						
	stx enemycooldown

	; see if new enemy object is available
	ldy #0
	sty temp
@loop:
	lda enemydata, y
beq :+
	tya								; increase counter by 10 (need to transfer to accumulator)
	adc #10
	tay
	inc temp
	lda temp
	cpy #10
	bne @loop
	rts								; did not find enemy to use
:

	; determine type of enemy to select and mark it as inuse
	sty temp + 1			; save y value

	; update enemycount
	inc enemycount
	lda #40
	cmp enemycount
	bne @notendoflevel
		inc level
		lda #64
		sta displaylevel
		lda #0
		sta enemycount
		lda #%00010000	; set flag for level to be displayed
@notendoflevel:

	jsr rand					; determine the enemy type
	ldy temp + 1
	and #%1111
	cmp #$0C
	bne @notSmartBomb
	lda #3						; set the enemy type as smart bomb
	jmp @setEnemyType
@notSmartBomb:
	and #%1						; A will be 0 or 1
	clc
	adc #1						; A will be 1 (large meteor) or 2 (small meteor)
@setEnemyType:
	sta enemydata, y	; mark the enemy as in use (with type)

	; copy enemy data from ROM table into RAM table
	sec								; get the enemy data
	sbc #1
	sta temp + 1			; save as our loop counter
	beq @skipMultiply	; skip if zero
	lda #0
	clc								; multiply the enemy type by 9
@loop5:
	adc #9
	dec temp + 1
	bne @loop5
@skipMultiply:
	tax
	tya								; save y
	pha
	iny
	lda #9						; copy 8 bytes
	sta temp + 1
@loop4:
	lda enemy_source_data, x
	sta enemydata, y
	inx
	iny
	dec temp + 1
	bne @loop4
	pla								; restore 7
	tay
	lda enemydata, y

	; calculate the start sprite position in oam sprite table
	lda temp					; calculate the first sprite oam position
	asl								; multiple by 16 (left shift four times)
	asl
	asl
	asl
	clc
	adc #20						; skip the first 5 sprites (20 bytes)
	tax

	; if our enemy has a change of x velocity
	; randomly start to the left or right
	lda enemydata + 4, y
beq @noAdjustX
	sty temp + 1
	jsr rand
	ldy temp + 1
	and #%1
	beq @noAdjustX
	lda enemydata + 4, y
	eor #$FF					; make negative
	clc
	adc #$01
	sta enemydata + 4, y
@noAdjustX:

	; now setup the enemy sprite
	lda enemydata + 3, y	; get number of sprites used
	cmp #1
	bne @fourSprites

	; only one sprite used
	lda #0						; set the y position (byte 0)
	sta oam, x
	lda #$FF					; hide sprites in the group
	sta oam + 4, x
	sta oam + 8, x
	sta oam + 12, x
	lda enemydata + 1, y	; get starting pattern
	sta oam + 1, x		; set the index number (byte 1) of sprite pattern
	lda enemydata + 9, y
	sta oam + 2, x		; set sprite attributes
	jsr rand
	and #%01110000
	clc
	adc #48
	sta oam + 3, x		; set the x pos
	rts

	; four sprites in use
@fourSprites:
	lda #0
	sta oam, x				; 1
	sta oam + 4, x
	lda #8
	sta oam + 8, x
	sta oam + 12, x
	lda enemydata + 1, y	; 2
	sta oam + 1, x		; 3
	clc
	adc #1
	sta oam + 5, x
	adc #1
	sta oam + 9, x
	adc #1
	sta oam + 13, x
	lda enemydata + 9, y
	sta oam + 2, x		; 4
	sta oam + 6, x
	sta oam + 10, x
	sta oam + 14, x
	jsr rand
	and #%11110000
	clc
	adc #48
	sta oam + 3, x		; 5
	sta oam + 11, x
	clc
	adc #8
	sta oam + 7, x
	sta oam + 15, x

	rts
.endproc

.proc move_enemies

	; load bullet args for collision detection between bullet and enemy
	lda oam + 16			; get bullet y
	sta cy1
	lda oam + 19			; get bullet x
	sta cx1
	lda #4						; bullet is 4 pixels heigh
	sta ch1
	lda #1						; bullet is 1 pixel wid
	sta cw1

	ldy #0
	lda #0
	sta temp + 2			; initialize loop counter
@loop:
	lda enemydata, y
	bne :+
		jmp @skip				; enemy not on screen, skip to next
	:
	lda temp + 2			; enemy is on screen, calculate first sprite oam position
	asl								; multiply by 16 (left shift four times)
	asl
	asl
	asl
	clc
	adc #20						; skip first five sprites
	tax

	; move smart bomb towards player
	lda enemydata, y
	cmp #3						; is it a smart bomb
	bne @notSmartBomb
	lda enemydata + 4, y
	and #%10000000		; get dx
	beq @movingRight
	lda oam + 3				; get player x
	cmp oam + 3, x
	bcc @notSmartBomb		; is smart bomb to the left of player
	sec
	sbc oam + 3, x
	cmp #32
	bcc @notSmartBomb
	lda enemydata + 4, y
	eor #$FF					; make negative
	clc
	adc #$01
	sta enemydata + 4, y
	jmp @notSmartBomb
@movingRight:
	lda oam + 3				; get player x pos
	clc
	adc #12						; adjust for width of player
	cmp oam + 3, x
	bcs @notSmartBomb	; is smart bomb to right of player
	lda oam + 3, x
	sec
	sbc oam + 3				; get difference between player and smart bomb
	cmp #44						; is it within 32 pixels
	bcc @notSmartBomb
	lda enemydata + 4, y
	eor #$FF					; make negative (flip all bits and add one)
	clc
	adc #$01
	sta enemydata + 4, y
@notSmartBomb:

	; adjust enemy x position
	lda enemydata + 4, y
beq @noMoveX
	clc
	adc oam + 3, x
	sta oam + 3, x
	sta oam + 11, x
	clc
	adc #8
	sta oam + 7, x
	sta oam + 15, x
@noMoveX:

	; adjust enemy y position
	lda oam, x				; get enemy y pos
	clc
	adc enemydata + 5, y	; add the change in y from table
	sta oam, x				; save the new y pos
	clc
	adc enemydata + 8, y	; add the enemy height
	cmp #204
	bcc @nohitbottom
	lda #255					; has reached the ground
	sta oam, x				; hide all sprites
	sta oam + 4, x
	sta oam + 8, x
	sta oam + 12, x

	;
	; we hit bottom!
	;

	; check if smartbomb, if so flash screen and shake screen
	lda enemydata, y
	cmp #3
	bne @notSmartBomb2
		lda #32
		sta flash
		sta shake

		lda #FAMISTUDIO_SFX_CH1	; play big boom sound effect
		sta sfx_channel
		lda #2
		jsr play_sfx
@notSmartBomb2:		

	; clear the enemy's in-use flag
	lda #0						
	sta enemydata, y
	clc								; check that the score is not zero
	lda score
	adc score + 1
	adc score + 2
	bne :+
		jmp @skip
	:

	; subtract 10 from score
	lda #1
	jsr subtract_score
	jmp @skip
@nohitbottom:

	; check for collision with player's ship
	lda enemydata + 3, y
	cmp #1						; does the enemy only have one pattern
beq :+
	lda oam, x				; update the other sprite y positions
	sta oam + 4, x
	clc
	adc $8
	sta oam + 8, x
	sta oam + 12, x
:

	; animate enemy sprite
	lda time
	and #%11					; only animate every four frames
bne @noanimate
	lda enemydata + 1, y	; get the starting patthern and check i the enemy has more than one pattern
	cmp enemydata + 2, y
beq @noanimate
	lda enemydata + 3, y	; split depending on whether we have one sprite or many
	cmp #1
beq @singleSprite
	lda oam + 1, x		; get the first sprite pattern number
	clc
	adc #4						; go to the next sprite pattern
	cmp enemydata + 2, y
	beq :+
	bcc :+
		lda enemydata + 1, y	; past the end patter, get the starting pattern
	:
	sta oam + 1, x		; update with next pattern
	clc
	adc #1
	sta oam + 5, x		; update the other patterns
	clc
	adc #1
	sta oam + 9, x
	clc
	adc #1
	sta oam + 13, x
	jmp @noanimate
@singleSprite:		; handle single sprite
	lda oam + 1, x	; get the first pattern
	clc
	adc #1					; go to next pattern
	cmp enemydata + 2, y
	beq :+
	bcc :+
		lda enemydata + 1, y	; past the end, so go to starting pattern
	:
	sta oam + 1, x
@noanimate:

	; check that the player is not currently dead
	lda player_dead
	cmp #0
	bne @notlevelwithplayer
	
	; if player is alive, check whether enemy is at same level as player
	lda oam, x				; get enemy y
	clc
	adc enemydata + 8, y	; add on the enemy height
	cmp #$c4					; is the enemy level with player
	bcc @notlevelwithplayer

	; compare x position of the enemy and player ship
	lda oam + 3				; get player x position
	clc
	adc #12						; add the width of player
	cmp oam + 3, x		; is the enemy's x larger than player pos + width
	bcc @notlevelwithplayer
	lda oam + 3, x		; get the enemy x position
	clc
	adc enemydata + 7, y	; add on it's width
	cmp oam + 3				; is the enemy's x plus width smaller than player's x pos
	bcc @notlevelwithplayer

	; player has been hit, decrease lives, set flag to diplay lives
	; play boom affect, and mark player as dead
	dec lives					; decrease our lives counter
	lda #%00000100		; set the flag so the lives are displayed
	ora update
	sta update
	lda #FAMISTUDIO_SFX_CH1	; play big boom sound effect
	sta sfx_channel
	lda #2
	jsr play_sfx
	lda #1						; mark the player as dead
	sta player_dead

	; remove the enemy sprite from the screen and clear data flag
	lda #$FF
	sta oam, x				; erase the enemy
	sta oam + 4, x
	sta oam + 8, x
	sta oam + 12, x
	lda #0						; clear enemy's data flag
	sta enemydata, y
	jmp @skip

@notlevelwithplayer:

	; check for collision with player's bullet
	lda oam + 16
	cmp #$FF					; is the bullet on the screen?
	beq @skip
	lda oam, x				; get enemy y pos
	sta cy2
	lda oam + 3, x		; get enemy x pos
	sta cx2
	lda enemydata + 7, y ; get enemy width
	sta cw2
	lda enemydata + 8, y ; get enemy height
	sta ch2
	jsr collision_test
	bcc @skip

	; remove bullet and enemy from screen and add to score
	lda #$FF
	sta oam + 16			; erase the player bullet
	sta oam, x				; erase the enemy
	sta oam + 4, x
	sta oam + 8, x
	sta oam + 12, x
	lda #0						; clear the enemy's data flag
	sta enemydata, y
	lda enemydata + 6, y ; add enemy's points to score
	jsr add_score

	lda #FAMISTUDIO_SFX_CH1
	sta sfx_channel
	lda #1						; play boom sound effect
	jsr play_sfx

	; increment counter and check if we have gone through all of them
@skip:
	tya								; go to next enemy
	clc
	adc #10
	tay
	inc temp + 2
	lda temp + 2
	cmp #10
	beq :+
		jmp @loop
	:

	rts
.endproc