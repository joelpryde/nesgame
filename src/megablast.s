.segment "HEADER"

INES_MAPPER = 0 ; NROM
INES_MIRROR = 0 ;0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM = 0 ; 1 = battery-backed SRAM at $6000-7FFF

.byte 'N', 'E', 'S', $1A ; nes ID
.byte $02 ; 16k PRG bang count
.byte $01 ; 8k CHR bank count
.byte INES_MIRROR | (INES_SRAM << 1) | ((INES_MAPPER & $f) << 4)
.byte (INES_MAPPER & %11110000)
.byte $0, $0, $0, $0, $0, $0, $0, $0 ; padding

; import background and sprite character sets
.segment "TILES"
.incbin "megablast.chr"

; interupt handlers
.segment "VECTORS" 
.word nmi
.word reset
.word irq

; variable storage (in zero-page)
.segment "ZEROPAGE"

text_address:	.res 2	; address of text to write
paddr: .res	2					; 16-bit address pointer

time: .res 2
lasttime: .res 1

level: .res 1
animate: .res 1
enemycooldown: .res 1
temp: .res 10
score: .res 3					; player's current score
update: .res 1				; flag to know when score has changed
lives: .res 1					; player lives
player_dead: .res 1		; is player dead, then tracks death animation frame 
flash: .res 1					; number of times left to flash screen
shake: .res 1					; number of times to shake screen

starlocations: .res 10 * 2 ; 2 bytes per star

; sprite oam data
.segment "OAM"
oam: .res 256

.include "neslib.s"

; larger memory allocations
.segment "BSS"
palette: .res 32 ; current palette buffer
enemydata: .res 100 ; enemy tracking data (10 * 10 bytes)

.segment "CODE"

; allow for irq clock interupt to call
irq:
	rti

; default palette for tiles and sprites
.segment "RODATA"
default_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$19,$29,$39 ; bg1 green
.byte $0F,$11,$21,$31 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$28,$21,$11 ; sp0 player
.byte $0F,$26,$28,$17 ; sp1 explosion
.byte $0F,$38,$28,$18 ; sp2 browns
.byte $0F,$12,$22,$32 ; sp3 marine

game_screen_mountain:
.byte 001,002,003,004,001,002,003,004,001,002,003,004,001,002,003,004
.byte 001,002,003,004,001,002,003,004,001,002,003,004,001,002,003,004
 
game_screen_scoreline:
.byte "SCORE 0000000"

gameovertext:
.byte " G A M E  O V E R", 0

; table of enemy data values (9 bytes)
; enemy type (1 = large meteor, 2 = small meteor, 3 = smart bomb, 4 = explosion)
; starting shape sprite number
; ending shape sprite number
; number of sprites
; dx, starting change in x
; dy, starting change in y
; value added to score when destroyed
; width for collision detection
; height for collision detection
; sprite attributes (set palette)
enemy_source_data:
.byte 008,012,004,000,002,002,012,012,003    	; large meteor
.byte 036,037,001,001,003,003,008,007,002    	; small meteor
.byte 016,019,001,002,003,006,008,008,003    	; smart bomb
.byte 040,044,004,000,000,000,000,000,001			; enemy explosion

; main application entry point for startup/reset
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
	sta PPU_VRAM_IO
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

	jsr animate_stars

	; shake screen using scroll and screen settings
	lda shake
	beq :+
		dec shake
		and #%11
		asl a
		asl a
	:
	sta PPU_VRAM_ADDRESS1
	sta PPU_VRAM_ADDRESS1

	lda ppu_ct10
	sta PPU_CONTROL
	lda ppu_ct11
	sta PPU_MASK

@skip_ppu_update:

	; flag that the ppu has update is complete
	ldx #0
	stx nmi_ready
	pla								; restor registers and returns
	tay
	pla
	tax
	pla
	rti
.endproc

.proc main
	; turn off rendering immediately
	lda #0
	sta PPU_MASK
	
	ldx #0
paletteloop:				; intitialize palette table
	lda default_palette, x
	sta palette, x
	inx
	cpx #32
	bcc paletteloop

	; setup first level
	lda #1
	sta level
	jsr setup_level

resetgame:

	; reset player's score
	lda #0
	sta score
	sta score + 1
	sta score + 2

	lda #3					; set the player's starting lives
	sta lives
	lda #0					; reset the player dead flag
	sta player_dead

	jsr clear_sprites

	jsr display_title_screen	; draw title screen

	; setup game settings
	lda #VBLANK_NMI|BG_0000|OBJ_1000
	sta ppu_ct10
	lda #BG_ON|OBJ_ON
	sta ppu_ct11

	jsr ppu_update

	; wait for gamepad to be pressed
titleloop:
	jsr gamepad_poll
	lda gamepad
	and #PAD_A|PAD_B|PAD_START|PAD_SELECT
	beq titleloop

	; use time of title screen clear to seen random values
	lda time
	sta SEED0
	lda time + 1
	sta SEED0 + 1
	jsr randomize
	sbc time + 1
	sta SEED2
	jsr randomize
	sbc time + 1
	sta SEED2
	jsr randomize
	sbc time
	sta SEED2 + 1

	jsr display_game_screen	; draw game screen

	jsr display_player			; display player ship

	jsr ppu_update

mainloop:
	lda time

	cmp lasttime						; ensure time has changed
	beq mainloop
	sta lasttime						; update lasttime

	lda lives								; handle game over message
	bne @notgameover
	lda player_dead
	cmp #1
	beq @notgameover
	cmp #240								; have waited long enough, jump back to title screen
	beq resetgame
	cmp #20
	bne @notgameoversetup
	lda #%00001000					; signal to display the game-over message
	ora update
	sta update
@notgameoversetup:
	inc player_dead
	jmp mainloop
@notgameover:

	jsr player_actions 			; update ship sprites from player input
	jsr move_player_bullet	; and bullet
	jsr spawn_enemies				; and attempt to spawn enemies
	jsr move_enemies				; and move enemies

	; swap two colors in the palette table for flash
	lda time
	and #%111
	bne @nopalettechange
		ldx palette + 1
		lda palette + 2
		sta palette + 1
		stx palette + 2
@nopalettechange:

	; decrement flash value and change background color between black ($0f) and white ($30)
	lda flash
	beq @noflash
		dec flash
		lda palette
		cmp #$0F
		bne @noflash
			lda #$30
			sta palette
			sta palette + 16
			jmp mainloop
@noflash:
	lda #$f
	sta palette
	sta palette + 16

	jsr ppu_update					; update ppu each frame

	jmp mainloop
.endproc

.proc write_text
	ldy #0
loop:
	lda (text_address), y 	; gets the btye at the current source address
	beq exit								; exit when we encounter a zero in the text
	sta PPU_VRAM_IO					; write the byte to video memory
	iny
	jmp loop
exit:
	rts
.endproc

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
	sta PPU_VRAM_IO
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
	
	rts
.endproc

.proc display_game_screen
	jsr ppu_off						; wait for screen clear

	jsr clear_nametable		; clear first name table

	; output mountain line
	vram_set_address (NAME_TABLE_0_ADDRESS + 22 * 32)
	assign_16i paddr, game_screen_mountain
	ldy #0
loop:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #32
	bne loop

	; draw baseline
	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32)
	ldy #0
	lda #9
loop2:
	sta PPU_VRAM_IO
	iny
	cpy #32
	bne loop2

	; output score section on next line
	assign_16i paddr, game_screen_scoreline
	ldy #0
loop3:
	lda (paddr),y
	sta PPU_VRAM_IO
	iny
	cpy #13
	bne loop3

	jsr display_lives

	jsr place_stars

	jsr ppu_update	;wait for screen to be drawn
	rts
.endproc

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

.proc setup_level

	; clear enemy data
	lda #0				
	ldx #0
@loop:
	sta enemydata, x
	inx
	cpx #100			; 100 bytes to clear
bne @loop

	; set initial cooldown
	lda #20				
	sta enemycooldown
	
	; hide all enemy sprites
	lda #$FF
	ldx #0
@loop2:
	sta oam + 20, x
	inx
	cpx #160
bne @loop2

	rts
.endproc

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
	; and mark player as dead
	dec lives					; decrease our lives counter
	lda #%00000100		; set the flag so the lives are displayed
	ora update
	sta update
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
	lda 204
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

.proc display_lives

	; write top row of tiles
	vram_set_address (NAME_TABLE_0_ADDRESS + 27 * 32 + 14)
	ldx lives
	beq @skip					; no lives to display
	and #%00000111		; limit to max of 8
@loop:
	lda #5
	sta PPU_VRAM_IO
	lda #6
	sta PPU_VRAM_IO
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
	sta PPU_VRAM_IO
	sta PPU_VRAM_IO
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
	sta PPU_VRAM_IO
	lda #8
	sta PPU_VRAM_IO
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
	sta PPU_VRAM_IO
	sta PPU_VRAM_IO
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
	sta PPU_VRAM_IO

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
	sta PPU_VRAM_ADDRESS2
	lda starlocations, x
	sta PPU_VRAM_ADDRESS2

	; using time counter, grab third bit to determine whether we show first or second star pattern
	lda time
	lsr
	lsr
	and #%1
	clc
	adc #12
	sta PPU_VRAM_IO

	; udpate x to point to next location and check to see if we have processed all
	inx
	inx
	cpx #20
	bne @loop
	rts
.endproc
