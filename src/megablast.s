; more code
.include "neslib.s"
.include "interupts.s"
.include "sound.s"
.include "score.s"
.include "title.s"
.include "background.s"
.include "player.s"
.include "enemies.s"

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
.incbin "../bin/megablast.chr"

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
enemycount: .res 1		; number of enemies that have appeard
displaylevel: .res 1  ; counter for displaying level 

starlocations: .res 10 * 2 ; 2 bytes per star

; sprite oam data
.segment "OAM"
oam: .res 256

; larger memory allocations
.segment "BSS"
palette: .res 32 ; current palette buffer

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

leveltext:
.byte " L E V E L ",0

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

.proc main
	jsr init_sound

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

	; initialize PPU control register for sprite pattern table
	lda #%10001000		; NMI enabled, sprites use pattern table 0
	sta ppu_ct10
	lda #%00011110		; sprites and background enabled
	sta ppu_ct11

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

	; display level at start of game
	lda #3					; set player starting lives
	sta lives
	lda #0					; reset our player dead flag
	sta player_dead
	jsr display_game_screen	; display game screen
	jsr display_player	; display player ship
	lda #64
	sta displaylevel
	lda #%00010001	;set flag so current score and level will be displayed
	ora update
	sta update

	jsr ppu_update

	; play first song
	lda #0
	jsr play_music

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

	; check for removing the level display
	lda displaylevel
	beq @nodisplaylevelcountdown
		dec displaylevel
		bne @nodisplaylevelcountdown
		lda #%00100000
		ora update
		sta update
@nodisplaylevelcountdown:

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
	sta PPUDATA					; write the byte to video memory
	iny
	jmp loop
exit:
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
	sta PPUDATA
	iny
	cpy #32
	bne loop

	; draw baseline
	vram_set_address (NAME_TABLE_0_ADDRESS + 26 * 32)
	ldy #0
	lda #9
loop2:
	sta PPUDATA
	iny
	cpy #32
	bne loop2

	; output score section on next line
	assign_16i paddr, game_screen_scoreline
	ldy #0
loop3:
	lda (paddr),y
	sta PPUDATA
	iny
	cpy #13
	bne loop3

	jsr display_lives
	jsr place_stars

	jsr ppu_update	;wait for screen to be drawn
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

	; reset enemy counter
	lda #0
	sta enemycount
	lda #64
	sta displaylevel
	
	; set a flag so current level will be displayed
	lda #%00010000
	ora update
	sta update

	rts
.endproc