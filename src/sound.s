;
; include sound engine and effects data
;
.segment "CODE"
 
; FamiStudio config.
FAMISTUDIO_CFG_EXTERNAL       = 1
FAMISTUDIO_CFG_DPCM_SUPPORT   = 1
FAMISTUDIO_CFG_SFX_SUPPORT    = 1 
FAMISTUDIO_CFG_SFX_STREAMS    = 2
FAMISTUDIO_CFG_EQUALIZER      = 1
FAMISTUDIO_USE_VOLUME_TRACK   = 1
FAMISTUDIO_USE_PITCH_TRACK    = 1
FAMISTUDIO_USE_SLIDE_NOTES    = 1
FAMISTUDIO_USE_VIBRATO        = 1
FAMISTUDIO_USE_ARPEGGIO       = 1
FAMISTUDIO_CFG_SMOOTH_VIBRATO = 1
FAMISTUDIO_USE_RELEASE_NOTES  = 1
FAMISTUDIO_DPCM_OFF           = $e000
 
; CA65-specifc config.
.define FAMISTUDIO_CA65_ZP_SEGMENT   ZEROPAGE
.define FAMISTUDIO_CA65_RAM_SEGMENT  BSS
.define FAMISTUDIO_CA65_CODE_SEGMENT CODE
 
.include "famistudio/famistudio_ca65.s"
.include "famistudio/megablast-sfx.s"
.include "famistudio/megablast-music.s"

.segment "DPCM"
.incbin "../bin/megablast-music.dmc"

.segment "ZEROPAGE"
sfx_channel: .res 1		; sound effect channel to use

.segment "CODE"

; init sound engine
.proc init_sound
	lda #1					; NTSC
	ldx #.lobyte(music_data_untitled)
	ldy #.hibyte(music_data_untitled)
	jsr famistudio_init
	ldx #.lobyte(sounds)	; set the address of sound effects
	ldy #.hibyte(sounds)
	jsr famistudio_sfx_init
.endproc

; play sound effect (a = sound effect slot, sfx_channel = sound effect channel to use)
.proc play_sfx
	sta temp + 9				; save the sound effect number
	tya									; save the current register values
	pha
	txa
	pha

	lda temp + 9				; get the sound effect number
	ldx sfx_channel			; choose the channel to play sound effect on
	jsr famistudio_sfx_play

	pla									; restore register values
	tax
	pla
	tay
	rts
.endproc

; play music (a = music track number)
.proc play_music
	sta temp + 9				; save the music track number
	tya									; save current register values
	pha
	txa
	pha

	lda temp + 9				; restore track number
	jsr famistudio_music_play

	pla									; restor register values
	tax
	pla
	tay
	rts
.endproc
