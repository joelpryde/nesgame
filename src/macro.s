; Sets the PPU VRAM address pointer to the specified address
.macro vram_set_address newaddress
	lda PPU_STATUS
	lda #>newaddress
	sta PPU_VRAM_ADDRESS2
	lda #<newaddress
	sta PPU_VRAM_ADDRESS2
.endmacro

; Sets the PPU VRAM address pointer to the specified address at pointer
.macro vram_set_address_i addresspointer
  lda PPU_STATUS
	lda addresspointer+1
	sta PPU_VRAM_ADDRESS2
	lda addresspointer+0
	sta PPU_VRAM_ADDRESS2
.endmacro

; Clears the PPU VRAM address pointer to $0000
.macro vram_clear_address
	lda #0
	sta PPU_VRAM_ADDRESS2
	sta PPU_VRAM_ADDRESS2
.endmacro

; Assigns a 16-bit immediate value to a memory location (little-endian)
.macro assign_16i dest, value
	lda #<value
	sta dest+0
	lda #>value
	sta dest+1
.endmacro

; Add 8-bit value to 16-bit value
.macro add_16_8 dest, value
	lda value
		bmi :+					; handle positive
		clc
		adc dest
		sta dest
		lda dest + 1
		adc #0
		sta dest + 1
			jmp :++
	:
		clc							; handle negative
		adc dest
		sta dest
		lda dest + 1
		adc #$FF
		sta dest + 1
	:
.endmacro