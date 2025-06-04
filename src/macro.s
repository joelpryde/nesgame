.macro vram_set_address newaddress
	lda PPU_STATUS
	lda #>newaddress
	sta PPU_VRAM_ADDRESS2
	lda #<newaddress
	sta PPU_VRAM_ADDRESS2
.endmacro

.macro assign_16i dest, value
	lda #<value
	sta dest+0
	lda #>value
	sta dest+1
.endmacro

.macro vram_clear_address
	lda #0
	sta PPU_VRAM_ADDRESS2
	sta PPU_VRAM_ADDRESS2
.endmacro
