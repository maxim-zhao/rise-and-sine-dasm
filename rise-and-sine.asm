.MEMORYMAP
SLOTSIZE 255
SLOT 0 $0000
DEFAULTSLOT 0
.ENDME
.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE 255
BANKS 1
.ENDRO

.enum $C000 export
RAM_SineTable1 dsb 256
RAM_SineTable2 dsb 256
RAM_SpriteTableYs dsb 64
RAM_SpriteTableGap dsb 64
RAM_SpriteTableXNs dsb 64*2
.ende

.enum $C3E0 export
RAM_TilemapStart dsb 32
.ende

; Ports
.define Port_VDPData $BE
.define Port_VDPAddress $BF

; Input Ports
.define Port_VCounter $7E

; Helper function for combining bytes to words
.function word(hi, lo) ((hi & 0xff) << 8) | (lo & 0xff)

.BANK 0 SLOT 0
.ORG $0000

boot:
	di
MakeSineTablesPart1:
	ld de, word(0, -2) ; $00FE ; Initial quadratic gradient as 8.8 fixed-point = -2/256
	ld h, d ; hl = fixed-point sine result = 0
	ld l, d
	jr +

HalfSetVDPAddress:
	; Sets half of the VDP address,
	; leaves c set to Port_VDPData and sets b to 18. 
	; This saves a few bytes as we can otir b bytes after calling it,
	; but the value 18 is only useful once.
	out (Port_VDPAddress), a
	ld bc, word(_sizeof_Palette, Port_VDPData)
	ret

MakeSineTablesPart2:
+:
	; Fill tables from $c000 to $c1ff with sine curves.
	; See https://github.com/neonz80/sine for an alternative explanation.

	; RAM_SineTable1 is sin(2πx/256) * 64 + 64, RAM_SineTable2 is double that
	ld bc, RAM_SineTable1
	; We make use of two extra parts of this pointer:
	; 1. The low byte is a counter from 0 to 255
	; 2. The high byte is -64
-:
	rrc c 
	; Low bit of c will alternate between 0 and 1. 
	; This also means that iterating from 0 to 255 actually gets us 0, 128, 1, 129, 2, 130, etc.
	; That means the even values populate the positive half of the sine wave, and the odd values the negative half.
	sbc a, a 
	; Will be 0 if no carry, -1 if carry
	xor h 
	; xor in our quadratic's integer part. This is approximately negating it for the negative half.
	sub b 
	; b is always $c0 = -64, so this shifts us up by 64

	; save into sine table, second half for odd c
	ld (bc), a 

	; And double it to save into RAM_SineTable2
	add a, a 
	inc b
	ld (bc), a
	dec b

	; Now we approximate sin by adding the fixed-point delta, which itself is linearly decreasing.
	; This means that our sin is really a sort of quadratic curve.
	add hl, de
	dec de ; Apply a constant rate of change to the gradient
	dec de

	rlc c ; Restore c before incrementing it
	inc c
	jr nz, - ; repeat for 256 entries

LoadTiles:
	; At the end, hl = $ff00

	; Set VDP address to $4000
	xor a
	rst HalfSetVDPAddress
	ld a, $40
	rst HalfSetVDPAddress

	; Load 256 variations of the same 1bpp tile into VRAM
	; Each selects the high 4 bits of its index, reversed, as its colour.
	; This means each 16 tiles are the same colour.
	ld d, l ; d = 0
---:
	ld hl, HeartBitmap
	ld c, 8 ; Rows
--:
	ld b, 4 ; Bitplanes
	ld e, d ; Bitplane mask from current value in d
	nop ; Not needed, not sure why this is here
-:
	rl e ; Rotate mask bits in as colour selectors
	sbc a, a ; Make 0 or -1 from high bit in e
	and (hl) ; Zero if 0, bitmap bits if -1
	out (Port_VDPData), a ; Write to VRAM
	djnz - ; Repeat for 4 bitplanes

	inc l ; Next row
	dec c
	jr nz, --
	
	inc d ; Next mask
	jr nz, ---
LoadPalette:
	; Set the VDP to the palette
	rst HalfSetVDPAddress
	ld a, $C0
	rst HalfSetVDPAddress
	; Load the palette as 18 colours (since HalfSetVDPAddress left b=18)
	otir

LoadRegisters:
	; Then increment the port number to set the VDP registers
	inc c
	; Then load the register values
	ld b, _sizeof_RegisterData
	otir
	; fall through to the main loop

WaitForVBlank:
	in a, (Port_VCounter)
	sub $C1
	jr nz, WaitForVBlank

	; We are on line $c1, which is vblank.

EmitTilemap:
	; Point the VDP at the tilemap, ready to write
	; Low address byte is a = 0
	rst HalfSetVDPAddress
	ld a, >TileMapAddress | $40
	rst HalfSetVDPAddress

	; Emit tilemap from the previous frame
	; The tilemap data is stored as 32 bytes, every 256 bytes from $c3e0.
	; This allows us to increment the high byte between rows, and use the 
	; low byte of the address as the loop counter for rows, as it will
	; be zero when we reach the end of each row.
	ld h, >RAM_TilemapStart
--:
	ld l, <RAM_TilemapStart ; = -32 - because each tilemap row has 32 entries and thus allows us to check l = 0 when producing it later
	ld b, 32 ; column count
	xor a ; High byte of each tilemap entry is 0 -> we just pick tiles from 0 to 256
-:	; Emit 32 tilemap entries from (hl) for one row.
	; The code is duplicated in the loop, for speed: we run out of VBlank time if we don't.
	; We could go faster with more unrolling if we had space...
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	jr nz, -
	; Repeat until we have output 24*2 rows
	ld a, h
	cp >RAM_TilemapStart + 24 ; $DB
	jr nz, --

EmitSpriteTable:
	ld h, >RAM_SpriteTableYs ; l is 0
	ld b, 64 * 3 ; We want the Ys, gap and XNs. More sprites would require copying more here. 
-:  outi
	jr nz, - ; 28 cycles loop = frame safe
	; We waste some frame time copying the 64 bytes "sprite table gap" to VRAM because
	; it would cost about 12 bytes more to skip it, but save about 8 lines.

ComputeNextFrameTilemap:
	; Now b=0
	inc b ; so it is 1; c = Port_VDPData = $be
	; ix is uninitialised, so acts as an arbitrary-start fixed-point counter, counting up by $01.be each frame
	add ix, bc
	; iy is similarly counted, by $01.00 each frame
	inc iyh
	; We make use of the fact that ix and iy are unaffected by the exx opcode, so we can apply these
	; to both sets of registers below.

	; Point de to the ix'th byte of the doubled sine table
	ld hl, RAM_SineTable2
	ld d, h
	ld e, ixh
	exx
		ld h, >RAM_SineTable2 ; hl' = iy'th byte of the doubled sine table
		ld a, iyh
		ld l, a
		ld d, >RAM_TilemapStart ; de' = $c3e0 at first
--:
		push hl ; Save hl'
			ld e, <RAM_TilemapStart ; de' = $xxe0
	exx
	; Read from de: a = sin(ix)
	ld a, (de)
	; Move de on by 2 entries
	inc e
	inc e
	; Add from hl: a += sin(t)
	add a, (hl)
	; Move on by 1 entry
	inc l
	exx
			ld b, a ; b' = sin(ix) + sin(t)
-:
			ld a, (hl) ; a = sin(iy)
			add a, b ; a = sin(iy) + sin(ix) + sin(t)
			inc l ; Move hl on by two entries
			inc l
			ld (de), a ; Save to the buffer at $c3e0+, then $c4e0+, ...
			inc e
			jr nz, - ; loop up to $xxff -> 32 entries
		pop hl ; restore hl'
		; Select next 32-entry chunk
		inc d
		; Loop until we've done 24 rows
		ld a, d
		cp >RAM_TilemapStart + 24 ; $DB
		jr nz, --

ComputeNextFrameSpriteTable:
		; Now the tilemap is done, we move onto the sprites

		; hl is left pointing into RAM_SineTable2; we move it on to RAM_SpriteTableYs
		ld l, e ; zero
		inc h 
		; Point de at the iy'th entry of RAM_SineTable1
		ld d, >RAM_SineTable1 ; $C0
		ld e, iyh
		; Read that in
		ld a, (de)
		ld e, a
		ld b, 32 ; Sprite count
-:		; Read in a Y coordinate
		ld a, (de) 
		; Add 32 because we are already in active display now; this shifts sprites down to after they are defined (!)
		add a, $20 
		; Save in the sprite table
		ld (hl), a
		; Add 16 to e, to offset each sprite from the previous one and spread them around the sine cycle
		ld a, e
		add a, 16 ; aka 256 entries / 32 sprites * 2 to make two loops total
		ld e, a
		inc l
		djnz -
		; Add a terminator to the sprite table
		ld (hl), $D0
	exx

	; Re-get our state counter into l
	ld a, ixh
	ld l, a
	ld de, word(>RAM_SineTable1, $18) ; $18 is the stepping through the sine table for sprite Xs
	exx
		; Point to the second half of the sprite table
		ld l, <RAM_SpriteTableXNs 
		ld b, 32 ; Sprite count
-:
		ld a, (de) ; Read the sprite Y again
	exx
	ld h, d ; So now a = (hl) = sin(ix)
	add a, (hl)
	add hl, de ; Move hl on by $18. The high byte moved too but we don't care
	srl a ; Divide the x value by 2, so it's 0..127
	exx
		sub d ; d is always $c0 do this is adding 64, making it centred horiontally
		; Save the X coordinate to the sprite table
		ld (hl), a
		inc l
		; Move our sine table counter on by 8, which is enough for one loop total
		ld a, e
		add a, $08
		ld e, a
		; Also choose tile $80 for every sprite. This maps to the white entry in the sprite palette.
		ld (hl), $80
		; Repeat for 32 sprites
		inc l
		djnz -
		jp WaitForVBlank

HeartBitmap:
.db %01101100 ; $6c
.db %11111110 ; $fe
.db %11111110 ; $fe
.db %11111110 ; $fe
.db %01111100 ; $7c
.db %00111000 ; $38
.db %00010000 ; $10
.db %00000000 ; $00
; Followed by the palette data
Palette:
; Cyclic palette for the bacjground. This has some duplicates!
.db $00, $0B, $31, $31, $20, $23, $23, $20, $10, $17, $32, $30, $30, $32, $17, $10
; Black borders, white sprites
.db $00, $3F
; Followed by the VDP register data
RegisterData:
.define TileMapAddress $3800 ; Normal location
.define SpriteTableAddress $3e00 ; Lower than normal, so it comes immediately after the visible tilemap
.define SpriteSet 0 ; Use lower 256 tiles for sprites
.db %00000100,$80
;    |||||||`- Disable sync
;    ||||||`-- Enable extra height modes
;    |||||`--- SMS mode instead of SG
;    ||||`---- Shift sprites left 8 pixels
;    |||`----- Enable line interrupts
;    ||`------ Blank leftmost column for scrolling
;    |`------- Fix top 2 rows during horizontal scrolling
;    `-------- Fix right 8 columns during vertical scrolling
.db %01000000,$81
;     |||| |`- Zoomed sprites -> 16x16 pixels
;     |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
;     |||`---- 30 row/240 line mode
;     ||`----- 28 row/224 line mode
;     |`------ Enable VBlank interrupts
;     `------- Enable display
.db (SpriteTableAddress>>7)|%10000001,$85
.db (TileMapAddress>>10)   |%11110001,$82
.db (SpriteSet<<2)         |%11111011,$86
