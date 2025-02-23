.MEMORYMAP
SLOTSIZE 255
SLOT 0 $0000
.ENDME
.ROMBANKMAP
BANKSTOTAL 1
BANKSIZE 255
BANKS 1
.ENDRO

.enum $C000 export
RAM_SineTable1 dsb 256
RAM_SineTable2 dsb 256
.ende

; Ports
.define Port_VDPData $BE
.define Port_VDPAddress $BF

; Input Ports
.define Port_VCounter $7E

.BANK 0 SLOT 0
.ORG $0000

boot:
	di
	ld de, $00FE ; Initial quadratic gradient = -2/256
	ld h, d ; hl = fixed-point sine result = 0
	ld l, d
	jr +

HalfSetVDPAddress:
	; Sets half of the VDP address,
	; leaves c set to Port_VDPData and sets b to 18. 
	; This saves a few bytes as we can otir b bytes after calling it,
	; but the value 18 is only useful once.
	out (Port_VDPAddress), a
	ld bc, (_sizeof_Palette << 8) | Port_VDPData
	ret

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
	nop ; Not needed? TODO check
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

	; Set the VDP to the palette
	rst HalfSetVDPAddress
	ld a, $C0
	rst HalfSetVDPAddress
	; Load the palette as 18 colours (since HalfSetVDPAddress left b=18)
	otir
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
	; Point the VDP at the tilemap, ready to write
	; Low address byte is a = 0
	rst HalfSetVDPAddress
	ld a, >TileMapAddress | $40
	rst HalfSetVDPAddress

	; Emit 
	ld h, $C3 ; $c3e0 is initially uninitialized, we fill it in later
--:
	ld l, $E0 ; = -32 - why? TODO
	ld b, 32 ; column count
	xor a ; High byte of each tilemap entry is 0 -> we just pick tiles from 0 to 256
-:	; Emit 32 tilemap entries from (hl) for one row.
	; The code is duplicated in the loop, perhaps for speed?
	outi
	out (Port_VDPData), a
	outi
	out (Port_VDPData), a
	jr nz, -
	; Repeat until we have output 24*2 rows
	ld a, h
	cp $c3 + 24 ; $DB
	jr nz, --

	; Write three more rows of tilemap data from the sine table
	ld h, >RAM_SineTable2 ; $C2
	ld b, 32*2*3 ; $C0
-:  outi
	jr nz, -

	; Now b=0
	inc b ; so it is 1? c = Port_VDPData = $be
	; ix is unset, so what does this do? Some sort of arbitrary-start counter, counting up by $01.be
	add ix, bc
	; iy is unset, so what does this do? Some sort of arbitrary-start counter, counting up by $01.00
	inc iyh

	; Point de to the ix'th byte of the doubled sine table
	ld hl, RAM_SineTable2
	ld d, h
	ld e, ixh
	exx
		ld h, >RAM_SineTable2 ; hl' = iy'th byte of the doubled sine table
		ld a, iyh
		ld l, a
		ld d, $C3 ; de' = $c3e0 at first
--:
		push hl ; Save hl'
		ld e, $E0 ; de' = $xxe0
	exx
	; Read from de = sin(ix)
	ld a, (de)
	; Move de on by 2 entries
	inc e
	inc e
	; Add from hl = sin(t)
	add a, (hl)
	; Move on by 1 entry
	inc l
	exx
		ld b, a ; b' = sin(ix) + sin(iy)
-:
		ld a, (hl) ; a = sin(iy)
		add a, b
		inc l
		inc l
		ld (de), a
		inc e
		jr nz, -
		pop hl
		inc d
		ld a, d
		cp $DB
		jr nz, --
		ld l, e
		inc h
		ld d, $C0
		ld e, iyh
		ld a, (de)
		ld e, a
		ld b, $20
-:
		ld a, (de)
		add a, $20
		ld (hl), a
		ld a, e
		add a, $10
		ld e, a
		inc l
		djnz -
		ld (hl), $D0
	exx
	ld a, ixh
	ld l, a
	ld de, $C018
	exx
	ld l, $80
	ld b, $20
-:
	ld a, (de)
	exx
	ld h, d
	add a, (hl)
	add hl, de
	srl a
	exx
	sub d
	ld (hl), a
	inc l
	ld a, e
	add a, $08
	ld e, a
	ld (hl), $80
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
.db $00, $0B, $31, $31, $20, $23, $23, $20, $10, $17, $32, $30, $30, $32, $17, $10
.db $00, $3F
; Followed by the VDP register data
RegisterData:
.define TileMapAddress $3800 ; Normal location
.define SpriteTableAddress $3f00 ; Normal location
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
