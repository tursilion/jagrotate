;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MOU.INC - Constant file for Minimum Object List Update Examples

; Picture is 320x240, we're only showing the center 280 pixels across

PPP			.equ	4				; Pixels per Phrase (16-bit RGB)
BMP_WIDTH	.equ	256				; Width in Pixels 
BMP_DWIDTH	.equ	(320/PPP)		; Width in Phrases for data
BMP_IWIDTH  .equ    (256/PPP)		; Width in Phrases to draw
BMP_HEIGHT	.equ	240				; Height in Pixels
BMP_LINES	.equ	(BMP_HEIGHT*2)	; Height in Half Scanlines

BITMAP_OFF1	.equ	(2*8)			; Two Phrases
BITMAP_OFF3 .equ	(4*8)
BITMAP_OFF2 .equ	(8*8)

;LISTSIZE	.equ	18				; List length (in phrases (7 sprites)
LISTSIZE	.equ	11				; 3 sprites (2bg + rotated overlay)
;LISTSIZE .equ 8					; 2 sprite

