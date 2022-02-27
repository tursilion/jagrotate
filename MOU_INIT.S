;
; Jaguar Example Source Code
; Jaguar Workshop Series #1
; Copyright (c)1994 Atari Corp.
; ALL RIGHTS RESERVED
;
; Program: mou.cof	- Minimum time object list update
;  Module: mou_init.s	- Program entry and initialization
;
; Revision History:
; 6/4/94   - SDS: Created
; 6/8/94   - SDS: Working 'black' screen (no bitmap yet)
; 6/13/94  - SDS: Bitmap working after zeroing the $@$%^@ VMODE variable
;                 Coalesced several .s files into this one
; 7/15/94  - SDS: Changed from RAM bitmap to ROM bitmap
;                 Changed move.w #$1F00,INT1 to move.w #$FFFF,VI
;                 Use UpdateFields instead of copying buffered list
; 9/28/94  - SDS: Slightly optimized some code - improved comments.
;----------------------------------------------------------------------------
; Program Description:
;
; This sample code demonstrates an efficient method for maintaining the
; a simple object list during the vertical blanking period.
;
; Steps are as follows:
; 1. Set GPU to Big-Endian mode
; 2. Set VI to $FFFF to disable video-refresh.
; 3. Initialize a stack pointer to high ram.
; 4. Initialize video registers.
; 5. Create an object list as follows:
;            BRANCH Object (Branches to stop object if past display area)
;            BRANCH Object (Branches to stop object if prior to display area)
;            BITMAP Object (Jaguar Logo)
;            STOP Object
; 6. Install interrupt handler, configure VI, enable video interrupts,
;    lower 68k IPL to allow interrupts.
; 7. Stuff OLP with pointer to object list.
; 8. Turn on video.
;----------------------------------------------------------------------------
		.include	"jaguar.inc"
		.include	"mou.inc"

; Globals
		.globl		a_vdb
		.globl		a_vde
		.globl		a_hdb
		.globl		a_hde
		.globl		width
		.globl		height
; Externals
		.extern		InitLister
		.extern		UpdateList
		.extern		FirstGPU
		.extern		StartGPU
		.extern		LastGPU
		.extern		RAMGPUAddress
		.extern		SLOPEADDR
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Program Entry Point Follows...

		.text

		move.l	#$00070007,G_END	; big-endian mode
		move.w	#$FFFF,VI			; disable video interrupts

		move.l	#INITSTACK,a7		; Setup a stack
			
; Copy over the GPU program
		move.l	#RAMGPUAddress,a0	; Get the address of the GPU code in main memory
		move.l	#FirstGPU,a1		; Get destination address
		move.l	#LastGPU,d0			; and calculate length of GPU code
		sub.l	#FirstGPU,d0
		asr.l	#2,d0				; divide by 4 since we're copying longs
.loop:
		move.l	(a0)+,(a1)+	; actually copy the code...
		dbra	d0,.loop
 
; Copy over the GPU lookup table
		move.l	#SlopeLookupTable,a0	; Get the address of the GPU data in main memory
		move.l	#SLOPEADDR,a1			; Get destination address
		move.l	#EndSlopeLookup,d0		; and calculate length of GPU data
		sub.l	#SlopeLookupTable,d0
		asr.l	#2,d0					; divide by 4 since we're copying longs
.loop2:
		move.l	(a0)+,(a1)+				; actually copy the data...
		dbra	d0,.loop2

; Init system
		jsr	InitVideo				; Setup our video registers.
		jsr	InitLister				; Initialize Object Display List
		jsr	InitVBint				; Initialize our VBLANK routine

; Finish the system init
; It may not be strictly accurate, but we can assume 224 lines
		move.l	d0,OLP				; Value of D0 from InitLister
;		move.w	#$6C1,VMODE			; Configure Video (Enable, 16b CRY, CSync, BGen, 4 clks/pix)
;		move.w	#$4C7,VMODE			; Configure Video (Enable, 16b RGB, CSync, BGen, 3 clks/pix) 533 pix

;		move.w	#$6C7,VMODE			; Configure Video (Enable, 16b RGB, CSync, BGen, 4 clks/pix) 400 pix
		move.w	#$8C7,VMODE			; Configure Video (Enable, 16b RGB, CSync, BGen, 5 clks/pix) 320 pix
;		move.w  #$ac7,VMODE			; Configure Video (Enable, 16b RGB, CSync, BGen, 6 clks/pix) 266 pix (minus overscan)
;		move.w	#$CC7,VMODE			; Configure Video (Enable, 16b RGB, CSync, BGen, 7 clks/pix) 228 pix (minus overscan)
;		move.w	#$647,VMODE			; Configure Video (Enable, 16b RGB, CSync,       4 clks/pix)

; According to Kevv, 5 clks per pixel is 320, so there are 1600 clocks?
; By that theory, 256 pixels is 6.25 (6 is 266.6 pixels - we can adjust the vdb and vde to make it up?)
	; Example: $8c1
	; 1000 1100 0001
	;              1 - Video Enable
	;            00  - Mode: 16 bit CRY
	;           0    - Genlock (disabled)
	;         0      - INCEN (Genlock switching)
	;        0       - Local border when INCEN enabled
	;       1        - CSync enabled
	;      1         - Clear line buffer to background register after display (16 bit only)
	;    0           - VARMOD (disabled)
	; 100            - Pixel width in clocks (this value + 1)

; start the GPU program
		move.l  #0,G_FLAGS			; Disable all GPU interrupts (GPU will enable them)
		move.l	#StartGPU,G_PC		; GPU Program counter to first address
		move.l	#$1,G_CTRL			; Set the GPU going

forevr:	stop #$2000			; go to sleep
		bra forevr			; and if someone wakes us up, go back to sleep

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedure: InitVBint 
; Install our vertical blank handler and enable interrupts
;

InitVBint:
		move.l	d0,-(sp)

		move.l	#UpdateList,LEVEL0	; Install 68K LEVEL0 handler

		move.w	a_vde,d0		; Must be ODD
		ori.w	#1,d0
		move.w	d0,VI

		move.w	INT1,d0			; Enable video interrupts
		ori.w	#1,d0
		move.w	d0,INT1

		move.w	sr,d0
		and.w	#$F8FF,d0		; Lower 68k IPL to allow
		move.w	d0,sr			; interrupts

		move.l	(sp)+,d0
		rts
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Procedure: InitVideo (same as in vidinit.s)
;            Build values for hdb, hde, vdb, and vde and store them.
;
 						
InitVideo:
		movem.l	d0-d6,-(sp)		
			
		move.w	CONFIG,d0		; Also is joystick register
		andi.w	#VIDTYPE,d0		; 0 = PAL, 1 = NTSC
		beq	palvals

		move.w	#NTSC_HMID,d2
		move.w	#NTSC_WIDTH,d0

		move.w	#NTSC_VMID,d6
		move.w	#NTSC_HEIGHT,d4

		bra	calc_vals
palvals:
		move.w	#PAL_HMID,d2
		move.w	#PAL_WIDTH,d0

		move.w	#PAL_VMID,d6
		move.w	#PAL_HEIGHT,d4

calc_vals:
		move.w	d0,width
		move.w	d4,height

		move.w	d0,d1
		asr	#1,d1				; Width/2

		sub.w	d1,d2			; Mid - Width/2
		add.w	#4,d2			; (Mid - Width/2)+4

		sub.w	#1,d1			; Width/2 - 1
		ori.w	#$400,d1		; (Width/2 - 1)|$400
		
		move.w	d1,a_hde
		move.w	d1,HDE

		move.w	d2,a_hdb
		move.w	d2,HDB1
		move.w	d2,HDB2
	
		move.w	d6,d5
		sub.w	d4,d5
		move.w	d5,a_vdb

		add.w	d4,d6
		move.w	d6,a_vde

		move.w	a_vdb,VDB
		move.w	#$FFFF,VDE
			
		move.l	#0,BORD1		; Black border
		move.w	#0,BG			; Init line buffer to black
			
		movem.l	(sp)+,d0-d6
		rts

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uninitialized Data!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;

		.bss

a_hdb:		.ds.w	1
a_hde:		.ds.w	1
a_vdb:		.ds.w	1
a_vde:		.ds.w	1
width:		.ds.w	1
height:		.ds.w	1

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialized Data!!!
;;;;;;;;;;;;;;;;;;;;;;;;;;
	.data

	; Note: this table must be 32-bit aligned in GPU RAM. Note that using alignment instructions like .phrase
	; will not work because the copy from the main code will not take it into account. 
SlopeLookupTable:
	; There are 32 entries here - 15 negative, 2 zero, and 15 positive (for easy masking)
	; Each entry is four dwords long - first is for STEP, the second is for FSTEP (both on the Y part only), third is for
	; the start Y address (to make it shift in the center of the screen). Fourth is slope.
	; The slope (for x) is a bit hacked at the end since we are faking that aspect with integers
	; (Lots of wasted space -- is it faster to read fewer dwords and process or read multiple dwords?)
	;		A1_STEP   A1_FSTEP  A1_PIXEL  slope (pix/change, high nibble is direction to shift)
	dc.l	$ffff0000,$00000000,$0020ffe0,$10000007		; max negative, +32 pixels
	dc.l	$ffff0000,$11110000,$001effe2,$10000008
	dc.l	$ffff0000,$22220000,$001cffe4,$1000000C
	dc.l	$ffff0000,$33330000,$001affe6,$10000010
	dc.l	$ffff0000,$44440000,$0018ffe8,$10000014
	dc.l	$ffff0000,$55550000,$0016ffea,$10000018
	dc.l	$ffff0000,$66660000,$0013ffed,$1000001C
	dc.l	$ffff0000,$77770000,$0011ffef,$10000020
	dc.l	$ffff0000,$88880000,$000ffff1,$10000024
	dc.l	$ffff0000,$99990000,$000dfff3,$10000028
	dc.l	$ffff0000,$aaaa0000,$000bfff5,$1000002C
	dc.l	$ffff0000,$bbbb0000,$0008fff8,$10000030
	dc.l	$ffff0000,$cccc0000,$0006fffa,$10000032
	dc.l	$ffff0000,$dddd0000,$0004fffc,$10000034
	dc.l	$ffff0000,$eeee0000,$0002fffe,$1000003C
	dc.l	$00000000,$00000000,$00000000,$0000ffff		; zero
	dc.l	$00000000,$00000000,$00000000,$0000ffff		; zero
	dc.l	$00000000,$11110000,$fffe0002,$f000003C
	dc.l	$00000000,$22220000,$fffc0004,$f0000034
	dc.l	$00000000,$33330000,$fffa0006,$f0000032
	dc.l	$00000000,$44440000,$fff80008,$f0000030
	dc.l	$00000000,$55550000,$fff5000b,$f000002C
	dc.l	$00000000,$66660000,$fff3000d,$f0000028
	dc.l	$00000000,$77770000,$fff1000f,$f0000024
	dc.l	$00000000,$88880000,$ffef0011,$f0000020
	dc.l	$00000000,$99990000,$ffed0013,$f000001C
	dc.l	$00000000,$aaaa0000,$ffea0016,$f0000018
	dc.l	$00000000,$bbbb0000,$ffe80018,$f0000014
	dc.l	$00000000,$cccc0000,$ffe6001a,$f0000010
	dc.l	$00000000,$dddd0000,$ffe4001c,$f000000C
	dc.l	$00000000,$eeee0000,$ffe2001e,$f0000008
	dc.l	$00000000,$ffff0000,$ffe00020,$f0000007		; max positive, -32 pixels
EndSlopeLookup:

		.end

