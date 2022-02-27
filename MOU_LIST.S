;
; Jaguar Example Source Code
; Jaguar Workshop Series #1
; Copyright (c)1994 Atari Corp.
; ALL RIGHTS RESERVED
;
; Program: mou.cof      - Minimum time object list update
;  Module: mou_list.s   - Object List Refresh and Initialization
; 
; Revision History:
; 6/4/94   - SDS: Created
; 6/13/94  - SDS: Added scr_height and scr_width variables for centering.
;                 Deleted copy_list routine and changed it to copy_phrases in
;                 mou_init.s.

		.include    "jaguar.inc"
		.include    "mou.inc"

		.globl      InitLister
		.globl		UpdateList
		.globl		horizon
		.globl		buffer
		.globl		TILTREG
		.globl		main_obj_list
		.globl		bmp_addr2

		.extern     a_vde
		.extern     a_vdb
		.extern     a_hdb
		.extern     a_hde
		.extern		width
		.extern		height
		.extern		jagbits
		.extern		StartGPU
		.extern		TILTREG
		

		.text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; InitLister: Initialize Object List Processor List
;
;    Returns: Pre-word-swapped address of current object list in d0.l
;
;  Registers: d1.l/d0.l - Phrase being built
;             d2.l      - Address of STOP object in destination buffer
;             d3.l      - Calculation register
;             d4.l      - Width of image in phrases
;             d5.l      - Height of image in scanlines
;             a0.l      - Roving object list pointer
		
InitLister:
		movem.l d1-d5/a0,-(sp)		; Save registers

		; set horizon and buffer address
		move.w #100,horizon

		; build list
		lea		stop_obj,a0
		move.l	a0,d2				; get address of fixed STOP object
		lea     main_obj_list,a0	; address of initial branches

; Write first BRANCH object (branch if YPOS > a_vde )

		clr.l   d1
		move.l  #(BRANCHOBJ|O_BRLT),d0	; $4000 = VC < YPOS
		jsr     format_link             ; Stuff in our LINK address
						
		move.w  a_vde,d3                ; for YPOS
		lsl.w   #3,d3                   ; Make it bits 13-3
		or.w    d3,d0

		move.l  d1,(a0)+                                
		move.l  d0,(a0)+                ; First OBJ is done.

; Write second branch object (branch if YPOS < a_vdb)   
; Note: LINK address is the same so preserve it
		andi.l  #$FF000007,d0           ; Mask off CC and YPOS
		ori.l   #O_BRGT,d0				; $8000 = VC > YPOS
		move.w  a_vdb,d3                ; for YPOS
		lsl.w   #3,d3                   ; Make it bits 13-3
		or.w    d3,d0

		move.l  d1,(a0)+                ; Second OBJ is done
		move.l  d0,(a0)+        

; For safety's sake, till the GPU rebuilds this list, write a BRANCH object here 


.if 1
; Write a standard BITMAP (must be dphrase aligned!) object ; Note: pure black (000) is transparent!
		clr.l   d1
		clr.l   d0                      ; Type = BITOBJ

		move.l  a0,d2					; jump address is next object	; why does the BMP obj have a link address?
		add.l   #16,d2          
		jsr     format_link

		move.l  #BMP_HEIGHT,d5			; Height of image
		lsl.l   #8,d5					; HEIGHT
		lsl.l   #6,d5
		or.l    d5,d0

		move.w  height,d3         		; Center bitmap vertically
		sub.w   #BMP_HEIGHT,d3
		add.w   a_vdb,d3
		; temp code - push her down a bit more
		add.w	#25,d3
		andi.w  #$FFFE,d3               ; Must be even

		lsl.w   #3,d3
		or.w    d3,d0                   ; Stuff YPOS in low phrase

		move.l  #jagbits+40,d3
		andi.l  #$FFFFF0,d3
		lsl.l   #8,d3                   ; Shift bitmap_addr into position
		or.l    d3,d1
     
		move.l	a0,bmp_addr1
		move.l  d1,(a0)+
		move.l	d1,bmp_highl
		move.l  d0,(a0)+
		move.l	d0,bmp_lowl

		move.l  #O_TRANS,d1             ; Now for PHRASE 2 of BITOBJ
		move.l  #O_DEPTH16|O_NOGAP,d0   ; Bit Depth = 16-bit, Contiguous data

		move.w	width,d3		; Get width in clocks
		lsr.w	#2,d3			; /4 Pixel Divisor
		sub.w   #BMP_WIDTH,d3
		lsr.w   #1,d3
		; temp code - push her over a bit
		add.w	#-60,d3
		andi	#$fff,d3		; only 12 bits
		or.w    d3,d0

		move.l  #BMP_DWIDTH,d4 
		lsl.l   #8,d4                   ; DWIDTH
		lsl.l   #8,d4
		lsl.l   #2,d4
		or.l    d4,d0

		move.l	#BMP_IWIDTH,d4
		move.l  d4,d3                   ; Copy for below
		lsl.l   #8,d4                   
		lsl.l   #8,d4
		lsl.l   #8,d4                   ; IWIDTH Bits 28-31
		lsl.l   #4,d4
		or.l    d4,d0

		lsr.l   #4,d3                   ; IWIDTH Bits 37-32
		or.l    d3,d1

		move.l  d1,(a0)+                ; Write second PHRASE of BITOBJ
		move.l  d0,(a0)+
.endif

; branch obj
		clr.l   d1
		move.l  #(BRANCHOBJ|O_BRLT),d0	; $4000 = VC < YPOS

		move.l	#gpuint_obj,d2
		jsr     format_link
						
		move.w  #$7ff,d3				; for YPOS ($7FF = always jump)
		lsl.w   #3,d3                   ; Make it bits 13-3
		or.w    d3,d0

		move.l  d1,(a0)+                                
		move.l  d0,(a0)+                ; write object


	; Now jump after to the GPU/frame buffer fixed objects. Unfortunately the frame buffer
	; object will still be broken every time.
		lea		gpuint_obj,a0

; Done drawing this line, write an interrupt object for the GPU
.if 1
		clr.l	d1
		move.l	#GPUOBJ,d0		; GPU interrupt active every scanline (Y value is ignored)
		move.l	d1,(a0)+	
		move.l	d0,(a0)+
 .else
		; branch (just to dphrase align the following bitmap object - if we skip the GPU Interrupt,
		; that's why it's before the GPU obj instead of after it)
		clr.l   d1
		move.l  #(BRANCHOBJ|O_BRLT),d0	; $4000 = VC < YPOS

		move.l  a0,d2					; jump address is next object
		add.l   #8,d2          
		jsr     format_link
						
		move.w  a_vde,d3                ; for YPOS
		lsl.w   #3,d3                   ; Make it bits 13-3
		or.w    d3,d0

		move.l  d1,(a0)+                                
		move.l  d0,(a0)+                ; write object
.endif

; Write the final frame buffer bitmap (must be dphrase aligned!) object ; Note: no transparent here!
; frame buffer is 256x240 today, that may change (bitmap2 from sample code)
		clr.l   d1
		clr.l   d0                      ; Type = BITOBJ
			
		move.l  a0,d2					; jump address is next object
		add.l   #16,d2          
		jsr     format_link

		move.l  #240,d5					; Height of image
		lsl.l   #8,d5					; HEIGHT
		lsl.l   #6,d5
		or.l    d5,d0

		move.w  a_vdb,d3				; get top of display
		; push it down a bit more to reduce racing
		add.w	#40,d3
		andi.w  #$FFFE,d3               ; Must be even

		lsl.w   #3,d3
		or.w    d3,d0                   ; Stuff YPOS in low phrase

		move.l  #buffer,d3
		andi.l  #$FFFFF0,d3
		lsl.l   #8,d3                   ; Shift bitmap_addr into position
		or.l    d3,d1
     
		move.l	a0,bmp_addr2
		move.l  d1,(a0)+
		move.l	d1,bmp_highl2
		move.l  d0,(a0)+
		move.l	d0,bmp_lowl2

		clr.l   d1						; Now for PHRASE 2 of BITOBJ
		move.l  #O_DEPTH16|O_NOGAP,d0   ; Bit Depth = 16-bit, Contiguous data

		; leave bits 0-11 of d0 blank for X=0
		move.l  #64,d4					; width of data in phrases
		lsl.l   #8,d4                   ; DWIDTH
		lsl.l   #8,d4
		lsl.l   #2,d4
		or.l    d4,d0

		move.l	#64,d4					; width in phrase to actually draw
		move.l  d4,d3                   ; Copy for below
		lsl.l   #8,d4                   
		lsl.l   #8,d4
		lsl.l   #8,d4                   ; IWIDTH Bits 28-31
		lsl.l   #4,d4
		or.l    d4,d0

		lsr.l   #4,d3                   ; IWIDTH Bits 37-32
		or.l    d3,d1

		move.l  d1,(a0)+                ; Write second PHRASE of BITOBJ
		move.l  d0,(a0)+

; Write a STOP object at end of list
		clr.l   d1
		move.l  #(STOPOBJ),d0

		move.l  d1,(a0)+                
		move.l  d0,(a0)+

; Now return swapped list pointer in D0                      

		move.l	#main_obj_list,d0  
		swap    d0

		movem.l (sp)+,d1-d5/a0
		rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Procedure: format_link
;
;    Inputs: d1.l/d0.l is a 64-bit phrase
;            d2.l contains the LINK address to put into bits 42-24 of phrase
;
;   Returns: Updated phrase in d1.l/d0.l

format_link:
		movem.l d2-d3,-(sp)

		andi.l  #$3FFFF8,d2             ; Ensure alignment/valid address
		move.l  d2,d3                   ; Make a copy

		swap	d2                   	; Put bits 10-3 in bits 31-24
		clr.w	d2
		lsl.l   #5,d2
		or.l    d2,d0

		lsr.l   #8,d3                   ; Put bits 21-11 in bits 42-32
		lsr.l   #3,d3
		or.l    d3,d1

		movem.l (sp)+,d2-d3             ; Restore regs
		rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UpdateList: Update list fields destroyed by the object processor.
;			  and do other vblank stuff
;
;  Registers:	a0.l      - General Purpose Pointer
;				d0,d1	  - used in joystick read

UpdateList:
		movem.l	d0-d1/a0,-(sp)

		; Make any necessary changes to the sprite list here
		; none yet - testing new code

		; Interrupt the GPU so it can do VSYNC stuff 
		; The GPU will also rebuild the OP list at this point,
		; as it's supposedly going to do it faster than the 68k ;)
		move.l	G_CTRL,d0
		or.l	#4,d0
		move.l	d0,G_CTRL
 

 .if 0
		; Update OP list - must be done before the first Bitmap is processed
		move.l	#main_obj_list+BITMAP_OFF1,a0
		move.l	bmp_highl,(a0)		; Phrase = d1.l/d0.l
		move.l	bmp_lowl,4(a0)

		move.l	#main_obj_list+BITMAP_OFF2,a0
		move.l	bmp_highl2,(a0)		; Phrase = d1.l/d0.l
		move.l	bmp_lowl2,4(a0)

;		move.l	#main_obj_list+BITMAP_OFF3,a0
;		move.l	bmp_highl3,(a0)		; Phrase = d1.l/d0.l
;		move.l	bmp_lowl3,4(a0)
.else
		move.l	bmp_addr1,a0
		move.l	bmp_highl,(a0)		; Phrase = d1.l/d0.l
		move.l	bmp_lowl,4(a0)

		move.l	bmp_addr2,a0
		move.l	bmp_highl2,(a0)		; Phrase = d1.l/d0.l
		move.l	bmp_lowl2,4(a0)
.endif

		; while we're here, let's just read the joystick real quick and update registers accordingly
		; bits end up a bit screwy. It looks like these. The first set is joystick 1 if enabled,
		; notice how it's selects are the opposite to joystick 0. Value to write is $81YZ, where Y
		; is the row for JOY1 and Z is the row for JOY0. The 8 enables read, and the 1 enables
		; audio (why that's in the joystick register... oi.). I can't test the Numpad or option
		; because my Genesis pad doesn't have those. ;)
		;           JOY1JOY0                       J1J0
		; row0 (e): 369#RLDU xxxxxxxx xxxxxxxx xxxxOxAP
		; row1 (d): 2580*741 xxxxxxxx xxxxxxxx xxxxCxBx		; note: numpad may be backwards here
		; row2 (b): *7412580 xxxxxxxx xxxxxxxx xxxxBxCx
		; row3 (7): RLDU369# xxxxxxxx xxxxxxxx xxxxAPOx		; also, option may be bitshifted by one :)

		move.w #$81fe,JOYSTICK	; enable read, audio, nothing on joystick 1, col 0 on joystick 0
		move.l JOYSTICK,d0		; by doing a long read, we get JOYSTICK and JOYBUTS, but the 68k

		btst #24,d0
		bne .joy1
		subq.w #1,horizon			; up
.joy1:
		btst #25,d0
		bne .joy2
		addq.w #1,horizon			; down
.joy2:	
		btst #26,d0
		bne .joy3
		cmpi.l #0,TILTREG
		beq .joy3
		sub.l #16,TILTREG			; left - note: tiltreg must move in multiples of 16
.joy3:	
		btst #27,d0
		bne .joy4
		cmpi.l #$1f0,TILTREG
		beq .joy4
		add.l #16,TILTREG			; right - note: tiltreg must move in multiples of 16
.joy4:

		; Signal we're done
		move.w	#$101,INT1
		move.w	#$0,INT2

		movem.l	(sp)+,d0-d1/a0
		rte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Storage space for our object lists

		.bss
		.dphrase			
bmp_addr1:
		.ds.l		1
bmp_highl:
		.ds.l		1
bmp_lowl:
		.ds.l		1
bmp_addr2:
		.ds.l		1
bmp_highl2:
		.ds.l		1
bmp_lowl2:
		.ds.l		1
bmp_highl3:
		.ds.l		1
bmp_lowl3:
		.ds.l		1
horizon:
		.ds.l		1		; 16-bit value for horizon
TILTREG:
		.ds.l		1		; 32-bit value for horizon

		.dphrase
buffer:
		.ds.l		256*400

; The list must start 4-phrase aligned
		.qphrase
main_obj_list:
		.ds.l		2*2		; room for the initial branch objects
first_free_obj:
		.ds.l		2*4*32	; room for 32 4-phrase aligned objects
padding_obj:
		.ds.l		2		; padding because the GPU object will throw off alignment
gpuint_obj:
		.ds.l		2*3		; room for the gpu object and frame buffer bitmap
stop_obj:
		.ds.l		2		; room for the stop object

		.end
