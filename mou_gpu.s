;
; (C) 2006 Mike Brent aka Tursi aka HarmlessLion.com
; This software is provided AS-IS. No warranty
; express or implied is provided.
;
; This notice defines the entire license for this software.
; All rights not explicity granted here are reserved by the
; author.
;
; You may redistribute this software provided the original
; archive is UNCHANGED and a link back to my web page,
; http://harmlesslion.com, is provided as the author's site.
; It is acceptable to link directly to a subpage at harmlesslion.com
; provided that page offers a URL for that purpose
;
; Source code, if available, is provided for educational purposes
; only. You are welcome to read it, learn from it, mock
; it, and hack it up - for your own use only.
;
; Please contact me before distributing derived works or
; ports so that we may work out terms. I don't mind people
; using my code but it's been outright stolen before. In all
; cases the code must maintain credit to the original author(s).
;
; Unless you have explicit written permission from me in advance,
; this code may never be used in any situation that changes these
; license terms. For instance, you may never include GPL code in
; this project because that will change all the code to be GPL.
; You may not remove these terms or any part of this comment
; block or text file from any derived work.
;
; -COMMERCIAL USE- Contact me first. I didn't make
; any money off it - why should you? ;) If you just learned
; something from this, then go ahead. If you just pinched
; a routine or two, let me know, I'll probably just ask
; for credit. If you want to derive a commercial tool
; or use large portions, we need to talk. ;)
;
; Commercial use means ANY distribution for payment, whether or
; not for profit.
;
; If this, itself, is a derived work from someone else's code,
; then their original copyrights and licenses are left intact
; and in full force.
;
; http://harmlesslion.com - visit the web page for contact info
;

		.include    "jaguar.inc"
		.include    "mou.inc"

		.extern     a_vde
		.extern     a_vdb
		.extern     a_hdb
		.extern     a_hde
		.extern		width
		.extern		height
		.extern		horizon
		.extern		jagbits
		.extern		buffer
		.extern		TILTREG
		.extern		main_obj_list
		.extern		bmp_addr2

		.globl		SLOPEADDR

ISTACK		.equ	$F03EFC			; Location of GPU interrupt stack (why is this below top of RAM?)
SLOPEADDR	.equ    (ISTACK-32-512)	; location to load slope lookup table, leave 32 bytes for int stack (should only need 4)

		.gpu

; Bank 0 (interrupt mode and GP)
; R0  - always 0
; R1  - temp
; R2  - temp
; R3  - temp
; R4  - temp
; R5  - Address of TILTREG
; R6  - Address of A1_STEP
; R7  - Mask for Y part of address ($ffff0000)
; R8  - Address of A1_FSTEP
; R9  - Blitter count data
; R10 - current slope from table (in lines per pixel change)
; R11 - A2_PIXEL reg
; R12 - A1_FPIXEL reg
; R13 - A1_PIXEL reg
; R14 - destination DRAM buffer address
; R15 - source line buffer address (LBUFC)
; R17 - mask for X part of address ($0000ffff)
; R18 - Blitter command value
; R19 - Blitter Counters
; R20 - Blitter Command/Status reg (B_CMD)
; R21 - A1 (Dest) Line y/x start
; R22 - OP Flags reg
; R23 - GPU Flags reg
; R24 - address of SlopeLookupTable
; R25 - Line increment ($00010000)
; R26 - A1_BASE reg for blitter
; R27 - A2_BASE reg for blitter
; R28 - line count for X shift
; R29 - X pos for tilted display
; R30 - Interrupt return calc (gets corrupted. not safe to use for anything outside int)
; R31 - Interrupt stack

; Bank 1 (general purpose and storage)
; R0  - temp
; R1  - temp
; R2  - temp
; R3  - temp
; R4  - $00000FFF - 12 bit OP mask for int routine
; R5  - Address in main ram of the Word in the OP list to alter (for int routine)
; R6  - $0000C000 - high bits in low word for OP list for in routine
; R17 - Horizon line
; R18 - Mask value for VC
; R20 - VC register address
; R21 - BG Reg
; R22 - Horizon register
; R23 - Flag to indicate processing of scanline color (0=go)
; R24 - Address of vblank routine

RAMGPUAddress::
		.org     G_RAM
FirstGPU::

; Interrupt vectors are every 16 bytes (8 words!)
; using .orgs to space the vectors results in failure when the code loads, so just
; pad each routine to exactly 8 words. This is because GPU .orgs don't pad the 68k image ;)
; These vectors should be 16 bytes ($10) apart - use a map to verify :)

Vector0:
; interrupt for CPU int 0. This is fired on a vblank, we need
; to update the line registers for the blitter and rebuild the OP List
	movefa r24,r1		; get address from other bank (BUG: scoreboarding doesn't work here!)
	load (r5),r2		; get tilt register (range must be 0-31 (premultiplied by 16 - 0 to $1f0)) (scoreboard delay)
	jump (r1)			; jump to the int routine - it's too large to leave here now (first inst in delay slot)
	load (r23),r4		; INT: get flags (jump delay slot)
	nop
	nop
	nop
	nop

Vector1:	; Jerry
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

Vector2:	; Timer
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop

; pad to vector 3 - vector 1 is Jerry and Vector 2 is timer
; Vector 3 should be preceded by 24 words
Vector3:
		; Vector for Object Proc interrupt 3
; Warning: By putting the OP interrupt here in full, we overwrite the
; blitter interrupt vector, so we can not use Blitter Interrupts without
; switching this back to a jump. But jumps are so bloody slow...
; Object proc interrupt 3
op_int:
; Wake up the hardware
	store r18,(r20)			; activate the blitter (registers were previously set up!!)
	; Note: Because the blitter is running in high priority bus mode, the OP will not
	; be able to resume until it is complete! This mode is only safe when the OP
	; is not currently processing. 

; Trigger the main code to run when we return
	moveta r0,r23

; Increment pixel address
	subq #1,r28		; dec line counter to 0
	jr NE,.nochangex
	add r25,r21		; adds 1 to Y always

	; get some values from the other reg bank (faster than movei!)
	movefa r4,r2	; get 12-bit mask $FFF
	movefa r5,r3	; get dest address in OP list
	movefa r6,r1	; get high bits of OP word

	move r10,r28	; reset line counter
	and r17,r28		; just the low word on the line counter

	move r10,r4		; amount to add is in high nibble
	sharq #28,r4	; shift and sign extend
	add r4,r29		; add step value to x 

	move r29,r4		; get new X in register
	and r2,r4		; reduce to 12 bits
	or r1,r4		; replace the top nibble (pitch 1, 16-bit data)

	storew r4,(r3)	; write it out (note we didn't need to read it in!)
.nochangex:

	; We've done as much as we can without the bus, so we can restart the OP now
	; We want to restart it as soon as possible, but at the same time when we
	; try to access the bus we will be blocked if the blitter is not done yet.
	; The pixel code above will have potentially touched the bus, so we go ahead
	; Doing it later than here is worse, but doing it earlier cost us a few pixels
	storew r0,(r22)		; Clear the OP flags, this restarts OP 

; Update the blitter for the next line - doing it here is more efficient
	store r15,(r27)		; src in A2
	store r14,(r26)		; dest in A1	; both must be phrase aligned
	store r9,(r19)		; pixel count to count reg

	; safety - we have to reset the pixel registers every call! 
	store r0,(r11)		; A2 (src) pixel (window) start
	store r21,(r13)		; A1 (dest) pixel (window) start
	store r6,(r12)		; A1 (dest) fractional pixel (set greater than 0 so max tilt wraps first time)
						; note: R6 has data in the low word, too, affects X frac! But saves us a reg to do this.

; interrupt return 
	load (r23),r4		; get flags
	load (r31),r30		; get last instruction address
	bclr #3,r4			; clear IMASK
	addq #2,r30			; point at next to be executed
	bset #12,r4			; clear OP int
	addq #4,r31			; updating the stack pointer
	jump (r30)			; and return
	store r4,(r23)		; restore flags (delay slot, enables int)
	
; Vector 4 should have been the blitter

	; CPU must have cleared the G_FLAGS register - at a minimum
	; we must be in reg bank 0 and it'd be nice if interrupts
	; were disabled ;)
StartGPU::
	; G_FLAGS starts out cleared by the CPU
	movei #G_FLAGS, r2	
	load (r2),r1

; Init some registers in bank 0 for interrupt usage
	moveq	#0,r0			; just have 0 handy

	movei	#TILTREG,r5		; address of TiltReg
	movei	#A1_STEP,r6		; Step register - though not active, it's used when FSTEP overflows
	movei	#$ffff0000,r7	; mask for Y part of an address
	movei	#A1_FSTEP,r8	; Fractional step register
	; Copy in inner loop X max is 416 ($1A0) (1 large sprite, copy line to DRAM, large sprite back from line)
	; Copy in outer loop X max is 380 ($005f0004) (1 large sprite, copy line to DRAM, large sprite back from line)
	; Adding fractional increment on the outer loop takes it down to 352 ($00580004)
	; Rotating code base managed 356 pixels ($00590004)
	; Moving the OP restart write to later topped out at 360 pixels ($005A0004)
	; With a lessor slope of 8:1 we manage 384 pixels ($00300008) (not enough more to be worth it)
	; A1 clipping seems to be essentially free (thankfully)
	; Note this is different than normal - one inner loop pass (4 pixels) and 'x' outer loop passes
	movei	#$00400004,r9	; Blitter count (high word y, low word x - $40*4=256 pixels)

	movei	#A2_PIXEL,r11	; A2 pixel address
	movei	#A1_FPIXEL,r12	; A1 fractional pixel address
	movei	#A1_PIXEL,r13	; A1 pixel address
	movei	#buffer,r14		; output address for blit from line buffer
	movei	#LBUFC,r15		; read address of line buffer (16 bit only, ugh)
	movei	#$0000ffff,r17	; mask for X part of an address
	movei	#(SRCEN|LFU_S|UPDA1F|BUSHI|CLIP_A1),r18	; Blitter command value - src read enable, Uupdate A1 Frac, write src data, bus high (only safe cause OP is not running), A1 window clipping
	movei	#B_COUNT,r19	; Blitter counters
	movei	#B_CMD,r20		; Blitter command/Status
	moveq	#0,r21			; Line y/x for start of line

	movei	#OBF,r22		; Object processor flags
	movei	#G_FLAGS,r23	; GPU flags
	movei	#SLOPEADDR,r24	; Slope lookup table
	movei	#$00010000,r25	; increment for y/x line pointer
	movei	#A1_BASE,r26	; A1 base address register for blitter
	movei	#A2_BASE,r27	; A2 base address register for blitter
	movei   #ISTACK,r31     ; Initialize Interrupt Stack

	; set up the blitter as well (it'll run from bank 0 anyway)
	
	; A1 (Dest)
	movei #A1_FLAGS,r3
	movei #(PITCH1|PIXEL16|ZOFFS0|WID256|XADDPHR|YADD0|XSIGNADD|YSIGNADD),r4
	store r4,(r3)			; flags
	movei #A1_CLIP,r3
	movei #$00f00100,r4		; 256x240
	store r4,(r3)			; set clip size
	store r0,(r6)			; step (not needed for x because this is automatic in phrase mode)
	store r0,(r8)			; fractional step
	movei #A1_INC,r3
	store r0,(r3)			; increment (in increment mode, we aren't using)
	movei #A1_FINC,r3
	store r0,(r3)			; fractional increment
	
	; A2 (Src)
	movei #A2_FLAGS,r3
	movei #(PITCH1|PIXEL16|ZOFFS0|WID320|XADDPHR|YADD0|XSIGNADD|YSIGNADD),r4
	store r4,(r3)			; flags
	movei #A2_MASK,r3
	movei #-1,r4
	store r4,(r3)			; no mask
	movei #A2_STEP,r3
	store r0,(r3)			; step
	
	; Blitter general
	movei #B_STOP,r3
	store r0,(r3)			; collision control
	
	; Blitter dynamic (prepare for first run)
	store r15,(r27)			; src in A2
	store r14,(r26)			; dest in A1	; both must be phrase aligned
	store r9,(r19)			; pixel count to count reg
	; safety - we have to reset the pixel registers every call! (in the future we may be able to use the changed values)
	store r0,(r11)			; A2 (src) pixel (window) start
	store r21,(r13)			; A1 (dest) pixel (window) start
	store r6,(r12)			; A1 (dest) fractional pixel (set >0 for max tilt to not require two steps)
							; note: R6 has stuff in the low word, affects X frac!
	moveq #0,r29			; clear current X pos

; Init our 'tilt register'
	movei #15*16,r3			; centered
	store r3,(r5)
	moveq #0,r28			; line 0
	
; Prepare to switch to bank 1
	bset #14,r1

; Copy R1 and R2 to bank 1, since we'll need them there for a few more lines
	moveta r1,r1
	moveta r2,r2

; Now go ahead and switch to bank 1 and init our registers there
	store r1,(r2)
	nop

; Init bank 1 registers
	movei	#$fff,r4		; 12-bit mask for OP list
	movei	#bmp_addr2,r0	; address at which our word is stored
	load	(r0),r5			; address of the object we want to edit in the OP list
	addq	#14,r5			; address of the word inside that object
	movei	#$C000,r6		; High bits of word for OP manip in int routine
	movei	#$7ff,r18		; mask for VC
	movei	#VC,r20			; Address of VC
	movei	#BG,r21			; get the address of the background register
	movei	#horizon,r22	; Horizon register
	moveq	#0,r23			; Flag to process next scanline (clear means to process)
	movei	#vblankint,r24	; address of vblank interrupt function

; Now go ahead and turn on ObjProc and CPU interrupts 
	bset	#7,r1			; enable object processor interrupts
	or		r1,r1			; delay/scoreboard force
	bset	#4,r1			; enable CPU interrupts
	store r1,(r2)
	nop

; And just in case the OP died on us, this wakes it up
; It turns out the OP will hang on GPU interrupts even if the GPU isn't reading them
	moveq   #0,r0
	movei   #OBF,r1         ; Write any value to OBF
	storew  r0,(r1)         ; to restart Object Processor

; Main loop - while waiting for interrupt, we calculate the next sky/ground gradient color
; we watch the line buffer, and update the clear color
; to get a nice sky and earth gradient ;)

; We may be 1 scanline late with this approach - that's okay, even on
; wraparound, because the first and last few scanlines are not drawn anyway
.lwait1:
	nop
	cmpq #0,r23			; test the 'flag' register, this keeps us off the bus till each line is done
	jr NE,.lwait1
	nop

	addq #1,r23			; mark the job done

	loadw (r20),r0		; VC in half scanlines
	and r18,r0			; mask it off to just a line counter
	shrq #1,r0			; divide by 2

	; preload some temp values for below
	movei #255,r2		; make negative positive
	movei #1984,r3		; saturated blue in RGB16
	
	loadw (r22),r17		; get the horizon line in lines
	shlq	#16,r17
	sharq	#16,r17		; sign extend so negative horizons work

	; It's time! Based on the current line (and the calculated horizon) set a line color
	; Color mode is rrrrrbbbbbgggggg. We already have the line in r0.
	sub r17,r0			; get the offset
	jr MI,.top
	nop

	; Here we are on the bottom. r0 contains a positive value
	; and we need to use it as a green value 
	shlq #1,r0			; multiply by 2
	addq #32,r0			; add a bit extra
	addq #32,r0			; add a bit extra
	sat8 r0				; saturate to 8 bits
	shrq #2,r0			; reduce to 6 bits 

	jr T,.setline		; jump to setting the register
	nop

.top:
	; Here we are on the top, r0 contains a negative value
	; which we need to add to 255 then use as red and green,
	; along with a full blue
	add r2,r0			; add 255 into r0 
	sat8 r0				; saturate to 8 bits 
	shrq #2,r0			; reduce to 6 bits
	move r0,r1			; make a copy 
	shrq #1,r1			; reduce to 5 bits 
	shlq #11,r1			; shift to red area 
	or r1,r0			; or the result in 
	or r3,r0			; or the blue in 

.setline:
	; update the BG register with the value calculated by the main routine
	; TODO: top few scanlines are coming up green - because the main code isn't running
	; for those lines anymore?? Probably the OP list is not starting soon enough, or
	; we should let it jump to the GPU interrupt for the top of the frame and just not
	; the bottom to ensure the line color is set when the top happens.
	storew r0,(r21)		; write the value into the BG register (should this be here or at the top of the loop?)

	movei #.lwait1,r0	; get loop address
	jump T,(r0)			; and loop
	nop

; The CPU interrupt, which represents vblank, comes down here.
; We have to prepare the blitter for a new frame of scanlines,
; and rebuild the object list from the sprite list.
vblankint:
; We interleave the interrupt routine to decrease stalls
; The first two instructions are done before the jump
;	load (r5),r2		; get tilt register (range must be 0-31 (premultiplied by 16 - 0 to $1f0))
;	load (r23),r4		; INT: get flags
	load (r31),r30		; INT: get last instruction address
	add r24,r2			; add offset into table
	bclr #3,r4			; INT: clear IMASK
	load (r2),r3		; read step value
	addq #4,r2			; increment R2
	store r3,(r6)		; store read value in A1_STEP

	load (r2),r3		; read fractional value
	addq #4,r2			; increment R2
	store r3,(r8)		; store read value in A1_FSTEP
	load (r2),r21		; read the new pixel start address
	addq #4,r2			; increment R2
	move r21,r29		; get the starting x pos
	and r7,r21			; restrict start pixel address to Y only
	and r17,r29			; restrict starting x to X only

	addq #2,r30			; INT: point at next to be executed
	load (r2),r10		; read slope from table
	bset #9,r4			; INT: clear CPU int #$200
	move r10,r28		; store in line counter
	addq #4,r31			; INT: updating the stack pointer
	and r17,r28			; take just the low word

	; Now build the object list based on a simple sprite list
	; The sprite list is organized for simplicity:
	;




	; Handle interrupt return
	jump (r30)			; and return
	store r4,(r23)		; restore flags (delay slot, enables int)


LastGPU::
	nop
		.end

