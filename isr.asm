;
; USB-HID keyboard driver
; Copyright (c) 2019 Mario Smit (S0urceror)
; 
; This program is free software: you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation, version 3.
;
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of 
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License 
; along with this program. If not, see <http://www.gnu.org/licenses/>.
;

    DEFINE DEBUG 0

H.TIMI EQU 0FD9Fh
SCNCNT EQU 0F3F6H
INTHZ EQU 50
NEWKEY EQU 0FBE5H
MYADDRESS EQU 1
PUTPNT EQU 0F3F8h
GETPNT EQU 0F3FAh
KEYBUF EQU 0FBF0h

; BLOAD header
    db 0x0fe
    dw BEGIN, ENDADR, START_BASIC
    org 0a004h
BEGIN:
START_BASIC:
    ld hl,TXT_DRIVER_START
    call PRINT

    ; install H.TIMI hook
    ld hl, H.TIMI
    ld (hl),0C3h ; JP
    inc hl
    ld (hl),low NEW_HTIMI
    inc hl
    ld (hl),high NEW_HTIMI
    ret

NEW_HTIMI:
    push hl,bc,af
    ; check if it's time to do a keyboard scan, just like the BIOS does it
    ; takes precedence over the internal keyboard scan because SCNCNT never reaches zero in BIOS
    LD	HL,SCNCNT
	DEC	(HL)			        ; time for a keyboard scan ?
	JR	NZ,_NEW_HTIMI_DONE		; nope, quit interrupt routine
	IF INTHZ = 60
		LD	(HL),250			    ; scanfrequency 2*1000/60 = 33 ms
	ELSE
		LD	(HL),250			    ; scanfrequency 1*1000/50 = 20 ms
	ENDIF

    ; convert a MSX scancode to a MSX keycode and add to buffer
    ;ld A, 11111110b ; inverse bitmask, SHIFT pressed
    ;LD (NEWKEY+6),A
    ;LD A,00000100b ; bitmask
	;LD B,11-4 ; row
	;CALL 0D89H
    ld a, 'M'
    call C0F55

_NEW_HTIMI_DONE:
    pop af,bc,hl
    ret 

;	Subroutine	put keycode in keyboardbuffer
;	Inputs		A = keycode
;	Outputs		________________________
;	Remark		entrypoint compatible among keyboard layout versions

C0F55:
	LD	HL,(PUTPNT)
	LD	(HL),A			; put in keyboardbuffer
	CALL	C10C2			; next postition in keyboardbuffer with roundtrip
	LD	A,(GETPNT)
	CP	L			; keyboard buffer full ?
	RET	Z			; yep, quit
	LD	(PUTPNT),HL		; update put pointer
    RET

;	Subroutine	increase keyboardbuffer pointer
;	Inputs		________________________
;	Outputs		________________________

C10C2:
	INC	HL			; increase pointer
	LD	A,L
	CP	(KEYBUF+40) AND 255
	RET	NZ			; not the end of buffer, quit
	LD	HL,KEYBUF		; warp around to start of buffer
	RET

    include "print_bios.asm"

TXT_NEWLINE:        DB "\r\n",0
TXT_DRIVER_START:   DB "USB HID Driver started\r\n",0

ENDADR: 