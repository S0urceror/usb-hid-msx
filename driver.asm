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

    DEFINE DEBUG 1

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
    ; allocate work area
    ld ix, WRKAREA
    ; initialise USB HID
    call INIT_USBHID_KEYBOARD
    ret c
    ; okay, install H.TIMI hook
    ld hl, H.TIMI
    ld (hl),0C3h ; JP
    inc hl
    ld (hl),low NEW_HTIMI
    inc hl
    ld (hl),high NEW_HTIMI
    ret

; Input: IX=pointer to work area
INIT_USBHID_KEYBOARD:
    ; check_exists
    call CH_HW_TEST
    ret c
    IF DEBUG==1
        ld hl,TXT_CHECK_EXISTS
        call PRINT
    ENDIF
    ; set_usb_mode (MODE_HOST_RESET)
    ld a, CH_MODE_HOST_RESET
    call CH_SET_USB_MODE
    ret c
    ; set_usb_mode (MODE_HOST)
    ld a, CH_MODE_HOST
    call CH_SET_USB_MODE
    ret c
    IF DEBUG==1
        ld hl, TXT_USB_MODE_SET
        call PRINT
    ENDIF
    ; get device descriptor
    ld hl, ix ;DEVICE_DESCRIPTOR
    call CH_GET_DEVICE_DESCRIPTOR
    jr nc, _INIT_USBHID_NEXT
    ; if not OK, set_speed (2), get device descriptor
    ld a, CH_SPEED_LOW
    call CH_SET_SPEED
    ret c
    IF DEBUG==1
        ld hl, TXT_SET_SPEED
        call PRINT
    ENDIF
    ld hl, ix ;DEVICE_DESCRIPTOR
    call CH_GET_DEVICE_DESCRIPTOR
    ret c
_INIT_USBHID_NEXT:
    IF DEBUG==1
        ld hl, TXT_DEVICE_DESCRIPTOR
        call PRINT
        ld hl, ix
        ld bc, 18
        call PRINTHEX_BUFFER
    ENDIF
    ; set address (1)
    ld a, MYADDRESS ; address to assign to attached USB device
    ld b, (ix+DEVICE_DESCRIPTOR.bMaxPacketSize0)
    call CH_SET_ADDRESS
    ret c
    IF DEBUG==1
        ld hl, TXT_SET_ADDRESS
        call PRINT
    ENDIF
    ; from now on the device only listens to address given
    ; get config descriptor
    ld bc, DEVICE_DESCRIPTOR ; sizeof
    ld hl, ix
    add hl, bc ; config lies after device descriptor
    ld a, 0 ; first configuration
    ld b, (ix+DEVICE_DESCRIPTOR.bMaxPacketSize0)
    ld c, CONFIG_DESCRIPTOR ; sizeof
    ld d, MYADDRESS ; assigned address
    call CH_GET_CONFIG_DESCRIPTOR ; call first with max packet size to discover real size
    ret c
    ld a, 0 ; first configuration
    ld ix, hl
    ld c, (ix+CONFIG_DESCRIPTOR.wTotalLength) ; lower 8 bits
    call CH_GET_CONFIG_DESCRIPTOR ; call again with real size
    ret c
    IF DEBUG==1
        push hl
        ld hl, TXT_CONFIG_DESCRIPTOR
        call PRINT
        pop hl
        ld ix, hl
        ld bc, (ix+CONFIG_DESCRIPTOR.wTotalLength)
        call PRINTHEX_BUFFER
    ENDIF
    ; found HID keyboard?
    call GET_DESCR_CONFIGURATION ; returns configuration_value in A and Cy to indicate error
    ret c
    ; set configuration 
    ld ix, WRKAREA
    ld b, (ix+DEVICE_DESCRIPTOR.bMaxPacketSize0)
    ld d, MYADDRESS
    call CH_SET_CONFIGURATION
    ret c

    call GET_HID_KEYBOARD_VALUES ; returns Cy when error, A contains interface number, B contains endpoint nr
    ret c
    ; save for convenience
    ld (KEYBOARD_INTERFACENR),a
    ld a, b
    ld (KEYBOARD_ENDPOINTNR), a
    IF DEBUG==1
        ld hl, TXT_KEYBOARD_FOUND
        call PRINT
    ENDIF
    ; set protocol (BOOT_PROTOCOL,keyboard_interface)
    ld ix, WRKAREA
    ld d, MYADDRESS ; assigned address
    ld b, (ix+DEVICE_DESCRIPTOR.bMaxPacketSize0)
    ld a, (KEYBOARD_INTERFACENR)
    ld e, a ; interface number
    ld a, CH_BOOT_PROTOCOL
    call CH_SET_PROTOCOL
    ret c
    IF DEBUG==1
        ld hl, TXT_KEYBOARD_BOOT_PROTOCOL
        call PRINT
    ENDIF
    ; set idle (0x80)
    ld ix, WRKAREA
    ld d, MYADDRESS ; assigned address
    ld b, (ix+DEVICE_DESCRIPTOR.bMaxPacketSize0)
    ld a, (KEYBOARD_INTERFACENR)
    ld e, a ; interface number
    ld a, 20h ; approximately 250ms
    ld c, 0 ; report id
    call CH_SET_IDLE
    ret c
    IF DEBUG==1
        ld hl, TXT_KEYBOARD_IDLE_SET
        call PRINT
    ENDIF
    ; we're now ready to receive keystrokes from the keyboard interface
    or a ; clear Cy
    ret

READ_HID_KEYBOARD:
    ld a, (USB_READ_TOGGLE) ; get stored toggle value
    rla ; high bit shifted to Cy
    ld ix, WRKAREA
    ld hl, USB_HID_BOOT_KEYBOARD_BUFFER
    ld bc, BOOT_KEYBOARD_INPUT_REPORT
    ld d, (ix+DEVICE_DESCRIPTOR.bMaxPacketSize0)
    ld a, (KEYBOARD_ENDPOINTNR)
    ld e, a
    ld a, MYADDRESS
    call HW_DATA_IN_TRANSFER ; A=USB result code, Cy=toggle bit
    push af
    xor a
    rra ; Cy stored in high bit of A
    ld (USB_READ_TOGGLE),a ; stored in memory
    pop af
    cp CH_USB_INT_SUCCESS
    ret nz
    ld ix, USB_HID_BOOT_KEYBOARD_BUFFER
    ld b, (ix+0)
    ld a, (ix+2)
    ;cp 0x14
    ;ret z ; pressed Q
    call CONVERT_SCANCODE_INSERT_CHAR
    ld a, (ix+3)
    call CONVERT_SCANCODE_INSERT_CHAR
    ;ld a, (ix+4)
    ;call CONVERT_SCANCODE_INSERT_CHAR
    ret

_SCANCODES_ASCII:
    DB 0x04,'a','A'
    DB 0x05,'b','B'
    DB 0x06,'c','C'
    DB 0x07,'d','D'
    DB 0x08,'e','E'
    DB 0x09,'f','F'
    DB 0x0a,'g','G'
    DB 0x0b,'h','H'
    DB 0x0c,'i','I'
    DB 0x0d,'j','J'
    DB 0x0e,'k','K'
    DB 0x0f,'l','L'
    DB 0x10,'m','M'
    DB 0x11,'n','N'
    DB 0x12,'o','O'
    DB 0x13,'p','P'
    DB 0x14,'q','Q'
    DB 0x15,'r','R'
    DB 0x16,'s','S'
    DB 0x17,'t','T'
    DB 0x18,'u','U'
    DB 0x19,'v','V'
    DB 0x1a,'w','W'
    DB 0x1b,'x','X'
    DB 0x1c,'y','Y'
    DB 0x1d,'z','Z'
    DB 0x1e,'1','!'
    DB 0x1f,'2','@'
    DB 0x20,'3','#'
    DB 0x21,'4','$'
    DB 0x22,'5','%'
    DB 0x23,'6','^'
    DB 0x24,'7','&'
    DB 0x25,'8','*'
    DB 0x26,'9','('
    DB 0x27,'0',')'
    DB 0x28,13,13 ; ENTER
    DB 0x29,27,27 ; ESC
    DB 0x2a,8,8  ; BACKSPACE
    DB 0x2b,9,9  ; TAB
    DB 0x2c,' ',' '; SPACE
    DB 0x2d,'-','_'
    DB 0x2e,'=','+'
    DB 0x2f,'[','{'
    DB 0x30,']','}'
    DB 0x31,"\\",'|'
    DB 0x33,';',':'
    DB 0x34,"\'","\""
    DB 0x35,'`','~'
    DB 0x36,',','<'
    DB 0x37,'.','>'
    DB 0x38,'/','?'
_SCANCODES_ASCII_END:

; A = SCANCODE
; B = MODIFIER
;     KEY_MOD_LCTRL  0x01
;     KEY_MOD_LSHIFT 0x02
;     KEY_MOD_LALT   0x04
;     KEY_MOD_LMETA  0x08
;     KEY_MOD_RCTRL  0x10
;     KEY_MOD_RSHIFT 0x20
;     KEY_MOD_RALT   0x40
;     KEY_MOD_RMETA  0x80
CONVERT_SCANCODE_INSERT_CHAR:
    or a
    ret z ; nothing to print when 0
    ld d,b ; modifier keys
    ld hl, _SCANCODES_ASCII
    ld bc, _SCANCODES_ASCII_END - _SCANCODES_ASCII
_AGAIN:
    cpi
    jr z, _FOUND
    inc hl
    inc hl
    dec bc
    dec bc
    jr nz,_AGAIN
    ret ; not found
_FOUND:
    ld a, d ; modifier keys
    or a
    jr z, _CONTINUE
    inc hl
    cp 0x02
    jr z, _CONTINUE
    cp 0x20
    jr z, _CONTINUE
    ret ; do not support
_CONTINUE
    ld a, (hl)
    call C0F55
    ret

NEW_HTIMI:
    push hl,bc,af
    ; check if it's time to do a keyboard scan, just like the BIOS does it
    ; takes precedence over the internal keyboard scan because SCNCNT never reaches zero in BIOS
    LD	HL,SCNCNT
	DEC	(HL)			        ; time for a keyboard scan ?
	JR	NZ,_NEW_HTIMI_DONE		; nope, quit interrupt routine
	IF INTHZ = 60
		LD	(HL),2			    ; scanfrequency 2*1000/60 = 33 ms
	ELSE
		LD	(HL),2			    ; scanfrequency 1*1000/50 = 20 ms
	ENDIF

    ; convert a MSX scancode to a MSX keycode and add to buffer
    ;ld A, 11111110b ; inverse bitmask, SHIFT pressed
    ;LD (NEWKEY+6),A
    ;LD A,00000100b ; bitmask
	;LD B,11-4 ; row
	;CALL 0D89H

    call READ_HID_KEYBOARD

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

; --------------------------------------
; GET_DESCR_CONFIGURATION
;
; Input: (none)
; Output: Cy=0 no error, Cy=1 error
;         A = configuration id
GET_DESCR_CONFIGURATION:
    ld ix, WRKAREA
    ld a, (ix+DEVICE_DESCRIPTOR.bNumConfigurations)
    cp 1
    jr nz,_ERR_GET_DESCR_CONFIGURATION ; only 1 configuration allowed
    ld bc, DEVICE_DESCRIPTOR
    add ix, bc
    ; ix now pointing to first (and only) configuration descriptor
    ld a, (ix+CONFIG_DESCRIPTOR.bConfigurationvalue)
    or a ; reset Cy
    ret
_ERR_GET_DESCR_CONFIGURATION:
    scf
    ret

HID_CLASS equ 0x03
HID_BOOT equ 0x01
HID_KEYBOARD equ 0x01
HID_MOUSE equ 0x02
; --------------------------------------
; GET_HID_KEYBOARD_VALUES
;
; Input: (none)
; Output: Cy=0 no error, Cy=1 error
;         A = interface number
;         B = endpoint address
GET_HID_KEYBOARD_VALUES:
    ld ix, WRKAREA
    ld a, (ix+DEVICE_DESCRIPTOR.bNumConfigurations)
    cp 1
    jr nz,_ERR_GET_HID_KEYBOARD_VALUES ; only 1 configuration allowed
    ld bc, DEVICE_DESCRIPTOR
    add ix, bc
    ; ix now pointing to first (and only) configuration descriptor
    ld c, CONFIG_DESCRIPTOR
    ld b, 0
    ld d, (ix+CONFIG_DESCRIPTOR.bNumInterfaces)
    add ix, bc
    ; ix now pointing to interface descriptor
_NEXT_INTERFACE:
    ld a, (ix+INTERFACE_DESCRIPTOR.bNumEndpoints)
    cp 1
    jr nz, _ERR_GET_HID_KEYBOARD_VALUES; not supported more then 1 endpoint per interface
    ; HID interface class?
    ld a, (ix+INTERFACE_DESCRIPTOR.bInterfaceClass)
    ld c, INTERFACE_DESCRIPTOR+ENDPOINT_DESCRIPTOR ; next interface, no HID block
    cp HID_CLASS
    jr nz, _NEXT_GET_HID_KEYBOARD
    ; HID BOOT interface subclass?
    ld c, INTERFACE_DESCRIPTOR+HID_DESCRIPTOR+ENDPOINT_DESCRIPTOR ; next interface, plus HID block
    ld a, (ix+INTERFACE_DESCRIPTOR.bInterfaceSubClass)
    cp HID_BOOT
    jr nz, _NEXT_GET_HID_KEYBOARD
    ; HID KEYBOARD interface protocol?
    ld a, (ix+INTERFACE_DESCRIPTOR.bInterfaceProtocol)
    cp HID_KEYBOARD
    jr nz, _NEXT_GET_HID_KEYBOARD
    ; found it
    ld a, (ix+INTERFACE_DESCRIPTOR+HID_DESCRIPTOR+ENDPOINT_DESCRIPTOR.bEndpointAddress)
    and 0x0f
    ld b,a
    ld a, (ix+INTERFACE_DESCRIPTOR.bInterfaceNumber)
    or a ; clear Cy
    ret
_NEXT_GET_HID_KEYBOARD:
    add ix, bc
    dec d ; more interfaces to scan?
    jr nz, _NEXT_INTERFACE
_ERR_GET_HID_KEYBOARD_VALUES:
    scf
    ret

    STRUCT DEVICE_DESCRIPTOR
BASE:
bLength: db
bDescriptorType: db
bcdUSB: dw
bDeviceClass: db
bDeviceSubClass: db
bDeviceProtocol: db
bMaxPacketSize0: db
idVendor: dw
idProduct: dw
bcdDevice: dw
iManufacturer: db
iProduct: db
iSerialNumber: db
bNumConfigurations: db
    ENDS

    STRUCT CONFIG_DESCRIPTOR
BASE:
bLength: DB
bDescriptorType: DB
wTotalLength: DW
bNumInterfaces: DB
bConfigurationvalue: DB
iConfiguration: DB
bmAttributes: DB
bMaxPower: DB
    ENDS

    STRUCT INTERFACE_DESCRIPTOR
bLength: DB
bDescriptorType: DB
bInterfaceNumber: DB
bAlternateSetting: DB
bNumEndpoints: DB
bInterfaceClass: DB
bInterfaceSubClass: DB
bInterfaceProtocol: DB
iInterface: DB
    ENDS

   STRUCT HID_DESCRIPTOR
bLength: DB
bDescriptorType: DB
hid_version: DW
country_code: DB
num_descriptors: DB
descriptor_type: DB
descriptor_length: DW
    ENDS

    STRUCT ENDPOINT_DESCRIPTOR
bLength: DB
bDescriptorType: DB
bEndpointAddress: DB
bmAttributes: DB
wMaxPacketSize: DW
bInterval: DB
    ENDS

    STRUCT BOOT_KEYBOARD_INPUT_REPORT
bModifierKeys: DB
bReserved: DB
Keycode1: DB
Keycode2: DB
Keycode3: DB
Keycode4: DB
Keycode5: DB
Keycode6: DB
    ENDS

    include "ch376s.asm"
    include "print_bios.asm"

TXT_NEWLINE: DB "\r\n",0
TXT_DRIVER_START: DB "USB HID Driver started\r\n",0
    IF DEBUG==1
TXT_CHECK_EXISTS: DB "+CH376s plugged in\r\n",0
TXT_USB_MODE_SET: DB "+USB Reset and Mode set\r\n",0
TXT_SET_SPEED: DB "+Lowered speed of USB bus\r\n",0
TXT_DEVICE_DESCRIPTOR: DB "+USB device descriptor:\r\n",0
TXT_SET_ADDRESS: DB "+USB address set\r\n",0
TXT_CONFIG_DESCRIPTOR: DB "+USB configuration descriptor:\r\n",0
TXT_KEYBOARD_FOUND: DB "+USB HID keyboard detected\r\n",0
TXT_KEYBOARD_BOOT_PROTOCOL: DB "+USB HID keyboard boot protocol set\r\n",0
TXT_KEYBOARD_IDLE_SET: DB "+USB HID keyboard idle repeat set to 250ms\r\n",0
    ENDIF

USB_READ_TOGGLE: db 0
KEYBOARD_INTERFACENR: DB 0
KEYBOARD_ENDPOINTNR: DB 0
USB_HID_BOOT_KEYBOARD_BUFFER: DS 8,0
WRKAREA: DS DEVICE_DESCRIPTOR+CONFIG_DESCRIPTOR+INTERFACE_DESCRIPTOR*3+ENDPOINT_DESCRIPTOR*3+HID_DESCRIPTOR*3,0

ENDADR: 