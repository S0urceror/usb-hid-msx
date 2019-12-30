;
; CH376s.ASM - Z80 assembly to communicate with the CH376s generic USB chip
; Copyright (c) 2019 Néstor Soriano (Konamiman), Mario Smit (S0urceror)
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
CH_DEBUG:       equ     1

; CH376 commands
CH_CMD_SET_SPEED: equ 04h
CH_CMD_RESET_ALL: equ 05h
CH_CMD_CHECK_EXIST: equ 06h
CH_CMD_SET_RETRY: equ 0Bh
CH_CMD_DELAY_100US: equ 0Fh
CH_CMD_SET_USB_ADDR: equ 13h
CH_CMD_SET_USB_MODE: equ 15h
CH_CMD_TEST_CONNECT: equ 16h
CH_CMD_ABORT_NAK: equ 17h
CH_CMD_GET_STATUS: equ 22h
CH_CMD_DIRTY_BUFFER: equ 25h
CH_CMD_RD_USB_DATA0: equ 27h
CH_CMD_WR_HOST_DATA: equ 2Ch
CH_CMD_SET_FILE_NAME: equ 2Fh
CH_CMD_DISK_CONNECT: equ 30h
CH_CMD_DISK_MOUNT: equ 31h
CH_CMD_OPEN_FILE: equ 32h
CH_CMD_FILE_ENUM_GO: equ 33h
CH_CMD_FILE_CLOSE: equ 36h
CH_CMD_SET_ADDRESS: equ 45h
CH_CMD_GET_DESCR: equ 46h
CH_CMD_SET_CONFIG: equ 49h
CH_CMD_SEC_LOCATE: equ 4Ah
CH_CMD_SEC_READ: equ 4Bh
CH_CMD_SEC_WRITE: equ 4Ch
CH_CMD_ISSUE_TKN_X: equ 4Eh
CH_CMD_DISK_READ: equ 54h
CH_CMD_DISK_RD_GO: equ 55h
CH_CMD_DISK_WRITE: equ 56h
CH_CMD_DISK_WR_GO: equ 57h

; return codes
CH_ST_RET_SUCCESS: equ 51h
CH_ST_RET_ABORT: equ 5Fh
; CH376 ports
CH_DATA_PORT           equ 0010h
CH_COMMAND_PORT        equ 0011h
; CH376 result codes
CH_USB_INT_SUCCESS:  equ 14h
CH_USB_INT_CONNECT:  equ 15h
CH_USB_INT_DISCONNECT: equ 16h
CH_USB_INT_BUF_OVER: equ 17h
CH_USB_INT_DISK_READ: equ 1dh
CH_USB_INT_DISK_WRITE: equ 1eh
CH_USB_ERR_OPEN_DIR: equ 41h
CH_USB_ERR_MISS_FILE: equ 42h
CH_USB_ERR_FOUND_NAME: equ 43h
CH_USB_ERR_FILE_CLOSE: equ 0b4h
;--- PIDs
CH_PID_SETUP: equ 0Dh
CH_PID_IN: equ 09h
CH_PID_OUT: equ 01h
; own result codes
USB_ERR_PANIC_BUTTON_PRESSED: equ 0C1h

CH_BOOT_PROTOCOL: equ 0
; 2-set to 1.5 Mhz low-speed mode, 0-set to 12 Mhz high-speed mode (default)
CH_SPEED_LOW: equ 2
CH_SPEED_HIGH: equ 0
CH_MODE_HOST_RESET: equ 7
CH_MODE_HOST: equ 6

; --------------------------------------
; CH_FILE_CLOSE
;
; Input: A = 1 if file-length is updated in dir, 0 if not
; Output: none
CH_FILE_CLOSE:
    ld b,a
    ld a, CH_CMD_FILE_CLOSE
    out (CH_COMMAND_PORT), a ; start reading
    ld a, b
    out (CH_DATA_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    ret

; --------------------------------------
; CH_DISK_READ
;
; Input: HL points to the read buffer
;        IX points to the IO_BUFFER
;        (IX) should contain a 4 byte sector allowed count plus a 4 byte LBA
;        can be overwritten.
; Output: Cy = 1 on error
CH_DISK_READ:
    ld a, CH_CMD_DISK_READ
    out (CH_COMMAND_PORT), a ; start reading

    ;ld ix, hl
    ld a,(ix+4)
    out (CH_DATA_PORT), a
    ld a,(ix+5)
    out (CH_DATA_PORT), a
    ld a,(ix+6)
    out (CH_DATA_PORT), a
    ld a,(ix+7)
    out (CH_DATA_PORT), a
    ld a, (ix)
    out (CH_DATA_PORT), a

_DISK_READ_NEXT:
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_DISK_READ ; data read
    jp z, _DISK_READ_DATA
    cp CH_USB_INT_SUCCESS ; done reading
    jp z, _DISK_READ_SUCCESS
    scf ; error flag
    ret

_DISK_READ_DATA:
    ; read the contents of the sector in the buffer pointed by HL
    call CH_READ_DATA 
    ld a, c
    or a
    scf 
    ret z
    
    ld a, CH_CMD_DISK_RD_GO
    out (CH_COMMAND_PORT), a ; request next block
    jp _DISK_READ_NEXT

_DISK_READ_SUCCESS:
    or a
    ret

; --------------------------------------
; CH_DISK_WRITE
;
; Input: HL points to the write buffer
;        IX points to the IO buffer
;        (IX) should contain a 4 byte sector allowed count plus a 4 byte LBA
; Output: Cy = 1 on error
CH_DISK_WRITE:
    ld a, CH_CMD_DISK_WRITE
    out (CH_COMMAND_PORT), a ; start writing

    ;ld ix, hl
    ld a,(ix+4)
    out (CH_DATA_PORT), a
    ld a,(ix+5)
    out (CH_DATA_PORT), a
    ld a,(ix+6)
    out (CH_DATA_PORT), a
    ld a,(ix+7)
    out (CH_DATA_PORT), a
    ld a, (ix)
    out (CH_DATA_PORT), a

    ; multiply sector count by 8 separate writes per sector
    sla a
    sla a
    sla a
    ld d, a

_DISK_WRITE_NEXT:
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_DISK_WRITE ; ready to write data
    jp z, _DISK_WRITE_DATA
    cp CH_USB_INT_SUCCESS ; done reading
    jp z, _DISK_WRITE_SUCCESS
    scf ; error flag
    ret

_DISK_WRITE_DATA:
    ; failsafe, check if d already points to zero
    ld a, d
    or a
    jr z, _DISK_WRITE_SUCCESS

    ; write the contents of the sector in the buffer pointed by HL
    ld b, 64 ; 64 bytes per write
    call CH_WRITE_DATA 
    dec d

    ld a, CH_CMD_DISK_WR_GO
    out (CH_COMMAND_PORT), a ; request next block
    jp _DISK_WRITE_NEXT

_DISK_WRITE_SUCCESS:
    or a
    ret

; --------------------------------------
; CH_SEC_WRITE
;
; Input: A = number of sectors requested to read from file pointer
;        IX = IO_BUFFER address
; Output: Cy = 1 on error
;         (IX) contains LBA
CH_SEC_WRITE:
    ld b,a
    ld a, CH_CMD_SEC_WRITE
    out (CH_COMMAND_PORT), a
    ld a, b
    out (CH_DATA_PORT), a
    jr _CH_SEC_IO

; --------------------------------------
; CH_SEC_READ
;
; Input: A = number of sectors requested to read from file pointer
;        IX = IO_BUFFER address
; Output: Cy = 1 on error
;         (IX) contains LBA
CH_SEC_READ:
    ld b,a
    ld a, CH_CMD_SEC_READ
    out (CH_COMMAND_PORT), a
    ld a, b
    out (CH_DATA_PORT), a

_CH_SEC_IO
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS ; file sector found
    scf ; error flag
    ret nz

    push hl,bc
    ld hl,ix
    call CH_READ_DATA ; read absolute sector number
    ld a, c
    pop bc,hl
    ; we should have 8 bytes
    ; READ_BUFFER + 0,1,2,3 = nr. of sectors that we are allowed to read/write, zero is EOF
    ; READ_BUFFER + 4,5,6,7 = LBA absolute disk sector
    or a
    scf 
    ret z ; return when no data read
    ; number of allowed sectors > 0
    ld a, (ix)
    cp b
    scf 
    ret nz ; return if the nr. allowed is not 1
    ; clear Cy
    or a
    ret 

; --------------------------------------
; CH_SEC_LOCATE
;
; Input: DE = points to address of 32 bit file sector pointer
; Output: Cy = 1 on error
CH_SEC_LOCATE:
    push de
    ld a, CH_CMD_SEC_LOCATE
    out (CH_COMMAND_PORT), a
    ld a,(de)
    out (CH_DATA_PORT), a
    inc de
    ld a,(de)
    out (CH_DATA_PORT), a
    inc de
    ld a,(de)
    out (CH_DATA_PORT), a
    inc de
    ld a,(de)
    out (CH_DATA_PORT), a
    pop de

    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS ; file sector found
    scf ; error flag
    ret nz

    or a ; clear error flag
    ret

; --------------------------------------
; CH_SET_FILE_NAME
;
; Input: HL = pointer to filename or search path, buffer should be filled out to 13 chars
; Output: none
CH_SET_FILE_NAME:
    ld a, CH_CMD_SET_FILE_NAME
    out (CH_COMMAND_PORT), a
    ; write filename or search path, zero terminated
    ld c,CH_DATA_PORT
_SET_FILE_NAME_REPEAT:
    ld a,(hl) ; read from buffer
    out (c),a 
    inc hl
    or a ; stop if we read and output a 0?
    jp nz, _SET_FILE_NAME_REPEAT
    ret

; --------------------------------------
; CH_RESET
;
; Clear the CH376 data buffer in case a reset was made
; while it was in the middle of a data transfer operation
;
; Input: none
; Output: none
CH_RESET:
    ret
;    ld b,64
;_HW_RESET_CLEAR_DATA_BUF:
;    in a,(CH_DATA_PORT)
;    djnz _HW_RESET_CLEAR_DATA_BUF

    ld a, CH_CMD_RESET_ALL
    out (CH_COMMAND_PORT), a
    
    ld bc,1000
_HW_RESET_WAIT:
    dec bc
    ld a,b
    or c
    jr nz,_HW_RESET_WAIT

    ld a,CH_CMD_TEST_CONNECT
    out (CH_COMMAND_PORT),a
_CH_WAIT_TEST_CONNECT:
    in a,(CH_DATA_PORT)
    or a
    jr z,_CH_WAIT_TEST_CONNECT
;    ld bc,350
;    call CH_DELAY
    ret 

    ;Input: BC = Delay duration in units of 0.1ms
CH_DELAY:
    ld a,CH_CMD_DELAY_100US
    out (CH_COMMAND_PORT),a
_CH_DELAY_LOOP:
    in a,(CH_DATA_PORT)
    or a
    jr z,_CH_DELAY_LOOP 
    dec bc
    ld a,b
    or c
    jr nz,CH_DELAY
    ret

; --------------------------------------
; CH_FILE_OPEN
;
; Input: none, opens the file previously set
; Output: Cy = 1 on error
CH_FILE_OPEN:
    ld a, CH_CMD_OPEN_FILE
    out (CH_COMMAND_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS ; file opened
    scf ; error flag
    ret nz
_FILE_OPEN_SUCCESS:
    or a ; clear error flag
    ret 
; --------------------------------------
; CH_DIR_OPEN
;
; Input: none, opens the directory previously set
; Output: Cy = 1 on error
CH_DIR_OPEN:
    ld a, CH_CMD_OPEN_FILE
    out (CH_COMMAND_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_ERR_OPEN_DIR ; dir opened
    scf ; error flag
    ret nz
    or a ; clear error flag
    ret 

; --------------------------------------
; CH_DIRTY_BUFFER
;
; Clear internal disk and file buffers
;
; Input: none
; Output: none
CH_DIRTY_BUFFER:
    ld a, CH_CMD_DIRTY_BUFFER
    out (CH_COMMAND_PORT), a
    ret 

; --------------------------------------
; CH_SEARCH_OPEN
;
; Input: none, opens the wildcard-search previously set
; Output: Cy = 1 on error
CH_SEARCH_OPEN:
    ;call CH_DIRTY_BUFFER

    ld a, CH_CMD_OPEN_FILE
    out (CH_COMMAND_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_DISK_READ ; search succesfull, at least 1 result
    scf ; error flag
    ret nz
    or a ; clear error flag
    ret 
; --------------------------------------
; CH_SEARCH_NEXT
;
; Input: none, iterates the search previously set
; Output: Cy = 1 on error
CH_SEARCH_NEXT:
    ld a, CH_CMD_FILE_ENUM_GO
    out (CH_COMMAND_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_DISK_READ ; search succesfull, at least 1 result
    scf ; error flag
    ret nz
    or a ; clear error flag
    ret 
; --------------------------------------
; CH_CHECK_INT_IS_ACTIVE
;
; Check the status of the INT pin of the CH376
; Input: none
; Output: Z if active, NZ if not active

CH_CHECK_INT_IS_ACTIVE:
    in a,(CH_COMMAND_PORT)
    and 80h
    ret

; -----------------------------------------------------------------------------
; HW_TEST: Check if the USB host controller hardware is operational
; -----------------------------------------------------------------------------
; Output: Cy = 0 if hardware is operational, 1 if it's not

CH_HW_TEST:
    ld a,34h
    call _HW_TEST_DO
    scf
    ret nz
    ld a,89h
    call _HW_TEST_DO
    scf
    ret nz
    or a
    ret

_HW_TEST_DO:
    ld b,a
    ld a,CH_CMD_CHECK_EXIST
    out (CH_COMMAND_PORT),a
    ld a,b
    xor 0FFh
    out (CH_DATA_PORT),a
    in a,(CH_DATA_PORT)
    cp b
    ret

; --------------------------------------
; CH_CONNECT_DISK
;
; Input: A = (none)
; Output: Cy = 1 on error
CH_CONNECT_DISK:
    ld a, CH_CMD_DISK_CONNECT
    out (CH_COMMAND_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS
    scf ; error flag
    ret nz
    or a ; clear error flag
    ret 

; --------------------------------------
; CH_MOUNT_DISK
;
; Input: A = (none)
; Output: Cy = 1 on error
CH_MOUNT_DISK:
    ld a, CH_CMD_DISK_MOUNT
    out (CH_COMMAND_PORT), a
    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS
    scf ; error flag
    ret nz
    or a ; clear error flag
    ret 

; --------------------------------------
; CH_SET_USB_MODE
;
; Input: A = new USB mode:
;            5: Host, no SOF
;            6: Host, generate SOF
;            7: Host, generate SOF + bus reset
; Output: Cy = 1 on error

CH_SET_USB_MODE:
    ld b,a
    ld a,CH_CMD_SET_USB_MODE
    out (CH_COMMAND_PORT),a
    ld a,b
    out (CH_DATA_PORT),a

    ld b,255
_CH_WAIT_USB_MODE:
    in a,(CH_DATA_PORT)
    cp CH_ST_RET_SUCCESS
    jp z,_CH_CONFIGURE_RETRIES
    djnz _CH_WAIT_USB_MODE ; TODO: indefinately?
    scf
    ret
_CH_CONFIGURE_RETRIES:
    or a
    call HW_CONFIGURE_NAK_RETRY
    or a
    ret

; -----------------------------------------------------------------------------
; HW_CONFIGURE_NAK_RETRY
; -----------------------------------------------------------------------------
; Input: Cy = 0 to retry for a limited time when the device returns NAK
;               (this is the default)
;             1 to retry indefinitely (or for a long time) 
;               when the device returns NAK 

HW_CONFIGURE_NAK_RETRY:
    ld a,0FFh
    jr nc,_HW_CONFIGURE_NAK_RETRY_2
    ld a,0BFh
_HW_CONFIGURE_NAK_RETRY_2:
    push af
    ld a,CH_CMD_SET_RETRY
    out (CH_COMMAND_PORT),a
    ld a,25h    ;Fixed value, required by CH376
    out (CH_DATA_PORT),a
    ;Bits 7 and 6:
    ;  0x: Don't retry NAKs
    ;  10: Retry NAKs indefinitely (default)
    ;  11: Retry NAKs for 3s
    ;Bits 5-0: Number of retries after device timeout
    ;Default after reset and SET_USB_MODE is 8Fh
    pop af
    out (CH_DATA_PORT),a
    ret

; --------------------------------------
; CH_WRITE_DATA
;
; Write data to the CH data buffer
;
; Input:  HL = Source address of the data
;         B  = Length of the data
; Output: HL = HL + B
CH_WRITE_DATA:
    ld a,CH_CMD_WR_HOST_DATA
    out (CH_COMMAND_PORT),a
    ld c,CH_DATA_PORT
    ld a,b  
    out (c),a
    or a
    ret z

    otir
    ret

; --------------------------------------
; CH_READ_DATA
;
; Read data from the CH data buffer
;
; Input:  HL = Destination address for the data
; Output: C  = Amount of data received (0-64)
;         HL = HL + C

CH_READ_DATA:
    ld a,CH_CMD_RD_USB_DATA0
    out (CH_COMMAND_PORT),a
    in a,(CH_DATA_PORT)
    or a ; not zero?
    ld c, 0
    ret z   ;No data to transfer at all

    ; read data to (HL)
    push af
    ld b,a ; set counter nr bytes to read, first byte
    ld c,CH_DATA_PORT
    inir
    ; prepare to return
    pop af
    ld c,a
    ld (hl),0 ; zero at end of buffer - just in case
    or a
    ret

; --------------------------------------
; CH_WAIT_INT_AND_GET_RESULT
;
; Wait for INT to get active, execute GET_STATUS, 
; and return the matching USB error code
;
; Output: A = Result of GET_STATUS (an USB error code)
CH_WAIT_INT_AND_GET_RESULT:
    push bc
    call PANIC_KEYS_PRESSED
    pop bc
    ld a,USB_ERR_PANIC_BUTTON_PRESSED
    ret z

    call CH_CHECK_INT_IS_ACTIVE
    jr nz,CH_WAIT_INT_AND_GET_RESULT    ;TODO: Perhaps add a timeout check here?
    call CH_GET_STATUS
    ret

; --------------------------------------
; CH_SET_NOSOF_MODE: Sets USB host mode without SOF
;
; This needs to run when a device disconnection is detected
;
; Output: A  = -1
;         Cy = 1 on error

CH_DO_SET_NOSOF_MODE:
    ld a,5
    call CH_SET_USB_MODE

    ld a,-1
    ret

; --------------------------------------
; CH_GET_STATUS
;
; Output: A = Status code
CH_GET_STATUS:
    ld a,CH_CMD_GET_STATUS
    out (CH_COMMAND_PORT),a
    in a,(CH_DATA_PORT)
    ret

; -----------------------------------------------------------------------------
; SNSMAT: Print a string describing an USB error code
; -----------------------------------------------------------------------------
PANIC_KEYS_PRESSED:
    ;Return Z=1 if CAPS+ESC is pressed
    ld a,6
    call DO_SNSMAT
    and 1000b
    ld b,a
    ld a,7
    call DO_SNSMAT
    and 100b
    or b
    ret

; -----------------------------------------------------------------------------
; SNSMAT: Read the keyboard matrix
;
; This is the same SNSMAT provided by BIOS, it's copied here to avoid
; having to do an interslot call every time it's used
; -----------------------------------------------------------------------------

DO_SNSMAT:
    ld c,a
    di
    in a,(0AAh)
    and 0F0h
    add a,c
    out (0AAh),a
    ei
    in a,(0A9h)
    ret

    ;row 6:  F3     F2       F1  CODE    CAPS  GRAPH  CTRL   SHIFT
    ;row 7:  RET    SELECT   BS  STOP    TAB   ESC    F5     F4
    ;row 8:	 right  down     up  left    DEL   INS    HOME  SPACE

; Generic USB command variables
target_device_address EQU 0
configuration_id EQU 0
string_id EQU 0
config_descriptor_size EQU 9

; Generic USB commands
CMD_GET_DEVICE_DESCRIPTOR: DB 0x80,6,0,1,0,0,18,0
CMD_SET_ADDRESS: DB 0x00,0x05,target_device_address,0,0,0,0,0
CMD_SET_CONFIGURATION: DB 0x00,0x09,configuration_id,0,0,0,0,0
CMD_GET_STRING: DB 0x80,6,string_id,3,0,0,255,0
CMD_GET_CONFIG_DESCRIPTOR: DB 0x80,6,configuration_id,2,0,0,config_descriptor_size,0

; USB HID command variables
report_id EQU 0
duration EQU 0x80
interface_id EQU 0
protocol_id EQU 0
; USB HID commands
CMD_SET_IDLE: DB 0x21,0x0A,report_id,duration,interface_id,0,0,0
CMD_SET_PROTOCOL: DB 0x21,0x0B,protocol_id,0,interface_id,0,0,0

; --------------------------------------
; CH_SET_TARGET_DEVICE_ADDRESS
;
; Set target USB device address for operation
;
; Input: A = Device address

CH_SET_TARGET_DEVICE_ADDRESS:
    push af
    ld a,CH_CMD_SET_USB_ADDR
    out (CH_COMMAND_PORT),a
    pop af
    out (CH_DATA_PORT),a
    ret

; --------------------------------------
; CH_ISSUE_TOKEN
;
; Send a token to the current target USB device
;
; Input: E = Endpoint number
;        B = PID, one of CH_PID_*
;        A = Toggle bit in bit 7 (for IN transfer)
;            Toggle bit in bit 6 (for OUT transfer)

CH_ISSUE_TOKEN:
    ld d,a
    ld a,CH_CMD_ISSUE_TKN_X
    out (CH_COMMAND_PORT),a
    ld a,d
    out (CH_DATA_PORT),a    ;Toggles
    ld a,e
    rla
    rla
    rla
    rla
    and 0F0h
    or b
    out (CH_DATA_PORT),a    ;Endpoint | PID
    ret

; -----------------------------------------------------------------------------
; HW_DATA_IN_TRANSFER: Perform a USB data IN transfer
; -----------------------------------------------------------------------------
; Input:  HL = Address of a buffer for the received data
;         BC = Data length
;         A  = Device address
;         D  = Maximum packet size for the endpoint
;         E  = Endpoint number
;         Cy = Current state of the toggle bit
; Output: A  = USB error code
;         BC = Amount of data actually received (only if no error)
;         Cy = New state of the toggle bit (even on error)

HW_DATA_IN_TRANSFER:
    call CH_SET_TARGET_DEVICE_ADDRESS

; This entry point is used when target device address is already set
CH_DATA_IN_TRANSFER:
    ld a,0
    rra     ;Toggle to bit 7 of A
    ld ix,0 ;IX = Received so far count
    push de
    pop iy  ;IY = EP size + EP number

_CH_DATA_IN_LOOP:
    push af ;Toggle in bit 7
    push bc ;Remaining length

    ld e,iyl
    ld b,CH_PID_IN
    call CH_ISSUE_TOKEN

    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS
    jr nz,_CH_DATA_IN_ERR   ;DONE if error

    call CH_READ_DATA
    ld b,0
    add ix,bc   ;Update received so far count
_CH_DATA_IN_NO_MORE_DATA:
    pop de
    pop af
    xor 80h     ;Update toggle
    push af
    push de

    ld a,c
    or a
    jr z,_CH_DATA_IN_DONE    ;DONE if no data received

    ex (sp),hl  ;Now HL = Remaining data length
    or a
    sbc hl,bc   ;Now HL = Updated remaning data length
    ld a,h
    or l
    ex (sp),hl  ;Remaining data length is back on the stack
    jr z,_CH_DATA_IN_DONE    ;DONE if no data remaining

    ld a,c
    cp iyh
    jr c,_CH_DATA_IN_DONE    ;DONE if transferred less than the EP size

    pop bc
    pop af  ;We need this to pass the next toggle to CH_ISSUE_TOKEN

    jr _CH_DATA_IN_LOOP

;Input: A=Error code (if ERR), in stack: remaining length, new toggle
_CH_DATA_IN_DONE:
    ld a, CH_USB_INT_SUCCESS
_CH_DATA_IN_ERR:
    ld d,a
    pop bc
    pop af
    rla ;Toggle back to Cy
    ld a,d
    push ix
    pop bc
    ret

; -----------------------------------------------------------------------------
; HW_DATA_OUT_TRANSFER: Perform a USB data OUT transfer
; -----------------------------------------------------------------------------
; Input:  HL = Address of a buffer for the data to be sent
;         BC = Data length
;         A  = Device address
;         D  = Maximum packet size for the endpoint
;         E  = Endpoint number
;         Cy = Current state of the toggle bit
; Output: A  = USB error code
;         Cy = New state of the toggle bit (even on error)

HW_DATA_OUT_TRANSFER:
    call CH_SET_TARGET_DEVICE_ADDRESS

; This entry point is used when target device address is already set
CH_DATA_OUT_TRANSFER:
    ld a,0
    rra     ;Toggle to bit 6 of A
    rra
    push de
    pop iy  ;IY = EP size + EP number

_CH_DATA_OUT_LOOP:
    push af ;Toggle in bit 6
    push bc ;Remaining length

    ld a,b 
    or a
    ld a,iyh
    jr nz,_CH_DATA_OUT_DO
    ld a,c
    cp iyh
    jr c,_CH_DATA_OUT_DO
    ld a,iyh

_CH_DATA_OUT_DO:
    ;Here, A = Length of the next transfer: min(remaining length, EP size)

    ex (sp),hl
    ld e,a
    ld d,0
    or a
    sbc hl,de
    ex (sp),hl     ;Updated remaining data length to the stack

    ld b,a
    call CH_WRITE_DATA

    pop bc
    pop af  ;Retrieve toggle
    push af
    push bc

    ld e,iyl
    ld b,CH_PID_OUT
    call CH_ISSUE_TOKEN

    call CH_WAIT_INT_AND_GET_RESULT
    cp CH_USB_INT_SUCCESS
    jr nz,_CH_DATA_OUT_DONE   ;DONE if error

    pop bc
    pop af
    xor 40h     ;Update toggle
    push af

    ld a,b
    or c
    jr z,_CH_DATA_OUT_DONE_2  ;DONE if no more data to transfer

    pop af  ;We need this to pass the next toggle to CH_ISSUE_TOKEN

    jr _CH_DATA_OUT_LOOP

;Input: A=Error code, in stack: remaining length, new toggle
_CH_DATA_OUT_DONE:
    pop bc
_CH_DATA_OUT_DONE_2:
    ld d,a
    pop af
    rla ;Toggle back to Cy
    rla
    ld a,d
    ret

; -----------------------------------------------------------------------------
; HW_CONTROL_TRANSFER: Perform a USB control transfer on endpoint 0
;
; The size and direction of the transfer are taken from the contents
; of the setup packet.
; -----------------------------------------------------------------------------
; Input:  HL = Address of a 8 byte buffer with the setup packet
;         DE = Address of the input or output data buffer
;         A  = Device address
;         B  = Maximum packet size for endpoint 0
; Output: A  = USB error code
;         BC = Amount of data actually transferred (if IN transfer and no error)

HW_CONTROL_TRANSFER:
    call CH_SET_TARGET_DEVICE_ADDRESS

    push hl
    push bc
    push de

    ; SETUP STAGE
    ; -----------
    ld b,8
    call CH_WRITE_DATA  ;Write SETUP data packet    

    xor a
    ld e,0
    ld b,CH_PID_SETUP
    call CH_ISSUE_TOKEN

    call CH_WAIT_INT_AND_GET_RESULT
    pop hl  ;HL = Data address (was DE)
    pop de  ;D  = Endpoint size (was B)
    pop ix  ;IX = Address of setup packet (was HL)
    cp CH_USB_INT_SUCCESS
    ld bc,0
    ret nz  ;DONE if error

    ld c,(ix+6)
    ld b,(ix+7) ;BC = Data length
    ld a,b
    or c
    jr z,_CH_CONTROL_STATUS_IN_TRANSFER
    ld e,0      ;E  = Endpoint number
    scf         ;Use toggle = 1
    bit 7,(ix)
    jr z,_CH_CONTROL_OUT_TRANSFER

    ; DATA IN STAGE
    ; -------------
_CH_CONTROL_IN_TRANSFER:
    call CH_DATA_IN_TRANSFER
    or a
    ret nz

    ; STATUS STAGE
    ; -----------
    push bc
    ld b,0
    call CH_WRITE_DATA
    ld e,0
    ld b,CH_PID_OUT
    ld a,40h    ;Toggle bit = 1
    call CH_ISSUE_TOKEN
    call CH_WAIT_INT_AND_GET_RESULT

    pop bc
    ret

    ; DATA OUT STAGE
    ; -------------
_CH_CONTROL_OUT_TRANSFER:
    call CH_DATA_OUT_TRANSFER
    or a
    ret nz

_CH_CONTROL_STATUS_IN_TRANSFER:
    ; STATUS STAGE
    ; -----------
    push bc
    ld e,0
    ld b,CH_PID_IN
    ld a,80h    ;Toggle bit = 1
    call CH_ISSUE_TOKEN
    ld hl,0
    call CH_READ_DATA
    call CH_WAIT_INT_AND_GET_RESULT

    pop bc
    ret

; --------------------------------------
; CH_GET_DEVICE_DESCRIPTOR
;
; Input: HL=pointer to memory to receive device descriptor
; Output: Cy=0 no error, Cy=1 error
;         A  = USB error code
;         BC = Amount of data actually transferred (if IN transfer and no error)
CH_GET_DEVICE_DESCRIPTOR:
    push ix,hl,de,bc
    ld de, hl ; Address of the input or output data buffer
    ld hl, CMD_GET_DEVICE_DESCRIPTOR ; Address of the command: 0x80,6,0,1,0,0,18,0
    ld a, 0 ; device address
    ld b, 8 ; length in bytes
    call HW_CONTROL_TRANSFER
    pop bc,de,hl,ix
    cp CH_USB_INT_SUCCESS
    ret z ; no error
    scf ; error
    ret

; --------------------------------------
; CH_GET_CONFIG_DESCRIPTOR
;
; Input: HL=pointer to memory to receive config descriptor
;        A=configuration index starting with 0 to DEVICE_DESCRIPTOR.bNumConfigurations
;        B=max_packetsize
;        C=config_descriptor_size
;        D=device address 
; Output: Cy=0 no error, Cy=1 error
CH_GET_CONFIG_DESCRIPTOR:
    push ix,hl,de,bc
    ld iy, hl ; Address of the input or output data buffer
    ld hl, CMD_GET_CONFIG_DESCRIPTOR ; Address of the command: 0x80,6,configuration_id,2,0,0,config_descriptor_size,0
    ld ix, hl
    ld (ix+2), a
    ld (ix+6), c
    ld a, d ; device address
    ld de, iy ; Address of the input or output data buffer
    call HW_CONTROL_TRANSFER
    pop bc,de,hl,ix
    cp CH_USB_INT_SUCCESS
    ret z ; no error
    scf ; error
    ret


; --------------------------------------
; CH_SET_SPEED
;
; Input: A=speed value
; Output: Cy=0 no error, Cy=1 error
CH_SET_SPEED:
    push bc
    ld b,a
    ld a,CH_CMD_SET_SPEED
    out (CH_COMMAND_PORT),a
    ld a,b
    out (CH_DATA_PORT),a
    or a; clear Cy
    pop bc
    ret

; --------------------------------------
; CH_SET_CONFIGURATION
;
; Input: A=configuration id
;        B=packetsize
;        D=device address 
; Output: Cy=0 no error, Cy=1 error
CH_SET_CONFIGURATION:
    push ix,hl
    ld hl, CMD_SET_CONFIGURATION ; Address of the command: 0x00,0x09,configuration_id,0,0,0,0,0
    ld ix, hl
    ld (ix+2),a
    ld a, d ; device address
    call HW_CONTROL_TRANSFER
    pop hl,ix
    cp CH_USB_INT_SUCCESS
    ret z ; no error
    scf ; error
    ret


; --------------------------------------
; CH_SET_PROTOCOL
;
; Input: A=protocol id (0=BOOT)
;        B=packetsize
;        D=device address 
;        E=interface id
; Output: Cy=0 no error, Cy=1 error
CH_SET_PROTOCOL:
    push ix,hl
    ld hl, CMD_SET_PROTOCOL ; Address of the command: 0x21,0x0B,protocol_id,0,interface_id,0,0,0
    ld ix, hl
    ld (ix+2),a
    ld (ix+4),e
    ld a, d ; device address
    call HW_CONTROL_TRANSFER
    pop hl,ix
    cp CH_USB_INT_SUCCESS
    ret z ; no error
    scf ; error
    ret

; --------------------------------------
; CH_SET_IDLE
;
; Input: A=idle value
;        B=packetsize
;        C=report id
;        D=device address 
;        E=interface id
; Output: Cy=0 no error, Cy=1 error
CH_SET_IDLE:
    push ix,hl
    ld hl, CMD_SET_IDLE ; Address of the command: 0x21,0x0A,report_id,duration,interface_id,0,0,0
    ld ix, hl
    ld (ix+2),c
    ld (ix+3),a
    ld (ix+4),e
    ld a, d ; device address
    call HW_CONTROL_TRANSFER
    pop hl,ix
    cp CH_USB_INT_SUCCESS
    ret z ; no error
    scf ; error
    ret

; --------------------------------------
; CH_SET_ADDRESS
;
; Input: A=address to assign to connected USB device
;        B=packetsize
; Output: Cy=0 no error, Cy=1 error
CH_SET_ADDRESS:
    push ix,hl,de
    ld de, hl ; Address of the input or output data buffer
    ld hl, CMD_SET_ADDRESS ; Address of the command: 0x00,0x05,target_device_address,0,0,0,0,0
    ld ix, hl
    ld (ix+2),a
    ld a, 0 ; device address
    call HW_CONTROL_TRANSFER
    pop de,hl,ix
    cp CH_USB_INT_SUCCESS
    ret z ; no error
    scf ; error
    ret