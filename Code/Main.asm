[org 0x100]

jmp gameStart

%INCLUDE "E:\Programming\Assembly x8088\FlapFlap\Extras\AestheticBG.asm"

;-------------------------------------Game Constants-------------------------------------
arrowUpScancode: db 0x48
birdX: dw 65
birdY: dw 45
currCol1: dw 140
currCol2: dw 280
gameSpeed: dw 5
gravitySpeed: dw 3
gameOverFlag: dw 0
gameOverString: db 'Game Over!'
heightDiff: dw 50
jumpFlag: dw 0
jumpSize: dw 15
pipeColor: dw 0x60
pipeLength1: dw 40
pipeLength2: dw 80
pressAnyString: db 'Press Any key to Start'
scoreCount: dw 0
scoreCountString: db 48, 48, 48
scoreString: db 'Score: '
skyColor: dw 0x01
startCol: dw 280
timerTicks: dw 0
titleString: db 'Flap Flap'
pausedString: db 'Paused'
string3: db "Credits        [c]"
string4: db "New Game     [space]" 
string5: db "Exit          [Esc]" 
thankYou: db "Thank you for playing!" 
string6:db "Abdul Muhaimin(23L-0755)"
string7:db "Ahmad Waleed(23L-0685)"
string8:db "Loading....."
string9:db "Press Space to jump"
string10:db "Press P to pause"
string11:db "Press R to resume"
string12:db "Instructions"

escScancode db 0x01       ; Scancode for Esc
cScancode db 0x2E         ; Scancode for 'C'
spaceScancode db 0x39     ; Scancode for Space
pScancode db 0x19         ; Scancode for 'P'
rScancode db 0x13         ; Scancode for 'R'
nScancode db 0x31 	   ; Scancode for 'N'

; Flags
pauseFlag: dw 0

; RNG constants
seed: dw 125       
multiplier: dw 11
increment: dw 123
modulus: dw 90

; OLDISRS
oldkbisr: dd 0

printStartScreen:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov ax, 13h
    int 10h
    mov ax, 0xA000     
    mov es, ax

	;Prints Title of Red Flap n Fly
   
    push word 0x04     				; Attribute
    push word 1886    			; Y-coordinate (0)
    push word 12     				; X-coordinate (7)
    push string12          ; String to print
    call printString 
	
	push word 0x0F     				; Attribute
    push word 2394    			; Y-coordinate (0)
    push word 23       				; X-coordinate (7)
    push pressAnyString           ; String to print
    call printString 
	
	push word 0x0F     				; Attribute
    push word 2690    			; Y-coordinate (0)
    push word 19       				; X-coordinate (7)
    push string9           ; String to print
    call printString 
	
	push word 0x0F     				; Attribute
    push word 2986    			; Y-coordinate (0)
    push word 16       				; X-coordinate (7)
    push string10           ; String to print
    call printString 
	
	push word 0x0F     				; Attribute
    push word 3282    			; Y-coordinate (0)
    push word 17       				; X-coordinate (7)
    push string11           ; String to print
    call printString 

	; ---------------------------------------
	;background filler 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,190      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 80      ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,140      ;row no 
	push ax
	mov ax, 120      ;column no
	add ax, 30
	push ax
	mov ax, 60       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 20     ;column no
	add ax, 30
	push ax
	mov ax, 30       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 180      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 18      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,180      ;row no 
	push ax
	mov ax, 70     ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 185     ;column no
	add ax, 30
	push ax
	mov ax, 40       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,130     ;row no 
	push ax
	mov ax, 235     ;column no
	add ax, 30
	push ax
	mov ax, 100     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	
	mov ax,120     ;row no 
	push ax
	mov ax, -25     ;column no
	add ax, 30
	push ax
	mov ax, 80     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle

	;-------------------------------
	
	mov ah, 0x00
	int 0x16
	
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	pop bp

	ret

printEndScreen:
	mov ax, 13h
    int 10h
    mov ax, 0xA000     
    mov es, ax

    ; Prints the main menu
    push word 0x04         ; Attribute
    push word 2003         ; Y-coordinate 
    push word 18           ; X-coordinate 
    push string3           ; String to print
    call printString
	
    push word 0x04         ; Attribute
    push word 2395         ; Y-coordinate
    push word 20           ; X-coordinate
    push string4           ; String to print
    call printString
	
    push word 0x04         ; Attribute
    push word 2731         ; Y-coordinate
    push word 19           ; X-coordinate
    push string5           ; String to print
    call printString
	
	mov ax,0      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,190      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 80      ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,140      ;row no 
	push ax
	mov ax, 120      ;column no
	add ax, 30
	push ax
	mov ax, 60       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 20     ;column no
	add ax, 30
	push ax
	mov ax, 30       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 180      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 18      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,180      ;row no 
	push ax
	mov ax, 70     ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 185     ;column no
	add ax, 30
	push ax
	mov ax, 40       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,130     ;row no 
	push ax
	mov ax, 235     ;column no
	add ax, 30
	push ax
	mov ax, 100     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	
	mov ax,120     ;row no 
	push ax
	mov ax, -25     ;column no
	add ax, 30
	push ax
	mov ax, 80     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle
	
	ret
	
	
introScreen:

    mov ax, 13h
    int 10h
    mov ax, 0xA000     
    mov es, ax
	
	push word 0x04     				; Attribute
    push word 2104    			; XY-coordinate 
    push word 9       				; Length
    push titleString   				; String to print
    call printString
	
	push word 0x04     				; Attribute
    push word 2519    			; XY-coordinate 
    push word 12       				; Length
    push string8   				; String to print
    call printString
	
	
	
	
	mov ax,0      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,190      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 80      ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,140      ;row no 
	push ax
	mov ax, 120      ;column no
	add ax, 30
	push ax
	mov ax, 60       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 20     ;column no
	add ax, 30
	push ax
	mov ax, 30       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 180      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 18      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,180      ;row no 
	push ax
	mov ax, 70     ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 185     ;column no
	add ax, 30
	push ax
	mov ax, 40       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,130     ;row no 
	push ax
	mov ax, 235     ;column no
	add ax, 30
	push ax
	mov ax, 100     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	
	mov ax,120     ;row no 
	push ax
	mov ax, -25     ;column no
	add ax, 30
	push ax
	mov ax, 80     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle
	
	ret
bigDelay:	
    call delay 
    call delay  
    call delay 
    call delay 
    call delay 
    call delay 
	call delay 
    call delay
	call delay 
	call delay 
	call delay 
	call delay
	call delay 
	call delay 
    call delay 
	call delay 
    call delay 
	call delay 
    call delay 
    call delay 
	call delay 
    call delay 
	call delay 
	call delay 
	call delay 
	call delay 
	call delay 
    call delay 
    call delay 
	call delay 
	call delay 
	call delay 
	call delay 
	call delay 
	call delay 
	ret

; playMusic:
    ; push bp
    ; mov bp, sp
    ; push ax
    ; push bx
    ; push cx
    ; push dx

    ; mov si, imfData
    ; mov cx, [imfCount]

    ; nextCommand:
        ; mov dx, 0x388
        ; mov al, [si + 0]
        ; out dx, al

        ; inc dx
        ; mov al, [si + 1]
        ; out dx, al

        ; mov al, [si + 2]
        ; mov ah, [si + 3]

        ; push cx

        ; repeatDelay:

        ; mov cx, 1

        ; delayLooop:
            ; nop
            ; loop delayLooop

        ; dec ax
        ; jnz repeatDelay

        ; pop cx
        ; add si, 4

        ; loop nextCommand

    ; pop dx
    ; pop cx
    ; pop bx
    ; pop ax
    ; mov sp, bp
    ; pop bp

    ; ret

printString:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx
	push es

	mov ah, 0x13            ; Subservice
	mov bh, 0               ; Page no.
	mov bl, [bp+10]         ; Attribute
	mov dx, [bp+8]        	; Coordinate on screen
	mov cx, [bp+6]        	; Length of the string
	mov bp, [bp+4]       	; bp = address of string

	push cs
	pop es                  ; String -> es:bp

	int 0x10

	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 8

; timerHandler:
;     inc word[timerTicks]
;     iret
	
kbISR:
    pusha	
	in al, 0x60	

	checkArrowUp:
		cmp al, [arrowUpScancode]
		je jump

	checkSpace:
		cmp al, [spaceScancode]
		je jump

	checkP:
		cmp al, [pScancode]
		je pauseGame

	checkR:
		cmp al, [rScancode]
		je resumeGame

	checkEsc:
	    cmp al, [escScancode]
	    je exitProgram       ; If Esc is pressed, exit program
	
	checkC:
	    cmp al, [cScancode]
	    je displayCredits   ; If 'C' is pressed, show thank you message
	
	checkN:
	    cmp al, [nScancode]
	    je restartProgram    ; If Space is pressed, restart program
		jmp endISR
	
	jump:
		mov ax, word [birdX]
		sub ax, word [jumpSize]
		mov word [birdX], ax
		mov word [jumpFlag], 1
		jmp endISR

	pauseGame:
		mov word [pauseFlag], 1
		jmp endISR

	resumeGame:
		mov word [pauseFlag], 0

	endISR:
		popa
		jmp far [cs:oldkbisr]
	
; kbISR2:                             ;This kbisr is for hooking keys at the endscreen
;     pusha	
;     in al, 0x60	           ; Read scancode from keyboard buffer
	
; 	checkEsc:
; 	    cmp al, [escScancode]
; 	    je exitProgram       ; If Esc is pressed, exit program
	
; 	checkC:
; 	    cmp al, [cScancode]
; 	    je displayCredits   ; If 'C' is pressed, show thank you message
	
; 	checkSpace2:
; 	    cmp al, [spaceScancode]
; 	    je restartProgram    ; If Space is pressed, restart program
	
; 	endISR2:
; 	    popa
; 	    jmp far [cs:oldkbisr]

exitProgram:
    call cls
    popa
    mov ah, 0x4C           
    int 0x21
	
displayCredits:
    popa
	mov ax, 13h
    int 10h
    mov ax, 0xA000     
    mov es, ax
    push word 0x04         ; Attribute
    push word 2098          ; Y-coordinate
    push word 22           ; X-coordinate
    push thankYou          ; String to print
    call printString
	
	push word 0x04         ; Attribute
    push word 2650          ; Y-coordinate
    push word 24           ; X-coordinate
    push string6          ; String to print
    call printString
	
	
	push word 0x04         ; Attribute
    push word 3082          ; Y-coordinate
    push word 22           ; X-coordinate
    push string7          ; String to print
    call printString
	
	
	
	
	mov ax,0      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,190      ;row no 
	push ax
	mov ax, 0      ;column no
	add ax, 30
	push ax
	mov ax, 10       ;length
	push ax
	mov ax,320   ;width
	push ax
	mov ax, 0x0A		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 80      ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,140      ;row no 
	push ax
	mov ax, 120      ;column no
	add ax, 30
	push ax
	mov ax, 60       ;length
	push ax
	mov ax, 50  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 20     ;column no
	add ax, 30
	push ax
	mov ax, 30       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 180      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,150      ;row no 
	push ax
	mov ax, 18      ;column no
	add ax, 30
	push ax
	mov ax, 80       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,180      ;row no 
	push ax
	mov ax, 70     ;column no
	add ax, 30
	push ax
	mov ax, 50       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle 
	
	mov ax,0      ;row no 
	push ax
	mov ax, 185     ;column no
	add ax, 30
	push ax
	mov ax, 40       ;length
	push ax
	mov ax, 40  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	mov ax,130     ;row no 
	push ax
	mov ax, 235     ;column no
	add ax, 30
	push ax
	mov ax, 100     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x04		 ;color
	push ax
	call drawRectangle 
	
	
	mov ax,120     ;row no 
	push ax
	mov ax, -25     ;column no
	add ax, 30
	push ax
	mov ax, 80     ;length
	push ax
	mov ax, 35  ;width
	push ax
	mov ax, 0x01		 ;color
	push ax
	call drawRectangle
    jmp waitKeyPress       ; Wait for a key press to return to start
	
restartProgram:
	; call clearScreen
	;call hookKeyboard
	call resetVariables
    jmp gameStart              ; Jump back to the start of the program
	
   
waitKeyPress:
    mov ah, 0x00           ; BIOS wait for key press
    int 0x16
    call clearScreen
	call hookKeyboard
	call resetVariables
    jmp gameStart           
	
	
	
	
clearScreen:
    mov ax, 0x13
    int 0x10
    ret

printScore:
	push word [skyColor]
	push word 0x0000
	push word 7
	push scoreString
	call printString

	push word [skyColor]
	push word 0x0007
	push word 3
	push scoreCountString
	call printString
	ret

drawSky:
    mov dx, 0x3C8         ; VGA palette index register
    mov al, [skyColor]    ; Set palette index to 1 (or any other index you need)
    out dx, al

    mov dx, 0x3C9         ; VGA palette data register
    mov al, 15            ; Red component (scaled 0-63)
    out dx, al
    mov al, 49            ; Green component (scaled 0-63)
    out dx, al
    mov al, 63            ; Blue component (scaled 0-63)
    out dx, al

    mov ax, 0xA000					
    mov es, ax 					
    mov di, 0 
    mov al, 1
    mov cx, 51200	

	cld
	rep stosb	

	ret

drawStrokedRectangle:
    push bp
	mov bp,sp
    push ax
	push bx
	push cx
	push dx
	
	mov ax, 0xA000
	mov es, ax
	xor bx, bx
	xor cx, cx
	
	mov ax, 320
	mul word[bp+12]
	add ax, word[bp+10]
	
	mov di, ax
		
	outerloop:
		; Checks if bx >= rows then end
		cmp bx, word[bp+8]
		je end

		; If its first row then outline
		cmp bx, 0
		je drawLine
		
		xor ax, ax
		xor cx, cx
		xor dx, dx

		mov dx, bx
		inc dx
		cmp dx, word[bp+8]
		je drawLine
		
		; Used for outlineLeft
		mov al, 0x00
		
		innerloop:
			cmp cx, word[bp+6]
			je reset

			mov dx, cx
			inc dx
			cmp dx, word[bp+6]
			je outlineRight
			
			mov byte[es:di], al
			inc di
			inc cx
			mov al,[bp+4]
			jmp innerloop
			
		reset:
			inc bx
			mov ax, 320
			mov cx, [bp+12]
			add cx, bx
			mul cx
			add ax, word[bp+10]
			
			mov di, ax
			jmp outerloop

		drawLine:
			cmp cx, word[bp+6]
			je reset
			
			mov al, 0x00
			mov byte[es:di], al
			inc di
			inc cx
			jmp drawLine

		outlineRight:
			mov al, 0x00
			mov byte[es:di], al
			inc di
			inc cx
			jmp innerloop
	
	end:	    
		pop dx
		pop cx
		pop bx
		pop ax 
		mov sp,bp
		pop bp
		ret 10

setupPaletteBG:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    mov dx, 03c8h   ; DAC write index register
    mov al, 0       ; Start at color index 0
    out dx, al
    mov dx, 03C9h   ; DAC data register
    mov cx, 768     ; 256 colors 3 (RGB)
    mov si, palette_data

    palette_loop:
        lodsb
        out dx, al
        loop palette_loop

    pop dx
    pop cx
    pop bx
    pop ax
    mov sp, bp
    pop bp

    ret

drawBackground:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    ;Plot pixels
    mov cx, 12800
    mov si, pixel_data
	add si, 19200
    mov di, 51200
    rep movsb

    pop dx
    pop cx
    pop bx
    pop ax
    mov sp, bp
    pop bp

    ret

drawRectangle:
    push bp
	mov bp,sp
    push ax
	push bx
	push cx
	push dx
	
	mov ax, 0xA000
	mov es, ax
	xor bx, bx
	xor cx, cx
	
	mov ax,320
	mul word[bp+12]
	add ax, word[bp+10]
	
	mov di, ax
		
	outerloop2:
		cmp bx, word[bp+8]
		je end2
		xor ax, ax
		xor cx, cx
		mov al,[bp+4]
		
		innerloop2:
			cmp cx, word[bp+6]
			je reset2
			 
			mov byte[es:di], al
			inc di
			inc cx
			jmp innerloop2
			
		reset2:
			inc bx
			mov ax, 320
			mov cx, [bp+12]
			add cx, bx
			mul cx
			add ax, word[bp+10]
			
			mov di, ax
			jmp outerloop2
	
	end2:	    
		pop dx
		pop cx
		pop bx
		pop ax 
		mov sp,bp
		pop bp
		ret 10

drawBirdXY:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
	
	;total width of bird is 25
	;total length of bird is 13
	
	;clear sky where bird can fly
	cmp word[jumpFlag], 0
	je drawBird
	mov word[jumpFlag], 0

	clearBelow:
		mov ax, [birdX]           ;row no 
		add ax, 11				  ;reach below bird
		push ax

		mov ax, word[birdY]  	  ;column no
		sub ax, 5
		push ax

		mov ax, [jumpSize]        ;length
		add ax, 2
		push ax

		mov ax, 25          	  ;width
		push ax

		push word [skyColor]      ;color
		call drawRectangle

	drawBird:
		;clear above
		mov ax, word [birdX]      ;row no 
		sub ax, word [gravitySpeed]
		push ax

		mov ax, word[birdY]  	  ;column no
		sub ax, 5
		push ax

		mov ax, 11  			  ;length
		push ax

		mov ax, 25          	  ;width
		push ax

		push word [skyColor]      ;color
		call drawRectangle

		;------------------------------------------------

		;draw bird
		mov bx, [birdX]      ;x coordinate
		mov cx, [birdY]      ;y coordinate
		xor dx, dx           ;temp reg

		;body
		mov ax, bx          ;row no 
		push ax

		mov ax, cx          ;column no
		push ax

		mov ax, 10          ;length
		push ax

		mov ax, 15          ;width
		push ax

		mov ax, 0x2D        ;color
		push ax

		call drawRectangle 

		;tail
		mov dx, bx
		add dx, 5
		mov ax, dx          ;row no 
		push ax

		mov dx, cx
		sub dx, 5
		mov ax, dx          ;column no
		push ax

		mov ax, 5           ;length
		push ax

		mov ax, 15          ;width
		push ax

		mov ax, 0x2D
		push ax

		call drawRectangle 

		;feather
		mov dx, bx
		add dx, 10
		mov ax, dx          ;row no 

		push ax
		mov ax, cx          ;column no
		push ax

		mov ax, 3           ;length
		push ax

		mov ax, 10          ;width
		push ax

		mov ax, 0x2D
		push ax

		call drawRectangle 

		;eye
		mov dx, bx
		add dx, 2
		mov ax, dx          ;row no
		push ax

		mov dx, cx
		add dx, 10
		mov ax, dx          ;column no
		push ax

		mov ax, 3           ;length
		push ax

		mov ax, 3           ;width
		push ax

		mov ax, 0x00
		push ax

		call drawRectangle 

		;beak
		mov dx, bx
		add dx, 7
		mov ax, dx          ;row no 
		push ax

		mov dx, cx
		add dx, 13
		mov ax, dx          ;column no
		push ax

		mov ax, 3           ;length
		push ax

		mov ax, 5           ;width
		push ax

		mov ax, 0x5C
		push ax

		call drawStrokedRectangle

		pop dx
		pop cx
		pop bx
		pop ax
		mov sp, bp
		pop bp

		ret 4

drawCloud:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov bx, [bp+8]		 ;x coordinate
	mov cx, [bp+6]       ;y coordinate
    mov dx, [bp+4]       ;colour

	mov ax, bx
	push ax
	mov ax, cx
	push ax
	mov ax, 5
	push ax
	mov ax, 5
	push ax
	mov ax, dx
	push ax
	call drawStrokedRectangle 

	mov ax, bx
	add ax, 5
	push ax
	mov ax, cx
	sub ax, 5
	push ax
	mov ax, 5
	push ax
	mov ax, 15
	push ax
	mov ax, dx
	push ax
	call drawStrokedRectangle 

	mov ax, bx
	add ax, 10
	push ax
	mov ax, cx
	sub ax, 10
	push ax
	mov ax, 5
	push ax
	mov ax, 25
	push ax
	mov ax, dx
	push ax
	call drawStrokedRectangle 

	mov ax, bx
	add ax, 15
	push ax
	mov ax, cx
	sub ax, 15
	push ax
	mov ax, 5
	push ax
	mov ax, 35
	push ax
	mov ax, dx
	push ax
	call drawStrokedRectangle 
	
	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	pop bp

	ret 6

drawPipes:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    ;Logic of building pipe (To be in valid range: l1 + l2 = 140 - height diff) --> This equation must be satisfied 
    ;Upper pipe = drawStrokedRectangle(0, starting_col, length, 30, colour)
    ;Upper pipe's boundary = drawStrokedRectangle(length, starting_col - 4, 10, 38, colour)
    ;Lower pipe's boundary = drawStrokedRectangle(length + 10 + height_diff, starting_col - 4, 10, 38, colour)
    ;Lower pipe = drawStrokedRectangle(length + 20 + height_diff, starting_col - 4, 140 - length - height_diff, 30, colour)

	;Clearing pipes
	mov ax, [currCol1]
	cmp ax, [startCol]
	je clearPipes

	mov ax, [currCol2]
	cmp ax, [startCol]
	je clearPipes

	clearPipesByPortions:
		;Redraw sky from where pipe is being shifted

		;Clearing upper pipe
		xor ax, ax           ;row no 
		push ax
		mov ax, [bp+10]      ;column no
		add ax, 30
		push ax
		mov ax, [bp+8]       ;length
		push ax
		mov ax, [gameSpeed]  ;width
		push ax
		mov ax, [skyColor]		 ;color
		push ax
		call drawRectangle 

		;Clearing upper pipe boundary
		mov ax, [bp+8]       ;row no 
		push ax
		mov ax, [bp+10]      ;column no
		add ax, 29
		add ax, [gameSpeed]
		push ax
		mov ax, 10           ;length
		push ax
		mov ax, [gameSpeed]  ;width
		push ax
		mov ax, [skyColor]		 ;color
		push ax
		call drawRectangle 

		;Clearing lower pipe boundary
		mov ax, [bp+8]       ;length
		add ax, [bp+6]		 ;height diff
		add ax, 10 
		push ax
		mov ax, [bp+10]      ;column no
		add ax, 29
		add ax, [gameSpeed]
		push ax
		mov ax, 10           ;length 
		push ax
		mov ax, [gameSpeed]  ;width
		push ax
		mov ax, [skyColor]		 ;color
		push ax
		call drawRectangle

		;Clearing lower pipe
		mov ax, [bp+8]       ;row no
		add ax, [bp+6]
		add ax, 20 
		push ax
		mov ax, [bp+10]      ;column no
		add ax, 30
		push ax
		mov ax, 140          ;length (l2 = 140 - height diff - l1)
		sub ax, [bp+8]
		sub ax, [bp+6]
		push ax
		mov ax, [gameSpeed]  ;width
		push ax
		mov ax, [skyColor]		 ;color
		push ax
		call drawRectangle

	drawPipes2:
		;Upper pipe
		xor ax, ax            ;row no 
		push ax
		mov ax, [bp+10]      ;column no
		push ax
		mov ax, [bp+8]       ;length
		push ax
		mov ax, 30           ;width
		push ax
		mov ax, [bp+4]		 ;color
		push ax
		call drawStrokedRectangle 

		;Upper pipe's boundary
		mov ax, [bp+8]       ;row no 
		push ax
		mov ax, [bp+10]      ;column no
		sub ax, 4
		push ax
		mov ax, 10           ;length
		push ax
		mov ax, 38           ;width
		push ax
		mov ax, [bp+4]
		push ax
		call drawStrokedRectangle 

		;Lower pipe's boundary
		mov ax, [bp+8]          ;row no 
		add ax, 10
		add ax, [bp+6]
		push ax
		
		mov ax, [bp+10]      ;column no
		sub ax, 4
		push ax

		mov ax, 10           ;length
		push ax

		mov ax, 38           ;width
		push ax

		mov ax, [bp+4]
		push ax
		call drawStrokedRectangle 

		;Lower pipe
		mov ax, [bp+8]          ;row no 
		add ax, 20
		add ax, [bp+6]
		push ax
		
		mov ax, [bp+10]      ;column no
		push ax

		mov ax, 140           ;length
		sub ax, [bp+8]
		sub ax, [bp+6]
		push ax

		mov ax, 30           ;width
		push ax

		mov ax, [bp+4]
		push ax
		call drawStrokedRectangle 
		jmp exitDrawPipes

	clearPipes:
		xor ax, ax       ;row no
		push ax
		push ax      	 ;column no
		mov ax, 160	     ;length
		push ax
		mov ax, 39       ;width
		push ax
		mov ax, [skyColor]		 ;color
		push ax
		call drawRectangle
		jmp clearPipesByPortions

	exitDrawPipes:
		pop dx
		pop cx
		pop bx
		pop ax
		mov sp, bp
		pop bp

    ret 8

printGameState:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx
	
	;delay at first
	call delay

	;draw bird at (birdX,birdY)
    call drawBirdXY

    ;PipePair1
	mov ax, [currCol1]
	push ax
	mov ax, [pipeLength1]
	push ax
	mov ax, [heightDiff]
	push ax
	mov ax, [pipeColor]
	push ax
	call drawPipes

	;PipePair2
	mov ax, [currCol2]
	push ax
	mov ax, [pipeLength2]
	push ax
	mov ax, [heightDiff]
	push ax
	mov ax, [pipeColor]
	push ax
	call drawPipes

	; ;XY + Colour of Cloud 1
	; mov ax, 10
	; push ax
	; mov ax, 50
	; push ax
	; mov ax, 0x77
	; push ax
	; call drawCloud

	; ;XY + Colour of Cloud 2
	; mov ax, 10
	; push ax
	; mov ax, 180
	; push ax
	; mov ax, 0x77
	; push ax
	; call drawCloud

	call printScore  

	;Clearing stack
    pop dx
    pop cx
    pop bx
    pop ax
    mov sp, bp
    pop bp

    ret 

delay:
    push bp
    mov bp, sp
    push cx

    mov cx, 0xFFFF
    
    delayLoop:
        loop delayLoop

    mov cx, 0xFFFF
    
    delayLoop2:
        loop delayLoop2

    mov cx, 0xFFFF
    
    delayLoop3:
        loop delayLoop3

    pop cx
    mov sp, bp
    pop bp

    ret

random:
	push bp
	mov bp, sp
	push bx
	push cx
	push dx

	; Load seed
	mov ax, [seed]

	; Calculate (multiplier * seed + increment) % modulus
	mov bx, [multiplier]
	mul bx
	add ax, [increment]
	adc dx, 0
	div word [modulus]

	; Store new seed
	mov [seed], dx

	; Return random number in AX
	mov ax, dx

	pop dx
	pop cx
	pop bx
	mov sp, bp
	pop bp

	ret

; hookTimerHandler:
; 	push bp
; 	mov bp, sp
; 	push ax
; 	push bx
; 	push cx
; 	push dx

; 	cli
; 	mov al, 36h
;     out 43h, al
;     mov ax, 11931         
;     out 40h, al
;     mov al, ah
;     out 40h, al
;     mov word [es:0x1C * 4],timerHandler
;     mov word [es:0x1C * 4 + 2],cs
;     sti

; 	pop dx
; 	pop cx
; 	pop bx
; 	pop ax
; 	mov sp, bp
; 	pop bp

; 	ret

hookKeyboard:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	    cli  
    xor ax, ax
    mov es, ax    

	mov ax, word [es:9*4]
	mov word [oldkbisr], ax
	mov ax, word [es:9*4+2]
	mov word [oldkbisr+2], ax

    mov word [es:9*4], kbISR
    mov word [es:9*4+2], cs
    sti  

	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	pop bp

	ret

detectCollisions1:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov ax, word [birdX]

	cmp ax, 0			  		 ;upper boundary check
	jle updateGameOverFlag

	cmp ax, 147           		 ;lower boundary check
	jge updateGameOverFlag

	touchedUpperPipe1:
		mov cx, word [pipeLength1]
		add cx, 10
		cmp word [birdX], cx					 ;(birdX <= pipeLength + 10)
		jle withinPipesRange1A

	touchedLowerPipe1:
		mov cx, word [pipeLength1]
		add cx, word [heightDiff]
		sub cx, 3
		cmp word [birdX], cx					 ;downward collision check
		jge withinPipesRange1A
		jmp endCollisionDetection1

	withinPipesRange1A:
		mov cx, word [currCol1]
		sub cx, 18
		cmp word [birdY], cx					 ;(birdY + 13 >= currCol - 5)
		jge withinPipesRange2A
		jmp endCollisionDetection1

	withinPipesRange2A:
		mov cx, word [currCol1]
		add cx, 39
		cmp word [birdY], cx					 ;(birdY <= currCol + 39)
		jle updateGameOverFlag					 ;(gameover touched upper or lower pipe pair 1)
		jmp endCollisionDetection1

	updateGameOverFlag:
		mov word [gameOverFlag], 1

	endCollisionDetection1:
		pop dx
		pop cx
		pop bx
		pop ax
		mov sp, bp
		pop bp

		ret

detectCollisions2:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov ax, word [birdX]

	cmp ax, 0			  		 ;upper boundary check
	jle updateGameOverFlag

	cmp ax, 147           		 ;lower boundary check
	jge updateGameOverFlag

	touchedUpperPipe2:
		mov cx, word [pipeLength2]
		add cx, 10
		cmp word [birdX], cx					 ;(birdX <= pipeLength + 10)
		jle withinPipesRange1B

	touchedLowerPipe2:
		mov cx, word [pipeLength2]
		add cx, word [heightDiff]
		sub cx, 3
		cmp word [birdX], cx					 ;downward collision check
		jge withinPipesRange1B
		jmp endCollisionDetection2

	withinPipesRange1B:
		mov cx, word [currCol2]
		sub cx, 18
		cmp word [birdY], cx					 ;(birdY + 13 >= currCol - 5)
		jge withinPipesRange2B
		jmp endCollisionDetection2

	withinPipesRange2B:
		mov cx, word [currCol2]
		add cx, 39
		cmp word [birdY], cx					 ;(birdY <= currCol + 39)
		jle updateGameOverFlag					 ;(gameover touched upper or lower pipe pair 1)
		jmp endCollisionDetection2

	updateGameOverFlag2:
		mov word [gameOverFlag], 1

	endCollisionDetection2:
		pop dx
		pop cx
		pop bx
		pop ax
		mov sp, bp
		pop bp

		ret

convertNumberToString:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    mov ax, [bp+4]      ; AX stores the number
    mov bx, 10          ; Divisor (decimal base)
    xor cx, cx          ; CX will count the digits

    extractDigit:
        xor dx, dx          ; Clear DX for division
        div bx              ; AX / 10, quotient in AX, remainder in DX
        add dl, '0'         ; Convert remainder (DX) to ASCII
        push dx             ; Store the ASCII digit on the stack
        inc cx              ; Increment digit count
        cmp ax, 0           ; Check if quotient is zero
        jne extractDigit    ; Repeat if not done

    mov di, scoreCountString
    mov ax, 3
    sub ax, cx
    add di, ax

    updateBuffer:
        pop dx                  ; Get the ASCII digit from the stack
        mov byte[cs:di], dl     ; Write digit in the buffer 
		inc di
        loop updateBuffer       ; Repeat for all digits

    pop dx
    pop cx
    pop bx
    pop ax
    mov sp, bp
    pop bp

    ret 2

cls:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov ax, 0xA000
	mov es, ax
	xor di, di
	mov cx, 64000
	mov al, 0x00
	
	loop1:
		mov byte[es:di], al
		inc di
		loop loop1

	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	pop bp

	ret

resetVariables:
	push bp
	mov bp, sp
	push ax
	push bx
	push cx
	push dx

	mov word [gameOverFlag]	, 0
	mov word [jumpFlag]		, 0
	mov word [birdX]	    , 65
	mov word [birdY]	    , 45
	mov word [currCol1]		, 140
	mov word [currCol2]		, 280
	mov word [pipeLength1]	, 40
	mov word [pipeLength2]	, 80
	mov word [scoreCount]	, 0

	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	pop bp

	ret

gameStart:
	call introScreen
    call bigDelay
 
 
	call printStartScreen
	call setupPaletteBG
	call drawSky 
    call drawBackground
	call hookKeyboard
    call gameLoop

updateCurrCol1:
	mov ax, word [startCol]
	mov word [currCol1], ax
	call random
	mov word [pipeLength1], ax
	inc word [scoreCount]
	push word [scoreCount]
	call convertNumberToString
	call drawBirdXY
	jmp gameLoop

updateCurrCol2:
	mov ax, word [startCol]
	mov word [currCol2], ax
	call random
	mov word [pipeLength2], ax
	inc word [scoreCount]
	push word [scoreCount]
	call convertNumberToString
	call drawBirdXY

gameLoop:
	cmp word [pauseFlag], 1
	je callPauseScreen

	skipPause:
		cmp word [jumpFlag], 1
		je skipGravity

		birdGravity:
			mov ax, word [birdX]      
			add ax, word [gravitySpeed]
			mov word [birdX], ax

		skipGravity:
			mov ax, word [currCol1]		         ; initial y coordinate of pipes	
			cmp ax, 4
			jl updateCurrCol1

			mov ax, word [currCol2]		         ; initial y coordinate of pipes	
			cmp ax, 4
			jl updateCurrCol2

			call printGameState

			call detectCollisions1
			cmp word [gameOverFlag], 1
			je gameOver
			call detectCollisions2
			cmp word [gameOverFlag], 1
			je gameOver

			mov ax, word [gameSpeed]		     ; move pillars to the left
			sub word [currCol1], ax

			mov ax, word [gameSpeed]		     ; move pillars to the left
			sub word [currCol2], ax

			jmp gameLoop
		
		callPauseScreen:			
			push word 0x2D
			push word 0xC0F0
			push word 6
			push pausedString
			call printString

			checkAgain:
				mov ah, 0x00         ; BIOS interrupt function for key input
				int 0x16             ; Call BIOS interrupt to get the pressed key
				cmp ah, [rScancode]  ; Compare it with the desired character
				jne checkAgain       ; If not equal, repeat input

			mov word [pauseFlag], 0
			jmp gameLoop

gameOver:
	mov ax, 13h
    int 10h
    mov ax, 0xA000     
    mov es, ax
	
	push word 0x2D
	push word 0xC0F0
	push word 10
	push gameOverString
	call printString
	call delay
	call delay
	call delay
	call delay
	call delay
	call printEndScreen

	mov ah, 0
    int 16h

exit:
    mov ax, 4c00h
    int 20h