[org 0x0100]

jmp start

scoreCountString: dw 0,0,0

convertNumberToString:
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    mov ax, [bp+4]      ; AX stores the number
    xor cx, cx          ; CX will count the digits

    extractDigit:
        xor dx, dx          ; Clear DX for division
        mov bx, 10          ; Divisor (decimal base)
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
        mov word[cs:di], dx     ; Write digit in the buffer 
        inc di
        loop updateBuffer       ; Repeat for all digits

    pop dx
    pop cx
    pop bx
    pop ax
    mov sp, bp
    pop bp

    ret 2

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


start:
    mov ax, 0xA000
    mov es, ax

    push word 1
    call convertNumberToString

    push word 0xF
    push word 0x0
    push word 3
    push scoreCountString
    call printString



exit:
    mov ax, 0x4c00
    int 0x21