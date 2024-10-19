; This prints string in text mode
[org 0x0100]

jmp start

string:     db 'String: '
len:        dw 8

;subroutine to clear the screen
cls:     
    ;backup previous register values
    push es
    push ax
    push di

    ;setup video mode
    mov ax, 0xb800
    mov es, ax
    mov di, 0

    ;clears one pixel at a time
    clrpixel:
        mov word[es:di], 0x0720
        add di, 2
        cmp di, 4000
        jne clrpixel

    ;set previous values of registers
    pop di
    pop ax
    pop es
    ret

;subroutine which takes a string and its length to print it
printString:
    ;backup previous register values
    push bp
    mov bp, sp
    push ax
    push cx
    push es
    push di
    push si

    ;setup video mode
    mov ax, 0xb800
    mov es, ax
    mov di, 0

    ;set len and str in cx and si respectively
    mov bx, [bp+4]
    mov cx, [bx]
    mov si, [bp+6]
    mov ah, 0x0F ;set color of text to be bright white

    ;print char by char
    printNextChar:
        mov al, [si] ;set char value in code byte
        mov word [es:di], ax
        add di, 2
        inc si
        loop printNextChar

    ;pop all reg values
    pop si
    pop di
    pop es
    pop cx
    pop ax
    pop bp

    ;return control
    ret 4

start:
    call cls

    mov ax, string
    push ax
    mov ax, len
    push ax

    call printString

exit:
    mov ax, 0x4c00
    int 0x21