[org 0x0100]
jmp start

%INCLUDE "E:\Programming\Assembly x8088\FlapFlap\Code\MusicData\12th.asm"

playMusic:
    mov si, imfData
    mov cx, [imfCount]

    nextCommand:
        mov dx, 0x388
        mov al, [si + 0]
        out dx, al

        inc dx
        mov al, [si + 1]
        out dx, al

        mov al, [si + 2]
        mov ah, [si + 3]

        push cx

        repeatDelay:

        mov cx, 1

        delayLooop:
            nop
            loop delayLooop

        dec ax
        jnz repeatDelay

        pop cx
        add si, 4

        loop nextCommand

    jmp playMusic


start:
   call playMusic

exit:
    mov ax, 0x4c00
    int 0x21