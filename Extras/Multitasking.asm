; Elementary multitasking of two threads
[org 0x0100]

jmp start

%INCLUDE "E:\Programming\Assembly x8088\FlapFlap\Code\Main.asm"
%INCLUDE "E:\Programming\Assembly x8088\FlapFlap\Code\MusicData\Music.asm"

; AX, BX, IP, CS, FLAGS storage area
taskstates:   dw   0, 0, 0, 0, 0      ; Task 0 registers
              dw   0, 0, 0, 0, 0      ; Task 1 registers
              dw   0, 0, 0, 0, 0      ; Task 2 registers

current:      db   0                  ; Index of the current task
chars:        db   '\|/-'             ; Shapes to form a bar

; ; First task to be multitasked
; taskone:
;         call gameStart
;         jmp taskone     

; Second task to be multitasked
tasktwo:
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

    jmp tasktwo

; Timer interrupt service routine
timer:
              push ax
              push bx

              mov  bl, [cs:current]   ; Read the index of the current task
              mov  ax, 10             ; Space used by one task
              mul  bl                 ; Multiply to get the start of the task
              mov  bx, ax             ; Load the start of the task in BX
              
              ; Save registers for the current task
              pop ax
              mov  [cs:taskstates+bx+2], ax ; Save BX
              pop ax
              mov  [cs:taskstates+bx+0], ax ; Save AX
              pop ax
              mov  [cs:taskstates+bx+4], ax ; Save IP
              pop ax 
              mov  [cs:taskstates+bx+6], ax ; Save CS
              pop ax 
              mov  [cs:taskstates+bx+8], ax ; Save FLAGS

              ; Switch to the next task
              inc  byte [cs:current]
              cmp  byte [cs:current], 2 ; Check if task index is out of range
              jne  skipreset
              mov  byte [cs:current], 0 ; Reset to task 0 if out of range

skipreset:
              mov  bl, [cs:current]   ; Read the new task index
              mov  ax, 10             ; Space used by one task
              mul  bl                 ; Multiply to get the start of the task
              mov  bx, ax             ; Load the start of the task in BX

              mov  al, 0x20
              out  0x20, al           ; Send EOI to PIC

              ; Restore registers for the new task
              push word [cs:taskstates+bx+8] ; FLAGS
              push word [cs:taskstates+bx+6] ; CS
              push word [cs:taskstates+bx+4] ; IP
              mov  ax, [cs:taskstates+bx+0]  ; AX
              mov  bx, [cs:taskstates+bx+2]  ; BX
              iret                    ; Return to the new task

start:
              ; Initialize task states
              mov  word [taskstates+4], taskone ; Task 1 IP
              mov  [taskstates+6], cs          ; Task 1 CS
              mov  word [taskstates+8], 0x0200 ; Task 1 FLAGS
              mov  word [taskstates+10+4], tasktwo ; Task 2 IP
              mov  [taskstates+10+6], cs          ; Task 2 CS
              mov  word [taskstates+10+8], 0x0200 ; Task 2 FLAGS
              mov  word [current], 0              ; Set current task index

              ; Hook timer interrupt
              xor  ax, ax
              mov  es, ax
              cli
              mov  word [es:8*4], timer
              mov  [es:8*4+2], cs
              mov  ax, 0xb800
              mov  es, ax                    ; Point ES to video memory
              xor  bx, bx                    ; Initialize BX
              sti

              taskone:
                call gameStart
                jmp taskone  
