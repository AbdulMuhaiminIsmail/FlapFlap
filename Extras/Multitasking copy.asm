[org 0x0100]

jmp start

%INCLUDE "E:\Programming\Assembly x8088\FlapFlap\Code\Main.asm"
%INCLUDE "E:\Programming\Assembly x8088\FlapFlap\Code\MusicData\MusicData.asm"

; Task states (store all 14 registers)
; AX, BX, CX, DX, SI, DI, BP, SP, SS, DS, ES, IP, CS, FLAGS
; 0,  2,  4,  6,  8,  10, 12, 14, 16, 18, 20, 22, 24, 26

taskstates:   times 2*28 dw 0 ; Space for 2 tasks (28 words each)

current:      db   0           ; Index of the current task

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

    jmp taskone

; Timer interrupt service routine
timer:
    cli                          ; Disable interrupts

    ; Push all 14 registers to stack
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push bp
    push sp                      ; Save stack pointer
    push ss                      ; Save stack segment
    push ds                      ; Save data segment
    push es                      ; Save extra segment

    mov  bl, [cs:current]        ; Get the current task index
    mov  ax, 28                  ; Each task state is 28 words
    mul  bl                      ; Calculate offset for current task
    mov  bx, ax                  ; BX = offset for current task

    ; Save all registers for the current task
    ; AX, BX, CX, DX, SI, DI, BP, SP, SS, DS, ES, IP, CS, FLAGS
    ; 0,  2,  4,  6,  8,  10, 12, 14, 16, 18, 20, 22, 24,  26

    pop  word [cs:taskstates+bx+20] ; ES
    pop  word [cs:taskstates+bx+18] ; DS
    pop  word [cs:taskstates+bx+16] ; SS
    pop  word [cs:taskstates+bx+14] ; SP
    pop  word [cs:taskstates+bx+12] ; BP
    pop  word [cs:taskstates+bx+10] ; DI
    pop  word [cs:taskstates+bx+8]  ; SI
    pop  word [cs:taskstates+bx+6]  ; DX
    pop  word [cs:taskstates+bx+4]  ; CX
    pop  word [cs:taskstates+bx+2]  ; BX
    pop  word [cs:taskstates+bx+0]  ; AX
    
    pop  word [cs:taskstates+bx+22] ; IP
    pop  word [cs:taskstates+bx+24] ; CS
    pop  word [cs:taskstates+bx+26] ; FLAGS

;     ; Switch to the next task
;     inc  byte [cs:current]       ; Increment task index
;     cmp  byte [cs:current], 2    ; Check if out of range
;     jne  skipreset
;     mov  byte [cs:current], 0    ; Reset to Task 0 if out of range

; skipreset:
;     mov  bl, [cs:current]        ; Get new task index
;     mov  ax, 28                  ; Each task state is 28 words
;     mul  bl                      ; Calculate offset for new task
;     mov  bx, ax                  ; BX = offset for new task

    mov al, 0x20
    out 0x20, al                 ; Send EOI to PIC

    ; Restore FLAGS, CS, IP for the next task
    push word [cs:taskstates+bx+26] ; FLAGS
    push word [cs:taskstates+bx+24] ; CS
    push word [cs:taskstates+bx+22] ; IP

    ; Restore all registers for the new task
    ; AX, BX, CX, DX, SI, DI, BP, SP, SS, DS, ES, IP, CS, FLAGS
    ; 0,  2,  4,  6,  8,  10, 12, 14, 16, 18, 20, 22, 24,  26

    mov  ax, [cs:taskstates+bx+0]  ; AX
    mov  cx, [cs:taskstates+bx+4]  ; CX
    mov  dx, [cs:taskstates+bx+6]  ; DX
    mov  si, [cs:taskstates+bx+8]  ; SI
    mov  di, [cs:taskstates+bx+10] ; DI
    mov  bp, [cs:taskstates+bx+12] ; BP
    mov  sp, [cs:taskstates+bx+14] ; SP
    mov  ss, [cs:taskstates+bx+16] ; SS
    mov  ds, [cs:taskstates+bx+18] ; DS
    mov  es, [cs:taskstates+bx+20] ; ES
    mov  bx, [cs:taskstates+bx+2]  ; BX
    
    sti                          ; Enable interrupts
    iret                         ; Return to new task

start:
    ; Initialize task states
    mov word [taskstates+22], taskone ; Task 1 IP
    mov word [taskstates+24], cs      ; Task 1 CS
    mov word [taskstates+26], 0x0200  ; Task 1 FLAGS
    ; mov word [taskstates+28+22], tasktwo ; Task 2 IP
    ; mov word [taskstates+28+24], cs      ; Task 2 CS
    ; mov word [taskstates+28+26], 0x0200  ; Task 2 FLAGS
    mov byte [current], 0                ; Start with Task 0

    ; Hook timer interrupt
    xor ax, ax
    mov es, ax
    cli
    mov word [es:8*4], timer             ; Set timer ISR
    mov word [es:8*4+2], cs
    sti
    
    ; Infinite loop
    taskone:
        call gameStart
        jmp taskone                                
