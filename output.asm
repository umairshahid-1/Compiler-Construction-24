section .data
; Data declarations go here (if any)
section .bss
section .text
global _start
_start:
    push ebp
    mov ebp, esp
    sub esp, 12
; counter = 11
    mov eax, 11
    mov [ebp-12], eax
; L1:
; t1 = counter <= 50
; ifFalse L2 goto 
; counter = 100
    mov eax, 100
    mov [ebp-12], eax
; goto L1
; L2:
; result = false
    mov eax, false
    mov [ebp-8], eax
; i = 7
    mov eax, 7
    mov [ebp-4], eax
; L3:
; t2 = i > 5
; ifFalse L4 goto 
; t4 = t3 + 1
; result = true
    mov eax, true
    mov [ebp-8], eax
; goto L3
; L4:
    mov esp, ebp
    pop ebp
    mov eax, 1
    xor ebx, ebx
    int 0x80
