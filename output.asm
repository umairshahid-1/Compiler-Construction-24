section .data
section .bss
section .text
global _start
_start:
    push ebp
    mov ebp, esp
    sub esp, 52
; Warning: Variable n is uninitialized.
    mov dword [ebp-24], 0
; Warning: Variable m is uninitialized.
    mov dword [ebp-20], 0
; Warning: Variable b is uninitialized.
    mov dword [ebp-8], 0
; Warning: Variable a is uninitialized.
    mov dword [ebp-4], 0
; t1 = a + b
    mov eax, [ebp-4]
    add eax, [ebp-8]
    mov t1, eax
; sum = t1
    mov eax, t1
    mov [ebp-12], eax
; return 
; greetMsg = Function!
    mov eax, Function!
    mov [ebp-16], eax
; t2 = m * n
    mov eax, [ebp-20]
    imul [ebp-24]
    mov t2, eax
; result = t2
    mov eax, t2
    mov [ebp-28], eax
; return 
; x = 10
    mov eax, 10
    mov [ebp-32], eax
; y = 3.14
    mov eax, 3.14
    mov [ebp-36], eax
; bc = false
    mov eax, false
    mov [ebp-40], eax
; c = Z
    mov eax, Z
    mov [ebp-44], eax
; msg = Hello!
    mov eax, Hello!
    mov [ebp-48], eax
; t3 = x > 5
    mov eax, [ebp-32]
    cmp eax, 5
    setg al
    movzx eax, al
    mov t3, eax
; t4 = x - 5
    mov eax, [ebp-32]
    sub eax, 5
    mov t4, eax
; x = t4
    mov eax, t4
    mov [ebp-32], eax
; t5 = x + 5
    mov eax, [ebp-32]
    add eax, 5
    mov t5, eax
; x = t5
    mov eax, t5
    mov [ebp-32], eax
; L1:
L1:
; t6 = x < 20
    mov eax, [ebp-32]
    cmp eax, 20
    setl al
    movzx eax, al
    mov t6, eax
; ifFalse L2 goto 
    mov eax, L2
    cmp eax, 0
    je 
; x = 30
    mov eax, 30
    mov [ebp-32], eax
; goto L1
    jmp L1
; L2:
L2:
; i = 0
    mov eax, 0
    mov [ebp-52], eax
; L3:
L3:
; t7 = i < 3
    mov eax, [ebp-52]
    cmp eax, 3
    setl al
    movzx eax, al
    mov t7, eax
; ifFalse L4 goto 
    mov eax, L4
    cmp eax, 0
    je 
; t9 = t8 + 1
    mov eax, t8
    add eax, 1
    mov t9, eax
; y = 7.5
    mov eax, 7.5
    mov [ebp-36], eax
; goto L3
    jmp L3
; L4:
L4:
; t10 = call (add)
    push add
    call 
    add esp, 4
    mov t10, eax
; x = t10
    mov eax, t10
    mov [ebp-32], eax
; t11 = call (multiply)
    push multiply
    call 
    add esp, 4
    mov t11, eax
; y = t11
    mov eax, t11
    mov [ebp-36], eax
; bc = true
    mov eax, true
    mov [ebp-40], eax
; return 
    mov esp, ebp
    pop ebp
    mov eax, 1
    xor ebx, ebx
    int 0x80
