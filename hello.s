;;; register jobs:
;;; %rax - instruction pointer
;;; %rbx - ip stack pointer
;;; %rcx - return value
;;; %rsp - data stack pointer
;;; %rbp - saves stack pointer for λs
        .text
        .global _start
_start:
        mov $retstack-8, %rbx
        mov $data-8, %rax

inside:
        add $8, %rax            
        push (%rax)             ; function address now also includes a base pointer
        jmp *(%eax)

enter:
        add $4, %rbx
        mov %eax, (%rbx)
        add $4, %rbx
        mov %ebp, (%rbx)
        jmp inside
        
return:
        sub $4, %rbx
        mov 4(%rbx), %ebp
        sub $4, %rbx
        mov 4(%rbx), %eax
        jmp inside
hello:
        push %rax
        push %rbx
        push %rcx
        push %rdx
        mov $4, %rax /* write */
        mov $1, %rbx
        mov $msg, %rcx
        mov $msgend-msg, %rdx
        int $0x80
        pop %rdx
        pop %rcx
        pop %rbx
        pop %rax
        jmp inside
msg:    .ascii "Hello, world\n"
msgend:

exit:   
        mov $1, %rax /* exit */
        mov $0, %rbx
        int $0x80        
        
        .data
data:
        .int hello
        .zero 4
        .int hello
        .zero 4
        .int exit
        .zero 4
retstack:
        .zero 65536
        