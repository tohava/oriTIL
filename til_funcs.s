        .text
.global popret
.global til_add
.global til_enter
.global til_mult
.global til_push
.global til_push_fake_frame
.global til_push_frame
.global til_inside        
.global var_0_0
.global var_0_1
.global var_0_2

til_enter:
        add $4, %rbx
        mov %ecx, (%rbx)
        add $4, %rbx
        mov %ebp, (%rbx)
        mov (%ecx), %ecx
        add $(5 - 8), %ecx /* size of jmp til_enter - 8 inside offset*/
        jmp til_inside
        
til_return:
        sub $4, %rbx
        mov 4(%rbx), %ebp
        sub $4, %rbx
        mov 4(%rbx), %ecx
        jmp til_inside

til_inside:
        add $8, %rcx            
        jmp *(%ecx)

til_exit:   
        mov $1, %rcx /* exit */
        mov $0, %rbx
        int $0x80                
        
msg:    .ascii "Hello, world\n"
msgend:

til_push:
        add $8, %ecx
        push (%ecx)
        jmp til_inside

til_push_frame:
        push %rbp
        jmp til_inside

til_push_fake_frame:
        push $0xdeadbee
        jmp til_inside
        
til_hello:
        push %rax
        push %rbx
        push %rcx
        push %rdx
        mov $4, %rcx /* write */
        mov $1, %rbx
        mov $msg, %rcx
        mov $msgend-msg, %rdx
        int $0x80
        pop %rdx
        pop %rcx
        pop %rbx
        pop %rax
        jmp til_inside

til_mult:
        mov 8(%rsp), %rax
        mov 16(%rsp), %rdx
        imul %rdx, %rax
        add $16, %rsp
        push %rax
        jmp til_inside

til_add:
        mov 8(%rsp), %rax
        add 16(%rsp), %rax
        add $16, %rsp
        push %rax
        jmp til_inside

popret:
        add $8,%rcx
        pop %rax
        add (%ecx), %rsp
        jmp til_return

var_0_0:
        push 8(%rsp)
        jmp til_inside
var_0_1:
        push 16(%rsp)
        jmp til_inside
var_0_2:
        push 24(%rsp)
        jmp til_inside
