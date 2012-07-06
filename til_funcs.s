        .text
.global popret
.global til_add
.global til_enter
.global til_mult
.global til_push
.global til_push_fake_frame
.global til_push_frame
.global til_inside        
.global til_exit
.global til_hello
.global til_display_int
.global var_0_0
.global var_0_1
.global var_0_2

        
til_enter:
        add $4, %rbx
        mov %ecx, (%rbx)
        add $4, %rbx
        mov %ebp, (%rbx)
        xor %rbp, %rbp
        or  %rsp, %rbp
        mov (%ecx), %ecx
        add $(5 - 8), %ecx /* size of jmp til_enter - 8 inside offset*/
        jmp til_inside
        
til_return:
        sub $4, %rbx
        mov 4(%rbx), %rbp
        mov $0x7FFF00000000, %rdx
        or  %rdx, %rbp
        sub $4, %rbx
        mov 4(%rbx), %ecx
        jmp til_inside

til_inside:
        add $8, %rcx            
        jmp *(%ecx)

til_exit:   
        mov $1, %rax /* exit */
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
        mov $4, %rax /* write */
        mov $1, %rbx
        mov $msg, %rcx
        mov $msgend-msg, %rdx
        int $0x80
        pop %rdx
        pop %rcx
        pop %rbx
        pop %rax
        jmp til_inside        
        
til_display_int:
        push %rbx
        push %rcx
        mov $1, %rbx
        mov $display_buf+30, %rbp /* we don't care about rbp here */
        movb $'\n, 1(%rbp)
        mov 24(%rsp), %rax
        or  %rax, %rax
        jnz til_display_int_nz
        jmp til_display_int_display_1
til_display_int_nz:
        mov $0x8000000000000000, %rbx
        and %rax, %rbx
        inc %rbx
        cqo
        xor %rdx, %rax
        sub %rdx, %rax
til_display_int_loop:
        xor %rdx, %rdx
        mov $10, %rcx
        div %rcx
        or  $0x30, %dl
        movb %dl, (%rbp)
        dec %rbp
        inc %rbx
        or  %rax, %rax
        jnz til_display_int_loop
til_display_int_post_loop:
        or %rbx, %rbx
        jns til_display_int_display
        movb $'-, (%rbp)
        inc %rbx
        dec %rbp
til_display_int_display:
        inc %rbp
        mov %ebx, %ebx /* zero extend ebx */
til_display_int_display_1:
        mov $4, %rax
        mov %rbp, %rcx
        mov %rbx, %rdx
        mov $1, %rbx
        int $0x80
til_display_int_end:        
        pop %rcx
        pop %rbx
        jmp til_inside
        
til_mult:
        mov 8(%rsp), %rax
        mov 16(%rsp), %rdx
        imul %rdx, %rax
        add $24, %rsp
        push %rax
        jmp til_inside

til_add:
        mov 8(%rsp), %rax
        add 16(%rsp), %rax
        add $24, %rsp
        push %rax
        jmp til_inside

popret:
        add $8,%rcx
        pop %rax
        add (%ecx), %rsp
        push %rax
        jmp til_return

var_0_0:
        push 8(%rbp)
        jmp til_inside
var_0_1:
        push 16(%rbp)
        jmp til_inside
var_0_2:
        push 24(%rbp)
        jmp til_inside

        
        .data
display_buf:
        .zero 32
        