/* register jobs:
 * %rcx - instruction pointer
 * %rbx - ip stack pointer
 * %rdx - can be destroyed by routines
 * %rsp - data stack pointer
 * %rbp - saves stack pointer for λs
 */
        .text
        .global _start
_start:
        mov $retstack-8, %rbx
        mov $main-8, %rcx
        jmp til_inside
        
        .data
retstack:
        .zero 65536
