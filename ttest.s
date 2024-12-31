        .text
        .globl  main
main:
        pushq   %rbp
        movq    %rsp, %rbp
        andq    $-16, %rsp

        movq    $8, %rdi       
        call    malloc
        movq    %rax, %r12      
        movq    $1, 0(%r12)     
        movq    %r12, %rax      
        pushq   %rax            
        movq    $8, %rdi
        call    malloc
        
        popq    %rbx            
        movq    0(%rbx), %rax   

        movq    $.fmt, %rdi
        movq    %rax, %rsi
        xorq    %rax, %rax
        call    printf

        movq    $0, %rax
        leave
        ret

        .data
fmt:    .string "VALUE = %ld\n"
