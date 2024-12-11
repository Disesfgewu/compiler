	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $32, %rdi
	call malloc
	movq %rax, %r12
	movq $3, 0(%r12)
	movq $2, %rax
	movq %rax, 8(%r12)
	movq $3, %rax
	movq %rax, 16(%r12)
	movq $4, %rax
	movq %rax, 24(%r12)
	movq %r12, %rax
	movq %rax, -16(%rbp)
	movq $48, %rdi
	call malloc
	movq %rax, %r12
	movq $5, 0(%r12)
	movq $1, %rax
	movq %rax, 8(%r12)
	movq $2, %rax
	movq %rax, 16(%r12)
	movq $1, %rax
	movq %rax, 24(%r12)
	movq $2, %rax
	movq %rax, 32(%r12)
	movq $4, %rax
	movq %rax, 40(%r12)
	movq %r12, %rax
	movq %rax, -24(%rbp)
	movq -16(%rbp), %r12
	movq $.LCstart, %rdi
	call printf
	call print_list
	movq -24(%rbp), %r12
	movq $.LCstart, %rdi
	call printf
	call print_list
	movq $.LCstart, %rdi
	call printf
	movq $2, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $3, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $4, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $5, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $6, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $7, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCend, %rdi
	call printf
	movq $10, %rdi
	call putchar
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	jl .LC0
	movq $0, %rax
	jmp .LC1
.LC0:
	movq $1, %rax
.LC1:
	cmpq $1, %rax
	jne .LC2
	movq $.LCtrue, %rdi
	jmp .LC3
.LC2:
	movq $.LCfalse, %rdi
.LC3:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $.LC417747598, %rdi
	pushq %rax
	movq $.LC629576243, %rdi
	popq %rbx
	movq $.LC629576243, %rdi
	call strlen
	movq %rax, %r10
	movq $.LC417747598, %rdi
	call strlen
	addq %r10, %rax
	addq $1, %rax
	movq %rax, %rdx
	call malloc
	movq %rax, %r12
	movq $.LC629576243, %rdi
	movq %rdi, %rsi
	movq %r12, %rdi
	call strcpy
	movq $.LC417747598, %rdi
	movq %rdi, %rsi
	movq %r12, %rdi
	call strcat
	movq %rax, %rsi
	movq $.LCs, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $1, %rax
	cmpq $1, %rax
	jne .LC4
	movq $.LCtrue, %rdi
	jmp .LC5
.LC4:
	movq $.LCfalse, %rdi
.LC5:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
	cmpq $1, %rax
	jne .LC6
	movq $.LCtrue, %rdi
	jmp .LC7
.LC6:
	movq $.LCfalse, %rdi
.LC7:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	andq %rbx, %rax
	cmpq $1, %rax
	jne .LC8
	movq $.LCtrue, %rdi
	jmp .LC9
.LC8:
	movq $.LCfalse, %rdi
.LC9:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	orq %rbx, %rax
	cmpq $1, %rax
	jne .LC10
	movq $.LCtrue, %rdi
	jmp .LC11
.LC10:
	movq $.LCfalse, %rdi
.LC11:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
	leave
	ret
	.globl	len
len:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rsi
	call strlen
	leave
	ret
print_list:
	pushq %r12
	pushq %r14
	pushq %r15
	movq 0(%r12), %r15
	xorq %r14, %r14
print_list_loop:
	cmpq %r14, %r15
	je print_list_end
	movq %r14, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	leaq 0(%r12), %rsi
	addq %rdx, %rsi
	movq 0(%rsi), %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	addq $1, %r14
	cmpq %r14, %r15
	je print_list_end
	movq $.LCcomma, %rdi
	call printf
	jmp print_list_loop
print_list_end:
	movq $.LCend, %rdi
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
	popq %r15
	popq %r14
	popq %r12
	leave
	ret
	.data
.LC629576243:
	.string "13"
.LC417747598:
	.string "asd"
.LCtrue:
	.string "True"
.LCfalse:
	.string "False"
.LCs:
	.string "%s"
.LCd:
	.string "%d"
.LCcomma:
	.string ", "
.LCstart:
	.string "["
.LCend:
	.string "]"
