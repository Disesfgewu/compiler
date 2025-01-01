	.text
str:
	pushq %rbp
	movq %rsp, %rbp
	movq $88, %rdi
	call malloc@PLT
	movq %rax, %r12
	movq $10, 0(%r12)
	movq $.LC0, %rax
	movq %rax, 8(%r12)
	movq $.LC1, %rax
	movq %rax, 16(%r12)
	movq $.LC2, %rax
	movq %rax, 24(%r12)
	movq $.LC3, %rax
	movq %rax, 32(%r12)
	movq $.LC4, %rax
	movq %rax, 40(%r12)
	movq $.LC5, %rax
	movq %rax, 48(%r12)
	movq $.LC6, %rax
	movq %rax, 56(%r12)
	movq $.LC7, %rax
	movq %rax, 64(%r12)
	movq $.LC8, %rax
	movq %rax, 72(%r12)
	movq $.LC9, %rax
	movq %rax, 80(%r12)
	movq %r12, %rax
	movq %rax, %r15
	movq $10, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jl .LC10
	movq $0, %rax
	jmp .LC11
.LC10:
	movq $1, %rax
.LC11:
	cmpq $0, %rax
	je .LC12
	movq 16(%rbp), %rax
	movq %rax, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq %r15, %r12
	movq %rax, %rsi
	leaq 0(%r12,%rdx,1), %rsi
	addq %rdx, %rax
	movq 0(%rsi), %rax
	leave
	ret
	jmp .LC13
.LC12:
	movq $10, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rdx, %rax
	movq %rax, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq %r15, %r12
	movq %rax, %rsi
	leaq 0(%r12,%rdx,1), %rsi
	addq %rdx, %rax
	movq 0(%rsi), %rax
	pushq %rax
	movq $10, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	cqto
	idivq %rbx
	pushq %rax
	call str
	addq $8, %rsp
	popq %rbx
	addq %rbx, %rax
	leave
	ret
.LC13:
	movq $0, %rax
	leave
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $0, %rax
	pushq %rax
	call str
	addq $8, %rsp
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $42, %rax
	pushq %rax
	call str
	addq $8, %rsp
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $1024, %rax
	pushq %rax
	call str
	addq $8, %rsp
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $0, %rax
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
	leaq 0(%r12,%rdx,1), %rsi
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
	ret
	.data
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
