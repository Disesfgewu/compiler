	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
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
	movq %rax, -16(%rbp)
	movq -16(%rbp), %r12
	movq $.LCstart, %rdi
	call printf
	call print_list
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
	movq %rax, -24(%rbp)
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
	movq $.LCend, %rdi
	call printf
	movq $10, %rdi
	call putchar
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
	movq $.LCend, %rdi
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
