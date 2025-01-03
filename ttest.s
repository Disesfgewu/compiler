	.text
factimp:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	movq %rax, %r9
	movq %rax, -16(%rbp)
	pushq %r9
	movq -24(%rbp), %r10
	movq 16(%rbp), %rax
	movq %rax, %r12
	movq %rax, %r13
	movq %r13, %rdi
	imulq $8, %rdi
	addq $8, %rdi
	call malloc
	movq %rax, %r14
	xorq %r15, %r15
range_loop:
	cmpq %r15, %r13
	je range_end
	movq %r15, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq %r15, 0(%r14,%rdx,1)
	addq $1, %r15
	jmp range_loop
range_end:
	movq %rax, %r12
	xorq %r14, %r14
	popq %r9
LC0:
	cmpq %r14, %r13
	je .LC1
	movq %r14, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq 0(%r12,%rdx,1), %rax
	movq %rax, -24(%rbp)
	movq $1, %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	movq %r9, %rax
	imulq %rbx, %rax
	movq %rax, %r9
	movq %rax, -16(%rbp)
	addq $1, %r14
	jmp LC0
.LC1:
	movq %r13, %rdx
	subq $1, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq 0(%r12,%rdx,1), %rax
	movq %rax, -24(%rbp)
	movq -16(%rbp), %rax
	leave
	ret
	movq $0, %rax
	leave
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $10, %rax
	pushq %rax
	call factimp
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
