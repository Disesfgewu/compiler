	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $42, %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq -16(%rbp), %r10
	movq $32, %rdi
	call malloc@PLT
	movq %rax, %r12
	movq $3, 0(%r12)
	movq $1, %rax
	movq %rax, 8(%r12)
	movq $2, %rax
	movq %rax, 16(%r12)
	movq $3, %rax
	movq %rax, 24(%r12)
	movq %r12, %rax
	movq %rax, %r12
	pushq %r12
	movq 0(%r12), %r13
	imulq $8, %r13
	addq $8, %r13
	movq %r13, %rdi
	call malloc@PLT
	movq %rax, %r14
	movq 0(%r12), %rax
	movq %rax, 0(%r14)
	xorq %r15, %r15
copy_list_loop:
	cmpq 0(%r12), %r15
	je copy_list_end
	movq %r15, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq 0(%r12,%rdx,1), %rax
	movq %rax, 0(%r14,%rdx,1)
	addq $1, %r15
	jmp copy_list_loop
copy_list_end:
	popq %r12
	xorq %r13, %r13
.LC0:
	cmpq 0(%r14), %r13
	je .LC1
	movq %r13, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq 0(%r14,%rdx,1), %rax
	movq %rax, -16(%rbp)
	movq %rax, %r9
	movq -16(%rbp), %rax
	movq %r9, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	addq $1, %r13
	jmp .LC0
.LC1:
	movq %r13, %rdx
	subq $1, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq 0(%r12,%rdx,1), %rax
	movq %rax, -16(%rbp)
	movq -16(%rbp), %rax
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
