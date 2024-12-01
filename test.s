	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LC0, %rdi
	movq $0, %rax
	call printf
	movq $2, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $8, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	addq %rbx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $8, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	subq %rbx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $8, %rax
	popq %rbx
	cqto
	idivq %rbx
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $8, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	imulq %rbx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $8, %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rdx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $2, %rax
	movq %rax, -16(%rbp)
	movq $8, %rax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	subq %rbx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq -16(%rbp), %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	cqto
	idivq %rbx
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	imulq %rbx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq -16(%rbp), %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rdx, %rax
	movq $.LCd, %rdi
	movq %rax, %rsi
	movq $0, %rax
	call printf
	movq $.LC642448509, %rdi
	movq %rax, -32(%rbp)
	movq -32(%rbp), %rax
	movq $.LCd, %rdx
	movq %rax, %rsi
	movq $0, %rax
	call printf
	leave
	ret
	.data
.LC0:
	.string "hello world!\n"
.LCd:
	.string "%d\n"
.LC642448509:
	.string "testing\n"
