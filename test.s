	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LC0, %rdi
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
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	je .LC1
.LC1:
	movq $1, %rax
	cmpq $0, %rax
	je .LC3
	movq $.LC2, %rdi
	movq $0, %rax
	call printf
	jmp .LC4
.LC3:
.LC4:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jl .LC5
.LC5:
	movq $1, %rax
	cmpq $0, %rax
	je .LC7
	movq $.LC6, %rdi
	movq $0, %rax
	call printf
	jmp .LC8
.LC7:
.LC8:
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	je .LC9
.LC9:
	movq $1, %rax
	cmpq $0, %rax
	je .LC12
	movq $.LC10, %rdi
	movq $0, %rax
	call printf
	jmp .LC13
.LC12:
	movq $.LC11, %rdi
	movq $0, %rax
	call printf
.LC13:
	leave
	ret
	.data
.LC0:
	.string "hello world!\n"
.LCd:
	.string "%d\n"
.LC642448509:
	.string "testing\n"
.LC2:
	.string "yes\n"
.LC6:
	.string "yes\n"
.LC10:
	.string "no\n"
.LC11:
	.string "yes\n"
