	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LC774705101, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $8, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	addq %rbx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $8, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	subq %rbx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $8, %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $8, %rax
	pushq %rax
	movq $2, %rax
	popq %rbx
	imulq %rbx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $8, %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rdx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
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
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	subq %rbx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq -16(%rbp), %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	imulq %rbx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq -16(%rbp), %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	cqto
	idivq %rbx
	movq %rdx, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $.LC642448509, %rdi
	movq %rax, -32(%rbp)
	movq -32(%rbp), %rdx
	movq %rax, %rsi
	movq $.LCd, %rdx
	movq $0, %rax
	call printf
	movq $1, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jne .LC0
	movq $1, %rax
.LC0:
	cmpq $0, %rax
	je .LC1
	movq $.LC197962146, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC2
.LC1:
	movq $.LC861657444, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC2:
	movq $2, %rax
	pushq %rax
	movq $8, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jg .LC3
	movq $1, %rax
.LC3:
	cmpq $0, %rax
	je .LC4
	movq $.LC290786532, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC5
.LC4:
	movq $.LC443447357, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC5:
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jne .LC6
	movq $1, %rax
.LC6:
	cmpq $0, %rax
	je .LC7
	movq $.LC572471771, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC8
.LC7:
	movq $.LC794097650, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC8:
	leave
	ret
	.data
.LC774705101:
	.string "hello world!\n"
.LCs:
	.string "%s\n"
.LCd:
	.string "%d\n"
.LC642448509:
	.string "testing\n"
.LC197962146:
	.string "test for 1 == 1 correct\n"
.LC861657444:
	.string "test for 1 == 1 fail\n"
.LC290786532:
	.string "test for x < y correct\n"
.LC443447357:
	.string "test for x < y fail\n"
.LC572471771:
	.string "test for 1 == 0 fail\n"
.LC794097650:
	.string "test for 1 == 0 correct\n"
