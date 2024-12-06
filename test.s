	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LC763646369, %rdi
	pushq %rax
	movq $.LC417747598, %rdi
	popq %rbx
	movq $.LC417747598, %rdi
	call strlen
	movq %rax, %r10
	movq $.LC763646369, %rdi
	call strlen
	addq %r10, %rax
	addq $1, %rax
	movq %rax, %rdi
	call malloc
	movq %rax, %r12
	movq $.LC417747598, %rdi
	movq %r12, %rdi
	call strcpy
	movq $.LC763646369, %rdi
	movq %r12, %rdi
	call strcat
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
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
	.data
.LC417747598:
	.string "asd\n"
.LC763646369:
	.string "foo\n"
.LCtrue:
	.string "True\n"
.LCfalse:
	.string "False\n"
.LCs:
	.string "%s\n"
.LCd:
	.string "%d\n"
