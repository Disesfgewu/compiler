	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LC417747598, %rdi
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
	.data
.LC417747598:
	.string "asd"
.LCtrue:
	.string "True\n"
.LCfalse:
	.string "False\n"
.LCs:
	.string "%s\n"
.LCd:
	.string "%d\n"
