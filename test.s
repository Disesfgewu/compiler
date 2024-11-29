	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LC0, %rdi
	movq $0, %rax
	call printf
	leave
	ret
	.data
.LC0:
	.string "hello, worldsdjfksdj\n"
