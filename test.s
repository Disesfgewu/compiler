	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $.LCstart, %rdi
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
	movq $.LCcomma, %rdi
	call printf
	movq $5, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $6, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $8, %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	call printf
	movq $.LCcomma, %rdi
	call printf
	movq $.LC127279375, %rdi
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
	.globl	len
len:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rsi
	call strlen
	leave
	ret
	.data
.LC127279375:
	.string "123"
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
