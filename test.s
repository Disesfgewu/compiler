	.text
loop:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rax
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $10, %rdi
	call putchar
	movq $3, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jl .LC0
	movq $0, %rax
	jmp .LC1
.LC0:
	movq $1, %rax
.LC1:
	cmpq $0, %rax
	je .LC2
	movq $1, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	call loop
	addq $8, %rsp
	jmp .LC3
.LC2:
.LC3:
	movq $0, %rax
	leave
	ret
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $0, %rax
	pushq %rax
	call loop
	addq $8, %rsp
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
