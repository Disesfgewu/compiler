	.text
fact:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jle .LC0
	movq $0, %rax
	jmp .LC1
.LC0:
	movq $1, %rax
.LC1:
	cmpq $0, %rax
	je .LC2
	movq $1, %rax
	leave
	ret
	jmp .LC3
.LC2:
.LC3:
	movq $1, %rax
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	subq %rbx, %rax
	pushq %rax
	call fact
	addq $8, %rsp
	pushq %rax
	movq 16(%rbp), %rax
	popq %rbx
	imulq %rbx, %rax
	leave
	ret
	movq $0, %rax
	leave
	ret
factimp:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	pushq %rax
	movq %rax, -16(%rbp)
	movq -24(%rbp), %r10
	movq 16(%rbp), %rax
	movq %rax, %rdi
	movq %rax, %rbx
	imulq $8, %rdi
	addq $8, %rdi
	call malloc
	movq %rax, %r14
	movq %rbx, %rdi
	movq %rbx, 0(%r14)
	xorq %r15, %r15
range_loop:
	cmpq %r15, %rdi
	je range_end
	movq %r15, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq %r15, 0(%r14,%rdx,1)
	addq $1, %r15
	jmp range_loop
range_end:
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
.LC4:
	cmpq 0(%r14), %r13
	je .LC5
	movq %r13, %rdx
	imulq $8, %rdx
	addq $8, %rdx
	movq 0(%r14,%rdx,1), %rax
	movq %rax, -24(%rbp)
	movq %rax, %r9
	movq $1, %rax
	pushq %rax
	movq -24(%rbp), %rax
	popq %rbx
	addq %rbx, %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	movq %rax, -16(%rbp)
	pushq %rax
	addq $1, %r13
	jmp .LC4
.LC5:
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
runtime_error:
	movq $.LCerror, %rdi
	call puts
	movq $1, %rdi
	call exit
	ret
	.data
.LCtrue:
	.string "True"
.LCfalse:
	.string "False"
.LCcomma:
	.string ", "
.LCstart:
	.string "["
.LCend:
	.string "]"
.LCs:
	.string "%s"
.LCd:
	.string "%d"
.LCnone:
	.string "None"
.LCerror:
	.string "Runtime Error"
