	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	andq $-16, %rsp
	movq $.LC0, %rdi
	pushq %rax
	movq $.LC0, %rdi
	popq %rbx
	cmpq %rbx, %rax
	je .LC1
	movq $0, %rax
	jmp .LC2
.LC1:
	movq $1, %rax
.LC2:
	cmpq $1, %rax
	jne .LC3
	movq $.LCtrue, %rdi
	jmp .LC4
.LC3:
	movq $.LCfalse, %rdi
.LC4:
	movq %rax, %rsi
	movq $.LCs, %rdx
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
	leaq 0(%r12), %rsi
	addq %rdx, %rsi
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
compare_lists:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rdi
	movq 24(%rbp), %rsi
	movq 0(%rdi), %rax
	movq 0(%rsi), %rbx
	cmpq %rax, %rbx
	jl list1_shorter
	jg list2_shorter
	movq $8, %rcx
	jmp compare_loop
list1_shorter:
	movq $-1, %rax
	jmp end_compare
list2_shorter:
	movq $1, %rax
	jmp end_compare
compare_loop:
	cmpq %rcx, %rax
	jge end_compare
	movq 0(%rdi,%rcx,8), %rdx
	movq 0(%rsi,%rcx,8), %r8
	cmpq %rdx, %r8
	jl list1_smaller
	jg list2_smaller
	addq $8, %rcx
	jmp compare_loop
list1_smaller:
	movq $-1, %rax
	jmp end_compare
list2_smaller:
	movq $1, %rax
	jmp end_compare
end_compare:
	leave
	ret
	.data
.LC0:
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
