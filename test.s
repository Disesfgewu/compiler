	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jl .LC0
	movq $0, %rax
.LC0:
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jne .LC1
	movq $1, %rax
.LC1:
	movq %rax, %rsi
	movq $.LCd, %rdi
	movq $0, %rax
	call printf
	movq $5, %rax
	movq %rax, -16(%rbp)
	movq $10, %rax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jne .LC2
	movq $1, %rax
.LC2:
	cmpq $0, %rax
	je .LC3
	movq $.LC1017497501, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC4
.LC3:
	movq $.LC64382845, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC4:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	je .LC5
	movq $1, %rax
.LC5:
	cmpq $0, %rax
	je .LC6
	movq $.LC211899742, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC7
.LC6:
	movq $.LC616959230, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC7:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jg .LC8
	movq $0, %rax
.LC8:
	cmpq $0, %rax
	je .LC9
	movq $.LC501853883, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC10
.LC9:
	movq $.LC611857265, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC10:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jl .LC11
	movq $0, %rax
.LC11:
	cmpq $0, %rax
	je .LC12
	movq $.LC991831047, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC13
.LC12:
	movq $.LC870948356, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC13:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jge .LC14
	movq $0, %rax
.LC14:
	cmpq $0, %rax
	je .LC15
	movq $.LC888186753, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC16
.LC15:
	movq $.LC550699791, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC16:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jle .LC17
	movq $0, %rax
.LC17:
	cmpq $0, %rax
	je .LC18
	movq $.LC343737688, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC19
.LC18:
	movq $.LC983072046, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC19:
	leave
	ret
	.data
.LCd:
	.string "%d\n"
.LC1017497501:
	.string "a == b is True\n"
.LCs:
	.string "%s\n"
.LC64382845:
	.string "a == b is False\n"
.LC211899742:
	.string "a != b is True\n"
.LC616959230:
	.string "a != b is False\n"
.LC501853883:
	.string "a > b is True\n"
.LC611857265:
	.string "a > b is False\n"
.LC991831047:
	.string "a < b is True\n"
.LC870948356:
	.string "a < b is False\n"
.LC888186753:
	.string "a >= b is True\n"
.LC550699791:
	.string "a >= b is False\n"
.LC343737688:
	.string "a <= b is True\n"
.LC983072046:
	.string "a <= b is False\n"
