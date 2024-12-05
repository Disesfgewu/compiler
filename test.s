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
	cmpq $1, %rax
	jne .LC1
	movq $.LCtrue, %rdi
	jmp .LC2
.LC1:
	movq $.LCfalse, %rdi
.LC2:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	jne .LC3
	movq $1, %rax
.LC3:
	cmpq $1, %rax
	jne .LC4
	movq $.LCtrue, %rdi
	jmp .LC5
.LC4:
	movq $.LCfalse, %rdi
.LC5:
	movq %rax, %rsi
	movq $.LCs, %rdx
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
	jne .LC6
	movq $1, %rax
.LC6:
	cmpq $0, %rax
	je .LC7
	movq $.LC1017497501, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC8
.LC7:
	movq $.LC64382845, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC8:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $0, %rax
	je .LC9
	movq $1, %rax
.LC9:
	cmpq $0, %rax
	je .LC10
	movq $.LC211899742, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC11
.LC10:
	movq $.LC616959230, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC11:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jg .LC12
	movq $0, %rax
.LC12:
	cmpq $0, %rax
	je .LC13
	movq $.LC501853883, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC14
.LC13:
	movq $.LC611857265, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC14:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jl .LC15
	movq $0, %rax
.LC15:
	cmpq $0, %rax
	je .LC16
	movq $.LC991831047, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC17
.LC16:
	movq $.LC870948356, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC17:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jge .LC18
	movq $0, %rax
.LC18:
	cmpq $0, %rax
	je .LC19
	movq $.LC888186753, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC20
.LC19:
	movq $.LC550699791, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC20:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	movq $1, %rax
	jle .LC21
	movq $0, %rax
.LC21:
	cmpq $0, %rax
	je .LC22
	movq $.LC343737688, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC23
.LC22:
	movq $.LC983072046, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC23:
	leave
	ret
	.data
.LC1017497501:
	.string "a == b is True\n"
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
.LCtrue:
	.string "True\n"
.LCfalse:
	.string "False\n"
.LCs:
	.string "%s\n"
.LCd:
	.string "%d\n"
