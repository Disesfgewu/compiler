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
	jl .LC0
	movq $0, %rax
	jmp .LC1
.LC0:
	movq $1, %rax
.LC1:
	cmpq $1, %rax
	jne .LC2
	movq $.LCtrue, %rdi
	jmp .LC3
.LC2:
	movq $.LCfalse, %rdi
.LC3:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $2, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	je .LC4
	movq $0, %rax
	jmp .LC5
.LC4:
	movq $1, %rax
.LC5:
	cmpq $1, %rax
	jne .LC6
	movq $.LCtrue, %rdi
	jmp .LC7
.LC6:
	movq $.LCfalse, %rdi
.LC7:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $1, %rax
	cmpq $1, %rax
	jne .LC8
	movq $.LCtrue, %rdi
	jmp .LC9
.LC8:
	movq $.LCfalse, %rdi
.LC9:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	cmpq $1, %rax
	jne .LC10
	movq $.LCtrue, %rdi
	jmp .LC11
.LC10:
	movq $.LCfalse, %rdi
.LC11:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	andq %rbx, %rax
	cmpq $1, %rax
	jne .LC12
	movq $.LCtrue, %rdi
	jmp .LC13
.LC12:
	movq $.LCfalse, %rdi
.LC13:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	orq %rbx, %rax
	cmpq $1, %rax
	jne .LC14
	movq $.LCtrue, %rdi
	jmp .LC15
.LC14:
	movq $.LCfalse, %rdi
.LC15:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	je .LC16
	movq $0, %rax
	jmp .LC17
.LC16:
	movq $1, %rax
.LC17:
	cmpq $1, %rax
	jne .LC18
	movq $.LCtrue, %rdi
	jmp .LC19
.LC18:
	movq $.LCfalse, %rdi
.LC19:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	jne .LC20
	movq $0, %rax
	jmp .LC21
.LC20:
	movq $1, %rax
.LC21:
	cmpq $1, %rax
	jne .LC22
	movq $.LCtrue, %rdi
	jmp .LC23
.LC22:
	movq $.LCfalse, %rdi
.LC23:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	jl .LC24
	movq $0, %rax
	jmp .LC25
.LC24:
	movq $1, %rax
.LC25:
	cmpq $1, %rax
	jne .LC26
	movq $.LCtrue, %rdi
	jmp .LC27
.LC26:
	movq $.LCfalse, %rdi
.LC27:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	jle .LC28
	movq $0, %rax
	jmp .LC29
.LC28:
	movq $1, %rax
.LC29:
	cmpq $1, %rax
	jne .LC30
	movq $.LCtrue, %rdi
	jmp .LC31
.LC30:
	movq $.LCfalse, %rdi
.LC31:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	jg .LC32
	movq $0, %rax
	jmp .LC33
.LC32:
	movq $1, %rax
.LC33:
	cmpq $1, %rax
	jne .LC34
	movq $.LCtrue, %rdi
	jmp .LC35
.LC34:
	movq $.LCfalse, %rdi
.LC35:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	cmpq %rbx, %rax
	jge .LC36
	movq $0, %rax
	jmp .LC37
.LC36:
	movq $1, %rax
.LC37:
	cmpq $1, %rax
	jne .LC38
	movq $.LCtrue, %rdi
	jmp .LC39
.LC38:
	movq $.LCfalse, %rdi
.LC39:
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	andq %rbx, %rax
	cmpq $0, %rax
	je .LC40
	movq $.LC857021081, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC41
.LC40:
	movq $.LC313269094, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC41:
	movq $0, %rax
	pushq %rax
	movq $1, %rax
	popq %rbx
	orq %rbx, %rax
	cmpq $0, %rax
	je .LC42
	movq $.LC121344955, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC43
.LC42:
	movq $.LC696102200, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC43:
	movq $5, %rax
	movq %rax, -16(%rbp)
	movq $10, %rax
	movq %rax, -24(%rbp)
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	je .LC44
	movq $0, %rax
	jmp .LC45
.LC44:
	movq $1, %rax
.LC45:
	cmpq $0, %rax
	je .LC46
	movq $.LC1017497501, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC47
.LC46:
	movq $.LC64382845, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC47:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jne .LC48
	movq $0, %rax
	jmp .LC49
.LC48:
	movq $1, %rax
.LC49:
	cmpq $0, %rax
	je .LC50
	movq $.LC211899742, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC51
.LC50:
	movq $.LC616959230, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC51:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jg .LC52
	movq $0, %rax
	jmp .LC53
.LC52:
	movq $1, %rax
.LC53:
	cmpq $0, %rax
	je .LC54
	movq $.LC501853883, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC55
.LC54:
	movq $.LC611857265, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC55:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jl .LC56
	movq $0, %rax
	jmp .LC57
.LC56:
	movq $1, %rax
.LC57:
	cmpq $0, %rax
	je .LC58
	movq $.LC991831047, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC59
.LC58:
	movq $.LC870948356, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC59:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jge .LC60
	movq $0, %rax
	jmp .LC61
.LC60:
	movq $1, %rax
.LC61:
	cmpq $0, %rax
	je .LC62
	movq $.LC888186753, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC63
.LC62:
	movq $.LC550699791, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC63:
	movq -24(%rbp), %rax
	pushq %rax
	movq -16(%rbp), %rax
	popq %rbx
	cmpq %rbx, %rax
	jle .LC64
	movq $0, %rax
	jmp .LC65
.LC64:
	movq $1, %rax
.LC65:
	cmpq $0, %rax
	je .LC66
	movq $.LC343737688, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
	jmp .LC67
.LC66:
	movq $.LC983072046, %rdi
	movq %rax, %rsi
	movq $.LCs, %rdx
	movq $0, %rax
	call printf
.LC67:
	leave
	ret
	.data
.LC857021081:
	.string "True and False is True\n"
.LC313269094:
	.string "True and False is False\n"
.LC121344955:
	.string "True or False is True\n"
.LC696102200:
	.string "True or False is False\n"
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
