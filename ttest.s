.text
.globl main
main:
    pushq %rbp
    movq %rsp, %rbp

    # 確保堆疊對齊到 16 字節
    andq $-16, %rsp

    # 分配內存並初始化第一個列表 [1]
    movq $16, %rdi              # 請求 16 位元組內存
    call malloc@PLT
    testq %rax, %rax            # 檢查 malloc 是否成功
    je malloc_fail
    movq %rax, %r12             # 保存第一個列表地址
    movq $1, 0(%r12)            # 設置長度為 1
    movq $1, 8(%r12)            # 設置第一個元素為 1

    # 分配內存並初始化第二個列表 [1]
    movq $16, %rdi              # 請求 16 位元組內存
    call malloc@PLT
    testq %rax, %rax            # 檢查 malloc 是否成功
    je malloc_fail
    movq %rax, %r13             # 保存第二個列表地址
    movq $1, 0(%r13)            # 設置長度為 1
    movq $1, 8(%r13)            # 設置第一個元素為 1

    # 調用 compare_lists 比較兩個列表
    movq %r12, %rdi             # 第一個列表地址傳遞到 %rdi
    movq %r13, %rsi             # 第二個列表地址傳遞到 %rsi
    call compare_lists          # 調用比較函數
    cmpq $0, %rax               # 比較結果是否為 0（相等）
    je print_true               # 如果相等，跳轉到輸出 True
    jmp print_false             # 否則，跳轉到輸出 False

print_true:
    movq $.LCtrue, %rdi
    call printf@PLT
    jmp program_end

print_false:
    movq $.LCfalse, %rdi
    call printf@PLT
    jmp program_end

malloc_fail:
    movq $.LCmalloc_fail, %rdi
    call printf@PLT
    movq $1, %rax
    leave
    ret

program_end:
    movq $0, %rax
    leave
    ret

# 比較兩個列表的函數
compare_lists:
    pushq %rbp
    movq %rsp, %rbp

    # 打印調試信息，確認參數是否正確
    movq %rdi, %rsi
    movq $.LCdebug_rdi, %rdi
    call printf@PLT

    movq %rsi, %rdi
    movq $.LCdebug_rsi, %rdi
    call printf@PLT

    # 獲取列表長度並比較
    movq 0(%rdi), %rax          # 獲取第一個列表的長度
    movq 0(%rsi), %rbx          # 獲取第二個列表的長度
    cmpq %rax, %rbx             # 比較長度
    jne not_equal               # 如果長度不同，跳轉到 not_equal

    # 長度相同，逐元素比較
    movq $0, %rcx               # 初始化索引
compare_loop:
    cmpq %rcx, %rax             # 檢查是否到達列表末尾
    jge equal                   # 如果所有元素相等，跳轉到 equal
    movq 0(%rdi,%rcx,8), %rdx   # 獲取第一個列表的當前元素
    movq 0(%rsi,%rcx,8), %r8    # 獲取第二個列表的當前元素
    cmpq %rdx, %r8              # 比較兩個元素
    jne not_equal               # 如果元素不相等，跳轉到 not_equal
    addq $1, %rcx               # 索引加 1
    jmp compare_loop            # 跳轉到下一次比較

not_equal:
    movq $1, %rax               # 返回 1 表示不相等
    leave
    ret

equal:
    xorq %rax, %rax             # 返回 0 表示相等
    leave
    ret

.data
.LCmalloc_fail:
    .string "Memory allocation failed!\n"
.LCtrue:
    .string "True\n"
.LCfalse:
    .string "False\n"
.LCdebug_rdi:
    .string "RDI address: %p\n"
.LCdebug_rsi:
    .string "RSI address: %p\n"
