# as count-leaves.s -o count-leaves.o
# ld count-leaves.o -o count-leaves
# ./count-leaves ; echo $?        
        
        .section .data
        .globl the_cars, the_cdrs, root
        
the_cars:        
        .long 'e',0, 'p',5, 'n',3, 'e',0, 'n',4, 'n',1, 'e',0, 'n',2
the_cdrs:        
        .long 'e',0, 'p',2, 'p',4, 'e',0, 'e',0, 'p',7, 'e',0, 'e',0
root:
        .long 'p', 1
        
        .section .text

        .globl _start

        # function: count_leaves
        # input: %rcx is a pointer to a pair (ex: 'n',1)
        # output: %rax is count
cl:
        call isnull
        cmpl $1, %eax
        je 1f
        call isnotpair
        cmpl $1, %eax
        je 2f
        push %rbx
        push %rdx
        movl 4(%ecx), %ebx               # save index
        lea the_cars(,%ebx,8), %ecx
        movl (%ecx), %eax
        call cl
        mov %rax, %rdx
        lea the_cdrs(,%ebx,8), %ecx
        movl (%ecx), %eax
        call cl
        add %rdx, %rax        
        pop %rdx
        pop %rbx
        ret
1:
        xor %eax, %eax
        ret
2:
        movl $1, %eax
        ret
        
        # function: isnull
        # input: %rcx is a pointer to a pair (ex: 'n',1)
        # output: %rax is 1 if is null, 0 otherwise
isnull:
        movl (%rcx), %eax
        xor $'e', %eax
        jnz 1f
        mov $1, %rax
        ret
1:
        mov $0, %rax        
        ret

        # function: isnotpair
        # input: %cax is a pointer to a pair (ex: 'n',1)
        # output: %rax is 1 if is null, 0 otherwise
isnotpair:
        movl (%rcx), %eax
        xor $'p', %eax
        jnz 1f
        mov $0, %rax
        ret
1:
        mov $1, %rax        
        ret        
        
_start:

        mov $root, %rcx
        call cl
        mov %rax, %rbx
        mov $1, %rax
        int $0x80
