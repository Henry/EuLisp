gcc_compiled.:
.text
	.align 2,144
.globl stack_switch_and_go
stack_switch_and_go:
	movl 4(%esp),%eax
	movl 8(%esp),%ebx
        movl %eax,%esp
        jmp *%ebx

