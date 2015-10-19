 #    2
 #    3
 #    4
 #    5
 #    6
 #    7
 #    8
 #    9
				# b_global_decl (I, alignment = 4, size = 4)
.globl I
	.data
	.align	4
	.type	I, @object
	.size	I, 4
I:
	.zero	4
 #   10
 #   11
 #   12
 #   13
				# b_func_prologue (main)
	.text
.global main
	.type	main, @function
main:
	pushl	%ebp
	movl	%esp, %ebp
	andl	$-16, %esp
 #   14
 #   15
				# b_push_ext_addr (I)
	subl	$8, %esp
	movl	$I, (%esp)
				# b_push_const_int (3)
	movl	$3, %eax
	subl	$8, %esp
	movl	%eax, (%esp)
				# b_assign (signed long int)
	movl	(%esp), %edx
	addl	$8, %esp
	movl	(%esp), %eax
	movl	%edx, (%eax)
	movl	%edx, (%esp)
				# b_pop ()
	addl	$8, %esp
 #   16
 #   17
 #   18
				# b_dispatch (  < ,signed long int, 0, .LC3, no pop on jump )
	movl	(%esp), %edx
	movl	$0, %eax
	cmpl	%eax, %edx
	jge	.LC4
				# b_jump ( destination = .LC3 )
	jmp	.LC3
.LC4:
				# b_dispatch (  <= ,signed long int, 1, .LC2, pop on jump )
	movl	(%esp), %edx
	movl	$1, %eax
	cmpl	%eax, %edx
	jg	.LC5
	addl	$8, %esp
				# b_jump ( destination = .LC2 )
	jmp	.LC2
.LC5:
.LC3:
				# b_jump ( destination = .LC1 )
	jmp	.LC1
.LC2:
				# b_alloc_arglist (0 bytes)
	movl	%esp, %eax
	subl	$4, %esp
	andl	$-16, %esp
	movl	%eax, (%esp)
	subl	$0, %esp
				# b_funcall_by_name (Print_dot, void)
	call	Print_dot
	addl	$0, %esp
	movl	(%esp), %ecx
	movl	%ecx, %esp
				# b_jump ( destination = .LC0 )
	jmp	.LC0
.LC1:
 #   19
				# b_dispatch (  < ,signed long int, 0, .LC8, no pop on jump )
	movl	(%esp), %edx
	movl	$0, %eax
	cmpl	%eax, %edx
	jge	.LC9
				# b_jump ( destination = .LC8 )
	jmp	.LC8
.LC9:
				# b_dispatch (  <= ,signed long int, 1, .LC7, pop on jump )
	movl	(%esp), %edx
	movl	$1, %eax
	cmpl	%eax, %edx
	jg	.LC10
	addl	$8, %esp
				# b_jump ( destination = .LC7 )
	jmp	.LC7
.LC10:
.LC8:
				# b_jump ( destination = .LC6 )
	jmp	.LC6
.LC7:
				# b_alloc_arglist (0 bytes)
	movl	%esp, %eax
	subl	$4, %esp
	andl	$-16, %esp
	movl	%eax, (%esp)
	subl	$0, %esp
				# b_funcall_by_name (Print_dot, void)
	call	Print_dot
	addl	$0, %esp
	movl	(%esp), %ecx
	movl	%ecx, %esp
				# b_jump ( destination = .LC0 )
	jmp	.LC0
.LC6:
 #   20
				# b_dispatch (  < ,signed long int, 0, .LC13, no pop on jump )
	movl	(%esp), %edx
	movl	$0, %eax
	cmpl	%eax, %edx
	jge	.LC14
				# b_jump ( destination = .LC13 )
	jmp	.LC13
.LC14:
				# b_dispatch (  <= ,signed long int, 3, .LC12, pop on jump )
	movl	(%esp), %edx
	movl	$3, %eax
	cmpl	%eax, %edx
	jg	.LC15
	addl	$8, %esp
				# b_jump ( destination = .LC12 )
	jmp	.LC12
.LC15:
.LC13:
				# b_jump ( destination = .LC11 )
	jmp	.LC11
.LC12:
 #   21
				# b_alloc_arglist (0 bytes)
	movl	%esp, %eax
	subl	$4, %esp
	andl	$-16, %esp
	movl	%eax, (%esp)
	subl	$0, %esp
				# b_funcall_by_name (Print_dot, void)
	call	Print_dot
	addl	$0, %esp
	movl	(%esp), %ecx
	movl	%ecx, %esp
				# b_jump ( destination = .LC0 )
	jmp	.LC0
.LC11:
				# b_pop ()
	addl	$8, %esp
.LC0:
 #   22
				# b_func_epilogue (main)
	leave
	ret
	.size	main, .-main
 #   23
