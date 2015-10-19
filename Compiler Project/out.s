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
				# b_dispatch (  == ,signed long int, 1, .LC2, no pop on jump )
	movl	(%esp), %edx
	movl	$1, %eax
	cmpl	%eax, %edx
	jne	.LC3
				# b_jump ( destination = .LC2 )
	jmp	.LC2
.LC3:
				# b_jump ( destination = .LC1 )
	jmp	.LC1
.LC2:
				# b_pop ()
	addl	$8, %esp
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
				# b_jump ( destination = (null) )
	jmp	(null)
.LC1:
 #   19
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
				# b_jump ( destination = (null) )
	jmp	(null)
.LC1:
 #   20
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
				# b_jump ( destination = (null) )
	jmp	(null)
.LC1:
				# b_pop ()
	addl	$8, %esp
(null):
 #   22
				# b_func_epilogue (main)
	leave
	ret
	.size	main, .-main
 #   23
