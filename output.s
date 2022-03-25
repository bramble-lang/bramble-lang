	.text
	.file	"test"
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:
	.cfi_startproc
	movb	$1, %al
	testb	$1, %al
	jne	.LBB0_1
	jmp	.LBB0_2
.LBB0_1:
	movq	$2, -16(%rsp)
	jmp	.LBB0_3
.LBB0_2:
	movq	$3, -16(%rsp)
.LBB0_3:
	movq	-16(%rsp), %rax
	movq	%rax, -8(%rsp)
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc

	.section	".note.GNU-stack","",@progbits
