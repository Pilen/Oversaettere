	.text	0x00400000
	.globl	main
	la	$28, _heap_
	jal	main
	ori	$2, $0, 10
	syscall
# 
main:
	addi	$29, $29, -8
	sw	$31, 4($29)
	sw	$16, 0($29)
	ori	$2, $0, 4
# was:	ori	_exps__3_, 0, 4
# 	ori	2,_exps__3_,0
	jal	walloc
# was:	jal	walloc, 2
# 	ori	_assign__2_,2,0
# 	ori	a_1_,_assign__2_,0
# 	ori	0,_assign__2_,0
	ori	$3, $0, 0
# was:	ori	_5_, 0, 0
	sll	$3, $3, 2
# was:	sll	_5_, _5_, 2
	add	$3, $3, $2
# was:	add	_5_, _5_, a_1_
	ori	$4, $0, 0
# was:	ori	_assign__4_, 0, 0
	sw	$4, 0($3)
# was:	sw	_assign__4_, 0(_5_)
# 	ori	0,_assign__4_,0
	ori	$3, $0, 1
# was:	ori	_7_, 0, 1
	sll	$3, $3, 2
# was:	sll	_7_, _7_, 2
	add	$3, $3, $2
# was:	add	_7_, _7_, a_1_
	ori	$4, $0, 1
# was:	ori	_assign__6_, 0, 1
	sw	$4, 0($3)
# was:	sw	_assign__6_, 0(_7_)
# 	ori	0,_assign__6_,0
	ori	$3, $0, 2
# was:	ori	_9_, 0, 2
	sll	$3, $3, 2
# was:	sll	_9_, _9_, 2
	add	$3, $3, $2
# was:	add	_9_, _9_, a_1_
	ori	$4, $0, 2
# was:	ori	_assign__8_, 0, 2
	sw	$4, 0($3)
# was:	sw	_assign__8_, 0(_9_)
# 	ori	0,_assign__8_,0
	ori	$3, $0, 3
# was:	ori	_11_, 0, 3
	sll	$3, $3, 2
# was:	sll	_11_, _11_, 2
	add	$3, $3, $2
# was:	add	_11_, _11_, a_1_
	ori	$2, $0, 3
# was:	ori	_assign__10_, 0, 3
	sw	$2, 0($3)
# was:	sw	_assign__10_, 0(_11_)
# 	ori	0,_assign__10_,0
	ori	$2, $0, 1
# was:	ori	_return__12_, 0, 1
# 	ori	2,_return__12_,0
	j	main_exit
main_exit:
	lw	$16, 0($29)
	lw	$31, 4($29)
	addi	$29, $29, 8
	jr	$31
putint:
	addi	$29, $29, -8
	sw	$2, 0($29)
	sw	$4, 4($29)
	ori	$4, $2, 0
	ori	$2, $0, 1
	syscall
	ori	$2, $0, 4
	la	$4, _cr_
	syscall
	lw	$2, 0($29)
	lw	$4, 4($29)
	addi	$29, $29, 8
	jr	$31
getint:
	ori	$2, $0, 5
	syscall
	jr	$31
putstring:
	ori	$4, $2, 0
	ori	$2, $0, 4
	syscall
	ori	$2, $0, 4
	la	$4, _cr_
	syscall
	jr	$31
getstring:
	ori	$5, $2, 0
	ori	$4, $2, 0
	ori	$2, $0, 9
	syscall
	ori	$4, $2, 0
	ori	$2, $0, 8
	syscall
	ori	$2, $4, 0
	jr	$31
walloc:
	sll	$2, $2, 2
	ori	$4, $2, 0
	ori	$2, $0, 9
	syscall
	jr	$31
balloc:
	ori	$4, $2, 0
	ori	$2, $0, 9
	syscall
	jr	$31
	.data	
	.align	2
_cr_:
	.asciiz	"\n"
	.align	2
_heap_:
	.space	100000
