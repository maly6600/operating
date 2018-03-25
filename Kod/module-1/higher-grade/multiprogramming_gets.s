###############################################################################
# A small multiprogramming system with only two non-terminating jobs. 
# Jobs can be in the following states: RUNNING, READY or WAITING.
#
# 2018-01-15
# 
# First version by Karl Marklund <karl.marklund@it.uu.se>
###############################################################################


###############################################################################
# MAIN
#
# Mars starts to execute at label main in the .text segment, i.e., Mars
# Mars starts to execute in user mode.
###############################################################################

	.globl main
	.text

main:
	j boot 


###############################################################################
# BOOT
#
# Initialization of the simple multiprogramming system.
###############################################################################

	.text
	
boot:
	# Enable keyboard interrupts. 
	
  	lw  $t0, RECEIVER_CONTROL	# Address of receiver controll register.
  	lw  $t1, 0($t0)			# Value of receiver controll register.
  	ori $t2, $t1, 2			# Set the interrupt enable bit to 1.
  	sw  $t2, 0($t0)			# Update the receiver controll register. 
 	
        # In MARS the status register is set to 0x0000ff11 on start-up, i.e., 
        # the interrupt enable bit (bit 0) is alredy set and all interrupt 
        # mask (bits 8-15) bits are set. 
        
        # If you use SPIM, the status register is set to 0x3000ff10 start-up, 
        # i.e., the interrupt enable (bit 0) is not set but all interrupt mask
        # bits (bits 8-15) are set. 
        
        # If you use SPIM you must enable interrupts by setting the interrupt
        # enable bit in the Status register to 1. 


	# Initialize kernel data structures.
		
	# In a job context, the value of the program counter ($pc) is stored at 
	# offset 0. 
	
	# Set PC in job 0 context
	
	la $t0, __job_0_context
	la $t1, job_increment
	sw $t1, 0($t0)
	
	# Set PC in job 1 context
	
	la $t0, __job_1_context
	la $t1, job_getc
	sw $t1, 0($t0)
	
	# Job 1 will start to execute.
	
	li $t0, 1
	sw $t0, __running
	
	# Mark job 0 as ready 
	
	li $t0, 0
	sw $t0, __ready

	# Start to execute the job marked as running. 
	
	# Get job id (jid) of the running job. 
	
	lw $t0, __running 
	
	# Offset to context pointer (0 or 4)  = jid * 4.
	
	sll $t1, $t0, 2   # jid * 4
	
	# Pointer to the context of the running job.
	
	lw $t3, __context_array($t1)
	
	# Load address of program counter from context at offset 0. 
	
	lw $t4, 0($t3)

	# Jump to address in $t5. 
	
	jr $t4


###############################################################################
# USER DATA SEGMENT
#
# Data used by the user level jobs. 
###############################################################################

	.data

# Strings used by the jobs to print messages to the Run I/O pane. 

JOB_GETC_HELLO:		.asciiz "Job started running getc\n"
JOB_ÌNCREMENT_HELLO:	.asciiz "Job started running increment\n"
JOB_GETS_HELLO: 	.asciiz "Job started running gets\n"

JOB_ID:			.asciiz "Job id = "
NL:			.asciiz "\n"

PRESS_KEY:		.asciiz "Press a key on the keyboard ...\n"
PRESS_MMIO_KEY:		.asciiz "Type a character in the MMIO Simulator keyboard input area ..\n"

# In these strings the X is at offset 17 and will be replaced before printing. 

MARS_GETC_RESULT: 	.asciiz "\nMARS   getc ==> X\n"
CUSTOM_GETC_RESULT: 	.asciiz "\nCustom getc ==> X\n"

# Memory addresses to the memory mapped receiver registers. 
RECEIVER_CONTROL:	.word 0xffff0000
RECEIVER_DATA:		.word 0xffff0004

#Data for custom gets system call - a 
BUFFER: .space 4 
BUFFER_SIZE: .word 0x00000004

###############################################################################
# NON-TERMINATING USER LEVEL JOBS
#
# Jobs must be non-terminating.
#
# Jobs are only allowed to use the following registers: $v0, $a0, $a1 and $s0
###############################################################################
	
	.text
	
#------------------------------------------------------------------------------
# INCREMENT
#
# User level job. 
#------------------------------------------------------------------------------

job_increment:
	
	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, JOB_ÌNCREMENT_HELLO
	syscall 

	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, JOB_ID
	syscall

	# Use the custom system call (getjid, system call 0) to get the kernel id of this job. 
	
	li $v0, 0
	teqi $zero, 0
	
	# $a0 now containts the job id. 
	
	# Use the MARS builtin system call (1) to print the id (integer).
	
	li $v0, 1
	syscall

	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, NL
	syscall
	
	# Done printing.
	
	# Initialize $s0.
	
	li $s0, 0
	
	# Enter infinite loop where $s0 is constantly incremented. 

job_increment_infinite_loop:
	addi $s0, $s0, 1
	j job_increment_infinite_loop
	

#------------------------------------------------------------------------------
# GETC
#
# User level job. 
#------------------------------------------------------------------------------

job_getc:
	j __gets
	# Use the MARS builtin system call (4) to print strings.
	
	li $v0, 4               # System call code (4) print string. 
	la $a0, JOB_GETC_HELLO  # String to print.
	syscall 		# Execute the MARS built-in system call (4) to print string.
	
	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, JOB_ID
	syscall
	
	# Use the custom system call (getjid, system call 0) to get the kernel id of this job. 
	
	li $v0, 0	# Custom system call code 0 (getjid).
	teqi $zero, 0   # Generate a trap exception to handle the system call.
	
	# $a0 now containts the job id. 
	
	# Use the MARS builtin system call (1) to print the id (integer).

	li $v0, 1
	syscall

	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, NL
	syscall

	# Use the MARS builtin system call (4) to print strings.
	
	li $v0, 4               # System call code (4) print string. 	
	la $a0, PRESS_KEY       # String to print.
	syscall 		# Execute the Mars built-in system call (4) to print string.	

	# Use the built in version of the getc system call
	
	li $v0, 12		# System call code (12) read_char.
	syscall 		# Execute the Mars built-in system call (12) read_char.	
	
	# ASCII value of key pressed now in $v0

	# Use the MARS builtin system call (4) to print strings.
	
	sb $v0, MARS_GETC_RESULT + 17
	li $v0, 4
	la $a0, MARS_GETC_RESULT
	syscall 
	
	# Set $s0 to 0xabcd1234.
	
	li $s0, 0xabcd1234
 
	# Enter infintite loop. 
	
job_getc_infinite_loop:
	
	# Use the MARS builtin system call (4) to print strings.
	
	li $v0, 4               # System call code (4) print string. 	
	la $a0, PRESS_MMIO_KEY  # String to print.
	syscall 		# Execute the Mars built-in system call (4) to print string.	

	# Execute the custom getc system call (system call 12).
	
	li $v0, 12
	teqi $zero, 0    
	
	# ASCII value of key pressed now in $v0
	
	# Use the MARS builtin system call (4) to print strings.
	
	sb $v0, CUSTOM_GETC_RESULT + 17
	li $v0, 4
	la $a0, CUSTOM_GETC_RESULT
	syscall 
	
	j job_getc_infinite_loop

#------------------------------------------------------------------------------
# GETS
#
# User level job. 
#------------------------------------------------------------------------------
__gets:

	li $v0, 4               # System call code (4) print string. 
	la $a0, JOB_GETS_HELLO  # String to print.
	syscall 		# Execute the MARS built-in system call (4) to print string.
	
	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, JOB_ID
	syscall
	
	# Use the custom system call (getjid, system call 0) to get the kernel id of this job. 
	
	li $v0, 0	# Custom system call code 0 (getjid).
	teqi $zero, 0   # Generate a trap exception to handle the system call.
	
	# $a0 now containts the job id. 
	
	# Use the MARS builtin system call (1) to print the id (integer).

	li $v0, 1
	syscall

	# Use the MARS builtin system call (4) to print string.
	
	li $v0, 4
	la $a0, NL
	syscall

	la $a0, BUFFER		# adress to buffer in $a0
	li $a1, 4  		# size of buffer in $a1
	
	li $v0, 8		# indicate that we want gets
	teqi $zero, 0		# trigger an exception trap
	
	# now the character that was typed is in register $v0
	
	
	
				
	
				
###############################################################################
# KERNEL DATA SEGMENT
#
# Data used by the exception and interrupt handler, i.e, data 
# used by the kernel.
###############################################################################

		.kdata

# When entering the kernel the values of all registers usedby the running job
# must be saved to memory. Later when the job i resumed, the values of the
# registers are restored from memory.  

# The contents of all registers used by a job is called context. 

# In this simplified system, each job is only allowed to use these five 
# registers: $pc (program counter), $v0, $a0, $s0 and $at. Each register is 
# four byte, hence 5*4 = 20 byte of storage is needed by each job for storing
# the job context. 

__job_0_context: .space 24 
__job_1_context: .space 24

# Array with pointers to job contexts. Job id used as index. 

__context_array:   __job_0_context
                 , __job_1_context

# For each of the states, store -1 if no job currently in this state or store
# the job id (0 or 1) of the jobb currently in this state. 

__running: 	.word -1 # ID of currently running job.
__ready: 	.word -1 # ID of job ready to run.
__waiting:	.word -1 # ID of job blocked waiting for I/O action to complete. 

# Allocate storage for the kernel stack. The stack grows from high addresses 
# towards low addresses. 

__kernel_stack_bottom: 		.space 128 # First allocate 128 bytes. 
__kernel_initial_stack_top:	.space 4   # Allocate four more bytes. 

# The $at (assembler temporary) register is generally reserved for use in 
# pseudo instructions. Loading addresses of labels (la) and using labels when 
# loading (lw) or storing (sw) are examples of pseudo instructions that 
# will be translated to more basic instructions using $at.

# To make it possible for the kernel to use pseudo instructions without 
# corrupting the $at value of the running job, the value of $at must first be
# saved to memory. Later, the saved value will be written to the context of 
# the job that was running when entering the kernel. 

# Allocate space to temporarily save $at. 

__at:	.word 0

# Initially, the kernel $sp will be set to address of __kernel_stack_top and
# grow downward towards __kernel_stack_bottom. 

# Strings used to print kernel error messages. 
	
__error_msg_1: .asciiz "ERROR: Unhandled exception (code "
__error_msg_2: .asciiz  ")\n"
__error_msg_3: .asciiz "ERROR: Unsuported system call (code "
WE_ARE_IN_KERNEL_MODE: .asciiz "Now we have entered exception mode\n"
		
###############################################################################
# KERNEL SUBROUTINES
#
# Subroutines used by the kernel.   
###############################################################################

		.ktext

#------------------------------------------------------------------------------
# DESCRIPTION  
#
# Returns the job id and context address of the running job. 
# 
#
# ARGUMENTS 
# 
# None.
#
# 
# RETURN VALUES
# 
# $k0 - Job id of the running job. 
# $k1 - Pointer to context.
# 			
#------------------------------------------------------------------------------  
__get_running_job:
         
	# The job id (jid) of the running job.
	
	lw $k0, __running
	
	# Offset in __context_array = jid * 4
	
	sll $k1, $k0, 2         # Multiply by 4.
	
	# Get pointer to context from the __context_array.
	
        lw $k1, __context_array($k1)  
           	    
	jr $ra    


#------------------------------------------------------------------------------
# DESCRIPTION
#
# Saves the context (registers) of the running job.
# 
# ARGUMNETS
# 
# None.
#
# RETRUN VALUE(S)
#
# None.
#------------------------------------------------------------------------------
__save_running_job_context:

	# Push return address
	
	addi $sp, $sp, -4  
       	sw   $ra, 0($sp)   
       	
       	# Get info about running job.
       	
	jal __get_running_job
	
	# $k0 - running task ID
	# $k1 - address of running task state struct  

	# Get value of EPC.
 
 	mfc0 $k0, $14
 			
 	# Save register values. 
 	 
	sw $k0,   0($k1)	# PC
	sw $v0,    4($k1)	# $v0
	
TODO_1: # Save $a0, $a1, $s0 to the context. 
        sw $a0, 8($k1)
        sw $a1, 12($k1)
        sw $s0, 16($k1) 	
	
	lw $a0, __at            # NOTE: $at was saved to memory when entering the kernel!
	sw $a0, 20($k1)         # $at
	
	# Pop return address.
	
	lw $ra, 0($sp)
	addi $sp, $sp, 4	
	
	jr	$ra

#------------------------------------------------------------------------------
# DESCRIPTION
# 
# Restore the context (registers) of a job.
# 
# ARGUMENT 
#
# $k0 - job id. 
#------------------------------------------------------------------------------
__restore_job_context:
	
	# Use the job id to index the __context_array
	
	sll $k0, $k0, 2 # id * 4
	
	# Use (id*4) to index the __context_array array.
	
        lw $k0, __context_array($k0)      
	
	# Reload registers. 

	lw $k1,  0($k0) # PC
	lw $v0,  4($k0) # $v0

	
TODO_2: # Restore $a0, $a1, $s0 from the context.
	lw $a0, 8($k0) 
	lw $a1, 12($k0)
	lw $s0, 16($k0)
	
	# NOTE: $at may still be used by the kernel and will be restored later, right before 
	# leaving kernel mode. 

	# Set EPC in Coprocessor 0 to the PC value read from context. 
	
	mtc0 $k1, $14		
	
	jr $ra
	
	

###############################################################################
# KERNEL TEXT SEGMENT
# 
# The kernel handles all exceptions, interrupts and custom system calls.
###############################################################################
   	
   	# The exception vector address for MIPS32.
   	.ktext 0x80000180	
   
__kernel:

      	# To make it possible for the kernel to use pseudo instructions and 
      	# subroutines without corrupting the $at and $ra register values of the
      	# running job, the  values of $at and $ra must first be saved to memory.
      	# Later, the saved values will be written to the context of the job 
      	# that was running when entering the kernel. 
      	
 	# NOTE: The sw instruction is a pseudo instruction using $at. 
	# To save the current value of $at we first have to copy the value of 
	# $at to $k0. 
		
	# .set noat       	# SPIM - Turn of warnings for using the $at register.
        move $k1, $at		# Copy value of $at to $k0.     
       	# .set at               # SPIM - Turn on warnings for using the $at register.

	# Now the value of $at can be saved to memory. 
        sw $k1, __at      	   
   	      	
      	# The registers $k0 and $k1 should only be used in the exception
        # handler and not by used level code. Therefore $k0 and $k1 can be 
        # used without saving and restoring.
		
	# Init kernel stack. 
	
	la $sp, __kernel_initial_stack_top
	
	# Save the context of the running job. 
	
	jal __save_running_job_context
	
	# The kernel can now use the following registers: $v0, $a0, $s0, $k0 and $k1.
   	
   	# Why are we entering the kernel, due to an exception or due to an interrupt?
   	
   	# Get the value of the Cause register from Coprocessor 0. 
 	
 	mfc0 $k0, $13
 	
 	# Extract the exception code (bits 2 - 6).          
   	
   	srl $k1, $k0, 2        
   	andi $k1, $k1, 0x1f
	
	# All interupts sets the exception code == 0.   

   	beqz $k1, __interrupt
	
	# A non-zero exception means its an exception. 
	
__exception:
	
	# $k0 - CAUSE
   	# $k1 - Exception code
   	
   	# The only exception handled by the kernel is the TRAP exception with 
   	# exception code 13.  
   	
  	li $k0, 13   
   	bne $k1, $k0, __unhandled_exception
   	
__trap_handler:

   	# Trap exception handler code. 
   	
   	jal __get_running_job
	
	# $k0 - running task ID
	# $k1 - address of running task state struct  
	
	# Dispatch on the system call code in $v0. 
	
   	beqz $v0, __system_call_getjid
   	
TODO_3: # Jump to label __system_call_getc for system call code 12.
	li $t0, 12
	beq $v0, $t0, __system_call_getc
	
	li $v0, 4              	 	# System call code (4) print string. 
	la $a0, WE_ARE_IN_KERNEL_MODE  	# String to print.
	syscall 			# Execute the MARS built-in system call (4) to print string.
	
	beq $v0, 8, __system_call_gets  
   	
   	j __unsported_system_call
 
 __system_call_getjid: 
	
	# A non-blocking system call
	
	# $k0 already contains jid of the caller. 


	jal __restore_job_context

TODO_4: # Put the jid of the caller in register $a0.
	lw $a0, __running
	
	# TIP: The kernel saved the jid of the running job in memory at label __running
        	
	
	j __return_from_exception
   	  	
__system_call_getc:

	# As of now: 
	#   $k0 - Job ID of caller.
	#   $k1 - Address of caller context.
	
	# Reading a character from the keyboard is a BLOCKING system call. 
		
	# Block the caller by changing the state of the caller to waiting. 
	
	sw $k0, __waiting
	
	# To be able to resume the caller later, must save the address where to 
	# resume once the requested character from the keyboard is ready.

	
	 
TODO_5:	# Get address of the instruction (teqi) causing the exception. 
	# and store it in $t0. 
	
	# TIP: When an exception (or interrupt) occurs, the address of the last 
	# executed instruction is saved in the EPC (Exception Program Counter) 
	# register in Coprocessor 0. The EPC register is the $14 of Coprocessor 0. 
	
	# TIP: Use the mfc0 (Move FRom Coprocessor 0) instruction to get the value of
	# EPC.
	mfc0 $t0, $14
	
	# If resuming the caller at of the EPC this will cause the very same
	# instruction to be executed again, i.e., executing the teqi used to initiate
	# the custom system call. 

	# Each instruction is 4 bytes. 
	
	
TODO_6:	# To resume at the instruction following the teqi instruction, 
	# add 4 to EPC and store the result in $t1. 
	
	# TIP: Use the addi (Add Immediate) instruction. 
	addi $t1, $t0, 4
	
	
TODO_7:	# The value of EPC + 4 must now be saved in the context of the caller.


	# TIP: In the context, the field use to store the value were to resume is at 
	# offset 0. 
			
	# TIP: $t1 should now hold EPC + 4. 
	
	# TIP: $k1 already contains the address to the context of the caller. 

	# TIP: Use the sw (Store Word) instruction to save the content of EPC + 4 ($t1) 
	# Save EPC + 4 in user context at offset 0 (program counter). 
	sw $t1, 0($k1)
	
		
	# Job id of job to resume while the calling job waits. 
	
   	lw $k0, __ready 
   	
   	# Set EPC to PC value stored in the context of the waiting job.
   	
   	# Index of job context in the __context_array
   	
   	sll $t0, $k0, 2 	# (job id) * 4 
   	
   	# Get address of job context. 
   	
   	lw $t1 __context_array($t0) 
   	
   	# Get PC value from context.
   	
   	lw $t2, 0($t1)	# PC at offset 0
   	
   	# Set ECP to the restored PC value.
   	   	
	mtc0 $t2, $14   # Update EPC
	
	# Change status of the resumed job to running. 
	
	sw $k0, __running
	

	# Restore context. 
	
	jal __restore_job_context
	
	# Restore $at. 
	
	lw $at, 20($k0)

	# NOTE: From this point no pseudo instructions can be used. 
	
	# Resume execution at address stored in EPC. 
	
	eret

	
__system_call_gets:

	# Block job that is reading string by changing state to waiting
	sw $k0, __waiting	
   	mfc0 $t0, $14
   	addi $t1, $t0, 4
   	sw $t1, 0($k1)
   	lw $k0, __ready 
   	sll $t0, $k0, 2 	# (job id) * 4
   	lw $t1 __context_array($t0)  
   	lw $t2, 0($t1)	# PC at offset 0
   	mtc0 $t2, $14   # Update EPC
   	sw $k0, __running
   	jal __restore_job_context
   	lw $at, 20($k0)
   	eret
   	
__unsported_system_call:

	li $v0, 4
	la $a0, __error_msg_3
	syscall 
	
	li $v0, 1
	move $a0, $k1	
   	syscall 
   	
   	li $v0, 4
   	la $a0, __error_msg_2
   	syscall 
   	
   	j __return_from_exception
   	
__unhandled_exception:

	li $v0, 4
	la $a0, __error_msg_1
	syscall
	
	li $v0, 1
	move $a0, $k1	
	syscall
	
	li $v0, 4
	la $a0, __error_msg_2
	syscall 
		
	j __return_from_exception

__return_from_exception:
 
	# Skip instruction causing the exception
   
  	mfc0 $k0,$14   # Coprocessor 0 register $14 (EPC) has address of trapping instruction
   	addi $k0,$k0,4 # Add 4 to point to next instruction
   	mtc0 $k0,$14   # Store new address back into $14 (EPC)
   	
   	# Transfer controll to the address in EPC.
   	
   	eret
   	
__interrupt:

	# Disable all interupts while handling the interrupt.
	
	# Load STATUS ($12) from Coprocessor 0.
	
	mfc0 $t0, $12
	
	# Set bit 0  (interrupt enable) in STATUS to 0
	
	li $t1, 0xfffe
	and $t0, $t0, $t1
	
	# Update STATUS ($12) in Coprocessor 0.
	
	mtc0 $t0, $12 
	
	# What kind of interrupt?
	
	# SPIM Hardware interrupt level 1 (keyboard)
	# li $k1, 0x00000080      

	# MARS Hardware interrupt level 1 (keyboard)
	
	li $k1, 0x00000100
		
	bne $k0, $k1, __return_from_interrupt
	
__kbd_interrupt:

	# Ignore the interrupt if no job is waiting for the getc system call to complete.
	
	lw $k0, __waiting
	li $k1, -1
	beq $k0, $k1, __return_from_interrupt 
	
	beq $v0, 8, __gets_system_call_pending
	
	j __getc_system_call_pending

__gets_system_call_pending:

	# job 1 waiting for string
	#put job 2 to waiting mode too
	
	li $k1, 0xffff0004
	# character is at adress 0($k1)
	
	sw $k1, ($a0) 	# storing character typed into buffer
	
	addi $a0, $a0, 1  		# increment buffer pointer
	
	# $k0 holds id of job waiting for input, restore the context of this job. 
	jal __restore_job_context  	# restore context of job 2
	
__getc_system_call_pending:
	
	# A job is waiting for input.
	
	# Svap waiting task and running task
	
	lw $k0, __waiting
	lw $k1, __running
	sw $k0, __running
	
	# No job is waiting anymore. 
	
	li $t0, -1
	sw $t0, __waiting
	
	# $k0 holds id of job waiting for input, restore the context of this job. 
	
	jal __restore_job_context

	# Get ASCII value of key pressed and save in $k1.
	
	# The memory mapped transiver data register is at address 0xffff0004.
	
	li $k1, 0xffff0004  
	
TODO_8:	# Before resuming the waiting job, put the ASCII value of the pressed
	# key in register $v0. 
	
	# TIP: $k1 contains the address of the memory mapped receiver data register. 
	
	# TIP: Use the (Load Word) instruction to read the ASCII value stored in 
	# receiver data into $v0. 
	
	lw $v0, 0($k1)
	
	# Restore $at.
	
	lw $at, 20($k0) 

	# NOTE: From this point no pseudo instructions can be used. 
	
	# Done handling the interupt, enable all interrupts.
	
	# Get content of the STATUS register.  
	
	mfc0 $k0, $12
	
	# Set bit 0 (interrupt enable) to 1.
	
	li $k1, 1
	or $k0, $k0, $k1
	
	# Update the STATUS register. 
	
	mtc0 $k0, $12 
	
	# Resume execution of the waiting job. The eret instruction sets $pc to the
	# value of EPC.
	
	eret

__return_from_interrupt:

   	# Error return; set PC to value in $14 (EPC).
   
   	eret
   

