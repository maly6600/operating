/* On Mac OS (aka OS X) the ucontext.h functions are deprecated and requires the
   following define.
*/
#define _XOPEN_SOURCE 700

/* On Mac OS when compiling with gcc (clang) the -Wno-deprecated-declarations
   flag must also be used to suppress compiler warnings.
*/

#include <signal.h>   /* SIGSTKSZ (default stack size), MINDIGSTKSZ (minimal
                         stack size) */
#include <stdio.h>    /* puts(), printf(), fprintf(), perror(), setvbuf(), _IOLBF,
                         stdout, stderr */
#include <stdlib.h>   /* exit(), EXIT_SUCCESS, EXIT_FAILURE, malloc(), free() */
#include <ucontext.h> /* ucontext_t, getcontext(), makecontext(),
                         setcontext(), swapcontext() */
#include <stdbool.h>  /* true, false */

#include "sthreads.h"

/* Stack size for each context. */
#define STACK_SIZE SIGSTKSZ*100

/*******************************************************************************
                             Global data structures

                Add data structures to manage the threads here.
********************************************************************************/
typedef struct ready_queue {
  thread_t *running;
  thread_t *first;
  thread_t *last;
} ready_queue_t;

ready_queue_t *ready_queue;

tid_t tid = 100;

/*******************************************************************************
                             Auxiliary functions

                      Add internal helper functions here.
********************************************************************************/

tid_t get_tid() {
  return tid++;
}

void dequeue_thread() {
  thread_t *thread_to_run = ready_queue->first;
  thread_to_run->state = running;
  ready_queue->running = thread_to_run;
  ready_queue->first = ready_queue->first->next;
}

void enqueue_thread(thread_t *thread) {

  if (ready_queue->first == NULL) {
    ready_queue->first = thread;
    ready_queue->last = thread;
  } else {
    ready_queue->last->next = thread;
    ready_queue->last = thread;
  }
}

void start() {
  dequeue_thread();
  setcontext(&ready_queue->running->ctx);
}
/*******************************************************************************
                    Implementation of the Simple Threads API
********************************************************************************/


int  init(){
 
  ready_queue = malloc(sizeof(ready_queue_t));

  if (ready_queue == NULL) {
    return -1;
  }

  return 1;
}


tid_t spawn(void (*start)(), ucontext_t *ctx){
  thread_t *new_thread = malloc(sizeof(thread_t));

  
  void *stack = malloc(STACK_SIZE);

  if (stack == NULL) {
    return -1;
  }

  if (getcontext(ctx)<0) {
    return -1;
  }
  ctx->uc_link           = NULL;
  ctx->uc_stack.ss_sp    = stack;
  ctx->uc_stack.ss_size  = STACK_SIZE;
  ctx->uc_stack.ss_flags = 0;

  makecontext(ctx, start, 0);
  new_thread->tid = get_tid();
  new_thread->state = ready;
  new_thread->ctx = *ctx;
  new_thread->next = NULL;

  enqueue_thread(new_thread);
  return new_thread->tid;
}

void yield(){
  ready_queue->running->state = ready;
  enqueue_thread(ready_queue->running);

  dequeue_thread();
  if(ready_queue->first) {
    swapcontext(&ready_queue->last->ctx, &ready_queue->running->ctx);
  }
  
}

void  done(){
}

tid_t join() {
  return -1;
}
