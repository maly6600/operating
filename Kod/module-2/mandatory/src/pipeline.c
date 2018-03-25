#include <stdio.h>    // puts(), printf(), perror(), getchar()
#include <stdlib.h>   // exit(), EXIT_SUCCESS, EXIT_FAILURE
#include <unistd.h>   // getpid(), getppid(),fork()
#include <sys/wait.h> // wait()

#define READ  0
#define WRITE 1

//function that child A should execute
void child_a(int fd[]) { 
  printf("We have called child a:s function\n");
  int return_close_A = close(fd[READ]); //fd[0] = 3 pipe read and we are closing it

   if (return_close_A == -1) {
    perror("Close function failed for A");
    exit(EXIT_FAILURE);
  }
  
  int return_dup2 = dup2(fd[WRITE], 1); // redirection of standout to write end of pipe

  if (return_dup2 < 0) {
    perror("dup2 function failed for A");
    exit(EXIT_FAILURE);
  }
  
  execlp("ls", "ls", "-F", "-1", NULL);

}

//function that child B should execute
void child_b(int fd[]) {
  printf("We have called child b:s function\n");
   
  int return_close_B = close(fd[WRITE]); //fd[1] = 4 pipe write and we are closing it       
   if (return_close_B == -1) {
    perror("Close function failed for B");
    exit(EXIT_FAILURE);
  }
  int return_dup2 = dup2(fd[READ], 0); // redirection of standin to read end of pipe

  if (return_dup2 < 0) {
    perror("dup2 function failed for B");
    exit(EXIT_FAILURE);
  }
  execlp("nl", "nl", NULL);
 
}

int main(void) {
  int fd[2];

  int return_pipe = pipe(fd);

  if (return_pipe == -1) {
    perror("Pipe creation failed!");
    exit(EXIT_FAILURE);
  }
  
  int pid_a;
  int pid_b;

  switch (pid_a = fork()) {
     case -1:
       perror("Fork failed\n");
       exit(EXIT_FAILURE);
     case 0:
       printf("In child_A\n");
       child_a(fd);
       exit(EXIT_SUCCESS);
     default:
       printf("In parent (child_a fork)\n");
       break;
  }

    switch (pid_b = fork()) {
     case -1:
       perror("Fork failed\n");
       exit(EXIT_FAILURE);
     case 0:
       printf("In child_B\n");
       child_b(fd);
       exit(EXIT_SUCCESS);
     default:
       printf("In parent (child_b fork)\n");
       break;
    }
    
    int close_read = close(fd[0]); //fd[0] = 3 pipe read och vi stänger den

    if (close_read == -1) {
    perror("Close function failed for Read end of pipe in parent");
    exit(EXIT_FAILURE);
    }
    
    int close_write = close(fd[1]); //fd[1] = 4 pipe write och vi stänger den

    if (close_write == -1) {
      perror("Close function failed for write end of pipe in parent");
      exit(EXIT_FAILURE);
    }
    
    

    
    int wait1 = wait(NULL);
    if (wait1 == -1) {
      perror("First wait function call failed");
      exit(EXIT_FAILURE);
    }

   int wait2 = wait(NULL);
    if (wait2 == -1) {
      perror("Second wait function call failed");
      exit(EXIT_FAILURE);
    }
       
    exit(EXIT_SUCCESS);

  
  // TODO: Add code here.

}
