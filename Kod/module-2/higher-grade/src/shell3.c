
#include "parser.h"

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/wait.h> // wait()

#define READ  0
#define WRITE 1

void fork_error() {
  perror("fork() failed)");
  exit(EXIT_FAILURE);
}


void fork_cmd(char* argv[], int fd[2], int child_NR, int n) {
  pid_t pid;

  if(child_NR == 0){

    switch (pid = fork()) {
    case -1:
      fork_error();
    case 0:
    
      //nu är vi i första barnet
      close(fd[READ]); //fd[0] = 3 pipe read och vi stänger den
      dup2(fd[WRITE], 1);
      int close_write = close(fd[1]); //fd[1] = 4 pipe write och vi stänger den

      if (close_write == -1) {
	perror("Close function failed for write end of pipe in parent");
	exit(EXIT_FAILURE);
      }
    
      execvp(argv[0], argv);
      
      perror("execvp");
      exit(EXIT_FAILURE);
    default:
      break;
    }
  }
  else if(child_NR == n - 1){ //nu är vi i sista barnet

    switch (pid = fork()) {
    case -1:
      fork_error();
    case 0:

      // FIX ME close alla tidigare!!!

      //nu är vi i andra barnet
      close(array_of_fd[n-2][WRITE]);
      dup2(array_of_fd[n-2][READ], 0);
    
      execvp(argv[0], argv);
      
      perror("execvp");
      exit(EXIT_FAILURE);
    default:
      break;
    }

    
    
  }
  else {

    switch (pid = fork()) {
    case -1:
      fork_error();
    case 0:

      //FIX ME close alla tidigare
    
      // pipen bakre
      close(array_of_fd[child_NR -1][WRITE]);
      dup2(array_of_fd[child_NR -1][READ], 0);

      // pipen främre
      close(array_of_fd[child_NR][READ]);
      dup2(array_of_fd[child_NR][WRITE], 1);

      
      execvp(argv[0], argv);
      
      perror("execvp");
      exit(EXIT_FAILURE);
    default:
      break;
    }
    
    
  }
 
}

void fork_cmd_for1(char* argv[], int fd[], int child_NR) {
  pid_t pid;
  switch (pid = fork()) {
  case -1:
    fork_error();
  case 0:
    if(child_NR == 0){
      //nu är vi i första barnet
      close(fd[READ]); //fd[0] = 3 pipe read och vi stänger den
      dup2(fd[WRITE], 1);
    } else if(child_NR == 1){
      //nu är vi i andra barnet
      close(fd[WRITE]);
      dup2(fd[READ], 0);
    }
    execvp(argv[0], argv);
      
    perror("execvp");
    exit(EXIT_FAILURE);
  default:
    break;
  }
}

void fork_cmds(char* argvs[MAX_COMMANDS][MAX_ARGV], int n, int fd[2]) {
  for (int i = 0; i < n-1; i++) {
    int return_pipe = pipe(fd);
    if (return_pipe == -1) {
      perror("Pipe creation failed!");
      exit(EXIT_FAILURE);
    }
    fork_cmd(argvs[i], fd, i, n);
  }
}

void fork_cmds_for1(char* argvs[MAX_COMMANDS][MAX_ARGV], int n, int fd[]) {
  for (int i = 0; i < n; i++) {
    fork_cmd_for1(argvs[i], fd, i);
  }
}

void get_line(char* buffer, size_t size) {
  int characters_read = getline(&buffer, &size, stdin);

  if (characters_read < 0) {
    perror("Could not read line");
  }
  
  buffer[strlen(buffer)-1] = '\0';
  
}

void wait_for_all_cmds_for1(int n, int fd[]) {
  // Not implemented yet!

  if (n==1) {

  } else {
    close(fd[0]);
    close(fd[1]);
  }

  for (int i = 0; i < n; i++) {
    wait(NULL);
  }
}

void wait_for_all_cmds(int n) {
  // Not implemented yet!

  /*  if (n==1) {

  } else {
    close(fd[0]);
    close(fd[1]);
  }*/

  for (int i = 0; i < n; i++) {
    wait(NULL);
  }
}
/*
void pipe_creation(int number_of_cmds,int array_of_fd[15][2]) {
  for (int i=0; i < (number_of_cmds - 1) ; i++){
    int return_pipe = pipe(array_of_fd[i]);

    if (return_pipe == -1) {
      perror("Pipe creation failed!");
      exit(EXIT_FAILURE);
    }
}
}
*/

int main() {

  //int array_of_fd[15][2];
  int fd[2];
  int n;
  char* argvs[MAX_COMMANDS][MAX_ARGV];
  size_t size = 128;
  char line[size];

  while(true) {
    printf(" >> ");
    get_line(line, size);

    n = parse(line, argvs);

    // Debug printouts.
    printf("%d commands parsed.\n", n);
    print_argvs(argvs);
    
    if (n==1) {
      fork_cmds_for1(argvs, n);
      wait_for_all_cmds_for1(n);
    }else { //nu vill vi göra pipes
      /*
      
      pipe_creation(n, array_of_fd);

      if (return_pipe == -1) {
	perror("Pipe creation failed!");
	exit(EXIT_FAILURE);
      }
      */
      fork_cmds(argvs, n, fd);
      wait_for_all_cmds(n);
      
    }  
  }
}
