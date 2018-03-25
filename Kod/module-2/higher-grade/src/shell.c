
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


void fork_cmd(char* argv[], int fd[], int child_NR) {
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

void fork_cmds(char* argvs[MAX_COMMANDS][MAX_ARGV], int n, int fd[]) {
  for (int i = 0; i < n; i++) {
    fork_cmd(argvs[i], fd, i);
  }
}

void get_line(char* buffer, size_t size) {
  int characters_read = getline(&buffer, &size, stdin);

  if (characters_read < 0) {
    perror("Could not read line");
  }
  
  buffer[strlen(buffer)-1] = '\0';
  
}

void wait_for_all_cmds(int n, int fd[]) {
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

int main() {
  int n;
  char* argvs[MAX_COMMANDS][MAX_ARGV];
  size_t size = 128;
  char line[size];
  int fd[2];
  
  while(true) {
    printf(" >> ");
    get_line(line, size);

    n = parse(line, argvs);

    // Debug printouts.
    printf("%d commands parsed.\n", n);
    print_argvs(argvs);
    
    if (n==1) {
      fork_cmds(argvs, n, fd);
      wait_for_all_cmds(n, fd);
    }else { //nu vill vi göra pipes
     

      int return_pipe = pipe(fd);

      if (return_pipe == -1) {
	perror("Pipe creation failed!");
	exit(EXIT_FAILURE);
      }
      
      fork_cmds(argvs, n, fd);
      wait_for_all_cmds(n, fd);
    }

   
  }

  exit(EXIT_SUCCESS);
}
