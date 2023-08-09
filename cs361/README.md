# Terminology
* fd_set object 
  * a data structure to implement a set of file descriptors for I/O operations.
* FD_SETSIZE (the number of open file a process is able to open)
  * the maximum number of file descriptor that can be stored in an fd_set object
* FD_ZERO :: fd_set -> void
  * taken a pointer to FD_SET object
  * init
* FD_SET :: fd -> fd_set -> void
  * include the fd in the fd_set
* FD_ISSET :: fd -> fd_set -> bool
  * indicet whether a fd is ready for I/O operation
* FD_CLR :: fd -> fd_set -> void
  * remove the file descript from the fd_set
* Open_listenfd (from csapp)
  * getaddrinfo: translate hostnames to corresponding network address
  * socket() : create a new socket 
  * bind() : associate a socket with a network address
  * listen() : make the socket a listen socket
* select
  * return value indicate the total number of file descriptors that are ready
  * readfds ready for read
  * side effects of select on FD_SET
    * modify the status fd in FS_SET to indicate they're ready
    * clearance: for those FDs that're not ready, those file descriptor will be cleared from the FD_SET
  * due to side effect of select, `ready_set = read_set`
* accept
  * used to accept a newincoming connection on a listening socket
  * return value indicate the file descriptor of the new socket created for the accepted connection
* readability
  * refers to the state of a file descriptor or socket where there is data available to read from it without blocking
* in C
  * array are copied by value when performing a copy assignment
* read syscall
* command line tool
  * nc
* state machine
* why using static?
  * put the variable directly on stack will casuse segmentation fault