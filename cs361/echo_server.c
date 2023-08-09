#include "csapp.h"

typedef struct {
  int maxfd;
  fd_set read_set;
  fd_set ready_set;
  int nready;
  int maxi;
  int clientfd[FD_SETSIZE];
  rio_t clientrio[FD_SETSIZE];
} pool;

int byte_cnt = 0;

void printSockaddr(const struct sockaddr_storage* addr) {
    char ip[INET_ADDRSTRLEN];
    const struct sockaddr_in* addr4 = (const struct sockaddr_in*)addr;
    inet_ntop(AF_INET, &(addr4->sin_addr), ip, INET_ADDRSTRLEN);
    int port = ntohs(addr4->sin_port);
    fprintf(stderr, "IPv4: %s:%d\n", ip, port);
}

void init_pool(int listenfd, pool *p) {
  p->maxfd = listenfd;
  p->maxi = -1;

  for (int i = 0; i < FD_SETSIZE; ++i) {
    p->clientfd[i] = -1;
  }

  FD_ZERO(&p->read_set);
  FD_SET(listenfd, &p->read_set);
}

void add_client(int connfd, pool *p) {
    p->nready--;
    for(int i = 0; i < FD_SETSIZE; ++i)
    {
        if (p->clientfd[i] < 0)
        {
            p->clientfd[i] = connfd;
            Rio_readinitb(&p->clientrio[i], connfd);
            FD_SET(connfd, &p->read_set);
            
            if (connfd > p->maxfd)
            {
                p->maxfd = connfd;
            }

            if (i > p->maxi)
            {
                p->maxi = i;
            }
            break;
        }
    }
}

void check_clients(pool *p)
{
    char buf[RIO_BUFSIZE];
    rio_t rio;
    for(int i = 0; (i <= p->maxi) && (p->nready > 0); i++)
    {
        int connfd = p->clientfd[i];
        rio = p->clientrio[i];
        if ((connfd > 0) && FD_ISSET(connfd, &p->ready_set)) {
            p->nready--;
            int n;
            if ((n = Rio_readlineb(&rio, buf, RIO_BUFSIZE)) != 0)
            {
                byte_cnt += n;
                Rio_writen(connfd, buf, n);
            }
            else
            {
                Close(connfd);
                FD_CLR(connfd, &p->read_set);
                p->clientfd[i] = -1;
            }
        }
    }
}

int main(int argc, char **argv)
{ 
    if (argc != 2)
    {
        fprintf(stderr, "usage: %s <port>\n", argv[0]);
        exit(0);
    }
    int listenfd = Open_listenfd(argv[1]);
    pool *p = malloc(sizeof(pool));
    init_pool(listenfd, p);
    while(1)
    {
        p->ready_set = p->read_set;
        p->nready =  Select(p->maxfd+1, &p->ready_set, NULL, NULL, NULL);
        if (FD_ISSET(listenfd, &p->ready_set))
        {
            socklen_t clientlen;
            struct sockaddr_storage clientaddr;
            int connfd = Accept(listenfd, (SA*) &clientaddr, &clientlen);
            printSockaddr(&clientaddr);
            add_client(connfd, p);
        }
        check_clients(p);
    }
    return 0; 
}