CC = clang
CFLAGS = -g
LDFLAGS = -lpthread

all: proxy

csapp.o: csapp.c csapp.h
	$(CC) $(CFLAGS) -c csapp.c

server.o: server.c csapp.h
	$(CC) $(CFLAGS) -c server.c

server: server.o csapp.o
	$(CC) $(CFLAGS) server.o csapp.o -o server $(LDFLAGS)

echo.o: echo.c csapp.h
	$(CC) $(CFLAGS) -c echo.c

echo: echo.o csapp.o
	$(CC) $(CFLAGS) echo.o csapp.o -o echo $(LDFLAGS)

echo_server.o: echo_server.c csapp.h
	$(CC) $(CFLAGS) -c echo_server.c

echo_server: echo_server.o csapp.o
	$(CC) $(CFLAGS) echo_server.o csapp.o -o echo_server $(LDFLAGS)

clean:
	rm -f *.o server echo