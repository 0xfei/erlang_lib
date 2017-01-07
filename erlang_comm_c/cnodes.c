#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>

#include "ei.h"
#include "erl_interface.h"

#define BUFSIZE 1024

int listen_port(int port);

int main(int argc, char* argv[])
{
	int port, listen, fd;
	struct in_addr addr;
	ErlConnect conn;

	int loop = 1;
	int got;
	unsigned char buf[BUFSIZE];
	ErlMessage emsg;

	ETERM *fromp, *tuplep, *resp; 
	ETERM *fnp, *s1, *s2 = NULL;
	int res;

	port = atoi(argv[1]);

	erl_init(NULL, 0);

	addr.s_addr = inet_addr("192.168.0.4");
	if (erl_connect_xinit("192.168.0.4", "cnode", "cnode@192.168.0.4",
				&addr, "123456", 0) == -1)
		erl_err_quit("erl_connect_init error");

	if ((listen = listen_port(port)) <= 0)
		erl_err_quit("listen_port error");

	if (erl_publish(port) == -1)
		erl_err_quit("erl_publish error");

	if ((fd = erl_accept(listen, &conn)) == ERL_ERROR)
		erl_err_quit("erl_accept error");

	fprintf(stderr, "Connected to %s\n", conn.nodename);

	while (loop) {
		got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
		if (got == ERL_ERROR) {
			loop = 0;
		} else if (got == ERL_TICK) {
			// pass
		} else {
			if (emsg.type == ERL_REG_SEND) {
				fromp = erl_element(2, emsg.msg);
				tuplep = erl_element(3, emsg.msg);
				fnp = erl_element(1, tuplep);
				s1 = erl_element(2, tuplep);
				
				if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strlen", 6) == 0) {
					res = strlen(erl_iolist_to_string(s1)); 
				} else if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strcmp", 6) == 0) {
					s2 = erl_element(3, tuplep);
					res = strcmp(erl_iolist_to_string(s1), erl_iolist_to_string(s2));
				}

				resp = erl_format("{cnode, ~i}", res);
				erl_send(fd, fromp, resp);

				erl_free_term(emsg.from);
				erl_free_term(emsg.msg);
				erl_free_term(fromp);
				erl_free_term(tuplep);
				erl_free_term(fnp);
				erl_free_term(s1);
				erl_free_term(s2);
				erl_free_term(resp);
			}
		}
	}
}

int listen_port(int port) {
	int listen_fd;
	struct sockaddr_in addr;
	int on = 1;

	if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
		return -1;

	setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

	memset((void*)&addr, 0, sizeof(addr));
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);

	if (bind(listen_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0)
		return -1;

	listen(listen_fd, 5);
	return listen_fd;
}
