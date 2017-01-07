#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include "ei.h"
#include "erl_interface.h"

#define BUFSIZE 1024

int main(int argc, char* argv[])
{
	int fd;
	struct in_addr addr;
	ErlConnect conn;

	int loop = 1;
	int got;
	unsigned char buf[BUFSIZE];
	ErlMessage emsg;

	ETERM *fromp, *tuplep, *resp;
	ETERM *fnp, *s1, *s2 = NULL;
	int res;

	erl_init(NULL, 0);

	addr.s_addr = inet_addr("192.168.0.4");
	if (erl_connect_xinit("192.168.0.4", "cnode", "cnode@192.168.0.4",
				&addr, "123456", 0) == -1)
		erl_err_quit("erl_connect_init error");

	if ((fd = erl_connect("e1@192.168.0.4")) < 0)
		erl_err_quit("erl_connect error");

	fprintf(stderr, "Connected to e1@192.168.0.4\n"); 

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
				if (s2) erl_free_term(s2);
				erl_free_term(resp);
			}
		}
	}
}

