ERL_INTERFACE_INC = /usr/local/opt/erlang/lib/erlang/lib/erl_interface-3.9.2/include
ERL_DRIVER_INC = /usr/local/opt/erlang/lib/erlang/erts-8.2/include
ERL_INTERFACE_LIB = /usr/local/opt/erlang/lib/erlang/lib/erl_interface-3.9.2/lib
ERL_DRIVER_LIB = /usr/local/lib/erlang/erts-8.2/lib

CC = gcc
CPATH = -I${ERL_DRIVER_INC} -I${ERL_INTERFACE_INC} -L${ERL_DRIVER_LIB} -L${ERL_INTERFACE_LIB} 
CLIBS_OPT = -fpic -bundle -flat_namespace -undefined suppress
CPLUS_LIB = -lerl_interface -lei


cnif:
	$(CC) -o cnif.so $(CPATH) $(CLIBS_OPT) cnif.c
ei:
	$(CC) -o ei $(CPATH) comm.c ei.c $(CPLUS_LIB) 

port_driver:
	$(CC) -o port_driver.so $(CPATH) $(CLIBS_OPT) port_driver.c

cnodes:
	$(CC) -o cnodes $(CPATH) $(CPLUS_LIB) cnodes.c

cnodec:
	$(CC) -o cnodec $(CPATH) $(CPLUS_LIB) cnodec.c

clean:
	rm -rf ei port_driver cnodes cnodec *.so *.beam

