#include <unistd.h>
#include <string.h>
#include "comm.h"
#include "erl_interface.h"
#include "ei.h"


int main()
{
	ETERM *tuplep, *intp;
	ETERM *fnp, *argp, *sp1, *sp2;

	byte buf[100] = {0};
	int res, allocated, freed;

	erl_init(NULL, 0);

    while (read_cmd(buf) > 0) {
		tuplep = erl_decode(buf);
		fnp = erl_element(1, tuplep);
		sp1 = erl_element(2, tuplep);

		if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strlen", 6) == 0) {
			res = strlen(erl_iolist_to_string(sp1));
		} else if (strncmp((const char*)ERL_ATOM_PTR(fnp), "strcmp", 6) == 0) {
			sp2 = erl_element(3, tuplep);
			res = strcmp(erl_iolist_to_string(sp1), erl_iolist_to_string(sp2));
		}

    	intp = erl_mk_int(res);
    	erl_encode(intp, buf);
   		write_cmd(buf, erl_term_len(intp));

    	erl_free_compound(tuplep);
    	erl_free_term(fnp);
    	erl_free_term(argp);
    	erl_free_term(intp);
	}
    return 0;
}
