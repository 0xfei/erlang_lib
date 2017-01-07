#include <unistd.h>
#include <string.h>

#include "erl_nif.h"

extern int cstrcmp(char *s, char *t);
extern int cstrlen(char *s);

static ERL_NIF_TERM strlen_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int ret;
	char s[100];
	
	if (!enif_get_string(env, argv[0], s, 100, 1)) {
		return enif_make_badarg(env);
	}

	ret = strlen(s);

	return enif_make_int(env, ret);
}

static ERL_NIF_TERM strcmp_nif(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
	int ret;
	char s1[100], s2[100];

	if (!enif_get_string(env, argv[0], s1, 100, 1) || 
			!enif_get_string(env, argv[1], s2, 100, 1)) {
		return enif_make_badarg(env);
	}

	ret = strcmp(s1, s2);

	return enif_make_int(env, ret);
}

static ErlNifFunc nif_func[] = {
	{"cstrlen", 1, strlen_nif},
	{"cstrcmp", 2, strcmp_nif}
};

ERL_NIF_INIT(complex5, nif_func, NULL, NULL, NULL, NULL)

