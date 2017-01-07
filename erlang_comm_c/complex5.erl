-module(complex5).
-export([cstrlen/1, cstrcmp/2]).

-on_load(init/0).

init() ->
	ok = erlang:load_nif("./cnif", 0).

cstrlen(_S) ->
	exit(nif_library_not_loaded).

cstrcmp(_S, _T) ->
	exit(nif_library_not_loaded).

