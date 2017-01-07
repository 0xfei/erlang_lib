%% simple test for string operation with c
-module(complex4).
-export([strlen/1, strcmp/2]).

-define(CNODE, 'cnode@192.168.0.4').

strlen(S) ->
    call_port({strlen, S}).
strcmp(S, T) ->
    call_port({strcmp, S, T}).

call_port(Msg) ->
	{any, ?CNODE} ! {call, self(), Msg},
    receive
        {cnode, Result} ->
            Result
    end.

