%% simple test for string operation with c
-module(complex3).
-export([start/1, stop/0, init/1]).
-export([strlen/1, strcmp/2]).

start(ProgLib) ->
	case erl_ddll:load_driver("./", ProgLib) of
		ok ->
			ok;
		{error, already_loaded} ->
			ok;
		Reason ->
			io:format("error: ~p~n", [Reason]),
			exit({error, could_not_load_driver})
	end,
    spawn(?MODULE, init, [ProgLib]).

stop() ->
    ?MODULE ! stop.

init(Prog) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Prog}, []),
    loop(Port).

strlen(S) ->
    call_port({strlen, S}).
strcmp(S, T) ->
    call_port({strcmp, S, T}).

call_port(Msg) ->
    ?MODULE ! {call, self(), Msg},
    receive
        {?MODULE, Result} ->
            Result
    end.

loop(Port) ->
    receive
        {call, From, Msg} ->
            Port ! {self(), {command, encode(Msg)}},
            receive
                {Port, {data, Data}} ->
                    From ! {?MODULE, decode(Data)}
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, _Reason} ->
            exit(port_exit_error)
    end.

encode({strlen, X}) -> [1, list_to_binary(X)];
encode({strcmp, X, Y}) -> [2, list_to_binary(X), 0, list_to_binary(Y), 0].

decode([Int]) -> Int.
