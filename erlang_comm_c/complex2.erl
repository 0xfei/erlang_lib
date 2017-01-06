%% simple test for string operation with c
-module(complex2).
-export([start/1, stop/0, init/1]).
-export([strlen/1, strcmp/2]).

start(Prog) ->
    spawn(?MODULE, init, [Prog]).

stop() ->
    ?MODULE ! stop.

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

init(Prog) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    Port = open_port({spawn, Prog}, [{packet, 2}, binary]),
    loop(Port).

loop(Port) ->
    receive
        {call, From, Msg} ->
            Port ! {self(), {command, term_to_binary(Msg)}},
            receive
                {Port, {data, Data}} ->
                    From ! {?MODULE, binary_to_term(Data)}
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

