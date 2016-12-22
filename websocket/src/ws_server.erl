%% Feel free to use, reuse and abuse the code in this file.

-module(ws_server).
-behaviour(gen_server).

%% export
-export([start_link/1]).
-export([connect/1, disconnect/1, send_message/1]).

%% API
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TAB, ?MODULE).

-record(state, {
    ref :: any()
}).

-spec start_link(Ref::any()) -> {ok, pid()}.
start_link(Ref) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Ref, []).

-spec connect(pid()) -> ok.
connect(Pid) ->
    gen_server:cast(?MODULE, {connect, Pid}).

-spec disconnect(pid()) -> ok.
disconnect(Pid) ->
    gen_server:cast(?MODULE, {disconnect, Pid}).

-spec send_message({pid(), string()}) -> ok.
send_message({Pid,Msg}) ->
    gen_server:cast(?MODULE, {send_message, Pid, Msg}).


%% callback
init(Ref) ->
    {ok, #state{ref = Ref}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({connect, Pid}, State=#state{ref = Ref}) ->
    ets:insert(Ref, {Pid, pid}),
    {noreply, State};
handle_cast({disconnect, Pid}, State=#state{ref = Ref}) ->
    ets:delete(Ref, Pid),
    {noreply, State};
handle_cast({send_message, From, Msg}, State=#state{ref = Ref}) ->
    [erlang:send(Pid, {message, From, Msg}) ||
        Pid <- ets:select(Ref, [{{'$1','$2'}, [], ['$1']}])],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
