%% Feel free to use, reuse and abuse the code in this file.

-module(ws_server).
-behaviour(gen_server).

%% export
-export([start_link/1]).
-export([connect/1, disconnect/1, send_message/1, name/1]).

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

-spec name(pid()) -> binary().
name(Pid) ->
    {ok, Name} = gen_server:call(?MODULE, {Pid, name}),
    Name.

%% callback
init(Ref) ->
    {ok, #state{ref = Ref}}.

handle_call({Pid,name}, _From, State=#state{ref = Ref}) ->
    try
        Name = ets:lookup_element(Ref, {Pid,name}, 2),
        {reply, {ok, Name}, State}
    catch
        _ ->
           {reply, {ok, <<"Nobody">>}, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({connect, Pid}, State=#state{ref = Ref}) ->
    ets:insert(Ref, {{Pid, pid}, Pid}),
    ets:insert(Ref, {{Pid, name}, random_name()}),
    {noreply, State};

handle_cast({disconnect, Pid}, State=#state{ref = Ref}) ->
    ets:delete(Ref, {Pid,pid}),
    ets:delete(Ref, {Pid, name}),
    {noreply, State};

handle_cast({send_message, From, Msg}, State=#state{ref = Ref}) ->
    [erlang:send(Pid, {message, From, Msg}) ||
        [Pid] <- ets:match(Ref, {{'$1', pid}, '$1'})],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% random name
random_name() ->
    integer_to_binary(rand:uniform(16#ffff)).
