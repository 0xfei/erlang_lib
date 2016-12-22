-module(ws_handler).

-export([init/2]).
-export([terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {pid::pid()}).

init(Req, _Opts) ->
	{cowboy_websocket, Req, #state{pid=self()}}.

websocket_init(State) ->
	ws_server:connect(self()),
	erlang:start_timer(1000, self(), <<"Hello! I'm in!">>),
	{ok, State#state{pid=self()}}.

websocket_handle({text, Msg}, State) ->
	ws_server:send_message({self(), Msg}),
	{ok, State};
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(600000, self(), <<"How' you doin'?">>),
	ws_server:send_message({self(), Msg}),
	{ok, State};

%% message response
websocket_info({message, Pid, Msg}, State) when Pid =:= self() ->
	Show = <<"[ME]  ", Msg/binary>>,
	{reply, {text, Show}, State};
websocket_info({message, _Pid, Msg}, State) ->
	%% here pid() cnanot add to binary
	%% and Msg must be binary
	Show = <<"[Ta]  ", Msg/binary>>,
	{reply, {text, Show}, State};
websocket_info(_Info, State) ->
	io:format("websocket_info ~p~n", [_Info]),
	{ok, State}.

terminate(_Reason, _Req, State=#state{pid=Pid}) ->
	ws_server:disconnect(Pid),
	{ok, State}.
