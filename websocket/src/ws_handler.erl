-module(ws_handler).

-export([init/2]).
-export([terminate/3]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {pid::pid()}).

init(Req, _Opts) ->
	{cowboy_websocket, Req, #state{pid=self()}}.

websocket_init(State=#state{pid=Pid}) ->
	ws_server:connect(Pid),
	erlang:start_timer(1000, Pid, <<"Hello! I'm in!">>),
	{ok, State}.

websocket_handle({text, Msg}, State) ->
	ws_server:send_message(Msg),
	{ok, State};
websocket_handle(_Data, State) ->
	{ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
	erlang:start_timer(60000, self(), <<"How' you doin'?">>),
	ws_server:send_message(Msg),
	{ok, State};

%% message response
websocket_info({message, Pid, Msg}, State) when Pid =:= self() ->
	Show = <<"[ME]", Msg>>,
	{reply, {text, Show}, State};
websocket_info({message, Pid, Msg}, State) ->
	Show = <<"[", Pid, "]", Msg>>,
	{reply, {text, Show}, State};
websocket_info(_Info, State) ->
	{ok, State}.

terminate(_Reason, _Req, State=#state{pid=Pid}) ->
	ws_server:disconnect(Pid),
	{ok, State}.
