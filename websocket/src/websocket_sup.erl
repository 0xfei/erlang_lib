%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	ok = mnesia:start(),
	?MODULE = ets:new(?MODULE, [
		ordered_set, public, named_table]),
	supervisor:start_link({local, ?MODULE}, ?MODULE, [?MODULE]).

%% supervisor.

init([Ref]) ->
	Procs = [#{id => ws_server, 
			   start => {ws_server, start_link, [Ref]},
			   restart => permanent},
			 #{id => ws_data,
			   start => {ws_data, start_link, []},
			   restart => permanent}
			],
	{ok, {{one_for_one, 10, 10}, Procs}}.

