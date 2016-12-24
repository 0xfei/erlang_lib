%% mnesia database
-module(ws_data).

-include_lib("mnesia/src/mnesia.hrl").

-behaviour(gen_server).

%% export
-export([start_link/0]).
-export([insert_user/2, insert_user/3]).
-export([insert_msg/2]).
-export([remove_user/1]).
%-export([remove_msg/1]).

-export([dirty_read/3, dirty_read2/3,val/1]).

%% callback
-export([init/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3
		]).

-record(state, {msgid = 0 :: integer(), limitid = 200 :: integer()}).

-define(DEFAULT_PERIOD, 50000).

-record(user, {
		  pid,
		  name,
		  ctime,
		  mtime,
		  period,
		  msg_period
		 }).

-record(msg, {
		  id,
		  pid,
		  ctime,
		  data
		 }).


-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

-spec insert_user(pid(), string()) -> ok | any().
insert_user(Pid, Name) ->
	insert_user(Pid, Name, ?DEFAULT_PERIOD).

-spec insert_user(pid(), string(), integer()) -> ok | any().
insert_user(Pid, Name, Period) ->
	gen_server:cast(?MODULE, {insert_user, Pid, Name, Period}).

insert_msg(Pid, Data) ->
	gen_server:cast(?MODULE, {insert_msg, Pid, Data}).

remove_user(Pid) ->
	gen_server:cast(?MODULE, {remove_user, Pid}).

%% callback
-spec init(any()) -> {ok, #state{}}.
init([]) ->
	mnesia:create_table(user,
						[{attributes, record_info(fields, user)}]),
	mnesia:create_table(msg,
						[{attributes, record_info(fields, msg)}]),
	{ok, #state{msgid=1000}}.

-spec handle_call(any(), pid(), #state{}) -> {reply, ok, #state{}}.
handle_call(_P, _From, State) ->
	{reply, ok, State}.

-spec handle_cast({insert_user, pid(), string(), integer()},
				  #state{}) -> {noreply, #state{}}.
handle_cast({insert_user, Pid, Name, Period}, State) ->
	mnesia:dirty_write(#user{pid=Pid, name=Name, period=Period}),
	%%Fun = fun() ->
	%%			  User = #user{pid=Pid, name=Name, period=Period},
	%%			  mnesia:write(User)
	%%	  end,
	%%mnesia:transaction(Fun).
	{noreply, State};

handle_cast({insert_msg, Pid, Data}, State=#state{msgid=Id}) ->	
	mnesia:dirty_write(#msg{id=Id+1, pid=Pid, data=Data}),
	%%Fun = fun() ->
	%%			  Msg = #msg{pid = Pid, data=Data},
	%%			  mnesia:write(Msg)
	%%	  end,
	%%mnesia:transaction(Fun).
	{noreply, State#state{msgid=Id+1}};

handle_cast({remove_user, Pid}, State) ->
    K = mnesia:dirty_select(msg, [{#msg{id = '$1', pid=Pid, _='_'}, [], ['$1']}]),
    [mnesia:dirty_delete(msg, Id) || Id <- K],

	%case mnesia:dirty_index_read(msg, Pid, #msg.pid) of
	%	[#msg{} = RecMsg] ->
    %        mnesia:dirty_delete_object(RecMsg);
    %	K ->
	%		ok
	%end,
	mnesia:dirty_delete(user, Pid),
	{noreply, State}.


dirty_read(Tab, IxKey, Pos) ->
    ResList = mnesia:dirty_rpc(Tab, ?MODULE, dirty_read2,
        [Tab, IxKey, Pos]),
    io:format("~p",[ResList]).

dirty_read2(Tab, IxKey, Pos) ->
    % io:format("dirty_Read2 val index ~p~n", [val({Tab,  {index, Pos}})]),
    Ix = mnesia_index:get_index_table(Tab, Pos),
    io:format("get_index_table ~p~n", [Ix]),
    Keys = mnesia_index:db_match(Ix, {IxKey, '$1'}),
    mnesia_index:r_keys(Keys, Tab, []).

val(Var) ->
    case ?catch_val(Var) of
        {'EXIT', Reason} ->
            io:format("val catch_val EXIT ~p~n", [Reason]),
            mnesia_lib:other_val(Var, Reason);
        Value -> Value
    end.

-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
	{noreply, State}.

-spec terminate(any(), #state{}) -> {ok, #state{}}.
terminate(_Reason, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

