%% mnesia operations
-module(mnesia_opt).

-export([mnesia_start/0]).
-export([create_table/0, create_table/2]).
-export([insert_user/2]).
-export([query_number/1]).
-export([query_user/1]).

-record(user, {id, name, phone_number, ctime}).

mnesia_start() ->
    %mnesia:delete_schema([node()]),
    mnesia:start().

-spec create_table() -> any().
create_table() ->
    create_table(user, record_info(fields, user)).
create_table(Name, Info) ->
    mnesia:create_table(Name, [{index,[name]}, {attributes, Info}]),
    mnesia:add_table_index(Name, #user.phone_number).

-spec insert_user(integer(), list()) -> any().
insert_user(Id, Name) ->
    mnesia:dirty_write(user, #user{id=Id, name=Name, ctime=erlang:universaltime(), phone_number=12345}).

-spec query_user(integer() | list()) -> {error, not_found_user} | any().
query_user(Id) when is_integer(Id) ->
    % wrong! index can just be used as secondary key.
    Fun = fun() ->
        mnesia:index_read(user, Id, #user.id)
        end,
    mnesia:transaction(Fun);
    %mnesia:dirty_index_read(user, Id, #user.id);
query_user(Name) when is_list(Name) ->
    mnesia:dirty_index_read(user, Name, #user.name);
query_user(_Name) ->
    {error, not_found_user}.

query_number(Number) ->
    mnesia:dirty_index_read(user, Number, #user.phone_number).
