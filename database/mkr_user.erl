%% mkr storage module
-module(mkr_user).

%% API
-export([init_table/0]).

%% database tables
-record(user, {
    id,
    name,
    pass,
    salt,
    email,
    state
}).

-record(prof, {
    id,
    address,
    number,
    link
}).

-record(link, {
    id,
    weibo,
    twitter,
    github,
    zhihu,
    douban,
    weixin,
    other
}).

-spec init_table() -> ok.
init_table() ->

    ok.
