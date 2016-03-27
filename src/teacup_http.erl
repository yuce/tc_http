-module(teacup_http).

-export([connect/2,
         connect/3,
         get/2,
         get/3]).

%% == API

connect(Host, Port) ->
    connect(Host, Port, #{}).

connect(Host, Port, Opts) ->
    TeacupOpts = teacup_opts(Opts),
    {ok, C} = teacup:new(http@teacup, TeacupOpts),
    teacup:connect(C, Host, Port),
    {ok, C}.

get(Conn, Url) ->
    teacup:cast(Conn, {get, Url}).

get(Conn, Url, Opts) ->
    teacup:cast(Conn, {get, Url, Opts}).

%% == Internal

teacup_opts(#{tls := Tls}) ->
    #{transport => #{tls => Tls}};

teacup_opts(_) ->
    #{transport => #{tls => false}}.

%% == Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_1_test() ->
    ok = application:start(teacup),
    {ok, C} = teacup_http:connect(<<"httpbin.org">>, 80),
    teacup_http:get(C, <<"/headers">>),
    receive
        {http@teacup, C, _Response} ->
            ok
    after 1000 ->
        ?assertEqual(true, false)
    end,
    ok = application:stop(teacup).

-endif.