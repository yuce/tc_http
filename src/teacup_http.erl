-module(teacup_http).

-export([connect/2,
         connect/3,
         get/2,
         get/3,
         get_sync/2,
         get_sync/3]).

%% == API

connect(Host, Port) ->
    connect(Host, Port, #{}).

connect(Host, Port, Opts) ->
    TeacupOpts = teacup_opts(Opts),
    {ok, C} = teacup:new(http@teacup, TeacupOpts),
    teacup:connect(C, Host, Port),
    {ok, C}.

get(Conn, Path) ->
    teacup:cast(Conn, {get, Path}).

get(Conn, Path, Opts) ->
    teacup:cast(Conn, {get, Path, Opts}).

get_sync(Conn, Path) ->
    teacup:call(Conn, {get, Path}).

get_sync(Conn, Path, Opts) ->
    teacup:call(Conn, {get, Path, Opts}).
%% == Internal

teacup_opts(Opts) ->
    Tls = maps:get(tls, Opts, false),
    #{transport => #{tls => Tls}}.

%% == Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_http_1_test() ->
    application:start(teacup),
    {ok, C} = teacup_http:connect(<<"httpbin.org">>, 80),
    teacup_http:get(C, <<"/headers">>),
    receive
        {http@teacup, C, {response, #{status_code := StatusCode}}} ->
            ?assertEqual(200, StatusCode)
    after 1000 ->
        ?assertEqual(true, false)
    end.

get_https_1_test() ->
    application:start(teacup),
    {ok, C} = teacup_http:connect(<<"httpbin.org">>, 443, #{tls => true}),
    teacup_http:get(C, <<"/headers">>),
    receive
        {http@teacup, C, {response, #{status_code := StatusCode}}} ->
            ?assertEqual(200, StatusCode)
    after 2000 ->
        ?assertEqual(true, false)
    end.

get_sync_http_1_test() ->
    application:start(teacup),
    {ok, C} = teacup_http:connect(<<"httpbin.org">>, 443, #{tls => true}),
    {ok, #{status_code := StatusCode}} = teacup_http:get_sync(C, <<"/headers">>),
    ?assertEqual(200, StatusCode).
    

-endif.