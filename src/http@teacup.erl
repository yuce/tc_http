% Copyright (c) 2016, Yuce Tekol <yucetekol@gmail.com>.
% All rights reserved.

% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are
% met:

% * Redistributions of source code must retain the above copyright
%   notice, this list of conditions and the following disclaimer.

% * Redistributions in binary form must reproduce the above copyright
%   notice, this list of conditions and the following disclaimer in the
%   documentation and/or other materials provided with the distribution.

% * The names of its contributors may not be used to endorse or promote
%   products derived from this software without specific prior written
%   permission.

% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(http@teacup).
-behaviour(teacup_server).

-export([teacup@init/1,
         teacup@data/2,
         teacup@cast/2,
         teacup@status/2,
         teacup@error/2]).
-export([safe_parse/3]).         

-define(MSG, ?MODULE).
-define(NL, "\r\n").

%% == Callbacks         

teacup@init(Opts) ->
    NewOpts = reset_response(Opts),
    {ok, NewOpts}.
    
teacup@status(Status, #{parent@ := Parent,
                        ref@ := Ref} = State) ->
    Parent ! {?MSG, Ref, {teacup@status, Status}},
    {noreply, State}.
    
teacup@error(Reason, #{parent@ := Parent,
                        ref@ := Ref} = State) ->
    Parent ! {?MSG, Ref, {teacup@error, Reason}},
    {noreply, State}.

teacup@data(Data, #{response := #{line := Line,
                                  headers := Headers,
                                  body := Body,
                                  rest := Rest} = Response} = State) ->
    NewData = <<Rest/binary, Data/binary>>,
    {Complete, NewLine, {NewHeaders, NewBody}, NewRest} =
        safe_parse(NewData, Line, {Headers, Body}), 
    NewResponse = Response#{line := NewLine,
                            headers := NewHeaders,
                            body := NewBody,
                            rest := NewRest},
    NewState = State#{response := NewResponse},
    case Complete of
        true ->
            % Clear the response once the parent is notified
            notify_parent(NewState),
            {onoreplyk, reset_response(NewState)};
        false ->
            {noreply, NewState}
    end.
    
teacup@cast({get, Url}, State) ->
    teacup@cast({get, Url, #{headers => #{}}}, State);

teacup@cast({get, Url, #{headers := Headers}},
            #{ref@ := Ref} = State) ->    
    EncodedUrl = urlencode(Url),
    RequestLine = make_request_line({get, EncodedUrl}, State),
    BinHeaders =  make_headers(Headers, State),
    RequestData = [RequestLine, BinHeaders, <<?NL>>],
    ok = teacup:send(Ref, RequestData),
    {noreply, State}.    

%% == Internal

reset_response(State) ->
    State#{response => #{line => undefined,
                         headers => undefined,
                         body => undefined,
                         rest => <<>>,
                         content_length => -1}}.

urlencode(Url) -> Url.
    
make_request_line({get, Url}, State) ->
    make_request_line(<<"GET">>, Url, State).
    
make_request_line(Method, Url, _State) ->
    [Method, <<" ">>, Url, <<" ">>, <<"HTTP/1.1">>, <<?NL>>].                             

make_headers(GivenHeaders, State) ->
    Headers = maps:to_list(maps:merge(default_headers(State), GivenHeaders)),
    lists:map(fun({K, V}) -> header(K, V) end, Headers).

default_headers(#{transport := #{host := Host}}) ->
    #{<<"host">> => Host}.
    
header(Name, Value) ->
    [Name, <<": ">>, Value, <<?NL>>].    

notify_parent(#{parent@ := Parent,
                ref@ := Ref,
                response := Response}) ->
    Res = make_response(Response),
    Parent ! {http@teacup, Ref, Res}.
    
make_response(#{line := {Http, StatusCode, StatusMsg},
                headers := Headers,
                body := Body}) ->
    #{http => Http,
      status_code => StatusCode,
      status_message => StatusMsg,
      headers => Headers,
      body => Body}.
      
safe_parse(Data, undefined, _) ->
    case safe_parse_status_line(Data) of
        {ok, undefined, Rest} ->
            {false, undefined, {undefined, undefined}, Rest};
        {ok, Line, Rest} ->
            safe_parse(Rest, Line, {undefined, undefined});
        Error ->
            Error
    end;
    
safe_parse(Data, Line, {undefined, undefined}) ->
    case safe_parse_headers(Data) of
        {ok, {undefined, undefined}, Rest} ->
            {false, Line, {undefined, undefined}, Rest};
        {ok, {Headers, Body}, Rest} ->
            safe_parse(Rest, Line, {Headers, Body});
        Error ->
            Error
    end;
    
safe_parse(Data, Line, {Headers, Body}) ->
    Body1 = <<Body/binary, Data/binary>>,
    ContentLength = content_length(Headers),
    BodySize = byte_size(Body1),
    {Complete, NewBody, NewRest} = if
        ContentLength =< 0 ->
            {true, <<>>, Body1};
        BodySize < ContentLength ->
            {false, Body1, <<>>};
        BodySize > ContentLength ->
            Body2 = binary:part(Body1, 0, ContentLength),
            Rest2 = binary:part(Body1, ContentLength, (BodySize - ContentLength)),  
            {true, Body2, Rest2};
        BodySize == ContentLength ->
            {true, Body1, <<>>}
    end,
    {Complete, Line, {Headers, NewBody}, NewRest}.

safe_parse_status_line(Data) ->
    try cow_http:parse_status_line(Data) of
        {Http, StatusCode, StatusMsg, Rest} ->
            {ok, {Http, StatusCode, StatusMsg}, Rest}
    catch
        error:_Error ->
            % search for \r\n to check whether the status line is complete
            case binary:match(Data, <<?NL>>) of
                nomatch ->
                    {ok, undefined, Data};
                _ ->
                    {error, status_line}
            end
    end.

safe_parse_headers(Data) ->
    try cow_http:parse_headers(Data) of
        {Headers, Body} ->
            {ok, {Headers, Body}, <<>>}
    catch
        error:_Error ->
            % search for \r\n to check whether the headers, body is complete
            case binary:match(Data, <<?NL, ?NL>>) of
                nomatch ->
                    {ok, {undefined, undefined}, Data};
                _ ->
                    {error, headers_body}
            end
    end.
    
content_length([]) -> -1;

content_length([{<<"content-length">>, BinLength} | _]) ->
    binary_to_integer(BinLength);

content_length([_|T]) -> content_length(T).
    
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

safe_parse_status_line_1_test() ->
    Data = <<"HTTP/1.1 200 OK\r\n\r\n">>,
    R = safe_parse_status_line(Data),
    E = {ok, {'HTTP/1.1', 200, <<"OK">>}, <<"\r\n">>},
    ?assertEqual(E, R).

safe_parse_status_line_2_test() ->
    Data = <<"HTTP/1.1">>,
    R = safe_parse_status_line(Data),
    E = {ok, undefined, <<"HTTP/1.1">>},
    ?assertEqual(E, R).

safe_parse_status_line_3_test() ->
    Data = <<"HTTP/5 200 OK\r\n\r\n">>,
    R = safe_parse_status_line(Data),
    E = {error, status_line},
    ?assertEqual(E, R).
    
safe_parse_headers_1_test() ->
    Data = <<"Host: foo\r\n\r\nFOO">>,
    R = safe_parse_headers(Data),
    E = {ok, {[{<<"host">>, <<"foo">>}], <<"FOO">>}, <<>>},
    ?assertEqual(E, R).

safe_parse_headers_2_test() ->
    Data = <<"Host: foo\r\n">>,
    R = safe_parse_headers(Data),
    E = {ok, {undefined, undefined}, <<"Host: foo\r\n">>},
    ?assertEqual(E, R).

content_length_1_test() ->
    Headers = [{<<"server">>,<<"nginx">>},
                {<<"date">>,<<"Wed, 16 Mar 2016 12:50:03 GMT">>},
                {<<"content-type">>,<<"text/html">>},
                {<<"content-length">>,<<"12688">>},
                {<<"connection">>,<<"keep-alive">>},
                {<<"vary">>,<<"Accept-Encoding">>}],
    ?assertEqual(12688, content_length(Headers)).                

content_length_2_test() ->
    Headers = [{<<"server">>,<<"nginx">>},
                {<<"date">>,<<"Wed, 16 Mar 2016 12:50:03 GMT">>},
                {<<"content-type">>,<<"text/html">>},
                {<<"connection">>,<<"keep-alive">>},
                {<<"vary">>,<<"Accept-Encoding">>}],
    ?assertEqual(-1, content_length(Headers)).                

headers_1_test() ->
    State = #{transport => #{host => <<"github.com">>}},
    R = make_headers(#{}, State),
    E = [[<<"host">>, <<": ">>, <<"github.com">>, <<?NL>>]],
    ?assertEqual(E, R).

headers_2_test() ->
    State = #{transport => #{host => <<"github.com">>}},
    R = make_headers(#{<<"host">> => <<"bitbucket.com">>,
                       <<"content-type">> => <<"application/json">>}, State),
    E = [[<<"content-type">>, <<": ">>, <<"application/json">>, <<?NL>>],
         [<<"host">>, <<": ">>, <<"bitbucket.com">>, <<?NL>>]],
    ?assertEqual(E, R).
    
-endif.
