%% Copyright 2018 Dairon Medina Caro (http://dairon.org)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(sentry_client).

-include("sentry.hrl").

-export([
    send_event/1
]).

send_event(Event) ->
    SampleRate = 1,
    case sample_event(SampleRate) of
        true ->
            encode_and_send(Event);
        false ->
            unsampled    
    end.

try_request(Method, Url, Headers, Body) ->
    try_request(Method, Url, Headers, Body, 1).
try_request(_Method, _Url, _Headers, _Body, Attempt) when Attempt > ?MAX_ATTEMPTS ->
    error;
try_request(Method, Url, Headers, Body, Attempt) ->
    case request(Method, Url, Headers, Body) of
        {ok, Id} ->
          {ok, Id};
        _Error ->
          sleep(Attempt),
          try_request(Method, Url, Headers, Body, Attempt + 1)
    end.


encode_and_send(Event) ->
        Body = jsx:encode(Event),
        case get_headers_and_endpoint() of
            {Endpoint, Headers} ->
                try_request(post, Endpoint, Headers, Body);
            _ ->
                error
        end.

%% @doc 
%% Makes the HTTP request to Sentry using hackney.
%% Hackney options can be set via the `hackney_opts` configuration option.
%% @end
request(Method, Url, Headers, Payload) ->
    case hackney:request(Method, Url, Headers, Payload) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            Json = jsx:decode(Body, [return_maps]),
            {ok, maps:get(Json, <<"id">>)};
        {ok, Status, RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            ErrorHeader = proplists:get_value("X-Sentry-Error", RespHeaders, ""),
            ?log_api_error("~s~nReceived ~p from Sentry server: ~s", [Payload, Status, ErrorHeader]),
            error;
        {error, _} = Error  ->
            ?log_api_error("~p~n~s", [Error, Payload]),
            error
    end.


%% @doc
%% Get a Sentry DSN which is a URL like:
%%
%% {PROTOCOL}://{PUBLIC_KEY}:{SECRET_KEY}]@{HOST}/{PATH}{PROJECT_ID}
%% Reference https://docs.sentry.io/clientdev/overview/#parsing-the-dsn
%% @end
get_dsn(Dsn) when is_list(Dsn) ->
    get_dsn(iolist_to_binary(Dsn));
get_dsn(Dsn) when is_binary(Dsn) ->
    case http_uri:parse(Dsn) of
        {ok, {Proto, Userinfo, Host, Port, Path, _Rest}} ->
            [PublicKey, SecretKey] = binary:split(Userinfo, <<":">>),
            [_, BinProjectID] = binary:split(Path, <<"/">>),
            ProjectID = binary_to_integer(BinProjectID),
            Endpoint = iolist_to_binary([
                            atom_to_binary(Proto, latin1),
                            "://",
                            Host,
                            ":",
                            integer_to_list(Port),
                            "/api/",
                            integer_to_list(ProjectID),
                            "/store/"
                        ]),
            {Endpoint, PublicKey, SecretKey};
        _ ->
            ?log_api_error("Cannot send event because of invalid DSN", []),
            error
    end.

get_headers_and_endpoint() ->
    %% Todo: This is for development only for now.
    Dsn = "",
    case get_dsn(Dsn) of
        {Endpoint, PublicKey, SecretKey} ->
            {Endpoint, authorization_headers(PublicKey, SecretKey)};
      _ ->
        error
    end.


%% @doc 
%% Exponential sleep of 2^n seconds.
%% @end
sleep(Attempt) ->
    N = erlang:round(1008 * math:pow(2, Attempt)),
    timer:sleep(N).


authorization(PublicKey, Secret) ->
    Timestamp = utils:unix_timestamp(),
    Authorization = [
			"Sentry sentry_version=", ?SENTRY_VERSION,",",
			"sentry_client=", utils:user_agent(), ",",
			"sentry_timestamp=", Timestamp, ",",
			"sentry_key=", binary_to_list(PublicKey), ",",
			"sentry_secret=", binary_to_list(Secret)
    ],
    lists:concat(Authorization).

authorization_headers(PublicKey, Secret) ->
    %%error_logger:info_msg("~p ~n", [authorization(PublicKey, Secret)]),
    [{"User-Agent", sentry_client}, 
     {"X-Sentry-Auth", authorization(PublicKey, Secret)}].



sample_event(1) -> true;
sample_event(1.0) -> true;
sample_event(0) -> false;
sample_event(0.0) -> false;
sample_event(Rate) ->
    rand:uniform() < Rate.

