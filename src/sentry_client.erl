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

-include("sentry.hrl").


try_request(Method, Url, Headers, Body, Attempt) ->
    case request(Method, Url, Headers, Body) of
        {ok, Id} ->
          {ok, Id};
        _Error ->
          sleep(Attempt),
          try_request(Method, Url, Headers, Body, Attempt + 1)
    end.

%% @doc 
%% Makes the HTTP request to Sentry using hackney.
%% Hackney options can be set via the `hackney_opts` configuration option.
%% @end
request(Method, Url, Headers, Body) ->
    case hackney:request(Method, Url, Headers, Body) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            {ok, Json} = jsx:decode(Body),
            {ok, maps:get(Json, "id")};
        {ok, Status, RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            ErrorHeader = proplists:get_value("X-Sentry-Error", RespHeaders, ""),
            ?log_api_error("~s~nReceived ~p from Sentry server: ~s", [Body, Status, ErrorHeader]),
            error;
        {error, _} = Error  ->
            ?log_api_error("~p~n~s", [Error, Body]),
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
            {ok, {Endpoint, PublicKey, SecretKey}};
        _ ->
            ?log_api_error("Cannot send event because of invalid DSN", []),
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
        {<<"sentry_version">>, ?SENTRY_VERSION},
        {<<"sentry_client">>, utils:user_agent()},
        {<<"sentry_timestamp">>, Timestamp},
        {<<"sentry_key">>, PublicKey},
        {<<"sentry_secret">>, Secret}
    ],
    Authorization.

authorization_headers(PublicKey, Secret) ->
    [{"User-Agent", sentry_client}, 
     {"X-Sentry-Auth", authorization(PublicKey, Secret)}].



sample_event(1) -> true;
sample_event(1.0) -> true;
sample_event(0) -> false;
sample_event(0.0) -> false;
sample_event(Rate) ->
    rand:uniform() < Rate.

