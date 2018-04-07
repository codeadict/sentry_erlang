-include("sentry.hrl").


try_request(Method, Url, Headers, Body, Attempt) ->
    case request(Method, Url, Headers, Body) of
        {ok, Id} ->
          {ok, Id};
        _Error ->
          sleep(Attempt),
          try_request(Method, Url, Headers, Body, Attempt + 1)
    end.

%% @doc Makes the HTTP request to Sentry using hackney.
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

