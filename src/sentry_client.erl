-include("sentry.hrl").

%% @doc Makes the HTTP request to Sentry using hackney.
%% Hackney options can be set via the `hackney_opts` configuration option.
%% @end
request(Method, Url, Headers, Body) ->
    ok.

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

