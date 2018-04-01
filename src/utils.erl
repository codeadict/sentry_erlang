-module(utils).


-spec user_agent() -> iolist().
user_agent() ->
    {ok, Version} = application:get_key(sentry_erlang, vsn),
    ["sentry_erlang/", Version].


%% @doc 
%% Generates a unix timestamp.
%% @end
unix_timestamp() ->
    os:system_time(seconds).