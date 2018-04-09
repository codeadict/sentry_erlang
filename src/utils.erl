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


%% @doc
%% Generates and hexadecimal uuid4 used for the Sentry event_id
%% @end
-spec event_id() -> binary(). 
event_id() ->
    <<U0:32, U1:16, _:4, U2:12, _:2, U3:30, U4:32>> = crypto:strong_rand_bytes(16),
    <<UUID:128>> = <<U0:32, U1:16, 4:4, U2:12, 2#10:2, U3:32, U4:30>>,
    iolist_to_binary(io_lib:format("~32.16.0b", [UUID])).