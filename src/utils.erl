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
	U0 = rand:uniform((2 bsl 32) - 1),
	U1 = rand:uniform((2 bsl 16) - 1),
	U2 = rand:uniform((2 bsl 12) - 1),
	U3 = rand:uniform((2 bsl 32) - 1),
	U4 = rand:uniform((2 bsl 30) - 1),
    <<UUID:128>> = <<U0:32, U1:16, 4:4, U2:12, 2#10:2, U3:32, U4:30>>,
    iolist_to_binary(io_lib:format("~32.16.0b", [UUID])).