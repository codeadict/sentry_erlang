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

-module(utils).

-export([
    user_agent/0,
    unix_timestamp/0,
    event_id/0,
    get_env/2,
    get_env/3
]).


-spec user_agent() -> iolist().
user_agent() ->
    "sentry_erlang/0.0.1".


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


get_env(App, Variable) ->
    get_env(App, Variable, "").
get_env(App, Variable, Default) when is_atom(Variable) ->
    get_env(App, term_to_binary(Variable), Default);
get_env(App, Variable, Default) ->
    case application:get_env(App, Variable) of
        {ok, Value} ->
             Value;
        _ ->
            case os:getenv(Variable) of
                false -> Default;
                Value -> Value
            end
    end.
