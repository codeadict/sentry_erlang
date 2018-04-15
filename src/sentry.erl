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

-module(sentry).
-include("sentry.hrl").

%% @doc
%% Creates an Event struct out of context collected and options
%%
%% Options:
%%  * `:exception` - expection
%%  * `:message` - message
%%  * `:stacktrace` - a list of Exception.stacktrace()
%%  * `:extra` - map of extra context
%%  * `:user` - map of user context
%%  * `:tags` - map of tags context
%%  * `:request` - map of request context
%%  * `:breadcrumbs` - list of breadcrumbs
%%  * `:level` - error level
%%  * `:fingerprint` -  list of the fingerprint for grouping this event
%% @end
-spec create_event() -> event(). 
create_event() ->
    #event{ event_id = utils:event_id()
        ,   platform = erlang
        ,   timestamp = utils:timestamp()
        }.