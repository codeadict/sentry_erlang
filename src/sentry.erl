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