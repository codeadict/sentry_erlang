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

-export([
    capture_exception/2
]).

-spec capture_exception(string() | binary(), [parameter()]) -> ok.
-type parameter() ::
	{stacktrace, [stackframe()]} |
	{exception, {exit | error | throw, term()}} |
	{atom(), binary() | integer()}.
-type stackframe() ::
	{module(), atom(), non_neg_integer() | [term()]} |
	{module(), atom(), non_neg_integer() | [term()], [{atom(), term()}]}.
capture_exception(Message, Params) when is_list(Message) ->
    capture_exception(unicode:characters_to_binary(Message), Params);
capture_exception(Message, _Params0) ->
    Event = #{
        event_id => utils:event_id(),
        platform => erlang,
        server_name => node(),
        timestamp => utils:unix_timestamp(),
        message => term_to_json(Message)
    },
    sentry_client:send_event(Event).

frame_to_json({Module, Function, Arguments}) ->
	frame_to_json({Module, Function, Arguments, []});
frame_to_json({Module, Function, Arguments, Location}) ->
	Arity = case is_list(Arguments) of
		true -> length(Arguments);
		false -> Arguments
	end,
	Line = case lists:keyfind(line, 1, Location) of
		false -> -1;
		{line, L} -> L
	end,
	{
		case is_list(Arguments) of
			true -> [{vars, [iolist_to_binary(io_lib:format("~w", [Argument])) || Argument <- Arguments]}];
			false -> []
		end ++ [
			{module, Module},
			{function, <<(atom_to_binary(Function, utf8))/binary, "/", (list_to_binary(integer_to_list(Arity)))/binary>>},
			{lineno, Line},
			{filename, case lists:keyfind(file, 1, Location) of
				false -> <<(atom_to_binary(Module, utf8))/binary, ".erl">>;
				{file, File} -> list_to_binary(File)
			end}
		]
	}.

term_to_json(Term) when is_binary(Term); is_atom(Term) ->
	Term;
term_to_json(Term) ->
    iolist_to_binary(io_lib:format("~120p", [Term])).
        
