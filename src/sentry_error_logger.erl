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

-module(sentry_error_logger).

-behaviour(gen_event).
-export([
	init/1,
	code_change/3,
	terminate/2,
	handle_call/2,
	handle_event/2,
	handle_info/2
]).

init(_) -> 
    {ok, []}.

handle_call({configure, NewKeys}, _State) ->
    {ok, ok, NewKeys}.

handle_event({error, _, {Pid, Format, Data}}, State) ->
    {ok, State};
handle_event({error_report, _, {Pid, Type, Report}}, State) ->
    {ok, State};

handle_event({warning_msg, _, {Pid, Format, Data}}, State) ->
    {ok, State};

handle_event({warning_report, _, {Pid, Type, Report}}, State) ->
    {ok, State};

handle_event(_, State) ->
    {ok, State}.

handle_info(_Msg, State) ->
    {ok, State}.

code_change(_Old, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
            
            