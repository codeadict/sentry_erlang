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

-record(event, {
    event_id=nil,
    culpirit=nil,
    timestamp=nil,
    message=nil,
    tags=#{},
    level=error,
    platform=erlang,
    server_name=nil,
    environment=nil,
    exception=nil,
    release=nil,
    stacktrace=#{frames =>  []},
    request=#{},
    extra=#{},
    user=#{},
    breadcrumbs=[],
    fingerprint=[],
    modules=#{}
}).

-type event() :: #event{}.
-export_type([event/0]).

-define(SENTRY_VERSION, 7).
-define(MAX_ATTEMPTS, 4).

-define(log_api_error(Format, Args), 
    error_logger:warning_msg("Failed to send Sentry event. ~n" ++ Format, Args)).
