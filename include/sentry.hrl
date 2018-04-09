
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

-define(SENTRY_VERSION, 5).
-define(MAX_ATTEMPTS, 4).

-define(log_api_error(Format, Args), 
    error_logger:warning_msg("Failed to send Sentry event. ~n" ++ Format, Args)).
