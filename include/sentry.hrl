
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

-define(SENTRY_VERSION, 5).
-define(MAX_ATTEMPTS, 4).
