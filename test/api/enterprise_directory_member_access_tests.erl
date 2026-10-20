-module(enterprise_directory_member_access_tests).
-include_lib("eunit/include/eunit.hrl").

access_http_test_() ->
    {timeout, 900, fun() -> isolated_pg_fixture:run(fun run/0) end}.
run() ->
    S = intbe02_http_support:setup_all(),
    try
        enterprise_internal_audit_http_checks:reset(),
        ok = enterprise_internal_audit_http_checks:directory_sync(S),
        enterprise_directory_member_http_checks:run(S)
    after
        intbe02_http_support:teardown_all(S),
        inttest_marker_db:release(S)
    end.
