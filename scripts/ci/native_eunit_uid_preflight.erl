%% This adapter selects existing test descriptors; no assertion or fixture changes.
-module(native_eunit_uid_preflight).
-export([run/0]).
run() ->
    Expected = ["ordinary/disk/provider_bypass_writer_denied",
                "enterprise_internal/disk/provider_bypass_writer_denied",
                "enterprise_seat/disk/provider_bypass_writer_denied",
                "cs_widget/disk/provider_bypass_writer_denied"],
    {setup, Setup, Cleanup, Instantiate} =
        attachment_lifecycle_v5_w403_driver_matrix_tests:w403_driver_matrix_test_(),
    Tests = {setup, Setup, Cleanup, fun(State) ->
        Selected = [T || {Name, _} = T <- Instantiate(State), lists:member(Name, Expected)],
        Sorted = lists:sort(Expected),
        Sorted = lists:sort([Name || {Name, _} <- Selected]),
        4 = length(Selected),
        Selected
    end},
    ReportDir = os:getenv("NATIVE_UID_XML"),
    case eunit:test(Tests, [verbose, {report, {eunit_surefire, [{dir, ReportDir}]}}]) of
        ok -> halt(0);
        error -> halt(2)
    end.
