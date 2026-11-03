%% IMBOY_ERLANG_QUALITY P2-09：CT fail-closed 判据器（CT04）。
-module(imboy_quality_ct).

-export([run_suites/2, classify/1]).

run_suites(Suites, TestDir) ->
    [run_one(S, TestDir) || S <- Suites].

run_one(S, TestDir) ->
    {S, classify(ct:run_test([{dir, TestDir}, {suite, atom_to_list(S)}]))}.

classify({Ok, Failed, {UserSkipped, AutoSkipped}}) when
    is_integer(Ok),
    is_integer(Failed),
    is_integer(UserSkipped),
    is_integer(AutoSkipped)
->
    classify_counts(Ok, UserSkipped + AutoSkipped, Failed);
classify({Ok, Skipped, Failed}) when
    is_integer(Ok),
    is_integer(Skipped),
    is_integer(Failed)
->
    classify_counts(Ok, Skipped, Failed);
classify({error, Reason}) ->
    {fail, {ct_error, Reason}};
classify(Other) ->
    {fail, {unexpected_ct_result, Other}}.

classify_counts(Ok, Skipped, Failed) ->
    Total = Ok + Skipped + Failed,
    if
        Total =:= 0 ->
            {fail, zero_testcases};
        Failed > 0 ->
            {fail, {failed, Failed}};
        Skipped > 0 ->
            {fail, {auto_or_user_skip, Skipped}};
        true ->
            pass
    end.
