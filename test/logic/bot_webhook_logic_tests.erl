-module(bot_webhook_logic_tests).

%% L-01：Bot webhook 外呼受 feature bot_webhook 守卫——
%% overseas_baseline 预设默认关闭（effective_features 无键或 false → 不外呼），
%% community/enterprise 无显式覆盖时保持开放。

-include_lib("eunit/include/eunit.hrl").

setup(Opts) ->
    meck:new(imboy_policy, [no_link]),
    meck:new(elib_async, [no_link]),
    meck:expect(elib_async, async, fun(_F) -> ok end),
    Effective = maps:get(effective, Opts, #{<<"bot_webhook">> => true}),
    meck:expect(imboy_policy, effective_features, fun() -> Effective end),
    ok.

teardown(_) ->
    meck:unload(imboy_policy),
    meck:unload(elib_async),
    ok.

from_user() -> #{<<"user_id">> => 55, <<"nickname">> => <<"tester">>}.

msg() ->
    #{
        <<"chat_id">> => 1000000051,
        <<"msg_id">> => <<"m-1">>,
        <<"msg_type">> => <<"text">>,
        <<"text">> => <<"hi">>
    }.

overseas_blocks_push_test_() ->
    {"overseas 语义（bot_webhook=false）→ push_message 不进入异步外呼", fun() ->
        setup(#{effective => #{bot_webhook => false}}),
        R = bot_webhook_logic:push_message(1000000051, from_user(), msg()),
        ?assertEqual(ok, R),
        ?assertNot(meck:called(elib_async, async, ['_'])),
        teardown(ok)
    end}.

missing_key_defaults_open_test_() ->
    {"community/enterprise 语义（无该键）→ 默认放行进入异步外呼", fun() ->
        setup(#{effective => #{}}),
        R = bot_webhook_logic:push_message(1000000051, from_user(), msg()),
        ?assertEqual(ok, R),
        ?assert(meck:called(elib_async, async, ['_'])),
        teardown(ok)
    end}.

explicit_true_opens_test_() ->
    {"bot_webhook=true → push 异步外呼", fun() ->
        setup(#{effective => #{bot_webhook => true}}),
        R = bot_webhook_logic:push(1000000051, #{<<"event">> => <<"test">>}),
        ?assertEqual(ok, R),
        ?assert(meck:called(elib_async, async, ['_'])),
        teardown(ok)
    end}.
