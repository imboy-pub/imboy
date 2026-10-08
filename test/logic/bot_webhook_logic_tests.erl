-module(bot_webhook_logic_tests).

%% L-01：Bot webhook 外呼受 feature bot_webhook 守卫——
%% overseas_baseline 预设默认关闭（effective_features 无键或 false → 不外呼），
%% 编译包含 Bot 的版本中 community/enterprise 无显式覆盖时保持开放；
%% 未编译 Bot 的交付版本即使显式开启也不得外呼。

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
    {setup, fun() -> setup(#{effective => #{bot_webhook => false}}) end, fun teardown/1, fun(_) ->
        fun() ->
            R = bot_webhook_logic:push_message(1000000051, from_user(), msg()),
            ?assertEqual(ok, R),
            ?assertNot(meck:called(elib_async, async, ['_']))
        end
    end}.

missing_key_defaults_open_test_() ->
    {setup, fun() -> setup(#{effective => #{}}) end, fun teardown/1, fun(_) ->
        fun() ->
            R = bot_webhook_logic:push_message(1000000051, from_user(), msg()),
            ?assertEqual(ok, R),
            ?assertEqual(
                imboy_feature:compiled(bot_webhook),
                meck:called(elib_async, async, ['_'])
            )
        end
    end}.

explicit_true_opens_test_() ->
    {setup, fun() -> setup(#{effective => #{bot_webhook => true}}) end, fun teardown/1, fun(_) ->
        fun() ->
            R = bot_webhook_logic:push(1000000051, #{<<"event">> => <<"test">>}),
            ?assertEqual(ok, R),
            ?assertEqual(
                imboy_feature:compiled(bot_webhook),
                meck:called(elib_async, async, ['_'])
            )
        end
    end}.

signature_uses_contract_lowercase_hex_test() ->
    Secret = <<"fixture-secret">>,
    Payload = <<"1700000000\n{\"event\":\"message\"}">>,
    Mac = crypto:mac(hmac, sha256, Secret, Payload),
    ?assertEqual(
        <<"sha256=", (binary:encode_hex(Mac, lowercase))/binary>>,
        bot_webhook_logic:sign_payload(Secret, Payload)
    ).
