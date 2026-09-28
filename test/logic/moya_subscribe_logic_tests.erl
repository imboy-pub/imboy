%% moya_subscribe_logic_tests
%% W3 服务端依赖 — 订阅消息下发编排测试（纯 logic 用例）。
%%
%% 全部外部依赖（config_ds / moya_subscribe_repo / sso_identity_repo /
%% moya_wechat_client）均 meck：零真实网络、零真实库、零真实密钥。
%% openid 用占位常量，仅用于验证「透传给 send 的就是反查到的 subject」，
%% 测试自身不产生任何 PII。
%%
%% 覆盖：
%%   report        —— 未配置模板 / 白名单命中落库（meck history 捕获参数）/
%%                    全不在白名单 / 畸形元素丢弃 / 非列表入参
%%   notify        —— 未配置 skipped / 正常链路（send 参数逐项断言）/
%%                    自定义 data 模板（平铺与嵌套两形态）/ 超长值截断 /
%%                    无额度 / 无微信身份 / 并发已消费 / send 失败 /
%%                    context 缺失 / guardians 错误 / 多监护人
%%   async         —— spawn 包装最终触发 send（轮询等待，非 sleep 死等）
%%   truncate_utf8 —— 短值不动 / 恰好上限 / 超限（unicode 字符数非字节数）/
%%                    非法 UTF-8 原样透传

-module(moya_subscribe_logic_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(TMPL, <<"tpl-abc123">>).
-define(SUBMISSION_ID, 12345).
-define(UID, 67890).
-define(UID2, 67891).
-define(GRANT_ID, 9001).
-define(OPENID, <<"oFAKE-openid-0001">>).

%%%===================================================================
%%% 夹具与 mock 构造
%%%===================================================================

ctx() ->
    #{
        <<"learner_id">> => 42,
        <<"learner_name">> => <<"小墨"/utf8>>,
        <<"task_title">> => <<"第三课横画练习"/utf8>>
    }.

%% 默认模板渲染结果：小墨的《第三课横画练习》已有点评（16 字符 ≤ 20 不截断）
default_thing1() ->
    <<"小墨的《第三课横画练习》已有点评"/utf8>>.

expected_page() ->
    <<
        "/packages/parent/submission-detail/submission-detail?id=12345&from=notify"
    >>.

%% report 用 mock：只含 config 与 insert_grants
report_mocks(TemplateId) ->
    [
        {config_ds, [
            {'env', 2, fun
                (wechat_mini_subscribe_template_id, _) -> TemplateId;
                (_, Default) -> Default
            end}
        ]},
        {moya_subscribe_repo, [
            {'insert_grants', 2, fun(_Uid, Ts) -> {ok, length(Ts)} end}
        ]}
    ].

%% notify 全链路 mock：每环节返回值可经 Opts 覆盖（缺省 = 全成功）
chain_mocks(Opts) ->
    TemplateId = maps:get(template, Opts, ?TMPL),
    DataCfg = maps:get(data_cfg, Opts, undefined),
    CtxRet = maps:get(ctx, Opts, {ok, ctx()}),
    GuardiansRet = maps:get(guardians, Opts, {ok, [?UID]}),
    GrantRet = maps:get(grant, Opts, {ok, ?GRANT_ID}),
    SubjectRet = maps:get(subject, Opts, {ok, [#{<<"subject">> => ?OPENID}]}),
    Consume = maps:get(consume, Opts, true),
    SendRet = maps:get(send, Opts, {ok, sent}),
    [
        {config_ds, [
            {'env', 2, fun
                (wechat_mini_subscribe_template_id, _) -> TemplateId;
                (wechat_mini_subscribe_data, _) -> DataCfg;
                (_, Default) -> Default
            end}
        ]},
        {moya_subscribe_repo, [
            {'notification_context', 1, fun(_Sid) -> CtxRet end},
            {'view_guardians', 1, fun(_Lid) -> GuardiansRet end},
            {'pending_grant', 2, fun(_Uid, _T) -> GrantRet end},
            {'consume_grant', 3, fun(_G, _U, _S) -> Consume end}
        ]},
        {sso_identity_repo, [
            {'find_subject_by_uid', 2, fun(_P, _U) -> SubjectRet end}
        ]},
        {moya_wechat_client, [
            {'subscribe_send', 4, fun(_O, _T, _D, _Pg) -> SendRet end}
        ]}
    ].

%% meck:history 条目为 {Pid, {Mod, Fun, Args}, Result} 三元组，其中 Args 是
%% 参数**列表**；转成元组后才能用 {Openid, Template, Data, Page} 模式解构
%% （send/4 恒四个参数，list_to_tuple 安全）。
send_calls() ->
    [
        list_to_tuple(Args)
     || {_Pid, {moya_wechat_client, subscribe_send, Args}, _R} <-
            meck:history(moya_wechat_client)
    ].

repo_calls(Fn) ->
    [
        Args
     || {_Pid, {moya_subscribe_repo, F, Args}, _R} <- meck:history(moya_subscribe_repo),
        F =:= Fn
    ].

%% 轮询等待 async spawn 完成 send（上限 1s，10ms 步进；非 sleep 死等）
wait_for_send_count(N) ->
    wait_for_send_count(N, 100).

wait_for_send_count(N, Attempts) when Attempts =< 0 ->
    ?assertEqual(N, length(send_calls()));
wait_for_send_count(N, Attempts) ->
    case length(send_calls()) >= N of
        true ->
            ?assertEqual(N, length(send_calls())),
            ok;
        false ->
            timer:sleep(10),
            wait_for_send_count(N, Attempts - 1)
    end.

%%%===================================================================
%%% report/2
%%%===================================================================

report_unconfigured_returns_zero_test_() ->
    ?WITH_MECKS(report_mocks(<<>>), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:report(?UID, [?TMPL])),
        ?assertEqual([], repo_calls(insert_grants))
    end).

report_whitelisted_inserts_grants_test_() ->
    ?WITH_MECKS(report_mocks(?TMPL), fun() ->
        %% 含重复 + 畸形元素（非 binary / 空串 / 超 64 字节 / 他模板）：
        %% 只保留白名单模板且去重
        Long = binary:copy(<<"x">>, 65),
        Input = [?TMPL, ?TMPL, 123, <<>>, Long, <<"tpl-other">>, [?TMPL]],
        ?assertEqual({ok, 1}, moya_subscribe_logic:report(?UID, Input)),
        ?assertEqual([[?UID, [?TMPL]]], repo_calls(insert_grants))
    end).

report_none_whitelisted_test_() ->
    ?WITH_MECKS(report_mocks(?TMPL), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:report(?UID, [<<"tpl-a">>, <<"tpl-b">>])),
        ?assertEqual([], repo_calls(insert_grants))
    end).

report_malformed_elements_dropped_test_() ->
    ?WITH_MECKS(report_mocks(?TMPL), fun() ->
        ?assertEqual(
            {ok, 0}, moya_subscribe_logic:report(?UID, [123, <<>>, atom_tpl, {<<"x">>, 1}])
        ),
        ?assertEqual([], repo_calls(insert_grants))
    end).

report_not_a_list_test_() ->
    ?WITH_MECKS(report_mocks(?TMPL), fun() ->
        ?assertEqual(
            {error, invalid_templates},
            moya_subscribe_logic:report(?UID, ?TMPL)
        ),
        ?assertEqual(
            {error, invalid_templates},
            moya_subscribe_logic:report(?UID, undefined)
        )
    end).

%%%===================================================================
%%% notify_review_published/1
%%%===================================================================

notify_unconfigured_skipped_test_() ->
    ?WITH_MECKS(chain_mocks(#{template => <<>>}), fun() ->
        ?assertEqual({ok, skipped}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls())
    end).

notify_full_chain_sends_test_() ->
    ?WITH_MECKS(chain_mocks(#{}), fun() ->
        ?assertEqual({ok, 1}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        %% send 参数逐项断言：openid 透传反查结果、模板、data、page
        [{Openid, Template, Data, Page}] = send_calls(),
        ?assertEqual(?OPENID, Openid),
        ?assertEqual(?TMPL, Template),
        ?assertEqual(#{<<"thing1">> => #{<<"value">> => default_thing1()}}, Data),
        ?assertEqual(expected_page(), Page),
        ?assertEqual([[?GRANT_ID, ?UID, ?SUBMISSION_ID]], repo_calls(consume_grant))
    end).

%% 配置了平铺形态的 data 模板：占位符替换后包成微信嵌套 value 结构
notify_custom_flat_data_template_test_() ->
    Cfg = #{<<"thing1">> => <<"{learner_name}《{task_title}》点评"/utf8>>},
    ?WITH_MECKS(chain_mocks(#{data_cfg => Cfg}), fun() ->
        ?assertEqual({ok, 1}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        [{_, _, Data, _}] = send_calls(),
        ?assertEqual(
            #{<<"thing1">> => #{<<"value">> => <<"小墨《第三课横画练习》点评"/utf8>>}},
            Data
        )
    end).

%% 配置了微信原生嵌套形态：保持嵌套，仅内层 value 做替换与截断
notify_custom_nested_data_template_test_() ->
    Cfg = #{<<"thing1">> => #{<<"value">> => <<"{learner_name}的回评已发布"/utf8>>}},
    ?WITH_MECKS(chain_mocks(#{data_cfg => Cfg}), fun() ->
        ?assertEqual({ok, 1}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        [{_, _, Data, _}] = send_calls(),
        ?assertEqual(
            #{<<"thing1">> => #{<<"value">> => <<"小墨的回评已发布"/utf8>>}},
            Data
        )
    end).

%% 超长值按 unicode 字符数截到 20（19 字符 + 省略号），非字节数
notify_long_value_truncated_test_() ->
    Cfg = #{<<"thing1">> => <<"一二三四五六七八九十一二三四五六七八九十一"/utf8>>},
    ?WITH_MECKS(chain_mocks(#{data_cfg => Cfg}), fun() ->
        ?assertEqual({ok, 1}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        [{_, _, Data, _}] = send_calls(),
        ?assertEqual(
            #{<<"thing1">> => #{<<"value">> => <<"一二三四五六七八九十一二三四五六七八九…"/utf8>>}},
            Data
        )
    end).

notify_no_grant_skips_test_() ->
    ?WITH_MECKS(chain_mocks(#{grant => {ok, undefined}}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls()),
        ?assertEqual([], repo_calls(consume_grant))
    end).

%% find_subject 无行：跳过且不消费额度（b 在 c 之前）
notify_no_subject_skips_test_() ->
    ?WITH_MECKS(chain_mocks(#{subject => {ok, []}}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls()),
        ?assertEqual([], repo_calls(consume_grant))
    end).

notify_consume_false_skips_test_() ->
    ?WITH_MECKS(chain_mocks(#{consume => false}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls())
    end).

%% send 失败：额度已消费不回滚，但该家长不计入实际通知人数
notify_send_failure_not_counted_test_() ->
    ?WITH_MECKS(chain_mocks(#{send => {error, send_failed}}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual(1, length(send_calls())),
        ?assertEqual([[?GRANT_ID, ?UID, ?SUBMISSION_ID]], repo_calls(consume_grant))
    end).

notify_context_missing_test_() ->
    ?WITH_MECKS(chain_mocks(#{ctx => {ok, undefined}}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls())
    end).

notify_context_error_test_() ->
    ?WITH_MECKS(chain_mocks(#{ctx => {error, db_down}}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls())
    end).

notify_guardians_error_test_() ->
    ?WITH_MECKS(chain_mocks(#{guardians => {error, db_down}}), fun() ->
        ?assertEqual({ok, 0}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual([], send_calls())
    end).

notify_multiple_guardians_test_() ->
    ?WITH_MECKS(chain_mocks(#{guardians => {ok, [?UID, ?UID2]}}), fun() ->
        ?assertEqual({ok, 2}, moya_subscribe_logic:notify_review_published(?SUBMISSION_ID)),
        ?assertEqual(2, length(send_calls()))
    end).

%%%===================================================================
%%% notify_review_published_async/1
%%%===================================================================

notify_async_eventually_sends_test_() ->
    ?WITH_MECKS(chain_mocks(#{}), fun() ->
        ?assertEqual(ok, moya_subscribe_logic:notify_review_published_async(?SUBMISSION_ID)),
        wait_for_send_count(1)
    end).

%%%===================================================================
%%% truncate_utf8/2 直测
%%%===================================================================

truncate_short_untouched_test() ->
    ?assertEqual(<<"abc">>, moya_subscribe_logic:truncate_utf8(<<"abc">>, 20)),
    ?assertEqual(<<>>, moya_subscribe_logic:truncate_utf8(<<>>, 20)).

%% 恰好 20 个 unicode 字符（60 字节）：不动——按字符数而非字节数判定
truncate_exactly_max_test() ->
    Exactly20 = binary:copy(<<"墨"/utf8>>, 20),
    ?assertEqual(Exactly20, moya_subscribe_logic:truncate_utf8(Exactly20, 20)).

truncate_over_max_test() ->
    %% ASCII 超限：19 字符 + 省略号 = 20
    ?assertEqual(
        <<"abcdefghijklmnopqrs…"/utf8>>,
        moya_subscribe_logic:truncate_utf8(<<"abcdefghijklmnopqrstu">>, 20)
    ),
    %% unicode 超限：19 个汉字 + 省略号
    Over21 = binary:copy(<<"墨"/utf8>>, 21),
    Expect = <<(binary:copy(<<"墨"/utf8>>, 19))/binary, "…"/utf8>>,
    ?assertEqual(Expect, moya_subscribe_logic:truncate_utf8(Over21, 20)),
    %% 上限为 1 时只剩省略号本身
    ?assertEqual(<<"…"/utf8>>, moya_subscribe_logic:truncate_utf8(<<"abc">>, 1)).

%% 非法 UTF-8 原样透传（不在截断处崩溃，微信侧自会拒发）
truncate_invalid_utf8_passthrough_test() ->
    Bad = <<255, 254, 253>>,
    ?assertEqual(Bad, moya_subscribe_logic:truncate_utf8(Bad, 20)).
