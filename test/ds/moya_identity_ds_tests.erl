%% moya_identity_ds_tests
%% 首登自动开户事务编排层单测（2026-09-20 试点方案 B）。
%%
%% 这一层的核心风险是**静默失败**，故断言都围绕「有没有真回滚 / 有没有多写」：
%%   ① 建号失败、绑身份失败、锁失败、account 号冲突、查映射失败 —— 五种失败都必须
%%      让调用方看到 {error, _}，绝不能留下「user 行已提交但映射没写」的半成品；
%%   ② 映射已存在时必须幂等短路，不重复建号；
%%   ③ advisory 锁必须排在复查之前（否则并发首登的复查形同虚设）；
%%   ④ 开户层构造的 user 行：必须带 account（uk_account 唯一索引，空串只容一条）、
%%      绝不用 user 层的 "password123" 兜底口令、reg_ip 取真实请求 IP。
%%
%% elib_pg:with_tx/1 的桩**忠实模拟真实契约**（throw({rollback,R}) → {rollback,R}）：
%% 若桩只是裸调 F，throw 会以异常形式穿出，测试就永远看不到回滚语义，
%% 变成「测试过了但生产不滚」的假绿。
%%
%% 参数捕获一律用进程字典（meck 只在 5/6 元版本提供 capture，本仓版本无 capture/3-4；
%% 且 mock 函数在调用方进程内执行，put/get 可靠，同仓 moya_auth_logic_tests 亦用此法）。
-module(moya_identity_ds_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

-define(UID, 98123).
-define(ACCOUNT, 50001).
-define(OPENID, <<"oMOYA_ds_test_openid">>).

%%%===================================================================
%%% 成功路径
%%%===================================================================

provision_creates_user_and_binds_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            ?assertEqual(
                {ok, ?UID},
                moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{
                    ip => <<"203.0.113.9">>
                })
            ),
            ?assertEqual(1, meck:num_calls(user_repo, create_tx, 2)),
            ?assertEqual(1, meck:num_calls(sso_identity_ds, bind_tx, 5)),
            %% 绑定的 subject 必须是 openid、uid 必须是刚落库的那个、email 不占位
            ?assertEqual({fake_conn, <<"wechat_mini">>, ?OPENID, ?UID, <<>>}, erased(bind_args))
        end
    ).

%% 开户层构造的 user 行必须自洽
user_row_shape_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{
                ip => <<"203.0.113.9">>
            }),
            {_, Data} = erased(create_args),
            %% account 必须真有值：uk_account 是普通唯一索引，空串全表只容一条
            ?assertEqual(?ACCOUNT, maps:get(account, Data)),
            ?assertEqual(<<"203.0.113.9">>, maps:get(reg_ip, Data)),
            ?assertEqual(<<"moya_wechat_mini">>, maps:get(source, Data)),
            %% 默认昵称有值（门店列表可读性），但绝不等同于 user 层的兜底口令
            ?assertEqual(<<"微信家长"/utf8>>, maps:get(nickname, Data)),
            Password = maps:get(password, Data),
            ?assertNotEqual(<<"password123">>, Password),
            ?assert(byte_size(Password) >= 32)
        end
    ).

%% 请求未带 ip 时不得留 127.0.0.1 占位（reg_ip 是 NOT NULL，必须显式给空串）
user_row_without_ip_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{}),
            {_, Data} = erased(create_args),
            ?assertEqual(<<>>, maps:get(reg_ip, Data)),
            ?assertEqual(<<"微信家长"/utf8>>, maps:get(nickname, Data))
        end
    ).

%% reg_cosv（客户端系统线索）必须显式给值。
%% 不显式传会落到 user_repo:normalize_legacy_create_data 的 "perf-test" 兜底 ——
%% 那是给内部压测/机器人账号设的占位值，写进真实家长账号后运营侧无法区分
%% 真实用户与压测数据（2026-09-21 生产库实测踩到：首登家长的 reg_cosv=perf-test）。
reg_cosv_passthrough_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{
                reg_cosv => <<"iOS 15.0">>
            }),
            {_, Data} = erased(create_args),
            ?assertEqual(<<"iOS 15.0">>, maps:get(reg_cosv, Data))
        end
    ).

%% 调用方没给 reg_cosv 时也必须给出**可区分**的值（unknown），而不是占位偏测值
reg_cosv_never_perf_placeholder_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{}),
            {_, Data} = erased(create_args),
            V = maps:get(reg_cosv, Data),
            ?assertNotEqual(<<"perf-test">>, V),
            ?assertNotEqual(<<>>, V),
            ?assertEqual(<<"unknown">>, V)
        end
    ).

%% 随机口令不得复用：两次开户的口令必须不同
password_is_random_per_provision_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{}),
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{}),
            [P2, P1] = erased(passwords),
            ?assertNotEqual(P1, P2)
        end
    ).

%%%===================================================================
%%% 幂等与并发
%%%===================================================================

%% 锁内复查命中 => 直接复用既有 uid，不建号、不绑身份
existing_mapping_short_circuits_test_() ->
    ?WITH_MECKS(
        ok_mocks_with_lookup({ok, ?UID}),
        fun() ->
            ?assertEqual(
                {ok, ?UID},
                moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{})
            ),
            ?assertEqual(0, meck:num_calls(user_repo, create_tx, 2)),
            ?assertEqual(0, meck:num_calls(sso_identity_ds, bind_tx, 5))
        end
    ).

%% advisory 锁必须**先于**复查：先复查后加锁等于没加锁
%% （两个并发请求都能看到 not_found，各自建号 → 一个账号被静默孤儿化）
advisory_lock_precedes_lookup_test_() ->
    ?WITH_MECKS(
        ok_mocks(),
        fun() ->
            {ok, _} = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{}),
            LockAt = erased(seq_lock),
            LookupAt = erased(seq_lookup),
            ?assertEqual(1, meck:num_calls(elib_pg, query, 3)),
            ?assert(LockAt < LookupAt),
            %% 锁必须也先于建号
            ?assert(LockAt < erased(seq_create))
        end
    ).

%%%===================================================================
%%% 失败路径：一律要真回滚（不留半成品）
%%%===================================================================

account_allocate_failure_test_() ->
    ?WITH_MECKS(
        [
            {account_ds, [{'allocate', 0, fun() -> {error, no_ids} end}]}
            | tail_mocks()
        ],
        fun() ->
            ?assertEqual(
                {error, account_unavailable},
                moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{})
            ),
            %% 拿不到号就不该开事务、不该建号
            ?assertEqual(0, meck:num_calls(elib_pg, with_tx, 1)),
            ?assertEqual(0, meck:num_calls(user_repo, create_tx, 2))
        end
    ).

%% 绑身份失败 => 整事务回滚（user 行不得留存）=> 对外 db_error
bind_failure_rolls_back_test_() ->
    ?WITH_MECKS(
        [
            {account_ds, [{'allocate', 0, fun() -> ?ACCOUNT end}]},
            {elib_pg, [
                {'with_tx', 1, fun(F) -> tx_stub(F) end},
                {'query', 3, fun(_, _, _) ->
                    seq(seq_lock),
                    {ok, []}
                end}
            ]},
            {sso_identity_ds, [
                {'find_uid_tx', 3, fun(_, _, _) ->
                    seq(seq_lookup),
                    not_found
                end},
                {'bind_tx', 5, fun(_, _, _, _, _) -> {error, boom} end}
            ]},
            {user_repo, [
                {'create_tx', 2, fun(_, _) ->
                    seq(seq_create),
                    {ok, ?UID}
                end}
            ]}
        ],
        fun() ->
            Result = moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{
                ip => <<"1.2.3.4">>
            }),
            ?assertEqual({error, db_error}, Result),
            %% 错误里绝不能带上 openid（身份映射层外泄=零容忍）
            ?assertEqual(nomatch, binary:match(term_to_binary(Result), ?OPENID))
        end
    ).

%% account 号撞车（23505）：不得当成成功，且不得继续绑身份
account_unique_violation_test_() ->
    ?WITH_MECKS(
        [
            {account_ds, [{'allocate', 0, fun() -> ?ACCOUNT end}]},
            {elib_pg, [
                {'with_tx', 1, fun(F) -> tx_stub(F) end},
                {'query', 3, fun(_, _, _) -> {ok, []} end}
            ]},
            {sso_identity_ds, [
                {'find_uid_tx', 3, fun(_, _, _) -> not_found end},
                {'bind_tx', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {user_repo, [
                {'create_tx', 2, fun(_, _) ->
                    {error, {error, error, <<"23505">>, unique_violation, <<>>, <<>>}}
                end}
            ]}
        ],
        fun() ->
            ?assertEqual(
                {error, account_unavailable},
                moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{})
            ),
            ?assertEqual(0, meck:num_calls(sso_identity_ds, bind_tx, 5))
        end
    ).

%% 查映射失败：不得退化成「当作新用户建号」（DB 抖动会批量造重复账号）
lookup_failure_rolls_back_test_() ->
    ?WITH_MECKS(
        ok_mocks_with_lookup({error, timeout}),
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{})
            ),
            ?assertEqual(0, meck:num_calls(user_repo, create_tx, 2))
        end
    ).

%% 拿不到 advisory 锁：整个事务不成立，宁可失败也不要并发重复建号
lock_failure_rolls_back_test_() ->
    ?WITH_MECKS(
        [
            {account_ds, [{'allocate', 0, fun() -> ?ACCOUNT end}]},
            {elib_pg, [
                {'with_tx', 1, fun(F) -> tx_stub(F) end},
                {'query', 3, fun(_, _, _) -> {error, dead_connection} end}
            ]},
            {sso_identity_ds, [
                {'find_uid_tx', 3, fun(_, _, _) -> not_found end},
                {'bind_tx', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {user_repo, [{'create_tx', 2, fun(_, _) -> {ok, ?UID} end}]}
        ],
        fun() ->
            ?assertEqual(
                {error, db_error},
                moya_identity_ds:provision_and_bind(<<"wechat_mini">>, ?OPENID, #{})
            ),
            ?assertEqual(0, meck:num_calls(user_repo, create_tx, 2))
        end
    ).

%%%===================================================================
%%% Helpers
%%%===================================================================

ok_mocks() ->
    ok_mocks_with_lookup(not_found).

ok_mocks_with_lookup(LookupResult) ->
    [
        {account_ds, [{'allocate', 0, fun() -> ?ACCOUNT end}]},
        {elib_pg, [
            {'with_tx', 1, fun(F) -> tx_stub(F) end},
            {'query', 3, fun(_, _, _) ->
                seq(seq_lock),
                {ok, []}
            end}
        ]},
        {sso_identity_ds, [
            {'find_uid_tx', 3, fun(_, _, _) ->
                seq(seq_lookup),
                LookupResult
            end},
            {'bind_tx', 5, fun(C, P, S, U, E) ->
                put(bind_args, {C, P, S, U, E}),
                ok
            end}
        ]},
        {user_repo, [
            {'create_tx', 2, fun(C, D) ->
                seq(seq_create),
                put(create_args, {C, D}),
                %% ⚠ 必须用 acc_list：累加器若默认 0 会拼出不当列表 [P | 0]，
                %% 匹配 [P1, P2] 时直接 badmatch（本测试首版即踩此坑）
                put(passwords, [maps:get(password, D) | acc_list(passwords)]),
                {ok, ?UID}
            end}
        ]}
    ].

%% 仅 account_ds 被替换、其余保持 ok_mocks 形态时用
tail_mocks() ->
    [
        {elib_pg, [
            {'with_tx', 1, fun(F) -> tx_stub(F) end},
            {'query', 3, fun(_, _, _) -> {ok, []} end}
        ]},
        {sso_identity_ds, [
            {'find_uid_tx', 3, fun(_, _, _) -> not_found end},
            {'bind_tx', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {user_repo, [{'create_tx', 2, fun(_, _) -> {ok, ?UID} end}]}
    ].

%% elib_pg:with_tx/1 的忠实桩，刻意做成**守卫桩**：
%%   ① 用连接真调 Fun（不是返回预置值，否则测试与实现脱钩）；
%%   ② throw({rollback,R}) → {rollback,R}，与 elib_pg 真实分支一致；
%%   ③ Fun 若返回 {ok,_} 之外的形态 → 直接报错。
%% 第 ③ 条是关键：真实 elib_pg:with_tx **原样透传** fun 返回值，返回 {error,_}
%% 会照常 COMMIT。所以「业务失败写成 return {error,_}」在生产是静默提交脏数据，
%% 而裸桩会把它当正常返回、测试照样绿。守卫桩让这种写法在单测阶段就炸。
tx_stub(F) ->
    try F(fake_conn) of
        {ok, _} = Ok ->
            Ok;
        Other ->
            erlang:error({tx_body_contract_violation, Other})
    catch
        throw:{rollback, Reason} -> {rollback, Reason}
    end.

%% 调用序号：用于断言「锁先于复查、复查先于建号」
seq(Key) ->
    N = acc_num(seq_counter) + 1,
    put(seq_counter, N),
    put(Key, N),
    ok.

%% 数值累加器：缺省 0
acc_num(Key) ->
    case get(Key) of
        undefined -> 0;
        N -> N
    end.

%% 列表累加器：缺省 []（**不能**复用 acc_num —— 0 会被当列表尾拼出不当列表）
acc_list(Key) ->
    case get(Key) of
        undefined -> [];
        L -> L
    end.

%% 读一次即清：避免同一测试内多次读取拿到陈旧值
erased(Key) ->
    V = get(Key),
    erase(Key),
    V.
