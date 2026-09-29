%%% @doc 坐席控制台嵌入的应用层用例套件（seat-console-embed SC-BE；零 DB，
%%% `cs_fake_store` + `cs_fake_id` 直驱）。
%%%
%%% 判定对应：
%%%   * A02：六禁 origin 形状类逐类拒绝（scheme/通配/userinfo/path/空白控制
%%%     字符/非法字符集与端口）、数量与字节上限、保序去重；
%%%   * A03：CRUD 编排（列表/创建/更新/吊销）、(Org,WS) 槽位冲突 → conflict、
%%%     吊销幂等、公开投影白名单；
%%%   * A04：PUT 只改 allowed_origins（public id / status 不可变更，显式提交
%%%     即 422）；revoke 后同 (Org,WS) 可重建；
%%%   * A10：public_seat_console_id = TSID 十进制 string（注入缺省走 id 端口
%%%     `cs_seat_console_public` 命名域）。
-module(cs_seat_console_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ORG, 7101001).
-define(WS, 91001).
-define(WS2, 91002).
-define(T0, 1700000100).

seat_console_app_test_() ->
    {setup,
        fun() ->
            ok = cs_fake_store:init(),
            ok = cs_fake_id:reset()
        end,
        fun(_) ->
            ok = cs_fake_store:destroy(),
            ok = cs_fake_id:reset()
        end,
        [
            fun invalid_origin_classes_are_rejected/0,
            fun origin_limits_are_enforced/0,
            fun dedupe_is_order_preserving/0,
            fun public_id_shape_gate/0,
            fun create_conflict_and_projection_whitelist/0,
            fun update_keeps_public_id_and_status/0,
            fun update_expected_version_gate_and_cas/0,
            fun revoke_is_idempotent_then_create_rebuilds/0,
            fun public_frame_by_public_id/0,
            fun generated_public_id_is_decimal_tsid/0
        ]}.

params() ->
    #{
        workspace_id => ?WS,
        allowed_origins => [<<"HTTPS://SHOP.EXAMPLE.COM:443">>],
        at => ?T0,
        store => cs_fake_store,
        id => cs_fake_id,
        new_public_seat_console_id => fun() -> <<"sc_pub_test">> end
    }.

%% 用例间隔离：共享进程内 fake（ETS）每个用例开头重建（槽位唯一约束跨用例
%% 不可见——隔离靠重建，不靠清理残留）。
reset() ->
    ok = cs_fake_store:init(),
    ok = cs_fake_id:reset().

%% ===================================================================
%% A02：六禁 origin 形状类（domain 归一复用 cs_widget:normalize_origin/1）
%% ===================================================================

invalid_origin_classes_are_rejected() ->
    reset(),
    Classes = [
        %% scheme 非 http(s)
        <<"ftp://shop.example.com">>,
        %% 通配
        <<"https://*.example.com">>,
        %% userinfo
        <<"https://user@shop.example.com">>,
        %% path/query/fragment
        <<"https://shop.example.com/path">>,
        <<"https://shop.example.com?q=1">>,
        %% 空白 / 控制字符（含无冒号 CRLF 形态）
        <<"https://shop.example.com\r\nEvil: 1">>,
        <<"https://shop.example.com X">>,
        %% 非法 host 字符集
        <<"https://bad!host.example.com">>,
        %% 非法端口（非纯数字 / 越界）
        <<"http://shop.example.com:80+">>,
        <<"https://shop.example.com:70000">>
    ],
    lists:foreach(
        fun(Origin) ->
            ?assertMatch(
                {error, {invalid_origin, _}},
                cs_seat_console_app:create_console(
                    ?ORG, (params())#{allowed_origins => [Origin]}
                )
            )
        end,
        Classes
    ),
    %% 非法 origin 在触达 store 前拒绝（零残留）。
    ?assertEqual(
        {ok, #{seat_consoles => [], next_after_id => undefined}},
        cs_seat_console_app:list_consoles(?ORG, params())
    ),
    %% 非 list 输入同拒。
    ?assertMatch(
        {error, {invalid_argument, allowed_origins}},
        cs_seat_console_app:create_console(?ORG, (params())#{allowed_origins => not_a_list})
    ).

origin_limits_are_enforced() ->
    reset(),
    %% 条数上限：11 条归一合法 origin → too_many_origins。
    Many = [
        <<"https://h", (integer_to_binary(N))/binary, ".example.com">>
     || N <- lists:seq(1, 11)
    ],
    ?assertMatch(
        {error, {too_many_origins, 11}},
        cs_seat_console_app:create_console(?ORG, (params())#{allowed_origins => Many})
    ),
    %% 单条上限：归一前 >255 字节 → origin_too_long。
    Long = <<"https://", (binary:copy(<<"a">>, 250))/binary, ".example.com">>,
    ?assertMatch(
        {error, {origin_too_long, _}},
        cs_seat_console_app:create_console(?ORG, (params())#{allowed_origins => [Long]})
    ),
    %% 总字节上限：10 条 × ~110B 归一 > 1024 → origins_total_too_large。
    Total = [
        <<"https://host", (integer_to_binary(N))/binary, ".", (binary:copy(<<"a">>, 90))/binary,
            ".example.com">>
     || N <- lists:seq(1, 10)
    ],
    ?assertMatch(
        {error, {origins_total_too_large, _}},
        cs_seat_console_app:create_console(?ORG, (params())#{allowed_origins => Total})
    ),
    %% 空白名单同拒（嵌入面 empty CSP 是合法态，但配置一个空 allowlist 的
    %% active 控制台 = 全拒嵌入，显式配置错误按 422 拒）。
    ?assertMatch(
        {error, {invalid_argument, allowed_origins}},
        cs_seat_console_app:create_console(?ORG, (params())#{allowed_origins => []})
    ).

dedupe_is_order_preserving() ->
    reset(),
    Origins = [
        <<"https://shop.example.com">>,
        <<"HTTPS://DOCS.EXAMPLE.COM">>,
        <<"https://shop.example.com:443">>,
        <<"http://shop.example.com:80">>
    ],
    {ok, #{seat_console := Created}} =
        cs_seat_console_app:create_console(
            ?ORG,
            (params())#{
                allowed_origins => Origins,
                new_public_seat_console_id => fun() -> <<"sc_pub_dedupe">> end
            }
        ),
    %% 归一 + 保序去重（首次出现位置保留；缺省端口折叠；scheme 参与身份，
    %% https 与 http 是不同 origin）。
    ?assertEqual(
        [
            <<"https://shop.example.com">>,
            <<"https://docs.example.com">>,
            <<"http://shop.example.com">>
        ],
        maps:get(allowed_origins, Created)
    ).

%% ===================================================================
%% public_seat_console_id 形状门
%% ===================================================================

public_id_shape_gate() ->
    reset(),
    Valid = [
        <<"sc_pub_1">>,
        <<"SC-PUB-1">>,
        <<"1234567890">>,
        <<"a">>,
        <<"-">>,
        <<"_">>
    ],
    lists:foreach(fun(Id) -> ?assert(cs_seat_console:valid_public_seat_console_id(Id)) end, Valid),
    Invalid = [
        <<>>,
        <<"a b">>,
        <<"a/b">>,
        <<"a?b">>,
        <<"a#b">>,
        <<"a\"b">>,
        <<"a\r\nb">>,
        binary:copy(<<"a">>, 129),
        not_a_binary,
        12345
    ],
    lists:foreach(
        fun(Id) -> ?assertNot(cs_seat_console:valid_public_seat_console_id(Id)) end, Invalid
    ).

%% ===================================================================
%% A03：CRUD 编排 + 槽位冲突 + 投影白名单
%% ===================================================================

create_conflict_and_projection_whitelist() ->
    reset(),
    {ok, #{seat_console := Created}} = cs_seat_console_app:create_console(
        ?ORG, (params())#{new_public_seat_console_id => fun() -> <<"sc_pub_c1">> end}
    ),
    Id = maps:get(id, Created),
    ?assertEqual(<<"sc_pub_c1">>, maps:get(public_seat_console_id, Created)),
    ?assertEqual(?WS, maps:get(workspace_id, Created)),
    ?assertEqual(active, maps:get(status, Created)),
    %% 公开投影白名单：created_by_user_id / revoked_at 不出本层。
    ?assertNot(maps:is_key(created_by_user_id, Created)),
    ?assertNot(maps:is_key(revoked_at, Created)),
    ?assertEqual(
        lists:sort([
            id,
            organization_id,
            workspace_id,
            public_seat_console_id,
            allowed_origins,
            status,
            version,
            created_at,
            updated_at
        ]),
        lists:sort(maps:keys(Created))
    ),

    %% 同 (Org, WS) 活跃槽位唯一 → conflict（409 面）。
    ?assertMatch(
        {error, conflict},
        cs_seat_console_app:create_console(
            ?ORG, (params())#{new_public_seat_console_id => fun() -> <<"sc_pub_c2">> end}
        )
    ),
    %% 不同 Workspace 同 Org → OK。
    {ok, #{seat_console := OtherWs}} =
        cs_seat_console_app:create_console(
            ?ORG,
            (params())#{
                workspace_id => ?WS2,
                new_public_seat_console_id => fun() -> <<"sc_pub_c3">> end
            }
        ),
    ?assertEqual(?WS2, maps:get(workspace_id, OtherWs)),
    %% 列表按 (Org, WS) 作用域：WS 与 WS2 各自一行。
    {ok, #{seat_consoles := WsList}} = cs_seat_console_app:list_consoles(?ORG, params()),
    ?assertEqual([Id], [maps:get(id, R) || R <- WsList]),
    {ok, #{seat_consoles := Ws2List}} =
        cs_seat_console_app:list_consoles(?ORG, (params())#{workspace_id => ?WS2}),
    ?assertEqual(1, length(Ws2List)),
    %% 跨 Org 不可见（tenant 门 + store 同语句 Org 谓词）。
    {ok, #{seat_consoles := []}} =
        cs_seat_console_app:list_consoles(?ORG + 1, params()).

%% ===================================================================
%% A04：PUT 只改 allowed_origins；吊销幂等 + 重建
%% ===================================================================

update_keeps_public_id_and_status() ->
    reset(),
    {ok, #{seat_console := Created}} = cs_seat_console_app:create_console(
        ?ORG, (params())#{new_public_seat_console_id => fun() -> <<"sc_pub_u1">> end}
    ),
    Id = maps:get(id, Created),
    Version0 = maps:get(version, Created),
    {ok, #{seat_console := Updated}} =
        cs_seat_console_app:update_console(?ORG, (params())#{id => Id, at => ?T0 + 10}),
    %% public id / status / workspace 不变；version 前进；origins 全量替换。
    ?assertEqual(<<"sc_pub_u1">>, maps:get(public_seat_console_id, Updated)),
    ?assertEqual(active, maps:get(status, Updated)),
    ?assertEqual(?WS, maps:get(workspace_id, Updated)),
    ?assertEqual(Version0 + 1, maps:get(version, Updated)),
    ?assertEqual([<<"https://shop.example.com">>], maps:get(allowed_origins, Updated)),
    ?assertEqual(?T0 + 10, maps:get(updated_at, Updated)),
    %% 非法 origins 拒绝（不触 store，零残留）。
    ?assertMatch(
        {error, {invalid_origin, _}},
        cs_seat_console_app:update_console(
            ?ORG, (params())#{id => Id, allowed_origins => [<<"https://x.com/y">>]}
        )
    ),
    %% 不可编辑键显式提交 → 422（纵深防御：HTTP 面动作表本就不投影这些键）。
    ?assertMatch(
        {error, {invalid_argument, seat_console_immutable_fields}},
        cs_seat_console_app:update_console(
            ?ORG, (params())#{id => Id, public_seat_console_id => <<"sc_pub_hijack">>}
        )
    ),
    ?assertMatch(
        {error, {invalid_argument, seat_console_immutable_fields}},
        cs_seat_console_app:update_console(?ORG, (params())#{id => Id, status => revoked})
    ),
    %% 更新后 public id 仍是原值（hijack 未生效）。
    {ok, #{seat_console := Fresh}} =
        cs_seat_console_app:update_console(?ORG, (params())#{id => Id, at => ?T0 + 20}),
    ?assertEqual(<<"sc_pub_u1">>, maps:get(public_seat_console_id, Fresh)),
    %% 缺 id / 非法 id → 422。
    ?assertMatch(
        {error, {invalid_argument, update_console}},
        cs_seat_console_app:update_console(?ORG, params())
    ).

%% ===================================================================
%% F-6（REVIEW-3）：expected_version 形状门 + store 透传 + CAS 裁决
%% ===================================================================

update_expected_version_gate_and_cas() ->
    reset(),
    {ok, #{seat_console := Created}} = cs_seat_console_app:create_console(
        ?ORG, (params())#{new_public_seat_console_id => fun() -> <<"sc_pub_ev">> end}
    ),
    Id = maps:get(id, Created),
    %% 缺省 = 旧 LWW 行为（既有调用方零破坏）
    {ok, #{seat_console := U1}} =
        cs_seat_console_app:update_console(?ORG, (params())#{id => Id, at => ?T0 + 1}),
    ?assertEqual(2, maps:get(version, U1)),
    %% 形状门：expected_version 非（正）整数 → 422，且不触 store（version 不动）
    ?assertMatch(
        {error, {invalid_argument, expected_version}},
        cs_seat_console_app:update_console(
            ?ORG, (params())#{id => Id, at => ?T0 + 2, expected_version => 0}
        )
    ),
    ?assertMatch(
        {error, {invalid_argument, expected_version}},
        cs_seat_console_app:update_console(
            ?ORG, (params())#{id => Id, at => ?T0 + 2, expected_version => <<"2">>}
        )
    ),
    {ok, #{seat_console := Still}} =
        cs_seat_console_app:update_console(?ORG, (params())#{id => Id, at => ?T0 + 3}),
    ?assertEqual(3, maps:get(version, Still)),
    %% store 透传 + CAS 裁决（fake store 镜像 PG 决策语义）：
    %% 匹配 → 成功；不匹配 → {error, {cas_mismatch, Detail}}（携带当前 version）。
    {ok, #{seat_console := U4}} = cs_seat_console_app:update_console(
        ?ORG, (params())#{id => Id, at => ?T0 + 4, expected_version => 3}
    ),
    ?assertEqual(4, maps:get(version, U4)),
    ?assertMatch(
        {error, {cas_mismatch, #{expected_version := 2, actual_version := 4}}},
        cs_seat_console_app:update_console(
            ?ORG, (params())#{id => Id, at => ?T0 + 5, expected_version => 2}
        )
    ),
    %% CAS 失败不改写行：不匹配 PUT 之后，以 version=4 为基准的下一次更新
    %% 仍成功（若失败 PUT 实际生效，version 已是 5，本次会 cas_mismatch），
    %% 且 origins 是本次提交值而非失败 PUT 的值。
    {ok, #{seat_console := Untouched}} = cs_seat_console_app:update_console(
        ?ORG,
        (params())#{
            id => Id,
            at => ?T0 + 6,
            expected_version => 4,
            allowed_origins => [<<"https://after.example.com">>]
        }
    ),
    ?assertEqual(5, maps:get(version, Untouched)),
    ?assertEqual([<<"https://after.example.com">>], maps:get(allowed_origins, Untouched)).

revoke_is_idempotent_then_create_rebuilds() ->
    reset(),
    {ok, #{seat_console := Created}} = cs_seat_console_app:create_console(
        ?ORG, (params())#{new_public_seat_console_id => fun() -> <<"sc_pub_r1">> end}
    ),
    Id = maps:get(id, Created),
    {ok, #{seat_console := Revoked}} =
        cs_seat_console_app:revoke_console(?ORG, (params())#{id => Id, at => ?T0 + 30}),
    ?assertEqual(revoked, maps:get(status, Revoked)),
    %% 公开投影白名单：revoked_at 不出本层（投影不含该键）。
    ?assertNot(maps:is_key(revoked_at, Revoked)),
    %% 幂等：重复 revoke 返回既有 revoked 行（不 404、时间戳不漂移）。
    {ok, #{seat_console := Revoked2}} =
        cs_seat_console_app:revoke_console(?ORG, (params())#{id => Id, at => ?T0 + 99}),
    ?assertEqual(Revoked, Revoked2),
    {ok, Raw} = cs_fake_store:fetch_seat_console(?ORG, ?WS, Id),
    ?assertEqual(?T0 + 30, maps:get(revoked_at, Raw)),
    %% 吊销后编辑拒绝（seat_console_revoked，403 面）。
    ?assertMatch(
        {error, seat_console_revoked},
        cs_seat_console_app:update_console(?ORG, (params())#{id => Id, at => ?T0 + 40})
    ),
    %% 吊销释放 (Org, WS) 活跃槽位 → 同槽位可重建。
    {ok, #{seat_console := Rebuilt}} = cs_seat_console_app:create_console(
        ?ORG, (params())#{new_public_seat_console_id => fun() -> <<"sc_pub_r2">> end}
    ),
    ?assertEqual(active, maps:get(status, Rebuilt)),
    ?assertNotEqual(Id, maps:get(id, Rebuilt)),
    %% 真不存在 → not_found。
    ?assertMatch(
        {error, not_found},
        cs_seat_console_app:revoke_console(
            ?ORG, (params())#{id => cs_fake_id:new_id(cs_seat_console), at => ?T0 + 50}
        )
    ).

%% ===================================================================
%% /seat/ 嵌入面：全局反查 + active 门 + frame 投影白名单
%% ===================================================================

public_frame_by_public_id() ->
    reset(),
    Origins = [<<"https://shop.example.com">>, <<"https://docs.example.com">>],
    {ok, #{seat_console := _Created}} = cs_seat_console_app:create_console(
        ?ORG,
        (params())#{
            allowed_origins => Origins,
            new_public_seat_console_id => fun() -> <<"sc_pub_f1">> end
        }
    ),
    %% active → frame 投影只含公开 id 与归一 origin 名单（零 org/workspace）。
    {ok, Frame} = cs_seat_console_app:public_frame_console_by_public_id(#{
        public_seat_console_id => <<"sc_pub_f1">>, store => cs_fake_store
    }),
    ?assertEqual(
        [allowed_origins, public_seat_console_id], lists:sort(maps:keys(Frame))
    ),
    ?assertEqual(<<"sc_pub_f1">>, maps:get(public_seat_console_id, Frame)),
    ?assertEqual(Origins, maps:get(allowed_origins, Frame)),
    %% 不存在 → seat_console_unavailable（与吊销同归一，无枚举）。
    ?assertMatch(
        {error, seat_console_unavailable},
        cs_seat_console_app:public_frame_console_by_public_id(#{
            public_seat_console_id => <<"sc_pub_absent">>, store => cs_fake_store
        })
    ),
    %% 吊销 → seat_console_unavailable（kill switch 立即生效）。
    {ok, #{seat_consoles := [R]}} = cs_seat_console_app:list_consoles(?ORG, params()),
    ok = cs_fake_store:revoke_seat_console(?ORG, ?WS, maps:get(id, R), ?T0 + 60),
    ?assertMatch(
        {error, seat_console_unavailable},
        cs_seat_console_app:public_frame_console_by_public_id(#{
            public_seat_console_id => <<"sc_pub_f1">>, store => cs_fake_store
        })
    ),
    %% 形状门失败 → invalid_argument（handler 已先 400，纵深防御）。
    ?assertMatch(
        {error, {invalid_argument, public_seat_console_id}},
        cs_seat_console_app:public_frame_console_by_public_id(#{
            public_seat_console_id => <<"bad id">>, store => cs_fake_store
        })
    ).

%% CSD（SC-BE A10）：public_seat_console_id 生成口径 = TSID 十进制 string
%% （缺省走 id 端口 `cs_seat_console_public` 命名域；与 widget 的 R4 同款）。
generated_public_id_is_decimal_tsid() ->
    reset(),
    {ok, #{seat_console := Created}} =
        cs_seat_console_app:create_console(
            ?ORG, maps:without([new_public_seat_console_id], params())
        ),
    PublicId = maps:get(public_seat_console_id, Created),
    ?assert(byte_size(PublicId) > 0),
    ?assert(byte_size(PublicId) =< 26),
    ?assert(lists:all(fun(C) -> C >= $0 andalso C =< $9 end, binary_to_list(PublicId))),
    %% 生成的 ID 经全局反查 round-trip 可服务（/seat/ 嵌入面）。
    {ok, Frame} = cs_fake_store:fetch_seat_console_by_public_id_global(PublicId),
    ?assertEqual(maps:get(id, Created), maps:get(id, Frame)).
