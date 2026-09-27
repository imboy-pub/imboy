%%% @doc CSD-BE-01：`GET /w/:public_widget_id` 动态 frame HTML + public_widget_id
%%% 全局反查套件（hosted-widget-contract S3/S4/S6；零 DB）。
%%%
%%% 分层夹具（同 cs_widget_frame_handler_tests 先例）：
%%%   * HTTP 面 = 真 cowboy 监听器（真路由表 Opts + cors/security_headers
%%%     中间件链）+ meck 的 facade——锁 200/404/405/400 线格式与响应头；
%%%   * application 面 = `cs_fake_store`（ETS）直驱
%%%     `cs_widget_app:public_frame_installation_by_public_id/1`——锁三态归一
%%%     （missing/disabled/revoked 同 `installation_unavailable`）、租户派生、
%%%     投影白名单；
%%%   * 静态面 = 纯函数 + 源码机械断言——锁 HTML 形状（无 installation_id）、
%%%     /w/ 形状谓词单源登记、全局反查 SQL 形状（单占位符、零 Org 谓词）。
-module(cs_widget_public_frame_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(FAKE, cs_fake_store).
-define(ORG, 7001001).
-define(INSTALL, 810001).
-define(PUBID, <<"wgt_pub_csdb01">>).
%% CSD-BE-01R + R2-F1/F3（hosted-widget-contract S4/S6）：/w/ 新面引用合同
%% S4 字面形状 `/widget-assets/cs-widget.v2.js`（v1 归旧 frame 面，新面取 v2
%% 避免同名互踩；widget 网关对该 location 下发 no-cache 重验证）。
-define(FRAME_JS, <<"/widget-assets/cs-widget.v2.js">>).
-define(FRAME_PATH, <<"/w/">>).

cs_widget_public_frame_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            ok
        end,
        [
            fun public_frame_http_tests/1,
            fun public_frame_app_tests/1,
            fun public_frame_static_tests/1
        ]}.

%% 真 /w/ 路由 Opts（从真路由表取）+ 完整中间件链（cors/security_headers 在位
%% ——XFO 豁免是响应头合同的一部分，必须在链上断言）。
with_public_frame_listener(Fun) ->
    {Pattern, Opts} = ?S:route_opt(widget, widget_public_frame_html),
    Name = list_to_atom(
        "cswwpublic_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Dispatch = cowboy_router:compile([
        {'_', [{binary_to_list(Pattern), cs_widget_handler, Opts}]}
    ]),
    {ok, _} = cowboy:start_clear(Name, [{port, 0}], #{
        env => #{dispatch => Dispatch},
        middlewares => [
            cowboy_router,
            cors_middleware,
            security_headers_middleware,
            cowboy_handler
        ]
    }),
    Port = ranch:get_port(Name),
    try
        Fun(Port)
    after
        _ = cowboy:stop_listener(Name)
    end.

frame_url(PubId) ->
    <<?FRAME_PATH/binary, PubId/binary>>.

installation_view(Origins) ->
    %% 投影白名单：/w/ 面只有 public id 与 origin 名单（S4：零内部 id）。
    #{public_widget_id => ?PUBID, allowed_origins => Origins}.

%% ===================================================================
%% HTTP 面（S4/S6 线格式合同）
%% ===================================================================

public_frame_http_tests(_) ->
    [
        {"S4 frame HTML carries public id only, exact CSP, no-store, no XFO", fun() ->
            meck:expect(customer_service_facade, widget_public_frame_html, fun(
                OrgId, Params
            ) ->
                %% OrgId=0 占位：反查面无 Org 输入（租户由命中行派生）。
                ?assertEqual(0, OrgId),
                ?assertEqual(#{public_widget_id => ?PUBID}, Params),
                {ok,
                    installation_view([
                        <<"https://shop.example.com">>, <<"https://other.example.com">>
                    ])}
            end),
            with_public_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(?PUBID), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                H = maps:get(headers, Resp),
                %% S4：allowed_origins 逐项 frame-ancestors。
                ?assertEqual(
                    <<"frame-ancestors https://shop.example.com https://other.example.com">>,
                    maps:get(<<"content-security-policy">>, H)
                ),
                %% S6：/w/:public_widget_id = no-store（revocation 立即生效）。
                ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, H)),
                ?assertEqual(
                    <<"text/html; charset=utf-8">>, maps:get(<<"content-type">>, H)
                ),
                %% S6：XFO 豁免面（/w/* 经 imboy_route_shape 单源登记）；
                %% 其余安全头照常。
                ?assertNot(is_map_key(<<"x-frame-options">>, H)),
                ?assertEqual(<<"nosniff">>, maps:get(<<"x-content-type-options">>, H)),
                Body = maps:get(body, Resp),
                ?assertMatch(
                    {_, _},
                    binary:match(Body, <<"data-public-widget-id=\"wgt_pub_csdb01\"">>)
                ),
                %% S4 冻结差异：新面**不得**输出 installation_id。
                ?assertEqual(nomatch, binary:match(Body, <<"data-installation-id">>)),
                %% 零 org/workspace/secret/token（S4）。
                ?assertEqual(nomatch, binary:match(Body, <<"organization">>)),
                ?assertEqual(nomatch, binary:match(Body, <<"workspace">>)),
                ?assertEqual(nomatch, binary:match(Body, <<"secret">>)),
                ?assertEqual(nomatch, binary:match(Body, <<"token">>)),
                %% 版本化脚本（S4；R2-F1/F3：新面 = 合同字面路径
                %% /widget-assets/cs-widget.v2.js；旧面 v1 文件名（新旧两形）
                %% 不得出现在 /w/ 文档——面隔离 oracle，升级即换 <N> 缓存键）。
                ?assertMatch(
                    {_, _}, binary:match(Body, <<"src=\"", ?FRAME_JS/binary, "\"">>)
                ),
                ?assertMatch({_, _}, binary:match(Body, ?FRAME_JS)),
                ?assertEqual(nomatch, binary:match(Body, <<"/widget-assets/cs-widget.v1.js">>)),
                ?assertEqual(nomatch, binary:match(Body, <<"/assets/cs-widget.v1.js">>))
            end)
        end},

        {"S4 empty allowlist yields frame-ancestors 'none'", fun() ->
            meck:expect(customer_service_facade, widget_public_frame_html, fun(
                _O, _P
            ) ->
                {ok, installation_view([])}
            end),
            with_public_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(?PUBID), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(
                    <<"frame-ancestors 'none'">>,
                    maps:get(<<"content-security-policy">>, maps:get(headers, Resp))
                )
            end)
        end},

        {"S3 unknown public id is 404 installation_unavailable (no enumeration)", fun() ->
            meck:expect(customer_service_facade, widget_public_frame_html, fun(
                _O, _P
            ) ->
                {error, installation_unavailable}
            end),
            with_public_frame_listener(fun(Port) ->
                Resp = ?S:request(
                    Port, <<"GET">>, frame_url(<<"wgt_pub_missing">>), <<>>, #{}
                ),
                ?assertEqual(404, ?S:status(Resp)),
                ?assertEqual(<<"installation_unavailable">>, ?S:msg(Resp))
            end)
        end},

        {"S4 non-GET method is 405 (facade never called)", fun() ->
            %% 同组内先前用例可能已调用过 facade——清历史再断言「未调用」。
            meck:reset(customer_service_facade),
            meck:expect(customer_service_facade, widget_public_frame_html, fun(
                _O, _P
            ) ->
                {ok, installation_view([])}
            end),
            with_public_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"POST">>, frame_url(?PUBID), <<>>, #{}),
                ?assertEqual(405, ?S:status(Resp)),
                ?assertNot(
                    meck:called(customer_service_facade, widget_public_frame_html, '_')
                )
            end)
        end},

        {"S4 credential-style query key is 400 (token never in URL)", fun() ->
            meck:reset(customer_service_facade),
            with_public_frame_listener(fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<?FRAME_PATH/binary, ?PUBID/binary, "?token=wtok-x">>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"credential_in_query_string">>, ?S:msg(Resp)),
                ?assertNot(
                    meck:called(customer_service_facade, widget_public_frame_html, '_')
                )
            end)
        end},

        {"S4 invalid public id shape is structured 400 (charset gate, no store hit)", fun() ->
            meck:reset(customer_service_facade),
            with_public_frame_listener(fun(Port) ->
                %% 空格（URL 编码）与点号都在 [A-Za-z0-9_-] 之外。
                lists:foreach(
                    fun(Bad) ->
                        Resp = ?S:request(
                            Port, <<"GET">>, frame_url(Bad), <<>>, #{}
                        ),
                        ?assertEqual(400, ?S:status(Resp)),
                        ?assertEqual(<<"invalid_public_widget_id">>, ?S:msg(Resp))
                    end,
                    [<<"bad%20id">>, <<"bad.id">>]
                ),
                ?assertNot(
                    meck:called(customer_service_facade, widget_public_frame_html, '_')
                )
            end)
        end},

        {"S4 server-side failures stay 500 (not disguised as 4xx)", fun() ->
            meck:expect(customer_service_facade, widget_public_frame_html, fun(
                _O, _P
            ) ->
                {error, {facts_unavailable, cs_store}}
            end),
            with_public_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(?PUBID), <<>>, #{}),
                ?assertEqual(500, ?S:status(Resp))
            end)
        end}
    ].

%% ===================================================================
%% application 面（S3 三态归一 + 租户派生 + 投影白名单；fake store，零 DB）
%% ===================================================================

public_frame_app_tests(_) ->
    [
        {"S3 global lookup hits and projection carries public id + origins only", fun() ->
            ?FAKE:init(),
            try
                ok = seed_installation(?ORG, ?INSTALL, ?PUBID),
                {ok, View} = cs_widget_app:public_frame_installation_by_public_id(
                    #{public_widget_id => ?PUBID, store => ?FAKE}
                ),
                ?assertEqual(?PUBID, maps:get(public_widget_id, View)),
                ?assertEqual(
                    [<<"https://shop.example.com">>],
                    maps:get(allowed_origins, View)
                ),
                %% S4：投影白名单——installation 内部 id / org 一律不出。
                ?assertNot(is_map_key(id, View)),
                ?assertNot(is_map_key(organization_id, View))
            after
                ?FAKE:destroy()
            end
        end},

        %% CP-SEC-05（DEC-VISIT-TOKEN=FIX_401_VISIT_TOKEN_INVALID）：digest
        %% 无命中行（伪造 secret）是凭证无效——application 层翻译为
        %% `visit_token_invalid`，不把 not_found 泄漏给 HTTP 面（404/500）。
        {"CP-SEC-05 forged digest translates to visit_token_invalid (not not_found)", fun() ->
            ?FAKE:init(),
            try
                ok = seed_installation(?ORG, ?INSTALL, ?PUBID),
                ?assertEqual(
                    {error, visit_token_invalid},
                    cs_widget_support:verify_bootstrap_token(
                        ?ORG,
                        #{
                            store => ?FAKE,
                            installation_id => ?INSTALL,
                            at => 1700000500,
                            secret => <<"wtok-forged-never-issued">>
                        }
                    )
                )
            after
                ?FAKE:destroy()
            end
        end},

        {"S3 tenant is derived from the hit row (store global fetch proves it)", fun() ->
            ?FAKE:init(),
            try
                ok = seed_installation(?ORG, ?INSTALL, ?PUBID),
                %% store 层：无 Org 输入，命中行的 organization_id 即派生租户。
                {ok, Row} = ?FAKE:fetch_widget_installation_by_public_id_global(?PUBID),
                ?assertEqual(?ORG, maps:get(organization_id, Row)),
                ?assertEqual(?INSTALL, maps:get(id, Row))
            after
                ?FAKE:destroy()
            end
        end},

        {"S3 missing / disabled / revoked collapse to the same installation_unavailable", fun() ->
            ?FAKE:init(),
            try
                %% disabled：正常生命周期不产出，测试注入面强制。
                ok = seed_installation(?ORG, ?INSTALL, ?PUBID),
                ok = ?FAKE:force_widget_installation_status(?INSTALL, disabled),
                Disabled =
                    cs_widget_app:public_frame_installation_by_public_id(
                        #{public_widget_id => ?PUBID, store => ?FAKE}
                    ),
                %% revoked：kill switch。
                ok = seed_installation(?ORG + 1, 810002, <<"wgt_pub_revoked1">>),
                ok = ?FAKE:revoke_widget_installation(?ORG + 1, 810002, 1700000001),
                Revoked =
                    cs_widget_app:public_frame_installation_by_public_id(
                        #{public_widget_id => <<"wgt_pub_revoked1">>, store => ?FAKE}
                    ),
                %% missing：不存在的 public id。
                Missing =
                    cs_widget_app:public_frame_installation_by_public_id(
                        #{public_widget_id => <<"wgt_pub_absent">>, store => ?FAKE}
                    ),
                %% 三态逐一归一为同一错误项（404 installation_unavailable）。
                ?assertEqual({error, installation_unavailable}, Disabled),
                ?assertEqual({error, installation_unavailable}, Revoked),
                ?assertEqual({error, installation_unavailable}, Missing)
            after
                ?FAKE:destroy()
            end
        end},

        {"S4 invalid public id shapes are rejected before the store", fun() ->
            ?FAKE:init(),
            try
                lists:foreach(
                    fun(Bad) ->
                        ?assertEqual(
                            {error, {invalid_argument, public_widget_id}},
                            cs_widget_app:public_frame_installation_by_public_id(
                                #{public_widget_id => Bad, store => ?FAKE}
                            )
                        )
                    end,
                    [
                        <<>>,
                        <<"bad id">>,
                        <<"bad.id">>,
                        <<"bad/id">>,
                        <<"' OR 1=1--">>,
                        binary:copy(<<"a">>, 129)
                    ]
                ),
                %% 非 binary 直接形状错。
                ?assertEqual(
                    {error, {invalid_argument, public_widget_id}},
                    cs_widget_app:public_frame_installation_by_public_id(
                        #{public_widget_id => 123, store => ?FAKE}
                    )
                )
            after
                ?FAKE:destroy()
            end
        end}
    ].

%% ===================================================================
%% 静态面（纯函数 + 机械断言，零 socket / 零 DB）
%% ===================================================================

public_frame_static_tests(_) ->
    [
        fun() ->
            %% S4：HTML 仅挂载点 + public id + 版本化脚本；属性值转义。
            Doc = cs_widget_handler:public_frame_document(<<"w&quot;x">>),
            ?assertMatch({_, _}, binary:match(Doc, <<"w&amp;quot;x">>)),
            ?assertMatch({_, _}, binary:match(Doc, <<"id=\"cs-widget-root\"">>)),
            ?assertMatch({_, _}, binary:match(Doc, <<"src=\"", ?FRAME_JS/binary, "\"">>)),
            ?assertEqual(nomatch, binary:match(Doc, <<"data-installation-id">>)),
            ?assertEqual(nomatch, binary:match(Doc, <<"organization_id">>))
        end,
        fun() ->
            %% /w/ 形状登记进共享谓词（XFO/CORS/免签三处消费的单一真源）；
            %% 旧 frame 形状原样保留（兼容窗口零回归），相似路径不放宽。
            ?assert(imboy_route_shape:is_cs_widget_frame_path(<<"/w/wgt_pub_x">>)),
            ?assert(
                imboy_route_shape:is_cs_widget_frame_path(<<"/api/v1/cs/widget/frame/810001">>)
            ),
            ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/w">>)),
            ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/w/a/b">>)),
            ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/www/wgt_pub_x">>)),
            ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/help">>))
        end,
        fun() ->
            %% 全局反查 SQL 形状机械断言：恰一个占位符且谓词为
            %% public_widget_id = $1（Org 是投影输出、零 Org 谓词）；且该语句
            %% **不进**「同语句带 Org」冻结集（铁律 6 断言集语义不适用，
            %% 租户纪律由本断言 + PG oracle 单独冻结）。
            Src = read_source("src/features/customer_service/infrastructure/cs_pg_widget.erl"),
            Global = global_sql_block(Src),
            ?assertMatch({match, _}, re:run(Global, <<"public_widget_id\\s*=\\s*\\$1">>)),
            ?assertEqual(nomatch, re:run(Global, <<"organization_id\\s*=">>)),
            %% 冻结集纪律原样：每条非 INSERT 语句仍同语句带 organization_id = $1。
            lists:foreach(
                fun(Sql) ->
                    case is_insert_statement(Sql) of
                        true ->
                            ok;
                        false ->
                            ?assertMatch(
                                {match, _}, re:run(Sql, <<"organization_id\\s*=\\s*\\$1">>)
                            )
                    end
                end,
                cs_pg_widget:sql_statements()
            )
        end
    ].

%% ===================================================================
%% 工具
%% ===================================================================

seed_installation(OrgId, InstallationId, PublicId) ->
    {ok, _} = ?FAKE:insert_widget_installation(OrgId, #{
        id => InstallationId,
        public_widget_id => PublicId,
        display_name => <<"csdb01-frame">>,
        allowed_origins => [<<"https://shop.example.com">>],
        branding => #{<<"primary">> => <<"#0a84ff">>},
        consent_version => <<"csdb01-consent-v1">>
    }),
    ok.

read_source(Path) ->
    {ok, Bin} = file:read_file(Path),
    unicode:characters_to_binary(Bin).

global_sql_block(Src) ->
    case string:find(Src, "-define(SQL_FETCH_INSTALLATION_BY_PUBLIC_ID_GLOBAL") of
        nomatch ->
            erlang:error(global_sql_define_missing);
        Rest ->
            Block = hd(binary:split(Rest, <<">>).">>)),
            Block
    end.

is_insert_statement(Sql) ->
    match =:= re:run(Sql, <<"^\\s*INSERT\\s+INTO">>, [{capture, none}]).
