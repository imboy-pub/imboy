-module(legacy_list_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% J27.a（channel-group-ux v2.2 §9）：旧客户端/旧 Admin 无新参数读取各
%%% 原清单——原 envelope / 完整旧集合 / ADM page,size,keyword 可用。
%%%
%%% 版本分界与「全部缺省零变化」结论引自就绪包
%%% /tmp/v22-blocked-prep/J28.d.md §2.2/§2.4（旧基线 Backend 313016123、
%%% 新候选 HEAD；新参数面：q / pagination=cursor / 创建 request_id）。
%%% 本文件在 handler 层钉住缺省兼容合同（旧基线锚点 §2.4）：
%%%
%%%   - channel_handler subscribed_legacy（channel_handler.erl:513-529，
%%%     经导出的 handle_action/3 分派 subscribed/2）：
%%%     无 pagination → 旧 {list, cursor, limit} envelope 逐字不变，
%%%     cursor/limit 回显；q 缺席 → get_subscribed_channels(Uid, <<>>)
%%%     行为零变化；managed 同款（{list} 单键 envelope）。
%%%   - group_handler page legacy（group_handler.erl:65-69,547-619）：
%%%     无 pagination → 旧 {total, page, size, list} envelope；
%%%     attr owner/join/manager 分派与缺省 q=<<>> 不变。
%%%   - workspace_handler group_list（workspace_handler.erl:287-303）：
%%%     无 pagination → 数值 cursor 旧语义（member 子集 cursor/limit
%%%     原样下推），无新键泄漏。
%%%   - 边界负控（双向）：显式 pagination=cursor 才进新协议——旧路径
%%%     逻辑函数零调用；cursor 分支 fail-closed（无效游标 400，
%%%     不回落首页，不调旧列表函数）。
%%%   - ADM 面（MW/MG/MC Backend 侧锚点）：adm_group_handler/adm_channel_
%%%     handler GET list 对 page/size/keyword 旧参数的下推不变
%%%     （adm_group_handler.erl:69-79 / adm_channel_handler.erl:141-173，
%%%     排序键 created_at DESC / id desc 原样）。
%%%
%%% 纯 mock 套件：不触 DB、不触真实签名密钥（游标解析/签发函数均 mock）。
%%%===================================================================

-define(UID, 1001).
-define(WS_ID_BIN, <<"880301">>).
-define(WS_ID, 880301).

%%%===================================================================
%%% BS：channel_handler 订阅/管理频道 legacy
%%%===================================================================

%% 无任何新参数：旧 envelope {list, cursor, limit} 逐字不变（cursor=undefined
%% 为旧路径缺省值），q 以 <<>> 传逻辑层（零变化），cursor 协议函数零调用。
channel_subscribed_legacy_default_envelope_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {channel_logic, [
                {'get_subscribed_channels', 2, fun(?UID, Q) ->
                    ?assertEqual(<<>>, Q),
                    {ok, [#{<<"id">> => 7, <<"name">> => <<"旧频道"/utf8>>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => ?UID},
            {resp, Payload} = channel_handler:handle_action(subscribed, Req, State),
            %% 旧 envelope 恰三键：无 has_more/next_cursor/q 新键泄漏
            ?assertEqual(
                #{
                    list => [#{<<"id">> => 7, <<"name">> => <<"旧频道"/utf8>>}],
                    cursor => undefined,
                    limit => 50
                },
                Payload
            ),
            %% 负控：cursor 协议入口零调用
            ?assertEqual(0, meck:num_calls(channel_logic, get_subscribed_channels_cursor, '_'))
        end
    ).

%% 旧 cursor/limit 回显合同保持（W07.a 注释承诺：cursor/limit 回显原样）。
channel_subscribed_legacy_cursor_limit_echo_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"cursor">>, <<"5">>}, {<<"limit">>, <<"80">>}]
                end}
            ]},
            {channel_logic, [
                {'get_subscribed_channels', 2, fun(_Uid, _Q) -> {ok, []} end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp, #{cursor := Cursor, limit := Limit, list := List}} =
                channel_handler:handle_action(subscribed, Req, #{current_uid => ?UID}),
            ?assertEqual(5, Cursor),
            ?assertEqual(80, Limit),
            ?assertEqual([], List),
            ?assertEqual(0, meck:num_calls(channel_logic, get_subscribed_channels_cursor, '_'))
        end
    ).

%% q 缺席=<<>>（零变化）；q 在场=原样透传且仍走 legacy 函数（服务端搜索
%% 是可选参数，不是新协议开关）。
channel_subscribed_q_passthrough_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [{<<"q">>, <<"新闻"/utf8>>}] end}
            ]},
            {channel_logic, [
                {'get_subscribed_channels', 2, fun(?UID, Q) ->
                    ?assertEqual(<<"新闻"/utf8>>, Q),
                    {ok, []}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp, _} = channel_handler:handle_action(subscribed, Req, #{current_uid => ?UID}),
            ?assertEqual(0, meck:num_calls(channel_logic, get_subscribed_channels_cursor, '_'))
        end
    ).

%% managed legacy：旧 envelope 单键 {list}（无 cursor/limit 回显）。
channel_managed_legacy_envelope_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {channel_logic, [
                {'get_managed_channels', 2, fun(?UID, Q) ->
                    ?assertEqual(<<>>, Q),
                    {ok, [#{<<"id">> => 9}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp, Payload} = channel_handler:handle_action(managed, Req, #{current_uid => ?UID}),
            ?assertEqual(#{list => [#{<<"id">> => 9}]}, Payload),
            ?assertEqual(0, meck:num_calls(channel_logic, get_managed_channels_cursor, '_'))
        end
    ).

%%%===================================================================
%%% BG：group_handler page legacy
%%%===================================================================

%% 旧 page/size envelope {total, page, size, list}；缺省 q=<<>>；
%% cursor 协议函数零调用。
group_page_legacy_envelope_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"page">>, <<"2">>}, {<<"size">>, <<"15">>}]
                end}
            ]},
            {group_logic, [
                {'page_by_owner', 4, fun(?UID, Page, Size, Q) ->
                    ?assertEqual(2, Page),
                    ?assertEqual(15, Size),
                    ?assertEqual(<<>>, Q),
                    {ok, #{total => 7, list => [#{<<"gid">> => 42}]}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => ?UID},
            {resp, Payload} = group_handler:handle_action(page, Req, State),
            %% 旧 envelope 恰四键
            ?assertEqual(
                #{total => 7, page => 2, size => 15, list => [#{<<"gid">> => 42}]},
                Payload
            ),
            ?assertEqual(0, meck:num_calls(group_logic, page_by_owner_cursor, '_'))
        end
    ).

%% join attr 同款（缺省 page/size + q=<<>>）；attr 分派不变。
group_page_legacy_join_attr_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [{<<"attr">>, <<"join">>}] end}
            ]},
            {group_logic, [
                {'page_joined', 4, fun(?UID, Page, Size, Q) ->
                    ?assertEqual(1, Page),
                    ?assertEqual(20, Size),
                    ?assertEqual(<<>>, Q),
                    {ok, #{total => 0, list => []}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp, Payload} =
                group_handler:handle_action(page, Req, #{current_uid => ?UID}),
            ?assertEqual(#{total => 0, page => 1, size => 20, list => []}, Payload)
        end
    ).

%% 边界负控：显式 pagination=cursor 才进新协议——首页 BeforeId=max bigint、
%% envelope {list, has_more, next_cursor, limit}；旧 page 函数零调用。
group_cursor_boundary_explicit_optin_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"pagination">>, <<"cursor">>}, {<<"attr">>, <<"owner">>}]
                end}
            ]},
            {group_logic, [
                {'page_by_owner', 4, fun(_U, _P, _S, _Q) ->
                    error(legacy_page_must_not_be_called_in_cursor_mode)
                end},
                {'page_by_owner_cursor', 4, fun(?UID, BeforeId, Limit, Q) ->
                    ?assertEqual(16#7FFFFFFFFFFFFFFF, BeforeId),
                    ?assertEqual(20, Limit),
                    ?assertEqual(<<>>, Q),
                    {ok, #{list => [], has_more => false, upper => 0}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp, Payload} =
                group_handler:handle_action(page, Req, #{current_uid => ?UID}),
            ?assertEqual(4, map_size(Payload)),
            ?assertEqual(false, maps:get(has_more, Payload)),
            ?assertEqual(null, maps:get(next_cursor, Payload)),
            ?assertEqual(20, maps:get(limit, Payload)),
            ?assertEqual(0, meck:num_calls(group_logic, page_by_owner, '_'))
        end
    ).

%%%===================================================================
%%% 工作区目录（workspace_handler group_list）：数值 cursor 旧语义
%%%===================================================================

%% 无 pagination → cursor=7 数值旧语义原样下推（J27.a：不带该参数 →
%% 数值 cursor 旧语义零变化）；envelope 为旧 Page + workspace_id。
workspace_group_list_legacy_numeric_cursor_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'binding', 2, fun(workspace_id, _Req) -> ?WS_ID_BIN end},
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"member_only">>, <<"1">>},
                        {<<"cursor">>, <<"7">>},
                        {<<"limit">>, <<"40">>}
                    ]
                end}
            ]},
            {workspace_logic, [
                {'ensure_member', 2, fun(?WS_ID, _Uid) -> {ok, 4} end}
            ]},
            {group_logic, [
                {'list_member_workspace_groups', 6, fun(?WS_ID, ?UID, Cursor, Limit, Preview, Q) ->
                    ?assertEqual(7, Cursor),
                    ?assertEqual(40, Limit),
                    ?assertEqual(false, Preview),
                    ?assertEqual(<<>>, Q),
                    {ok, #{list => [#{<<"gid">> => 5}], cursor => 7, limit => 40}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp, Payload} =
                workspace_handler:handle_action(group_list, Req, #{current_uid => ?UID}),
            ?assertEqual(?WS_ID, maps:get(workspace_id, Payload)),
            ?assertEqual(7, maps:get(cursor, Payload)),
            ?assertEqual(40, maps:get(limit, Payload)),
            ?assertEqual(false, maps:is_key(next_cursor, Payload)),
            ?assertEqual(false, maps:is_key(has_more, Payload))
        end
    ).

%% 边界负控：pagination=cursor + 无效游标 → 400 fail-closed，不回落首页、
%% 旧列表函数零调用（J27.c 同源合同在 handler 边界的钉子）。
workspace_group_cursor_invalid_token_no_fallback_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'binding', 2, fun(workspace_id, _Req) -> ?WS_ID_BIN end},
                {'parse_qs', 1, fun(_Req) ->
                    [
                        {<<"member_only">>, <<"1">>},
                        {<<"pagination">>, <<"cursor">>},
                        {<<"cursor">>, <<"tampered-token">>}
                    ]
                end}
            ]},
            {workspace_logic, [
                {'ensure_member', 2, fun(?WS_ID, _Uid) -> {ok, 4} end}
            ]},
            {list_cursor, [
                {'resolve', 4, fun(_Token, _Uid, _Kind, _Ctx) ->
                    {error, invalid_request}
                end}
            ]},
            {group_logic, [
                {'list_member_workspace_groups', 6, fun(_A, _B, _C, _D, _E, _F) ->
                    error(legacy_list_must_not_be_called_in_cursor_mode)
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, Msg, Status) -> {resp_err, Msg, Status} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {resp_err, <<"invalid_request：游标无效"/utf8>>, 400} =
                workspace_handler:handle_action(group_list, Req, #{current_uid => ?UID})
        end
    ).

%%%===================================================================
%%% ADM 面（MW/MG/MC Backend 侧锚点）：page,size,keyword 旧参数下推不变
%%%===================================================================

%% adm_group GET list：elib_param:page 解析的 Page/Size 与 keyword LIKE
%% 条件原样下推 group_ds:page，排序键 created_at DESC 不变；企业过滤参数
%% 全缺省（preset=<<>>/org=0/ws=0）不改变 Where。
adm_group_list_legacy_params_passthrough_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {imboy_plugin_registry, [
                {'required_feature', 3, fun(_Domain, _Handler, _Action) -> undefined end}
            ]},
            {adm_acl, [
                {'ensure_permission', 3, fun(_State, _Perm, _Req) -> ok end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {2, 20} end},
                {'int', 3, fun
                    (page, _Req, _Def) -> {ok, 2};
                    (size, _Req, _Def) -> {ok, 20};
                    (status, _Req, _Def) -> {ok, -1};
                    (type, _Req, _Def) -> {ok, -1};
                    (organization_id, _Req, _Def) -> {ok, 0};
                    (workspace_id, _Req, _Def) -> {ok, 0};
                    (Other, _Req, Def) -> error({unexpected_int_param, Other, Def})
                end},
                {'binary', 3, fun
                    (keyword, _Req, _Def) -> {ok, <<"客服"/utf8>>};
                    (preset, _Req, _Def) -> {ok, <<>>};
                    (Other, _Req, Def) -> error({unexpected_bin_param, Other, Def})
                end}
            ]},
            {adm_enterprise_filter, [
                {'org_workspace_ids', 1, fun(_ScopeParams) -> not_requested end},
                {'group_where', 3, fun(Where0, _ScopeParams, _OrgWsIds) -> Where0 end}
            ]},
            {group_ds, [
                {'page', 4, fun(Page, Size, Where, Order) ->
                    erlang:put(adm_group_page_args, {Page, Size, Where, Order}),
                    {ok, #{total => 0, list => []}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {ok, {resp, Payload}, _State} = adm_group_handler:init(Req, #{action => list}),
            %% envelope 旧形状原样透传（list/total 键保留）
            ?assertEqual(#{total => 0, list => []}, Payload),
            %% 旧参数下推：page=2、size=20（缺省）、keyword LIKE 条件、
            %% created_at DESC 排序不变
            Args = erlang:get(adm_group_page_args),
            ?assertMatch({2, 20, #{<<"__or">> := [_, _]}, <<"created_at DESC">>}, Args),
            {2, 20, Where, <<"created_at DESC">>} = Args,
            %% keyword LIKE 条件：title/introduction 双字段、{op, LIKE, 模式}
            %% 三元组、转义后的 %客服% 模式
            [
                #{title := {op, <<"LIKE">>, LikeTitle}},
                #{introduction := {op, <<"LIKE">>, LikeIntro}}
            ] =
                maps:get(<<"__or">>, Where),
            %% binary:match/2 成功返回 {Pos, Length}：%客服% → pos=1、len=6
            ?assertMatch({1, 6}, binary:match(LikeTitle, <<"客服"/utf8>>)),
            ?assertMatch({1, 6}, binary:match(LikeIntro, <<"客服"/utf8>>)),
            %% 包裹形态：%keyword%（escape_like 原值仅转义 LIKE 特殊字符）
            ?assertEqual(<<"%">>, binary:part(LikeTitle, 0, 1)),
            ?assertEqual(
                <<"%">>,
                binary:part(LikeTitle, byte_size(LikeTitle) - 1, 1)
            )
        end
    ).

%% adm_channel GET list：page/size 下推 channel_ds:page，缺省 status=-1 →
%% 空 Where、排序 id desc 不变；keyword 在该接口不参与过滤（旧合同原样）。
adm_channel_list_legacy_params_passthrough_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'parse_qs', 1, fun(_Req) -> [{<<"status">>, <<"-1">>}] end}
            ]},
            {imboy_plugin_registry, [
                {'required_feature', 3, fun(_Domain, _Handler, _Action) -> undefined end}
            ]},
            {adm_acl, [
                {'ensure_permission', 4, fun(_State, _Perm, _Req, _DefStatus) -> ok end}
            ]},
            {elib_param, [
                {'page', 1, fun(_Req) -> {3, 20} end},
                {'int', 3, fun
                    (page, _Req, _Def) -> {ok, 3};
                    (size, _Req, _Def) -> {ok, 20};
                    (organization_id, _Req, _Def) -> {ok, 0};
                    (workspace_id, _Req, _Def) -> {ok, 0};
                    (Other, _Req, Def) -> error({unexpected_int_param, Other, Def})
                end},
                {'binary', 3, fun
                    (preset, _Req, _Def) -> {ok, <<>>};
                    (Other, _Req, Def) -> error({unexpected_bin_param, Other, Def})
                end}
            ]},
            {adm_enterprise_filter, [
                {'org_workspace_ids', 1, fun(_ScopeParams) -> not_requested end},
                {'channel_where', 3, fun(Where0, _ScopeParams, _OrgWsIds) -> Where0 end}
            ]},
            {channel_ds, [
                {'page', 5, fun(Column, Where, Order, Page, Size) ->
                    erlang:put(adm_channel_page_args, {Where, Order, Page, Size}),
                    ?assert(is_binary(Column)),
                    {ok, #{total => 0, list => []}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            {ok, {resp, Payload}, _State} = adm_channel_handler:init(Req, #{action => list}),
            ?assertEqual(#{total => 0, list => []}, Payload),
            ?assertMatch({#{}, <<"id desc">>, 3, 20}, erlang:get(adm_channel_page_args))
        end
    ).

%%%===================================================================
%%% 辅助
%%%===================================================================

req_mock() ->
    #{mock_req => true}.
