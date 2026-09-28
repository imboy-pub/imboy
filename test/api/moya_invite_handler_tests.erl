%% moya_invite_handler_tests
%% W3：老师邀请码 → 家长加入班级 —— handler 契约测试。
%%
%% 直接模块调用（不起 cowboy，模式照 moya_learner_bind_handler_tests）：
%%   create_code —— path :id 解析、201 语义 payload（code + group_id
%%                  string，TSID 契约）、reason → HTTP/envelope 映射
%%   info       —— parse_qs code 参数缺失/空 → 422；成功 payload 的
%%                  learners id 一律 string（TSID 契约）
%%   join       —— body code/learner_id 必填（learner_id 兼容字符串）、
%%                  joined/already_joined 状态回显、learner_not_in_class
%%                  → 422 + 5489（?ERR_INVITE_CODE_INVALID 预留值）
%%
%% Mock 纪律：同一模块的全部期望合并为单条目（meck_helper 对同模块二次
%% setup 会清掉先装期望）。logic 模块 meck 时 code_fingerprint/1 一并
%% 打桩（handler 访问日志用，避免依赖 passthrough 行为）。
%%
%% 全部 meck，零真实网络、零真实库。

-module(moya_invite_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 990101).
-define(GROUP_ID, 771001).
-define(LEARNER_ID, 994101).
-define(CODE, <<"7Q3NB1M5KZ">>).
-define(FP, <<"fp0000000000">>).

%%%===================================================================
%%% Mock 基建
%%%===================================================================

%% BindingId：path :id；Qs：parse_qs 返回；Body：elib_param:post 返回。
%% 注意：moya_invite_logic 的期望**不在**这里——各 action mock 单独提供
%%（同模块二次 setup 会清掉先装期望，见文件头纪律）。
handler_mocks(BindingId, Qs, Body) ->
    [
        {cowboy_req, [
            {'binding', 2, fun(id, _Req) -> BindingId end},
            {'parse_qs', 1, fun(_Req) -> Qs end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> Body end}
        ]},
        {elib_response, [
            {'success_rfc3339', 2, fun(_Req, Payload) ->
                #{resp => success, payload => Payload}
            end},
            {'success_rfc3339', 3, fun(_Req, Payload, _Msg) ->
                #{resp => success, payload => Payload}
            end},
            {'error_with_status', 4, fun(_Req, Status, _Msg, Code) ->
                #{resp => error, http => Status, code => Code}
            end}
        ]}
    ].

create_logic_mock(Return) ->
    {moya_invite_logic, [
        {'create_or_get_code', 2, fun(Uid, GroupId) ->
            self() ! {logic_create, Uid, GroupId},
            Return
        end},
        {'code_fingerprint', 1, fun(_Code) -> ?FP end}
    ]}.

info_logic_mock(Return) ->
    {moya_invite_logic, [
        {'invite_info', 1, fun(Code) ->
            self() ! {logic_info, Code},
            Return
        end},
        {'code_fingerprint', 1, fun(_Code) -> ?FP end}
    ]}.

join_logic_mock(Return) ->
    {moya_invite_logic, [
        {'join', 3, fun(Uid, Code, LearnerId) ->
            self() ! {logic_join, Uid, Code, LearnerId},
            Return
        end},
        {'code_fingerprint', 1, fun(_Code) -> ?FP end}
    ]}.

%%%===================================================================
%%% create_code
%%%===================================================================

create_code_success_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"771001">>, [], #{}) ++ [create_logic_mock({ok, ?CODE})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                create_code,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            ?assertEqual(?CODE, maps:get(<<"code">>, Payload)),
            %% TSID 契约：group_id 回显一律 string
            ?assertEqual(integer_to_binary(?GROUP_ID), maps:get(<<"group_id">>, Payload)),
            receive
                {logic_create, ?UID, ?GROUP_ID} -> ok
            after 0 -> ?assert(false, "logic create_or_get_code not called with parsed args")
            end
        end
    ).

create_code_bad_path_id_test_() ->
    ?WITH_MECKS(
        handler_mocks(<<"not-an-int">>, [], #{}) ++ [create_logic_mock({ok, ?CODE})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                create_code,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req)),
            ?assertEqual(?ERR_MISSING_PARAM, maps:get(code, Req))
        end
    ).

create_code_error_mapping_test_() ->
    Cases = [
        {not_authorized, 403, ?ERR_FORBIDDEN},
        {not_found, 404, ?ERR_NOT_FOUND},
        {db_error, 500, ?ERR_ERROR}
    ],
    [
        begin
            {Reason, Http, Code} = Case,
            {
                lists:flatten(io_lib:format("create_code ~p -> ~p/~p", [Reason, Http, Code])),
                ?WITH_MECKS(
                    handler_mocks(<<"771001">>, [], #{}) ++ [create_logic_mock({error, Reason})],
                    fun() ->
                        Req = moya_invite_handler:handle_action(
                            create_code,
                            req0,
                            #{current_uid => ?UID}
                        ),
                        ?assertEqual(error, maps:get(resp, Req)),
                        ?assertEqual(Http, maps:get(http, Req)),
                        ?assertEqual(Code, maps:get(code, Req))
                    end
                )
            }
        end
     || Case <- Cases
    ].

%%%===================================================================
%%% info
%%%===================================================================

info_success_test_() ->
    Info = #{
        org_name => <<"逸云书法"/utf8>>,
        group_name => <<"周五班"/utf8>>,
        learners => [
            #{id => 994102, display_name => <<"陈小二"/utf8>>},
            #{id => ?LEARNER_ID, display_name => <<"张小一"/utf8>>}
        ]
    },
    ?WITH_MECKS(
        handler_mocks(undefined, [{<<"code">>, ?CODE}], #{}) ++ [info_logic_mock({ok, Info})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                info,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            Payload = maps:get(payload, Req),
            ?assertEqual(<<"逸云书法"/utf8>>, maps:get(<<"org_name">>, Payload)),
            ?assertEqual(<<"周五班"/utf8>>, maps:get(<<"group_name">>, Payload)),
            %% TSID 契约（STEP-04）：64-bit ID 一律 string，防 JS 精度丢失
            Learners = maps:get(<<"learners">>, Payload),
            ?assertEqual(
                [
                    #{<<"id">> => integer_to_binary(994102), <<"display_name">> => <<"陈小二"/utf8>>},
                    #{
                        <<"id">> => integer_to_binary(?LEARNER_ID),
                        <<"display_name">> => <<"张小一"/utf8>>
                    }
                ],
                Learners
            ),
            receive
                {logic_info, ?CODE} -> ok
            after 0 -> ?assert(false, "logic invite_info not called")
            end
        end
    ).

info_missing_code_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{}) ++ [info_logic_mock({ok, #{}})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                info,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req)),
            ?assertEqual(?ERR_MISSING_PARAM, maps:get(code, Req))
        end
    ).

info_empty_code_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [{<<"code">>, <<>>}], #{}) ++ [info_logic_mock({ok, #{}})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                info,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req))
        end
    ).

%% 码不存在/撤销/过期统一 404+404（防探测）
info_not_found_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [{<<"code">>, ?CODE}], #{}) ++
            [info_logic_mock({error, not_found})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                info,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(404, maps:get(http, Req)),
            ?assertEqual(?ERR_NOT_FOUND, maps:get(code, Req))
        end
    ).

%%%===================================================================
%%% join
%%%===================================================================

join_success_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{<<"code">> => ?CODE, <<"learner_id">> => ?LEARNER_ID}) ++
            [join_logic_mock({ok, joined})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                join,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            ?assertEqual(#{<<"status">> => <<"joined">>}, maps:get(payload, Req)),
            receive
                {logic_join, ?UID, ?CODE, ?LEARNER_ID} -> ok
            after 0 -> ?assert(false, "logic join not called with parsed args")
            end
        end
    ).

join_already_joined_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{<<"code">> => ?CODE, <<"learner_id">> => ?LEARNER_ID}) ++
            [join_logic_mock({ok, already_joined})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                join,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(success, maps:get(resp, Req)),
            ?assertEqual(#{<<"status">> => <<"already_joined">>}, maps:get(payload, Req))
        end
    ).

%% learner_id 字符串形态（JSON 数字字符串，TSID 前端常传 string）→ 解析为 integer
join_learner_id_string_test_() ->
    ?WITH_MECKS(
        handler_mocks(
            undefined,
            [],
            #{<<"code">> => ?CODE, <<"learner_id">> => <<"994101">>}
        ) ++
            [join_logic_mock({ok, joined})],
        fun() ->
            _ = moya_invite_handler:handle_action(
                join,
                req0,
                #{current_uid => ?UID}
            ),
            receive
                {logic_join, ?UID, ?CODE, ?LEARNER_ID} -> ok
            after 0 -> ?assert(false, "string learner_id not parsed to integer")
            end
        end
    ).

join_missing_learner_id_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{<<"code">> => ?CODE}) ++ [join_logic_mock({ok, joined})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                join,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req)),
            ?assertEqual(?ERR_MISSING_PARAM, maps:get(code, Req))
        end
    ).

join_missing_code_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{<<"learner_id">> => ?LEARNER_ID}) ++
            [join_logic_mock({ok, joined})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                join,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req))
        end
    ).

%% 学员不在该班 → 422 + 5489（error_code.hrl 的 ?ERR_INVITE_CODE_INVALID
%% 预留值，宏由并行任务注册；此处断言字面量，宏就位后随 handler 一并替换）
join_learner_not_in_class_test_() ->
    ?WITH_MECKS(
        handler_mocks(undefined, [], #{<<"code">> => ?CODE, <<"learner_id">> => ?LEARNER_ID}) ++
            [join_logic_mock({error, learner_not_in_class})],
        fun() ->
            Req = moya_invite_handler:handle_action(
                join,
                req0,
                #{current_uid => ?UID}
            ),
            ?assertEqual(error, maps:get(resp, Req)),
            ?assertEqual(422, maps:get(http, Req)),
            ?assertEqual(5489, maps:get(code, Req))
        end
    ).

join_error_mapping_test_() ->
    Cases = [
        {not_found, 404, ?ERR_NOT_FOUND},
        {db_error, 500, ?ERR_ERROR}
    ],
    [
        begin
            {Reason, Http, Code} = Case,
            {
                lists:flatten(io_lib:format("join ~p -> ~p/~p", [Reason, Http, Code])),
                ?WITH_MECKS(
                    handler_mocks(
                        undefined,
                        [],
                        #{<<"code">> => ?CODE, <<"learner_id">> => ?LEARNER_ID}
                    ) ++
                        [join_logic_mock({error, Reason})],
                    fun() ->
                        Req = moya_invite_handler:handle_action(
                            join,
                            req0,
                            #{current_uid => ?UID}
                        ),
                        ?assertEqual(error, maps:get(resp, Req)),
                        ?assertEqual(Http, maps:get(http, Req)),
                        ?assertEqual(Code, maps:get(code, Req))
                    end
                )
            }
        end
     || Case <- Cases
    ].
