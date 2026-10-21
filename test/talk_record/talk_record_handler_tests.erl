%%% @doc talk_record handler action 级测试（真实 DB 55433 + meck 传输边缘）。
%%%
%%% 覆盖层级声明：Handler action 函数 → logic → ds → repo → 真库全链路；
%%% 仅传输边缘（elib_param:post 读 body、elib_response 回复、
%%% cowboy_req:reply 201/cowboy_req_h）按仓内 bot_handler_tests 惯例 meck。
%%% 真实 HTTP（cowboy listener + middleware 链）留给 T22 集成验证。
-module(talk_record_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY1, <<"t05h-idem-key-1">>).
-define(UUID1, <<"00000000-0000-0000-0000-000000020001">>).

personal_body() ->
    #{
        <<"client_recording_id">> => ?UUID1,
        <<"notice_ack">> => true,
        <<"policy_version">> => <<"talk-record-notice-v1">>,
        <<"scope">> => <<"personal">>,
        <<"title">> => <<"处理器层记录"/utf8>>
    }.

setup() ->
    State = talk_record_t05_support:setup_all(),
    talk_record_t05_support:seed_fixtures(maps:get(conn, State)),
    HandlerState = setup_mocks(),
    State#{handler_mocks => HandlerState}.

setup_mocks() ->
    ok = meck:new(elib_response, [passthrough, unstick, no_link]),
    %% success/2：捕获 payload（其余 success 元数走 passthrough——真回复需要
    %% socket，本套件全部经捕获断言）
    ok = meck:expect(elib_response, success, fun(Req, Data) ->
        Req#{response_status => 200, response_body => Data}
    end),
    ok = meck:expect(elib_response, error_with_status, fun(Req, Status, Msg, Code) ->
        Req#{
            response_status => Status,
            response_body => #{<<"msg">> => Msg, <<"code">> => Code}
        }
    end),
    ok = meck:new(elib_param, [passthrough, unstick, no_link]),
    ok = meck:new(cowboy_req, [passthrough, unstick, no_link]),
    %% 201 信封（handler 本地 reply_created 走 cowboy_req:reply/4）；
    %% 405 走 reply/3。捕获合并 resp_headers（与 cowboy 真实 reply 的
    %% response_headers/2 合并语义一致）。
    ok = meck:expect(cowboy_req, reply, fun(Status, Headers, Body, Req) ->
        Req#{
            response_status => Status,
            response_headers => maps:merge(maps:get(resp_headers, Req, #{}), Headers),
            response_body => Body
        }
    end),
    ok = meck:expect(cowboy_req, reply, fun(Status, Req) ->
        Req#{response_status => Status}
    end),
    ok.

cleanup(State) ->
    catch meck:unload(cowboy_req),
    catch meck:unload(elib_param),
    catch meck:unload(elib_response),
    talk_record_t05_support:teardown_all(State).

mock_body(Body) ->
    ok = meck:expect(elib_param, post, fun(_Req) -> Body end).

req(Opts) ->
    cowboy_req_h:new(Opts).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% capabilities
%%%===================================================================

capabilities_action_test_() ->
    with_db(fun(_State) ->
        R = talk_record_handler:handle_action(
            capabilities, req(#{method => <<"GET">>}), #{current_uid => 700001}
        ),
        ?assertEqual(200, maps:get(response_status, R)),
        Cap = maps:get(response_body, R),
        ?assertEqual(false, maps:get(<<"asr_enabled">>, Cap)),
        ?assertEqual(false, maps:get(<<"processing_available">>, Cap))
    end).

%%%===================================================================
%%% create（201 信封 / 重放头 / 422-5461 / 422-5520）
%%%===================================================================

create_201_with_etag_test_() ->
    with_db(fun(_State) ->
        mock_body(personal_body()),
        R = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            #{current_uid => 700001}
        ),
        ?assertEqual(201, maps:get(response_status, R)),
        Env = jsone:decode(maps:get(response_body, R)),
        ?assertEqual(0, maps:get(<<"code">>, Env)),
        Payload = maps:get(<<"payload">>, Env),
        ?assert(is_integer(maps:get(<<"id">>, Payload))),
        ?assertEqual(1, maps:get(<<"revision">>, Payload)),
        %% ETag 同时落在响应头（set_resp_header）与 payload
        Hdrs = maps:get(resp_headers, R),
        ?assertEqual(<<"\"1\"">>, maps:get(<<"etag">>, Hdrs)),
        ?assertEqual(<<"\"1\"">>, maps:get(<<"etag">>, Payload)),
        ?assertNot(maps:is_key(<<"idempotent-replayed">>, Hdrs))
    end).

create_replay_header_test_() ->
    with_db(fun(_State) ->
        mock_body(personal_body()),
        S = #{current_uid => 700001},
        R1 = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            S
        ),
        Id1 = maps:get(
            <<"id">>, maps:get(<<"payload">>, jsone:decode(maps:get(response_body, R1)))
        ),
        R2 = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            S
        ),
        ?assertEqual(201, maps:get(response_status, R2)),
        Id2 = maps:get(
            <<"id">>, maps:get(<<"payload">>, jsone:decode(maps:get(response_body, R2)))
        ),
        ?assertEqual(Id1, Id2),
        ?assertEqual(
            <<"true">>,
            maps:get(<<"idempotent-replayed">>, maps:get(resp_headers, R2))
        )
    end).

create_missing_idem_key_test_() ->
    with_db(fun(_State) ->
        mock_body(personal_body()),
        R = talk_record_handler:handle_action(
            collection, req(#{method => <<"POST">>}), #{current_uid => 700001}
        ),
        ?assertEqual(422, maps:get(response_status, R)),
        ?assertEqual(5461, maps:get(<<"code">>, maps:get(response_body, R)))
    end).

create_notice_ack_missing_test_() ->
    with_db(fun(_State) ->
        mock_body(maps:remove(<<"notice_ack">>, personal_body())),
        R = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            #{current_uid => 700001}
        ),
        ?assertEqual(422, maps:get(response_status, R)),
        ?assertEqual(5520, maps:get(<<"code">>, maps:get(response_body, R)))
    end).

create_org_context_changed_409_test_() ->
    with_db(fun(_State) ->
        mock_body((personal_body())#{
            <<"scope">> => <<"organization">>,
            <<"organization_id">> => 710002,
            <<"client_recording_id">> => <<"00000000-0000-0000-0000-000000020002">>
        }),
        R = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            %% U1（700001）非 ORG_B 成员
            #{current_uid => 700001}
        ),
        ?assertEqual(409, maps:get(response_status, R)),
        ?assertEqual(5521, maps:get(<<"code">>, maps:get(response_body, R)))
    end).

%%%===================================================================
%%% detail / update（ETag 头 / 428 / 412 / 422 immutable）
%%%===================================================================

detail_with_etag_test_() ->
    with_db(fun(_State) ->
        mock_body(personal_body()),
        S = #{current_uid => 700001},
        R0 = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            S
        ),
        Id = maps:get(<<"id">>, maps:get(<<"payload">>, jsone:decode(maps:get(response_body, R0)))),
        R = talk_record_handler:handle_action(
            item, req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Id)}}), S
        ),
        ?assertEqual(200, maps:get(response_status, R)),
        Payload = maps:get(response_body, R),
        ?assertEqual(Id, maps:get(<<"id">>, Payload)),
        ?assert(maps:is_key(<<"capabilities_for_actor">>, Payload)),
        ?assertEqual(<<"\"1\"">>, maps:get(<<"etag">>, maps:get(resp_headers, R))),

        %% 无权主体（700004）：统一 404
        R2 = talk_record_handler:handle_action(
            item,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Id)}}),
            #{current_uid => 700004}
        ),
        ?assertEqual(404, maps:get(response_status, R2))
    end).

update_preconditions_test_() ->
    with_db(fun(_State) ->
        mock_body(personal_body()),
        S = #{current_uid => 700001},
        R0 = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            S
        ),
        Id = maps:get(<<"id">>, maps:get(<<"payload">>, jsone:decode(maps:get(response_body, R0)))),
        ItemReq = req(#{method => <<"PATCH">>, bindings => #{id => integer_to_binary(Id)}}),

        %% 缺 If-Match → 428（5525）
        mock_body(#{<<"notes">> => <<"n"/utf8>>}),
        R1 = talk_record_handler:handle_action(item, ItemReq, S),
        ?assertEqual(428, maps:get(response_status, R1)),
        ?assertEqual(5525, maps:get(<<"code">>, maps:get(response_body, R1))),

        %% 旧版本 → 412（5526）
        R2 = talk_record_handler:handle_action(
            item,
            req(#{
                method => <<"PATCH">>,
                bindings => #{id => integer_to_binary(Id)},
                headers => #{<<"if-match">> => <<"\"9\"">>}
            }),
            S
        ),
        ?assertEqual(412, maps:get(response_status, R2)),
        ?assertEqual(5526, maps:get(<<"code">>, maps:get(response_body, R2))),

        %% 正确版本 → 200 + revision=2 + 新 ETag 头
        R3 = talk_record_handler:handle_action(
            item,
            req(#{
                method => <<"PATCH">>,
                bindings => #{id => integer_to_binary(Id)},
                headers => #{<<"if-match">> => <<"\"1\"">>}
            }),
            S
        ),
        ?assertEqual(200, maps:get(response_status, R3)),
        ?assertEqual(2, maps:get(<<"revision">>, maps:get(response_body, R3))),
        ?assertEqual(<<"\"2\"">>, maps:get(<<"etag">>, maps:get(resp_headers, R3))),

        %% 保留字段 → 422（5523）
        mock_body(#{<<"scope">> => <<"organization">>}),
        R4 = talk_record_handler:handle_action(
            item,
            req(#{
                method => <<"PATCH">>,
                bindings => #{id => integer_to_binary(Id)},
                headers => #{<<"if-match">> => <<"\"2\"">>}
            }),
            S
        ),
        ?assertEqual(422, maps:get(response_status, R4)),
        ?assertEqual(5523, maps:get(<<"code">>, maps:get(response_body, R4)))
    end).

%%%===================================================================
%%% list（真 qs 解析 + 响应形态）
%%%===================================================================

list_action_test_() ->
    with_db(fun(_State) ->
        mock_body(personal_body()),
        S = #{current_uid => 700001},
        _ = talk_record_handler:handle_action(
            collection,
            req(#{method => <<"POST">>, headers => #{<<"idempotency-key">> => ?KEY1}}),
            S
        ),
        R = talk_record_handler:handle_action(
            collection, req(#{method => <<"GET">>, qs => <<"filter=owned&limit=5">>}), S
        ),
        ?assertEqual(200, maps:get(response_status, R)),
        Payload = maps:get(response_body, R),
        ?assertEqual(1, length(maps:get(<<"list">>, Payload))),
        ?assertEqual(null, maps:get(<<"next_cursor">>, Payload)),

        %% 无权主体看不到
        R2 = talk_record_handler:handle_action(
            collection, req(#{method => <<"GET">>, qs => <<>>}), #{current_uid => 700004}
        ),
        ?assertEqual(200, maps:get(response_status, R2)),
        ?assertEqual(0, length(maps:get(<<"list">>, maps:get(response_body, R2))))
    end).

creation_contexts_action_test_() ->
    with_db(fun(_State) ->
        R = talk_record_handler:handle_action(
            creation_contexts, req(#{method => <<"GET">>, qs => <<>>}), #{current_uid => 700001}
        ),
        ?assertEqual(200, maps:get(response_status, R)),
        Payload = maps:get(response_body, R),
        ?assertEqual(<<"personal">>, maps:get(<<"scope">>, maps:get(<<"personal">>, Payload))),
        ?assertEqual(1, length(maps:get(<<"organizations">>, Payload)))
    end).

method_not_allowed_test_() ->
    with_db(fun(_State) ->
        R = talk_record_handler:handle_action(
            collection, cowboy_req_h:new(#{method => <<"DELETE">>}), #{}
        ),
        ?assertEqual(405, maps:get(response_status, R))
    end).
