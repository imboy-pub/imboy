%%% @doc talk_record T20 handler action 级测试（真实 DB 55436 + meck 传输边缘）。
%%%
%%% 覆盖（与 T17 handler 测试同法）：DELETE /:id 与删除后 GET 白名单的
%%% HTTP 语义（202/404/403/412/428 码位、T21 冻结的 logical_deleted/
%%% cleanup_status/physical_delete_state 三段拆分形状、白名单键集）。
-module(talk_record_t20_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REC(N), <<"31000000-0000-0000-0000-0000000000", (integer_to_binary(N))/binary>>).

setup() ->
    State = talk_record_t20_support:setup_all(),
    talk_record_t20_support:seed(State),
    _HandlerState = setup_mocks(),
    State.

setup_mocks() ->
    ok = meck:new(elib_response, [passthrough, unstick, no_link]),
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
    ok = meck:expect(cowboy_req, reply, fun(Status, Headers, Body, Req) ->
        Req#{
            response_status => Status,
            response_headers => maps:merge(maps:get(resp_headers, Req, #{}), Headers),
            response_body => Body
        }
    end),
    ok.

cleanup(State) ->
    catch meck:unload(cowboy_req),
    catch meck:unload(elib_param),
    catch meck:unload(elib_response),
    talk_record_t20_support:teardown_all(State).

req(Opts) ->
    cowboy_req_h:new(Opts).

with_db(TestFun) ->
    {timeout, 120,
        {setup, fun setup/0, fun cleanup/1, fun(State) ->
            ?_test(TestFun(State))
        end}}.

%%%===================================================================
%%% 夹具（编号段 11x）
%%%===================================================================

ready(State, N) ->
    Ids = talk_record_t20_support:ids(),
    U1 = maps:get(u1, Ids),
    {ok, {Rid, Cid, _}} = talk_record_t12_support:ready_record(
        U1, ?REC(N), <<"record_only">>, [0, 1], <<"t20h-", (integer_to_binary(N))/binary>>
    ),
    {ok, created, _} = talk_record_t12_support:complete(
        U1, Rid, Cid, <<"t20h-", (integer_to_binary(N))/binary, "-cmp">>
    ),
    {ok, #{outcome := succeeded}} = talk_record_job_worker:run_once(),
    {Rid, U1}.

delete_req(Rid, IfMatch) ->
    req(#{
        method => <<"DELETE">>,
        bindings => #{id => integer_to_binary(Rid)},
        headers => header(IfMatch)
    }).

header(undefined) ->
    #{};
header(IfMatch) ->
    #{<<"if-match">> => IfMatch}.

%% 202 走 reply/3（jsone 编码信封 binary）；200 走 elib_response mock
%% （map——success mock 不包信封，payload 即 body 本身）。
payload_of(R) ->
    Body = maps:get(response_body, R),
    case is_map(Body) of
        true ->
            case maps:is_key(<<"payload">>, Body) of
                true -> maps:get(<<"payload">>, Body);
                false -> Body
            end;
        false ->
            maps:get(<<"payload">>, jsone:decode(Body))
    end.

%%%===================================================================
%%% DELETE /:id
%%%===================================================================

delete_handler_202_test_() ->
    with_db(fun(State) ->
        {Rid, U1} = ready(State, 61),
        R1 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"1">>),
            #{current_uid => U1}
        ),
        ?assertEqual(202, maps:get(response_status, R1)),
        %% 信封 payload（reply/3 走 jsone 编码——解码后断言；T21 冻结三段拆分）
        Payload = payload_of(R1),
        ?assertEqual(Rid, maps:get(<<"id">>, Payload)),
        ?assertEqual(true, maps:get(<<"logical_deleted">>, Payload)),
        ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, Payload)),
        ?assert(is_binary(maps:get(<<"physical_delete_state">>, Payload)))
    end).

delete_handler_error_codes_test_() ->
    with_db(fun(State) ->
        C = talk_record_t20_support:conn_of(State),
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        U7 = maps:get(u7, Ids),
        {Rid, _} = ready(State, 62),
        %% 428：缺 If-Match
        R1 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, undefined),
            #{current_uid => U1}
        ),
        ?assertEqual(428, maps:get(response_status, R1)),
        %% 422：非法 If-Match
        R2 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"x">>),
            #{current_uid => U1}
        ),
        ?assertEqual(422, maps:get(response_status, R2)),
        %% 412：旧版本
        R3 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"9">>),
            #{current_uid => U1}
        ),
        ?assertEqual(412, maps:get(response_status, R3)),
        %% 403：viewer（可见非 owner）
        talk_record_t20_support:sql(C, [
            "INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, granted_by)"
            " VALUES (",
            integer_to_binary(Rid + 910000),
            ", ",
            integer_to_binary(Rid),
            ", ",
            integer_to_binary(U2),
            ", 'viewer', ",
            integer_to_binary(U1),
            ")"
        ]),
        R4 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"1">>),
            #{current_uid => U2}
        ),
        ?assertEqual(403, maps:get(response_status, R4)),
        %% 404：陌生人
        R5 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"1">>),
            #{current_uid => U7}
        ),
        ?assertEqual(404, maps:get(response_status, R5))
    end).

%%%===================================================================
%%% 删除后 GET 白名单（§11.3）
%%%===================================================================

get_after_delete_whitelist_test_() ->
    with_db(fun(State) ->
        Ids = talk_record_t20_support:ids(),
        U1 = maps:get(u1, Ids),
        U2 = maps:get(u2, Ids),
        {Rid, _} = ready(State, 63),
        %% 删除前 U2 是 viewer 可读（404 对比前置）
        talk_record_t20_support:sql(maps:get(conn, State), [
            "INSERT INTO talk_record_grant(id, record_id, grantee_user_id, role, granted_by)"
            " VALUES (",
            integer_to_binary(Rid + 920000),
            ", ",
            integer_to_binary(Rid),
            ", ",
            integer_to_binary(U2),
            ", 'viewer', ",
            integer_to_binary(U1),
            ")"
        ]),
        R0 = talk_record_handler:handle_action(
            item,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}}),
            #{current_uid => U2}
        ),
        ?assertEqual(200, maps:get(response_status, R0)),
        %% 删除
        RD = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"1">>),
            #{current_uid => U1}
        ),
        ?assertEqual(202, maps:get(response_status, RD)),
        %% owner GET：白名单 200（无正文域；read_content=false）
        R1 = talk_record_handler:handle_action(
            item,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}}),
            #{current_uid => U1}
        ),
        ?assertEqual(200, maps:get(response_status, R1)),
        P1 = payload_of(R1),
        ?assertEqual(true, maps:get(<<"logical_deleted">>, P1)),
        ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, P1)),
        ?assertNot(maps:is_key(<<"title">>, P1)),
        ?assertEqual(
            false,
            maps:get(
                <<"read_content">>,
                maps:get(<<"capabilities_for_actor">>, P1)
            )
        ),
        %% 非 owner GET：404（撤权 + 白名单仅 owner）
        R2 = talk_record_handler:handle_action(
            item,
            req(#{method => <<"GET">>, bindings => #{id => integer_to_binary(Rid)}}),
            #{current_uid => U2}
        ),
        ?assertEqual(404, maps:get(response_status, R2)),
        %% DELETE 重放（幂等 202）
        R3 = talk_record_handler:handle_action(
            item,
            delete_req(Rid, <<"1">>),
            #{current_uid => U1}
        ),
        ?assertEqual(202, maps:get(response_status, R3))
    end).

%%%===================================================================
%%% 方法面
%%%===================================================================

item_method_not_allowed_test_() ->
    with_db(fun(_State) ->
        R = talk_record_handler:handle_action(
            item,
            req(#{
                method => <<"PUT">>,
                bindings => #{id => <<"1">>},
                headers => #{<<"if-match">> => <<"1">>}
            }),
            #{current_uid => 1}
        ),
        ?assertEqual(405, maps:get(response_status, R))
    end).
