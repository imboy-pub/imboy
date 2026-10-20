-module(attach_direct_handler_tests).
-moduledoc "attach_direct_handler HTTP 面路由测试（code review P1 修复回归）".
%%%
% 覆盖 review 修复的必经路径：
%   * finalize 的 attempt_rejected 分支——此前逗号表达式把 Reason 当 Req
%     返回给 cowboy（该 HTTP 路径行为未定义，20b 面无此用例）。修复后
%     必须返回合法 Req + 409 信封 + reason 透出；
%   * 其余分支为 logic 层既有覆盖，此处不重复。
%%%

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

-define(UID, 990935).

mock_req() ->
    #{method => <<"POST">>, path => <<"/api/v1/attachment/direct">>}.

finalize_rejected_returns_req_and_reason_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'method', 1, fun(_) -> <<"POST">> end},
                {'bindings', 1, fun(_) -> #{action => <<"finalize">>} end},
                {'read_body', 1, fun(_) ->
                    {ok,
                        jsone:encode(#{
                            <<"object_key">> => <<"u1/f/x">>,
                            <<"attempt_id">> => 1
                        }), req_left}
                end}
            ]},
            {auth_ds, [{'current_uid', 1, fun(_) -> ?UID end}]},
            {attach_direct_logic, [
                %% require_key 归属校验放行
                {'status', 2, fun(_Key, _Uid) -> {ok, #{state => <<"GRANTED">>}} end},
                %% 被拒路径：copy 实际成功但 size 与 expected 不符
                {'finalize', 2, fun(_Key, _No) ->
                    {error, {rejected, size_mismatch}}
                end},
                {'reason_bin', 1, fun(R) -> atom_to_binary(R, utf8) end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Data) -> Req#{response_status => 200, data => Data} end},
                {'error', 4, fun(Req, _Msg, Code, Meta) ->
                    Req#{response_status => Code, err_meta => Meta}
                end}
            ]}
        ],
        fun() ->
            Req = mock_req(),
            %% 修复前：该分支返回 Reason 原子（垃圾 Req）→ 此处 badmatch 即暴露
            {ok, RespReq, _State} = attach_direct_handler:init(Req, #{}),
            ?assertEqual(?ERR_CONFLICT, maps:get(response_status, RespReq)),
            %% reason 一并透出（retry_action 之外）
            ?assertEqual(
                #{
                    <<"retry_action">> => <<"retry_attempt">>,
                    <<"reason">> => <<"size_mismatch">>
                },
                maps:get(err_meta, RespReq)
            )
        end
    ).
