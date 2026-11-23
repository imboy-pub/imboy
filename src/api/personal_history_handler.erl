-module(personal_history_handler).

-moduledoc "个人消息历史域 HTTP 适配层（MH-01 合同 v1.1 / MH-02）。".
%%%
%%% personal_history_handler —— 个人消息历史域薄 HTTP 适配层
%%%
%%% 原生面 /api/v1/personal-history/*（verify_sign 设备签名链不变）；
%%% Web N1 面 /api/v1/web/personal-history/*（web_human 认证链，路由别名
%%% 由 imboy_router:web_human_routes/1 生成，两面同合同同权限谓词 §10.1）。
%%% 认证主体 = JWT uid（auth_ds:current_uid，服务端权威 §1）。
%%%

-behaviour(cowboy_handler).

-export([init/2]).
-export([handle_action/3]).

-include("common.hrl").
-include("error_code.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 = handle_action(Action, Req0, State),
    {ok, Req1, State}.

%% @doc Action 分发处理
-spec handle_action(atom() | false, cowboy_req:req(), map()) -> cowboy_req:req().
handle_action(status, Req, State) -> status(Req, State);
handle_action(opt_in, Req, State) -> opt_in(Req, State);
handle_action(upload, Req, State) -> upload(Req, State);
handle_action(changes, Req, State) -> changes(Req, State);
handle_action(ack_cursor, Req, State) -> ack_cursor(Req, State);
handle_action(page, Req, State) -> page(Req, State);
handle_action(key_package, Req, State) -> key_package(Req, State);
handle_action(disable, Req, State) -> disable(Req, State);
handle_action(delete_status, Req, State) -> delete_status(Req, State);
handle_action(false, Req, _State) -> Req.

%% ===================================================================
%% 2.1 GET /personal-history/status
%% ===================================================================

-spec status(cowboy_req:req(), map()) -> cowboy_req:req().
status(Req0, State) ->
    reply(Req0, personal_history_logic:status(auth_ds:current_uid(State))).

%% ===================================================================
%% 2.2 PUT /personal-history/opt-in
%% ===================================================================

-spec opt_in(cowboy_req:req(), map()) -> cowboy_req:req().
opt_in(Req0, State) ->
    PostVals = elib_param:post(Req0),
    ClientInstanceId = maps:get(<<"client_instance_id">>, PostVals, <<>>),
    case is_binary(ClientInstanceId) andalso byte_size(ClientInstanceId) > 0 of
        false ->
            elib_response:error(Req0, human_msg(<<"malformed_request">>), ?ERR_INVALID_PARAM);
        true ->
            reply(Req0, personal_history_logic:opt_in(auth_ds:current_uid(State), ClientInstanceId))
    end.

%% ===================================================================
%% 2.3 POST /personal-history/upload
%% ===================================================================

-spec upload(cowboy_req:req(), map()) -> cowboy_req:req().
upload(Req0, State) ->
    PostVals = elib_param:post(Req0),
    reply(Req0, personal_history_logic:upload(auth_ds:current_uid(State), PostVals)).

%% ===================================================================
%% 2.4 GET /personal-history/changes | ack-cursor | page
%% ===================================================================

-spec changes(cowboy_req:req(), map()) -> cowboy_req:req().
changes(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Opts = #{
        cursor => int_qs(<<"cursor">>, Qs, 0),
        limit => int_qs(<<"limit">>, Qs, 50),
        client_instance => proplists:get_value(<<"client_instance">>, Qs, <<>>)
    },
    reply_cursor(Req0, State, Opts, fun personal_history_logic:changes/2).

-spec ack_cursor(cowboy_req:req(), map()) -> cowboy_req:req().
ack_cursor(Req0, State) ->
    PostVals = elib_param:post(Req0),
    Opts = #{
        cursor => maps:get(<<"cursor">>, PostVals, -1),
        generation => maps:get(<<"generation">>, PostVals, undefined),
        client_instance => maps:get(<<"client_instance">>, PostVals, <<>>)
    },
    reply_cursor(Req0, State, Opts, fun personal_history_logic:ack_cursor/2).

reply_cursor(Req0, State, #{client_instance := Instance} = Opts, Run) ->
    case valid_client_instance(State, Instance) of
        true -> reply(Req0, Run(auth_ds:current_uid(State), Opts));
        false -> reply_error(Req0, <<"identity_rejected">>)
    end.

valid_client_instance(#{surface := web_human} = State, Instance) ->
    is_binary(Instance) andalso byte_size(Instance) > 0 andalso
        Instance =:= auth_ds:current_did(State);
valid_client_instance(_State, _Instance) ->
    true.

-spec page(cowboy_req:req(), map()) -> cowboy_req:req().
page(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Opts = #{
        conversation_id => proplists:get_value(<<"conversation_id">>, Qs, <<>>),
        before => int_qs(<<"before">>, Qs, 0),
        page_token => proplists:get_value(<<"page_token">>, Qs, undefined)
    },
    reply(Req0, personal_history_logic:page(auth_ds:current_uid(State), Opts)).

%% ===================================================================
%% 2.5 PUT /personal-history/key-package
%% 2.5b GET /personal-history/key-package（mh04-key-package-read-addendum）
%% ===================================================================

-spec key_package(cowboy_req:req(), map()) -> cowboy_req:req().
key_package(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> ->
            get_key_package(Req0, State);
        _ ->
            PostVals = elib_param:post(Req0),
            reply(
                Req0, personal_history_logic:put_key_package(auth_ds:current_uid(State), PostVals)
            )
    end.

%% @doc 恢复读取入口：查询参数恰好一份 key_version 与一份 generation
%% （十进制 ASCII 数字，无符号/前导零/指数/浮点；域 0..9007199254740991）。
%% 所有响应（含错误）带 Cache-Control: no-store；错误同域固定包络。
-spec get_key_package(cowboy_req:req(), map()) -> cowboy_req:req().
get_key_package(Req0, State) ->
    Qs = cowboy_req:parse_qs(Req0),
    Req1 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-store">>, Req0),
    KeyVersions = proplists:get_all_values(<<"key_version">>, Qs),
    Generations = proplists:get_all_values(<<"generation">>, Qs),
    case {KeyVersions, Generations} of
        {[KeyVersion], [Generation]} ->
            reply(
                Req1,
                personal_history_logic:get_key_package(
                    auth_ds:current_uid(State),
                    #{key_version => KeyVersion, generation => Generation}
                )
            );
        _ ->
            %% 重复/缺失份数：与参数无效同型（422 malformed_package_request）。
            reply_error(Req1, <<"malformed_package_request">>)
    end.

%% ===================================================================
%% 2.6 POST /personal-history/disable | GET delete-status
%% ===================================================================

-spec disable(cowboy_req:req(), map()) -> cowboy_req:req().
disable(Req0, State) ->
    Uid = auth_ds:current_uid(State),
    case personal_history_logic:disable(Uid) of
        {ok, Payload} ->
            %% 202 Accepted：purge 已受理（在线段同步执行完毕或可重入补齐）
            elib_response:error_with_status(
                Req0, 202, <<"purge accepted">>, Payload, ?ERR_OK
            );
        {error, Code, _HttpStatus} ->
            reply_error(Req0, Code)
    end.

-spec delete_status(cowboy_req:req(), map()) -> cowboy_req:req().
delete_status(Req0, State) ->
    reply(Req0, personal_history_logic:delete_status(auth_ds:current_uid(State))).

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

-spec int_qs(binary(), list(), integer()) -> integer().
int_qs(Key, Qs, Default) ->
    case proplists:get_value(Key, Qs, undefined) of
        undefined ->
            Default;
        Bin ->
            try
                binary_to_integer(Bin)
            catch
                _:_ -> Default
            end
    end.

reply(Req0, {ok, Payload}) ->
    elib_response:success(Req0, Payload);
reply(Req0, {error, Code, HttpStatus}) ->
    Req1 = reply_error(Req0, Code),
    _ = HttpStatus,
    Req1.

reply_error(Req0, Code) when is_binary(Code) ->
    elib_response:error_with_status(
        Req0, http_status(Code), human_msg(Code), #{<<"error">> => Code}, envelope_code(Code)
    ).

%% 合同 §2.7 错误表 → HTTP 状态 / envelope 业务码
http_status(<<"not_enabled">>) -> 403;
http_status(<<"not_participant">>) -> 403;
http_status(<<"identity_rejected">>) -> 403;
http_status(<<"generation_stale">>) -> 409;
http_status(<<"purge_in_progress">>) -> 409;
http_status(<<"version_conflict">>) -> 409;
http_status(<<"nonce_reused">>) -> 409;
http_status(<<"generation_expired">>) -> 410;
http_status(<<"page_token_expired">>) -> 410;
http_status(<<"rescan_bound_exceeded">>) -> 410;
http_status(<<"key_package_missing">>) -> 422;
http_status(<<"backfill_evidence_missing">>) -> 422;
http_status(<<"malformed_package">>) -> 422;
http_status(<<"malformed_package_request">>) -> 422;
http_status(<<"malformed_request">>) -> 422;
http_status(<<"transient_failure">>) -> 503;
http_status(_) -> 500.

envelope_code(<<"not_enabled">>) -> ?ERR_FORBIDDEN;
envelope_code(<<"not_participant">>) -> ?ERR_FORBIDDEN;
envelope_code(<<"identity_rejected">>) -> ?ERR_FORBIDDEN;
envelope_code(<<"generation_stale">>) -> ?ERR_CONFLICT;
envelope_code(<<"purge_in_progress">>) -> ?ERR_CONFLICT;
envelope_code(<<"version_conflict">>) -> ?ERR_CONFLICT;
envelope_code(<<"nonce_reused">>) -> ?ERR_CONFLICT;
envelope_code(<<"generation_expired">>) -> ?ERR_GONE;
envelope_code(<<"page_token_expired">>) -> ?ERR_GONE;
envelope_code(<<"rescan_bound_exceeded">>) -> ?ERR_GONE;
envelope_code(<<"key_package_missing">>) -> ?ERR_MISSING_PARAM;
envelope_code(<<"backfill_evidence_missing">>) -> ?ERR_UNPROCESSABLE_ENTITY;
envelope_code(<<"malformed_package">>) -> ?ERR_UNPROCESSABLE_ENTITY;
envelope_code(<<"malformed_package_request">>) -> ?ERR_UNPROCESSABLE_ENTITY;
envelope_code(<<"malformed_request">>) -> ?ERR_UNPROCESSABLE_ENTITY;
envelope_code(<<"transient_failure">>) -> 503;
envelope_code(_) -> ?ERR_ERROR.

%% 机器码 → 中文人类可读消息（客户端按 HTTP/envelope 码分支，不匹配消息串）
human_msg(<<"not_enabled">>) -> <<"未开启个人云端历史"/utf8>>;
human_msg(<<"not_participant">>) -> <<"非会话参与方"/utf8>>;
human_msg(<<"identity_rejected">>) -> <<"身份校验失败"/utf8>>;
human_msg(<<"generation_stale">>) -> <<"授权代次已过期，请刷新状态"/utf8>>;
human_msg(<<"purge_in_progress">>) -> <<"历史清理进行中，请稍后"/utf8>>;
human_msg(<<"version_conflict">>) -> <<"版本冲突，请刷新后重试"/utf8>>;
human_msg(<<"nonce_reused">>) -> <<"nonce 已使用，禁止重用"/utf8>>;
human_msg(<<"generation_expired">>) -> <<"授权代次已失效"/utf8>>;
human_msg(<<"page_token_expired">>) -> <<"分页令牌已过期，请重新扫描"/utf8>>;
human_msg(<<"rescan_bound_exceeded">>) -> <<"重扫链长超限，请从头分页"/utf8>>;
human_msg(<<"key_package_missing">>) -> <<"缺少对应版本的密钥包"/utf8>>;
human_msg(<<"backfill_evidence_missing">>) -> <<"缺少消息受理证明"/utf8>>;
human_msg(<<"malformed_package">>) -> <<"密钥包格式非法"/utf8>>;
human_msg(<<"malformed_package_request">>) -> <<"密钥包查询参数非法"/utf8>>;
human_msg(<<"malformed_request">>) -> <<"请求参数非法"/utf8>>;
human_msg(<<"transient_failure">>) -> <<"服务暂时不可用，请安全重试"/utf8>>;
human_msg(_) -> <<"操作失败"/utf8>>.

%% ===================================================================
%% EUnit tests.
%% ===================================================================

-ifdef(EUNIT).
web_cursor_device_binding_test() ->
    Web = #{surface => web_human, current_did => <<"did-a">>},
    ?assert(valid_client_instance(Web, <<"did-a">>)),
    ?assertNot(valid_client_instance(Web, <<"did-b">>)),
    ?assertNot(valid_client_instance(maps:remove(current_did, Web), <<>>)),
    ?assert(valid_client_instance(#{current_did => <<"did-a">>}, <<"legacy-instance">>)).
-endif.
