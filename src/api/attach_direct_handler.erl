-module(attach_direct_handler).
-moduledoc "direct-v1 直传会话 HTTP 面（协议 §2；HTTP200 信封 + state/retry_action）。".
%%%
% 端点：POST /api/v1/attachment/direct/:action
%   register | declare | grant | renew | finalize（POST，JSON 体）
%   status（GET ?object_key=）
% 认证沿 auth_middleware + auth_ds:current_uid；跨主体一律 not found（不泄露）。
% FINALIZING 等处理中状态以信封 state 表达（禁 202）；客户端按 retry_action 行动。
%%%

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    #{action := Action} = cowboy_req:bindings(Req0),
    %% cowboy 普通协议：回复后必须返回 {ok, Req, State}（否则每请求
    %% try_clause 崩 cowboy_handler 且拖死 lager handler）
    Req = route(Method, Action, Req0, State),
    {ok, Req, State}.

route(<<"POST">>, <<"register">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = body_map(Req0),
    FileName = maps:get(<<"filename">>, Body, <<"file">>),
    Scope = maps:get(<<"scope">>, Body, <<"private">>),
    ScopeRef = maps:get(<<"scope_ref">>, Body, undefined),
    ClientUploadId = maps:get(<<"client_upload_id">>, Body, undefined),
    case attach_direct_logic:register(Uid, Scope, ScopeRef, ClientUploadId, FileName) of
        {ok, Info} ->
            reply_ok(Req0, Info);
        {error, declaration_conflict} ->
            reply_err(Req0, ?ERR_CONFLICT, <<"declaration_conflict">>, new_session);
        {error, _} ->
            reply_err(Req0, ?ERR_SERVICE_UNAVAILABLE, <<"service_unavailable">>, retry_session)
    end;
route(<<"POST">>, <<"declare">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = body_map(Req0),
    case require_key(Body, Uid, Req0) of
        {ok, Key} ->
            Sha = maps:get(<<"sha256">>, Body, <<>>),
            Size = elib_cnv:safe_to_integer(maps:get(<<"size_bytes">>, Body, 0)),
            Mime = maps:get(<<"mime">>, Body, <<"application/octet-stream">>),
            case valid_sha(Sha) andalso Size >= 0 of
                true ->
                    case attach_direct_logic:declare(Key, Sha, Size, Mime) of
                        ok ->
                            reply_ok(Req0, #{object_key => Key, state => <<"DECLARED">>});
                        {error, size_limit_exceeded} ->
                            reply_err(
                                Req0, ?ERR_BAD_REQUEST, <<"size_limit_exceeded">>, new_session
                            );
                        {error, expected_conflict} ->
                            reply_err(Req0, ?ERR_CONFLICT, <<"expected_conflict">>, abort);
                        {error, not_found} ->
                            reply_err(Req0, ?ERR_NOT_FOUND, <<"not_found">>, retry_session);
                        {error, _} ->
                            reply_err(
                                Req0,
                                ?ERR_SERVICE_UNAVAILABLE,
                                <<"service_unavailable">>,
                                retry_session
                            )
                    end;
                false ->
                    reply_err(Req0, ?ERR_BAD_REQUEST, <<"invalid declaration">>, retry_session)
            end;
        {error, R} ->
            R
    end;
route(<<"POST">>, <<"grant">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = body_map(Req0),
    case require_key(Body, Uid, Req0) of
        {ok, Key} ->
            Sha = maps:get(<<"sha256">>, Body, <<>>),
            case attach_direct_logic:grant(Key, Sha) of
                {ok, Info} ->
                    reply_ok(Req0, Info);
                {error, illegal_state} ->
                    reply_err(Req0, ?ERR_CONFLICT, <<"illegal_state">>, poll);
                {error, not_found} ->
                    reply_err(Req0, ?ERR_NOT_FOUND, <<"not_found">>, retry_session);
                {error, _} ->
                    reply_err(
                        Req0, ?ERR_SERVICE_UNAVAILABLE, <<"service_unavailable">>, retry_session
                    )
            end;
        {error, R} ->
            R
    end;
route(<<"POST">>, <<"renew">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = body_map(Req0),
    case require_key(Body, Uid, Req0) of
        {ok, Key} ->
            Sha = maps:get(<<"sha256">>, Body, <<>>),
            case attach_direct_logic:renew_grant(Key, Sha) of
                {ok, Info} ->
                    reply_ok(Req0, Info);
                {error, attempt_budget_exhausted} ->
                    reply_err(Req0, ?ERR_CONFLICT, <<"attempt_budget_exhausted">>, abort);
                {error, illegal_state} ->
                    reply_err(Req0, ?ERR_CONFLICT, <<"illegal_state">>, poll);
                {error, not_found} ->
                    reply_err(Req0, ?ERR_NOT_FOUND, <<"not_found">>, retry_session);
                {error, _} ->
                    reply_err(
                        Req0, ?ERR_SERVICE_UNAVAILABLE, <<"service_unavailable">>, retry_session
                    )
            end;
        {error, R} ->
            R
    end;
route(<<"POST">>, <<"finalize">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Body = body_map(Req0),
    case require_key(Body, Uid, Req0) of
        {ok, Key} ->
            AttemptNo = elib_cnv:safe_to_integer(maps:get(<<"attempt_id">>, Body, 0)),
            case AttemptNo >= 1 of
                true ->
                    case attach_direct_logic:finalize(Key, AttemptNo) of
                        {ok, #{state := <<"CONFIRMED">>} = Info} ->
                            reply_ok(Req0, Info);
                        {ok, #{outcome := <<"UNKNOWN_QUARANTINED">>} = Info} ->
                            %% 处理中/未知：200 信封 + state=FINALIZING；重试=新 attempt
                            reply_ok(Req0, Info#{retry_action => retry_attempt});
                        {error, {rejected, Reason}} ->
                            %% reason 一并透出（客户端需区分 size_mismatch/
                            %% copy_source_missing 等；此前仅 attempt_rejected）
                            reply_err(
                                Req0,
                                ?ERR_CONFLICT,
                                <<"attempt_rejected">>,
                                retry_attempt,
                                #{
                                    <<"reason">> =>
                                        attach_direct_logic:reason_bin(Reason)
                                }
                            );
                        {error, attempt_not_dispatchable} ->
                            reply_err(Req0, ?ERR_CONFLICT, <<"attempt_not_dispatchable">>, poll);
                        {error, not_found} ->
                            reply_err(Req0, ?ERR_NOT_FOUND, <<"not_found">>, retry_session);
                        {error, _} ->
                            reply_err(
                                Req0, ?ERR_SERVICE_UNAVAILABLE, <<"service_unavailable">>, poll
                            )
                    end;
                false ->
                    reply_err(Req0, ?ERR_BAD_REQUEST, <<"attempt_id required">>, retry_session)
            end;
        {error, R} ->
            R
    end;
route(<<"GET">>, <<"status">>, Req0, State) ->
    Uid = auth_ds:current_uid(State),
    Qs = cowboy_req:parse_qs(Req0),
    case proplists:get_value(<<"object_key">>, Qs, undefined) of
        undefined ->
            reply_err(Req0, ?ERR_BAD_REQUEST, <<"object_key required">>, retry_session);
        Key ->
            case attach_direct_logic:status(Key, Uid) of
                {ok, Info} ->
                    reply_ok(Req0, Info);
                {error, not_found} ->
                    reply_err(Req0, ?ERR_NOT_FOUND, <<"not_found">>, retry_session);
                {error, _} ->
                    reply_err(Req0, ?ERR_SERVICE_UNAVAILABLE, <<"service_unavailable">>, poll)
            end
    end;
route(_, _, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% ===== 内部 =====

body_map(Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, _} ->
            case jsone:decode(Data, [{object_format, map}]) of
                M when is_map(M) -> M;
                _ -> #{}
            end;
        _ ->
            #{}
    end.

require_key(Body, Uid, Req0) ->
    case maps:get(<<"object_key">>, Body, undefined) of
        undefined ->
            {error, reply_err(Req0, ?ERR_BAD_REQUEST, <<"object_key required">>, retry_session)};
        Key ->
            %% 归属校验：跨主体不泄露（status 同合同）
            case attach_direct_logic:status(Key, Uid) of
                {ok, _} ->
                    {ok, Key};
                {error, not_found} ->
                    {error, reply_err(Req0, ?ERR_NOT_FOUND, <<"not_found">>, retry_session)};
                {error, _} ->
                    {error,
                        reply_err(
                            Req0, ?ERR_SERVICE_UNAVAILABLE, <<"service_unavailable">>, retry_session
                        )}
            end
    end.

valid_sha(Bin) when is_binary(Bin), byte_size(Bin) =:= 64 ->
    lists:all(
        fun(C) -> (C >= $0 andalso C =< $9) orelse (C >= $a andalso C =< $f) end,
        binary_to_list(Bin)
    );
valid_sha(_) ->
    false.

reply_ok(Req0, Info) ->
    elib_response:success(Req0, to_js(Info)).

to_js(M) when is_map(M) ->
    maps:map(fun(_K, V) -> to_js(V) end, M);
to_js(A) when is_atom(A) -> atom_to_binary(A, utf8);
%% jsone 不编码 tuple：required_headers 等 KV 对转二元列表（I04 线格式）
to_js(L) when is_list(L) -> [to_js(E) || E <- L];
to_js(T) when is_tuple(T) -> [to_js(E) || E <- tuple_to_list(T)];
to_js(V) ->
    V.

reply_err(Req0, Code, Msg, RetryAction) ->
    reply_err(Req0, Code, Msg, RetryAction, #{}).

reply_err(Req0, Code, Msg, RetryAction, ExtraMeta) ->
    elib_response:error(
        Req0,
        to_js(Msg),
        Code,
        maps:merge(
            #{<<"retry_action">> => to_js(RetryAction)},
            ExtraMeta
        )
    ).
