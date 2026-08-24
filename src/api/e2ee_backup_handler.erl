-module(e2ee_backup_handler).

%% Thin HTTP adapter for the E2EE encrypted key backup boundary (4S 模式).
%% 服务端只存客户端加密后的密文包，本模块零密码学。

-behavior(cowboy_rest).

-export([init/2]).

-include("common.hrl").
-include("error_code.hrl").

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Req1 =
        case Action of
            put ->
                put_backup(Req0, State);
            get ->
                get_backup(Req0, State);
            info ->
                info(Req0, State);
            delete ->
                delete_backup(Req0, State);
            _ ->
                elib_response:error(Req0, <<"not_found">>, 404)
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Capability Gate
%% ===================================================================

%% @doc E2EE 能力闸门，与 e2ee_handler 一致
-spec ensure_e2ee_enabled(cowboy_req:req()) -> ok | {error, cowboy_req:req()}.
ensure_e2ee_enabled(Req0) ->
    case imboy_policy:e2ee_enabled() of
        true ->
            ok;
        false ->
            {error,
                elib_response:error(
                    Req0,
                    imboy_error:error_msg(?ERR_FEATURE_DISABLED),
                    ?ERR_FEATURE_DISABLED
                )}
    end.

%% ===================================================================
%% API Handlers
%% ===================================================================

-spec put_backup(cowboy_req:req(), map()) -> cowboy_req:req().
put_backup(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_put_backup(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_put_backup(cowboy_req:req(), map()) -> cowboy_req:req().
do_put_backup(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case throttle:check(e2ee_backup, CurrentUid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"操作过于频繁，请稍后再试"/utf8>>, 429);
        _ ->
            PostVals = elib_param:post(Req0),
            case e2ee_backup_logic:put_backup(CurrentUid, PostVals) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Code} ->
                    elib_response:error(Req0, backup_msg(Msg), Code)
            end
    end.

-spec get_backup(cowboy_req:req(), map()) -> cowboy_req:req().
get_backup(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_get_backup(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_get_backup(cowboy_req:req(), map()) -> cowboy_req:req().
do_get_backup(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case e2ee_backup_logic:get_backup(CurrentUid) of
        {ok, Payload} ->
            elib_response:success(Req0, Payload);
        {error, Msg, Code} ->
            elib_response:error(Req0, backup_msg(Msg), Code)
    end.

-spec info(cowboy_req:req(), map()) -> cowboy_req:req().
info(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_info(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_info(cowboy_req:req(), map()) -> cowboy_req:req().
do_info(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case e2ee_backup_logic:info(CurrentUid) of
        {ok, Payload} ->
            elib_response:success(Req0, Payload);
        {error, Msg, Code} ->
            elib_response:error(Req0, backup_msg(Msg), Code)
    end.

-spec delete_backup(cowboy_req:req(), map()) -> cowboy_req:req().
delete_backup(Req0, State) ->
    case ensure_e2ee_enabled(Req0) of
        ok ->
            do_delete_backup(Req0, State);
        {error, Req1} ->
            Req1
    end.

-spec do_delete_backup(cowboy_req:req(), map()) -> cowboy_req:req().
do_delete_backup(Req0, State) ->
    CurrentUid = auth_ds:current_uid(State),
    case throttle:check(e2ee_backup, CurrentUid) of
        {limit_exceeded, _, _} ->
            elib_response:error(Req0, <<"操作过于频繁，请稍后再试"/utf8>>, 429);
        _ ->
            case e2ee_backup_logic:delete_backup(CurrentUid) of
                {ok, Payload} ->
                    elib_response:success(Req0, Payload);
                {error, Msg, Code} ->
                    elib_response:error(Req0, backup_msg(Msg), Code)
            end
    end.

%% @doc e2ee_backup_logic 返回的英文错误码 → 中文人类可读消息。
%% 逻辑层保持稳定的机器可读码（version_conflict / backup_not_found 等），
%% handler 层统一映射，避免英文码直达用户；
%% 已中文化的 binary 直接透传，非 binary 兜底通用中文。
%% 客户端按 HTTP code 分支（如 409 版本冲突自动重试），不匹配消息串，映射安全。
-spec backup_msg(term()) -> binary().
backup_msg(<<"version_conflict">>) -> <<"备份版本冲突，请刷新后重试"/utf8>>;
backup_msg(<<"backup_not_found">>) -> <<"无云端备份"/utf8>>;
backup_msg(<<"backup_query_failed">>) -> <<"备份查询失败，请稍后重试"/utf8>>;
backup_msg(<<"backup_save_failed">>) -> <<"备份保存失败，请稍后重试"/utf8>>;
backup_msg(<<"backup_delete_failed">>) -> <<"备份删除失败，请稍后重试"/utf8>>;
backup_msg(<<"invalid_backup_version">>) -> <<"备份版本号不合法"/utf8>>;
backup_msg(<<"invalid_kdf_salt">>) -> <<"加密盐值参数不合法"/utf8>>;
backup_msg(<<"invalid_kdf_iterations">>) -> <<"加密迭代次数不合法"/utf8>>;
backup_msg(<<"invalid_payload_hash">>) -> <<"备份内容哈希不合法"/utf8>>;
backup_msg(<<"invalid_algo">>) -> <<"加密算法标识不合法"/utf8>>;
backup_msg(<<"encrypted_payload_required">>) -> <<"缺少加密备份内容"/utf8>>;
backup_msg(<<"encrypted_payload_too_large">>) -> <<"加密备份内容过大"/utf8>>;
backup_msg(<<"plaintext_payload_rejected">>) -> <<"拒绝明文备份，请加密后上传"/utf8>>;
backup_msg(<<"invalid_base64_payload">>) -> <<"备份内容编码格式错误"/utf8>>;
backup_msg(Msg) when is_binary(Msg) -> Msg;
backup_msg(_Reason) -> <<"操作失败，请稍后重试"/utf8>>.
