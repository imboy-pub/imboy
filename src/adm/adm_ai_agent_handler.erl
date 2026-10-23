-module(adm_ai_agent_handler).

%%%
% AI Agent 账号管理控制器 / AI Agent account management controller
%
% 路由（前端 baseURL=/api/adm）：
%   GET  /adm/ai_agent/list          -> 分页列出 agent
%   GET  /adm/ai_agent/detail        -> ?user_id 单个 agent 详情
%   POST /adm/ai_agent/create        -> 新建 agent（建号+绑 provider/role/owner）
%   POST /adm/ai_agent/update        -> 更新既有 agent 绑定
%   POST /adm/ai_agent/set_status    -> 启用/停用（status 0|1）
%   GET  /adm/ai_agent/onboarding_config -> 读取新手引导配置（enabled/欢迎文案/默认频道等）
%   POST /adm/ai_agent/onboarding_config -> 半量保存新手引导配置（白名单键校验）
%   GET  /adm/ai_agent/knowledge_config  -> 读取知识库配置（群规/FAQ，供 @管家 注入）
%   POST /adm/ai_agent/knowledge_config  -> 半量保存知识库配置（白名单键校验）
%   GET  /adm/ai_agent/roles            -> 读取 ai_roles 人格 KV 全量
%   POST /adm/ai_agent/roles            -> 保存/删除单个角色（action=save|delete, role_id, prompt?）
%   POST /adm/ai_agent/upload_avatar    -> multipart 上传头像到 Garage，返回 URL
%   POST /adm/ai_agent/mandate_create-> 【admin 应急入口】代运营为 agent 创建受控支付授权
%
% 权限：users:read 读，users:create 建，users:update 改，finance:write 授权代付。
%%%

-behaviour(cowboy_rest).

-export([init/2]).

-include("log.hrl").
-include("error_code.hrl").

-define(PERM_READ, <<"users:read">>).
-define(PERM_CREATE, <<"users:create">>).
-define(PERM_UPDATE, <<"users:update">>).
-define(PERM_FINANCE_WRITE, <<"finance:write">>).

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            list -> list(Method, Req0, State);
            detail -> detail(Method, Req0, State);
            create -> create(Method, Req0, State);
            update -> update(Method, Req0, State);
            set_status -> set_status(Method, Req0, State);
            onboarding_config -> onboarding_config(Method, Req0, State);
            knowledge_config -> knowledge_config(Method, Req0, State);
            role_list -> role_list(Method, Req0, State);
            role_detail -> role_detail(Method, Req0, State);
            role_create -> role_create(Method, Req0, State);
            role_draft -> role_draft(Method, Req0, State);
            role_publish -> role_publish(Method, Req0, State);
            role_set_status -> role_set_status(Method, Req0, State);
            roles -> roles(Method, Req0, State);
            upload_avatar -> upload_avatar(Method, Req0, State);
            avatar_direct -> avatar_direct(Method, Req0, State);
            mandate_create -> mandate_create(Method, Req0, State);
            _ -> Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
list(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Category = elib_param:get(category, Req0, <<>>),
        case ai_agent_ds:list(Page, Size, Category) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"读取 Agent 列表失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
list(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
detail(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {ok, UserId} = elib_param:int(user_id, Req0, 0),
        case ai_agent_ds:get(UserId) of
            {ok, Agent} -> elib_response:success(Req0, Agent);
            {error, notfound} -> elib_response:error(Req0, <<"Agent 不存在"/utf8>>, ?ERR_BAD_REQUEST);
            {error, _} -> elib_response:error(Req0, <<"读取 Agent 详情失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
detail(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
create(<<"POST">>, Req0, State) ->
    with_perm(?PERM_CREATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case ai_agent_ds:create(PostVals) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"创建成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
create(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec update(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
update(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        UserId = ec_cnv:to_integer(maps:get(<<"user_id">>, PostVals, 0)),
        case ai_agent_ds:update(UserId, PostVals) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"更新成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
update(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec set_status(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
set_status(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        UserId = ec_cnv:to_integer(maps:get(<<"user_id">>, PostVals, 0)),
        Status = ec_cnv:to_integer(maps:get(<<"status">>, PostVals, 1)),
        case ai_agent_ds:set_status(UserId, Status) of
            {ok, _} -> elib_response:success(Req0, #{}, <<"操作成功"/utf8>>);
            {error, _} -> elib_response:error(Req0, <<"操作失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
set_status(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 新手引导配置：GET 读全量 / POST 半量保存（白名单键 + 类型校验在 logic 层）。
%% 键：enabled / welcome_agent_uid / default_channels / welcome_template / welcome_llm_enabled
-spec onboarding_config(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
onboarding_config(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        elib_response:success(Req0, user_onboarding_logic:get_config())
    end);
onboarding_config(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case user_onboarding_logic:put_config(PostVals) of
            {ok, _} ->
                %% 回全量最新配置，前端表单直接回显
                elib_response:success(
                    Req0, user_onboarding_logic:get_config(), <<"保存成功"/utf8>>
                );
            {error, Reason} ->
                elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
onboarding_config(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 知识库配置：GET 读全量 / POST 半量保存（群规/FAQ，供 @管家 答疑注入）。
%% 键：enabled / group_rule / faq（白名单 + 类型校验在 logic 层）
-spec knowledge_config(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
knowledge_config(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        elib_response:success(Req0, ai_agent_kb_logic:get_config())
    end);
knowledge_config(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        case ai_agent_kb_logic:put_config(PostVals) of
            {ok, _} ->
                %% 回全量最新配置，前端表单直接回显
                elib_response:success(
                    Req0, ai_agent_kb_logic:get_config(), <<"保存成功"/utf8>>
                );
            {error, Reason} ->
                elib_response:error(Req0, Reason, ?ERR_BAD_REQUEST)
        end
    end);
knowledge_config(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 版本化角色模板分页管理。角色模板是唯一行为配置来源，助手只绑定 code。
-spec role_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
role_list(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        {Page, Size} = elib_param:page(Req0),
        Keyword = elib_param:get(keyword, Req0, <<>>),
        Status = parse_optional_status(elib_param:get(status, Req0, <<>>)),
        Filters = maybe_status_filter(#{keyword => Keyword}, Status),
        case ai_agent_role_ds:page(Page, Size, Filters) of
            {ok, P} -> elib_response:success(Req0, P);
            {error, _} -> elib_response:error(Req0, <<"读取 AI 角色列表失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
role_list(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec role_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
role_detail(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        Code = elib_param:get(role_code, Req0, <<>>),
        case ai_agent_role_ds:find(Code) of
            {ok, Role} -> elib_response:success(Req0, Role);
            {error, notfound} -> elib_response:error(Req0, <<"AI 角色不存在"/utf8>>, ?ERR_BAD_REQUEST);
            {error, _} -> elib_response:error(Req0, <<"读取 AI 角色详情失败"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
role_detail(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec role_create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
role_create(<<"POST">>, Req0, State) ->
    with_perm(?PERM_CREATE, State, Req0, fun() ->
        PostVals = (elib_param:post(Req0))#{<<"created_by">> => maps:get(adm_user_id, State, 0)},
        case ai_agent_role_ds:create(PostVals) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"创建角色成功"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, reason_binary(Reason), ?ERR_BAD_REQUEST)
        end
    end);
role_create(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec role_draft(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
role_draft(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = (elib_param:post(Req0))#{<<"created_by">> => maps:get(adm_user_id, State, 0)},
        Code = maps:get(<<"role_code">>, PostVals, <<>>),
        case ai_agent_role_ds:save_draft(Code, PostVals) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"草稿已保存"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, reason_binary(Reason), ?ERR_BAD_REQUEST)
        end
    end);
role_draft(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec role_publish(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
role_publish(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        Code = maps:get(<<"role_code">>, PostVals, <<>>),
        Version = ec_cnv:to_integer(maps:get(<<"version">>, PostVals, 0)),
        %% 发布人必须来自已认证管理态，不能由请求体伪造。
        AdminUid = ec_cnv:to_integer(maps:get(adm_user_id, State, 0)),
        case ai_agent_role_ds:publish(Code, Version, AdminUid) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"角色已发布"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, reason_binary(Reason), ?ERR_BAD_REQUEST)
        end
    end);
role_publish(_, Req0, _State) ->
    method_not_allowed(Req0).

-spec role_set_status(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
role_set_status(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        Code = maps:get(<<"role_code">>, PostVals, <<>>),
        Status = ec_cnv:to_integer(maps:get(<<"status">>, PostVals, 1)),
        case ai_agent_role_ds:set_status(Code, Status) of
            {ok, Result} -> elib_response:success(Req0, Result, <<"角色状态已更新"/utf8>>);
            {error, Reason} -> elib_response:error(Req0, reason_binary(Reason), ?ERR_BAD_REQUEST)
        end
    end);
role_set_status(_, Req0, _State) ->
    method_not_allowed(Req0).

parse_optional_status(<<>>) ->
    undefined;
parse_optional_status(Value) ->
    ec_cnv:to_integer(Value).

maybe_status_filter(Filters, undefined) ->
    Filters;
maybe_status_filter(Filters, Status) ->
    Filters#{status => Status}.

reason_binary(Reason) when is_binary(Reason) ->
    Reason;
reason_binary(Reason) ->
    ec_cnv:to_binary(Reason).

%% @doc ai_roles 人格 KV 管理：GET 读全量；POST 保存/删除单个角色。
%% Body: {"action": "save"|"delete", "role_id": "...", "prompt": "..."}
%% 保存后回全量角色，前端下拉与列表直接回显。
-spec roles(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
roles(<<"GET">>, Req0, State) ->
    with_perm(?PERM_READ, State, Req0, fun() ->
        elib_response:success(Req0, #{<<"roles">> => ai_agent_ds:roles()})
    end);
roles(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        Action = maps:get(<<"action">>, PostVals, <<>>),
        RoleId = maps:get(<<"role_id">>, PostVals, <<>>),
        case {Action, RoleId} of
            {<<"save">>, RId} when RId =/= <<>> ->
                Prompt = maps:get(<<"prompt">>, PostVals, <<>>),
                case Prompt of
                    <<>> ->
                        elib_response:error(Req0, <<"角色提示词不能为空"/utf8>>, ?ERR_BAD_REQUEST);
                    _ ->
                        ok = ai_agent_ds:save_role(RId, Prompt),
                        elib_response:success(Req0, #{<<"roles">> => ai_agent_ds:roles()})
                end;
            {<<"delete">>, RId} when RId =/= <<>> ->
                ok = ai_agent_ds:delete_role(RId),
                elib_response:success(Req0, #{<<"roles">> => ai_agent_ds:roles()});
            _ ->
                elib_response:error(Req0, <<"参数不合法"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
roles(_, Req0, _State) ->
    method_not_allowed(Req0).

%% ===================================================================
%% direct-v1 AI 头像（step-13，协议 §5 Admin 身份域）：
%%   POST /api/adm/ai_agent/avatar_direct  {op: register|declare|grant|
%%     finalize|publish, ...} —— application 主体（org/app），不借 Human
%%     凭据；正文不经本入口（FormData 旧入口保留至前端切换后退役）。
%% ===================================================================
-spec avatar_direct(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
avatar_direct(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        {ok, Body, Req1} = elib_req:body(Req0, []),
        Op = maps:get(<<"op">>, Body, <<>>),
        case avatar_direct_op(Op, Body) of
            ok ->
                elib_response:success(Req1, #{});
            {ok, Data} ->
                elib_response:success(Req1, Data);
            {error, not_confirmed} ->
                elib_response:error(Req1, <<"会话未确认"/utf8>>, ?ERR_BAD_REQUEST);
            {error, not_application_session} ->
                elib_response:error(Req1, <<"会话主体不符（仅 application 会话）"/utf8>>, ?ERR_FORBIDDEN);
            {error, session_not_found} ->
                elib_response:error(Req1, <<"会话不存在"/utf8>>, ?ERR_NOT_FOUND);
            {error, declaration_conflict} ->
                elib_response:error(Req1, <<"声明冲突"/utf8>>, ?ERR_CONFLICT);
            {error, quota_exceeded} ->
                elib_response:error(Req1, <<"上传会话配额已满"/utf8>>, ?ERR_CONFLICT);
            {error, {publish_failed, _R}} ->
                %% 公开复制失败可恢复：winner 未动，前端可直接重试 publish
                elib_response:error(Req1, <<"头像发布失败，可重试"/utf8>>, ?ERR_FILE_UPLOAD_FAILED);
            {error, _Reason} ->
                elib_response:error(Req1, <<"头像会话处理失败"/utf8>>, ?ERR_FILE_UPLOAD_FAILED)
        end
    end);
avatar_direct(_, Req0, _State) ->
    method_not_allowed(Req0).

avatar_direct_op(<<"register">>, Body) ->
    AppId = maps:get(<<"application_id">>, Body, <<>>),
    OrgId = maps:get(<<"org_id">>, Body, <<>>),
    FileName = maps:get(<<"filename">>, Body, <<"avatar.png">>),
    case {AppId, OrgId} of
        {<<>>, _} ->
            {error, missing_app};
        {_, <<>>} ->
            {error, missing_org};
        _ ->
            Scope = <<"public">>,
            attach_direct_logic:register_application(
                AppId,
                OrgId,
                Scope,
                maps:get(<<"client_upload_id">>, Body, undefined),
                FileName
            )
    end;
%% declare/grant/finalize/publish 前置主体断言（review P1 修复）：
%% 仅 application 会话可经 Admin 面推进/发布——Human 用户会话的 key 不受
%% 本入口操作（通用 logic 函数按 key 寻址不校验主体，防线在入口）。
avatar_direct_op(<<"declare">>, Body) ->
    with_app_session(Body, fun(Key) ->
        attach_direct_logic:declare(
            Key,
            maps:get(<<"sha256">>, Body, <<>>),
            elib_cnv:safe_to_integer(maps:get(<<"size_bytes">>, Body, 0)),
            maps:get(<<"mime">>, Body, <<"image/png">>)
        )
    end);
avatar_direct_op(<<"grant">>, Body) ->
    with_app_session(Body, fun(Key) ->
        attach_direct_logic:grant(Key, maps:get(<<"sha256">>, Body, <<>>))
    end);
avatar_direct_op(<<"finalize">>, Body) ->
    with_app_session(Body, fun(Key) ->
        attach_direct_logic:finalize(
            Key, elib_cnv:safe_to_integer(maps:get(<<"attempt_id">>, Body, 0))
        )
    end);
avatar_direct_op(<<"publish">>, Body) ->
    with_app_session(Body, fun(Key) ->
        attach_direct_logic:publish_avatar(
            Key, maps:get(<<"agent_id">>, Body, <<>>), <<>>
        )
    end);
avatar_direct_op(_, _) ->
    {error, unknown_op}.

with_app_session(Body, Fun) ->
    Key = maps:get(<<"object_key">>, Body, <<>>),
    case attach_direct_logic:require_application_session(Key) of
        ok -> Fun(Key);
        {error, not_found} -> {error, session_not_found};
        {error, R} -> {error, R}
    end.

%% @doc multipart 上传头像到 Garage，返回 URL（前端存入表单随 update 提交，
%% 由 ai_agent_ds:update 同步写 user.avatar）。
%% 复用 group_file_handler 的流式 part 读取模式（read_all_parts 未导出，
%% 此处按同一结构内联，字段仅取 file）。
-spec upload_avatar(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
upload_avatar(<<"POST">>, Req0, State) ->
    with_perm(?PERM_UPDATE, State, Req0, fun() ->
        {Parts, Req1} = read_all_parts(Req0),
        case find_file_part(Parts) of
            {ok, FileName, FileBinary, FileType} ->
                case elib_oss:upload(FileBinary, FileName, #{mime_type => FileType}) of
                    {ok, FileUrl, _FileId} ->
                        elib_response:success(Req1, #{<<"url">> => FileUrl});
                    {error, file_too_large} ->
                        elib_response:error(
                            Req1, <<"文件大小超出限制"/utf8>>, ?ERR_FILE_SIZE_EXCEEDED
                        );
                    {error, invalid_file_type} ->
                        elib_response:error(
                            Req1, <<"不允许的文件类型"/utf8>>, ?ERR_FILE_TYPE_NOT_ALLOWED
                        );
                    {error, _} ->
                        elib_response:error(Req1, <<"文件上传失败"/utf8>>, ?ERR_FILE_UPLOAD_FAILED)
                end;
            error ->
                elib_response:error(Req1, <<"缺少文件参数"/utf8>>, ?ERR_MISSING_PARAM)
        end
    end);
upload_avatar(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 流式读完 multipart part，归一化为 [{FieldName, Value}]：
%% 普通字段 Value 为 binary；文件字段为 #{filename, data, content_type}。
-spec read_all_parts(cowboy_req:req()) -> {list(), cowboy_req:req()}.
read_all_parts(Req0) ->
    read_all_parts(Req0, []).

read_all_parts(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {Body, Req2} = read_full_part_body(Req1, <<>>),
                    read_all_parts(Req2, [{FieldName, Body} | Acc]);
                {file, FieldName, Filename, CType} ->
                    {Body, Req2} = read_full_part_body(Req1, <<>>),
                    read_all_parts(Req2, [
                        {FieldName, #{filename => Filename, data => Body, content_type => CType}}
                        | Acc
                    ])
            end;
        {done, Req1} ->
            {lists:reverse(Acc), Req1}
    end.

read_full_part_body(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, Data, Req1} -> {<<Acc/binary, Data/binary>>, Req1};
        {more, Data, Req1} -> read_full_part_body(Req1, <<Acc/binary, Data/binary>>)
    end.

%% 从 parts 提取 file 字段：{ok, FileName, FileBinary, FileType} | error
-spec find_file_part(list()) -> {ok, binary(), binary(), binary()} | error.
find_file_part([{<<"file">>, #{filename := FileName, data := Data, content_type := Type}} | _]) when
    FileName =/= undefined, Data =/= undefined
->
    {ok, FileName, Data, Type};
find_file_part([_ | Rest]) ->
    find_file_part(Rest);
find_file_part([]) ->
    error.

%% @doc 【admin 应急入口(c)】代运营为指定 agent 创建受控支付授权。
%% ⚠️ owner_uid **不取管理员身份**，而是目标 agent 的 ai_agent.owner_uid
%%    （代运营替 owner 决策，扣款方仍是 owner 本人钱包）。金钱红线（归属/边界/单活）
%%    全部复用 agent_payment_mandate_logic:authorize/2，本层只解析目标 owner。
%% Body: agent_uid, max_amount_fen, max_total_fen, expires_in_secs, period_secs?
-spec mandate_create(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
mandate_create(<<"POST">>, Req0, State) ->
    with_perm(?PERM_FINANCE_WRITE, State, Req0, fun() ->
        PostVals = elib_param:post(Req0),
        AgentUid = ec_cnv:to_integer(maps:get(<<"agent_uid">>, PostVals, 0)),
        case ai_agent_ds:is_agent(AgentUid) of
            {true, #{<<"owner_uid">> := OwnerUid0}} ->
                OwnerUid = ec_cnv:to_integer(OwnerUid0),
                case agent_payment_mandate_logic:authorize(OwnerUid, PostVals) of
                    {ok, R} ->
                        elib_response:success(Req0, R, <<"授权成功"/utf8>>);
                    {error, not_agent_owner} ->
                        elib_response:error(
                            Req0, <<"该 Agent 未绑定 owner，无法授权"/utf8>>, ?ERR_BAD_REQUEST
                        );
                    {error, invalid_params} ->
                        elib_response:error(Req0, <<"参数不合法"/utf8>>, ?ERR_BAD_REQUEST);
                    {error, _} ->
                        elib_response:error(Req0, <<"授权失败"/utf8>>, ?ERR_BAD_REQUEST)
                end;
            false ->
                elib_response:error(Req0, <<"Agent 不存在或已停用"/utf8>>, ?ERR_BAD_REQUEST)
        end
    end);
mandate_create(_, Req0, _State) ->
    method_not_allowed(Req0).

%% @doc 权限门控：ok 则执行 Fun，否则回 acl 的拒绝 Req
-spec with_perm(binary(), map(), cowboy_req:req(), fun(() -> cowboy_req:req())) ->
    cowboy_req:req().
with_perm(Perm, State, Req0, Fun) ->
    case adm_acl:ensure_permission(State, Perm, Req0) of
        ok -> Fun();
        {error, Req1} -> Req1
    end.

-spec method_not_allowed(cowboy_req:req()) -> cowboy_req:req().
method_not_allowed(Req0) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).
