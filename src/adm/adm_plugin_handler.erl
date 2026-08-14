-module(adm_plugin_handler).
%%%
% adm_plugin 控制器模块 / adm_plugin controller module
% 插件生命周期管理 API / Plugin lifecycle management API (lifecycle.md §10)
%%%
-behavior(cowboy_rest).

-export([init/2]).

-ifdef(TEST).
-export([safe_plugin_name/1, is_lifecycle_mutation/1]).
-endif.

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").
-include("common.hrl").
-include("error_code.hrl").

%% ===================================================================
%% API
%% ===================================================================

-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    Action = maps:get(action, State0, false),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        %% A-28：动态插件生命周期写操作默认禁用（IMBOY_PLUGIN_LIFECYCLE_ENABLED）。
        %% 该子系统 @status FROZEN，install 的 Path 无白名单（审计 #43）、签名 100%
        %% 放行（#44），admin 可达即代码加载面。内置开关是纯 manifest，不受影响。
        case is_lifecycle_mutation(Action) andalso not imboy_plugin_manager:lifecycle_enabled() of
            true ->
                elib_response:error(
                    Req0, imboy_error:error_msg(?ERR_FEATURE_DISABLED), ?ERR_FEATURE_DISABLED
                );
            false ->
                case Action of
                    list -> do_list(Method, Req0, State);
                    detail -> do_detail(Method, Req0, State);
                    state_query -> do_state_query(Method, Req0, State);
                    health -> do_health(Method, Req0, State);
                    install -> do_install(Method, Req0, State);
                    enable -> do_enable(Method, Req0, State);
                    disable -> do_disable(Method, Req0, State);
                    upgrade -> do_upgrade(Method, Req0, State);
                    uninstall -> do_uninstall(Method, Req0, State);
                    reset -> do_reset(Method, Req0, State);
                    force_uninstall -> do_force_uninstall(Method, Req0, State);
                    logs -> do_logs(Method, Req0, State);
                    _ -> Req0
                end
        end,
    {ok, Req1, State}.

%% @doc 是否为动态插件生命周期写操作（A-28 门控对象）。
%% 7 个写 action 受 IMBOY_PLUGIN_LIFECYCLE_ENABLED 门控；5 个只读端点不受影响。
-spec is_lifecycle_mutation(atom()) -> boolean().
is_lifecycle_mutation(Action) ->
    lists:member(Action, [
        install,
        enable,
        disable,
        upgrade,
        uninstall,
        reset,
        force_uninstall
    ]).

%% ===================================================================
%% GET endpoints
%% ===================================================================

%% @doc 列出所有插件
-spec do_list(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_list(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:read">>, Req0) of
        ok ->
            {ok, Items} = imboy_plugin_manager:list_plugins(),
            elib_response:success(Req0, #{items => Items});
        {error, Req1} ->
            Req1
    end;
do_list(_, Req0, _State) ->
    Req0.

%% @doc 单个插件详情
-spec do_detail(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_detail(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:read">>, Req0) of
        ok ->
            case get_name(Req0) of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                Name ->
                    case imboy_plugin_manager:get_plugin(Name) of
                        {ok, Info} ->
                            elib_response:success(Req0, Info);
                        {error, not_found} ->
                            elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_detail(_, Req0, _State) ->
    Req0.

%% @doc 插件当前状态（轻量级）
-spec do_state_query(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_state_query(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:read">>, Req0) of
        ok ->
            case get_name(Req0) of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                Name ->
                    case imboy_plugin_manager:get_state(Name) of
                        {ok, Info} ->
                            elib_response:success(Req0, Info);
                        {error, not_found} ->
                            elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, _} ->
                            elib_response:error(Req0, <<"进程错误"/utf8>>, ?ERR_BAD_REQUEST)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_state_query(_, Req0, _State) ->
    Req0.

%% @doc 健康检查
-spec do_health(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_health(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:read">>, Req0) of
        ok ->
            case get_name(Req0) of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                Name ->
                    case imboy_plugin_manager:health_check(Name) of
                        {ok, Info} ->
                            elib_response:success(Req0, Info);
                        {error, not_found} ->
                            elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, _} ->
                            elib_response:error(Req0, <<"健康检查失败"/utf8>>, ?ERR_BAD_REQUEST)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_health(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% POST endpoints — lifecycle mutations
%% ===================================================================

%% @doc 安装插件
-spec do_install(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_install(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:install">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            case
                {
                    maps:get(<<"name">>, PostVals, undefined),
                    maps:get(<<"path">>, PostVals, undefined)
                }
            of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少插件路径"/utf8>>, ?ERR_BAD_REQUEST);
                {NameBin, Path} when is_binary(NameBin), is_binary(Path) ->
                    case safe_plugin_name(NameBin) of
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {ok, Name} ->
                            case imboy_plugin_manager:install(Name, Path) of
                                {ok, Info} ->
                                    elib_response:success(Req0, Info);
                                {error, {invalid_state_transition, _}} ->
                                    elib_response:error(Req0, <<"非法状态转换"/utf8>>, ?ERR_BAD_REQUEST);
                                {error, Reason} ->
                                    format_lifecycle_error(Req0, Reason)
                            end
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_install(_, Req0, _State) ->
    Req0.

%% @doc 启用插件
-spec do_enable(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_enable(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:enable">>, Req0) of
        ok ->
            case get_name(Req0) of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                Name ->
                    case imboy_plugin_manager:enable(Name) of
                        ok ->
                            elib_response:success(Req0, #{name => Name, state => enabled});
                        {error, not_found} ->
                            elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, Reason} ->
                            format_lifecycle_error(Req0, Reason)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_enable(_, Req0, _State) ->
    Req0.

%% @doc 禁用插件
-spec do_disable(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_disable(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:disable">>, Req0) of
        ok ->
            case get_name(Req0) of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                Name ->
                    case imboy_plugin_manager:disable(Name) of
                        ok ->
                            elib_response:success(Req0, #{name => Name, state => disabled});
                        {error, not_found} ->
                            elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, Reason} ->
                            format_lifecycle_error(Req0, Reason)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_disable(_, Req0, _State) ->
    Req0.

%% @doc 升级插件
-spec do_upgrade(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_upgrade(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:upgrade">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            case
                {
                    maps:get(<<"name">>, PostVals, undefined),
                    maps:get(<<"version">>, PostVals, undefined)
                }
            of
                {undefined, _} ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {_, undefined} ->
                    elib_response:error(Req0, <<"缺少版本号"/utf8>>, ?ERR_BAD_REQUEST);
                {NameBin, Version} when is_binary(NameBin), is_binary(Version) ->
                    case safe_plugin_name(NameBin) of
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {ok, Name} ->
                            case imboy_plugin_manager:upgrade(Name, Version) of
                                ok ->
                                    elib_response:success(Req0, #{name => Name, version => Version});
                                {error, not_found} ->
                                    elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                                {error, Reason} ->
                                    format_lifecycle_error(Req0, Reason)
                            end
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_upgrade(_, Req0, _State) ->
    Req0.

%% @doc 卸载插件
-spec do_uninstall(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_uninstall(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:uninstall">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            NameBin = maps:get(<<"name">>, PostVals, undefined),
            Mode = maps:get(<<"mode">>, PostVals, <<"preserve_data">>),
            case NameBin of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case safe_plugin_name(NameBin) of
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {ok, Name} ->
                            case imboy_plugin_manager:uninstall(Name, Mode) of
                                ok ->
                                    elib_response:success(Req0);
                                {error, not_found} ->
                                    elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                                {error, Reason} ->
                                    format_lifecycle_error(Req0, Reason)
                            end
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_uninstall(_, Req0, _State) ->
    Req0.

%% @doc 从 failed 重置到 unknown
-spec do_reset(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_reset(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:reset">>, Req0) of
        ok ->
            case get_name(Req0) of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                {error, Msg} ->
                    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                Name ->
                    case imboy_plugin_manager:reset(Name) of
                        ok ->
                            elib_response:success(Req0, #{name => Name, state => unknown});
                        {error, not_found} ->
                            elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                        {error, Reason} ->
                            format_lifecycle_error(Req0, Reason)
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_reset(_, Req0, _State) ->
    Req0.

%% @doc 从 failed 强制卸载
-spec do_force_uninstall(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_force_uninstall(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:force_uninstall">>, Req0) of
        ok ->
            PostVals = elib_param:post(Req0),
            NameBin = maps:get(<<"name">>, PostVals, undefined),
            Mode = maps:get(<<"mode">>, PostVals, <<"preserve_data">>),
            case NameBin of
                undefined ->
                    elib_response:error(Req0, <<"缺少插件名称"/utf8>>, ?ERR_BAD_REQUEST);
                _ ->
                    case safe_plugin_name(NameBin) of
                        {error, Msg} ->
                            elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
                        {ok, Name} ->
                            case imboy_plugin_manager:force_uninstall(Name, Mode) of
                                ok ->
                                    elib_response:success(Req0);
                                {error, not_found} ->
                                    elib_response:error(Req0, <<"插件不存在"/utf8>>, ?ERR_NOT_FOUND);
                                {error, Reason} ->
                                    format_lifecycle_error(Req0, Reason)
                            end
                    end
            end;
        {error, Req1} ->
            Req1
    end;
do_force_uninstall(_, Req0, _State) ->
    Req0.

%% @doc 查询插件审计日志（分页）
-spec do_logs(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
do_logs(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"plugins:read">>, Req0) of
        ok ->
            QS = cowboy_req:parse_qs(Req0),
            PluginName = proplists:get_value(<<"plugin_name">>, QS, <<"">>),
            try
                {
                    binary_to_integer(proplists:get_value(<<"limit">>, QS, <<"20">>)),
                    binary_to_integer(proplists:get_value(<<"offset">>, QS, <<"0">>))
                }
            of
                {Limit, Offset} ->
                    case imboy_plugin_audit_ds:list(PluginName, Limit, Offset) of
                        {ok, Rows} ->
                            elib_response:success(Req0, #{
                                <<"list">> => Rows,
                                <<"limit">> => Limit,
                                <<"offset">> => Offset
                            });
                        {error, Reason} ->
                            elib_response:error(
                                Req0, iolist_to_binary(io_lib:format("~p", [Reason])), 500
                            )
                    end
            catch
                error:badarg ->
                    elib_response:error(Req0, <<"limit/offset 参数无效"/utf8>>, ?ERR_BAD_REQUEST)
            end;
        {error, Req1} ->
            Req1
    end;
do_logs(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% Helpers
%% ===================================================================

%% @doc 校验插件名称格式（仅允许 a-z A-Z 0-9 _，最长 64 字符），防止 atom 表溢出。
-spec safe_plugin_name(binary()) -> {ok, atom()} | {error, binary()}.
safe_plugin_name(NameBin) when
    is_binary(NameBin), byte_size(NameBin) > 0, byte_size(NameBin) =< 64
->
    case re:run(NameBin, <<"^[a-zA-Z][a-zA-Z0-9_]*$">>) of
        nomatch ->
            {error, <<"插件名称格式无效"/utf8>>};
        _ ->
            %% 用 binary_to_existing_atom：合法插件的原子在启动注册 manifest
            %% （persistent_term {imboy_plugin_manifest, Name}）时已存在；
            %% 未知随机名的原子不存在 → badarg → 拒绝，永不新建原子，
            %% 杜绝外部输入耗尽原子表的 DoS。
            try
                {ok, binary_to_existing_atom(NameBin, utf8)}
            catch
                error:badarg -> {error, <<"插件不存在"/utf8>>}
            end
    end;
safe_plugin_name(_) ->
    {error, <<"插件名称无效"/utf8>>}.

%% @doc 从请求中提取插件名称（GET 用 query string，POST 用 body）。
-spec get_name(cowboy_req:req()) -> atom() | undefined | {error, binary()}.
get_name(Req0) ->
    {ok, NameBin} = elib_param:binary(<<"name">>, Req0, <<>>),
    case NameBin of
        <<>> ->
            undefined;
        _ ->
            case safe_plugin_name(NameBin) of
                {ok, Name} -> Name;
                {error, _} = Err -> Err
            end
    end.

%% @doc 将 lifecycle 错误映射为 HTTP 响应。
-spec format_lifecycle_error(cowboy_req:req(), term()) -> cowboy_req:req().
format_lifecycle_error(Req0, {deps_not_enabled, Names}) ->
    NameBins = [atom_to_binary(N) || N <- Names],
    elib_response:error(
        Req0,
        <<"依赖未启用:"/utf8,
            (iolist_to_binary(
                lists:join(<<",">>, NameBins)
            ))/binary>>,
        ?ERR_BAD_REQUEST
    );
format_lifecycle_error(Req0, {has_dependents, Names}) ->
    NameBins = [atom_to_binary(N) || N <- Names],
    elib_response:error(
        Req0,
        <<"存在依赖项:"/utf8,
            (iolist_to_binary(
                lists:join(<<",">>, NameBins)
            ))/binary>>,
        ?ERR_BAD_REQUEST
    );
format_lifecycle_error(Req0, invalid_state_transition) ->
    elib_response:error(Req0, <<"非法状态转换"/utf8>>, ?ERR_BAD_REQUEST);
format_lifecycle_error(Req0, {Step, _}) when is_atom(Step) ->
    Msg = iolist_to_binary([<<"步骤失败:"/utf8>>, atom_to_binary(Step)]),
    elib_response:error(Req0, Msg, ?ERR_BAD_REQUEST);
format_lifecycle_error(Req0, Reason) ->
    elib_response:error(Req0, iolist_to_binary(io_lib:format("~p", [Reason])), ?ERR_BAD_REQUEST).

%% ===================================================================
%% RBAC 权限检查 / RBAC permission checks
%% ===================================================================
