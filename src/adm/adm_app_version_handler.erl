-module(adm_app_version_handler).
-dialyzer({nowarn_function, [version_stats/3]}).

%%%
% adm_app_version 控制器模块
% adm_app_version controller module
%%%
-behavior(cowboy_rest).

-export([init/2]).

-include_lib("eunit/include/eunit.hrl").

-include("log.hrl").

-include_lib("kernel/include/logger.hrl").

-include("common.hrl").

%% ===================================================================
%% API
%% ===================================================================

%% @doc 初始化应用版本管理 REST 处理器
%% 根据请求中的 action 参数分发到不同的处理函数
%% @param Req0 Cowboy 请求对象
%% @param State0 状态映射，包含 action 等信息
%% @return {ok, Req, State} 更新后的请求和状态
-spec init(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()}.
init(Req0, State0) ->
    % ?DEBUG_LOG(State),
    Action = maps:get(action, State0),
    State = maps:remove(action, State0),
    Method = cowboy_req:method(Req0),
    Req1 =
        case Action of
            index ->
                {ok, Ajax} = elib_param:int(ajax, Req0, -2),
                % elib_log:info(["AjaxAjaxAjaxAjaxAjax: ", Ajax, ";  Ajax"]),
                index(Method, Ajax, Req0, State);
            save ->
                save(Method, Req0, State);
            delete ->
                delete(Method, Req0, State);
            version_stats ->
                version_stats(Method, Req0, State);
            false ->
                Req0
        end,
    {ok, Req1, State}.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc 处理应用版本索引请求（纯 JSON）
%% @param Method HTTP 方法
%% @param Ajax Ajax 标志，1 表示返回 JSON 数据
%% @param Req0 Cowboy 请求对象
%% @param State 状态映射
%% @return cowboy_req:req() 更新后的请求对象
-spec index(binary(), integer(), cowboy_req:req(), map()) -> cowboy_req:req().
index(<<"GET">>, _Ajax, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"settings:version:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            {Page, Size} = elib_param:page(Req0),
            Where = #{},
            Column = <<"*">>,
            OrderBy = <<"sort desc, updated_at desc">>,
            {ok, P} = app_version_ds:page(Column, Where, OrderBy, Page, Size),
            elib_response:success(Req0, P)
    end;
index(_Method, _Ajax, Req0, _State) ->
    cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0).

%% @doc 保存应用版本信息
%% 处理 POST 请求，保存或更新应用版本配置
-spec save(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
save(<<"POST">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"settings:version:create">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            save_channel_version(Req0)
    end;
save(_, Req0, _State) ->
    Req0.

-spec save_channel_version(cowboy_req:req()) -> cowboy_req:req().
save_channel_version(Req0) ->
    PostVals = elib_param:post(Req0),

    RCode = maps:get(<<"region_code">>, PostVals, <<"cn">>),
    Type = maps:get(<<"type">>, PostVals, <<>>),
    PkgName = maps:get(<<"package_name">>, PostVals, <<>>),
    AppName = maps:get(<<"app_name">>, PostVals, <<>>),
    Vsn = maps:get(<<"vsn">>, PostVals, <<>>),
    SKey = maps:get(<<"sign_key">>, PostVals, <<>>),
    DUrl = maps:get(<<"download_url">>, PostVals, <<>>),
    Desc = maps:get(<<"description">>, PostVals, <<>>),
    ForceUpdate = maps:get(<<"force_update">>, PostVals, 2),
    Status = maps:get(<<"status">>, PostVals, 0),
    Id = maps:get(<<"id">>, PostVals, 0),
    %% 新增字段
    MinSupportedVsn = maps:get(<<"min_supported_vsn">>, PostVals, <<"0.0.0">>),
    GrayscalePercent = maps:get(<<"grayscale_percent">>, PostVals, 100),
    UpgradeType = maps:get(<<"upgrade_type">>, PostVals, <<"recommend">>),
    Changelog = maps:get(<<"changelog">>, PostVals, []),
    FileSize = maps:get(<<"file_size">>, PostVals, 0),
    FileHash = maps:get(<<"file_hash">>, PostVals, <<>>),
    Data =
        #{
            id => Id,
            region_code => RCode,
            type => Type,
            package_name => PkgName,
            app_name => AppName,
            vsn => Vsn,
            sort => adm_app_version_logic:vsn_sort(Vsn),
            sign_key => SKey,
            download_url => DUrl,
            description => Desc,
            force_update => ec_cnv:to_integer(ForceUpdate) =:= 1,
            status => ec_cnv:to_integer(Status),
            %% 升级策略字段
            min_supported_vsn => MinSupportedVsn,
            grayscale_percent => ec_cnv:to_integer(GrayscalePercent),
            upgrade_type => UpgradeType,
            changelog => jsone:encode(Changelog, [
                native_utf8, {float_format, [{decimals, 4}, compact]}
            ]),
            file_size => ec_cnv:to_integer(FileSize),
            file_hash => FileHash
        },
    _ = adm_app_version_logic:save(Data),
    elib_response:success(Req0, PostVals, <<"success."/utf8>>).

%% @doc 删除应用版本信息
%% 处理 DELETE 请求，删除指定的应用版本配置
-spec delete(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
delete(<<"DELETE">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"settings:version:delete">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            PostVals = elib_param:post(Req0),
            Id = ec_cnv:to_integer(maps:get(<<"id">>, PostVals, 0)),
            case adm_app_version_logic:delete_by_id(Id) of
                {ok, _} ->
                    elib_response:success(Req0, PostVals, <<"success."/utf8>>);
                {error, invalid_id} ->
                    elib_response:error(Req0, <<"无效的版本ID"/utf8>>);
                {error, _Reason} ->
                    elib_response:error(Req0, <<"删除失败"/utf8>>)
            end
    end;
delete(_, Req0, _State) ->
    Req0.

%% @doc 查询版本分布统计和升级事件统计
%% GET /adm/app_version/version_stats
%%
%% 返回：
%%   distribution - 各版本设备数分布
%%   events       - 近7天升级事件统计
-spec version_stats(binary(), cowboy_req:req(), map()) -> cowboy_req:req().
version_stats(<<"GET">>, Req0, State) ->
    case adm_acl:ensure_permission(State, <<"settings:version:read">>, Req0) of
        {error, RespReq} ->
            RespReq;
        ok ->
            Distribution = app_upgrade_log_ds:version_distribution(),
            %% 近7天事件统计
            Now = elib_dt:now(),
            SevenDaysAgo = elib_dt:minus(Now, {7 * 86400, second}),
            Events = app_upgrade_log_ds:event_stats(SevenDaysAgo, Now),
            elib_response:success(Req0, #{
                <<"distribution">> => Distribution,
                <<"events">> => Events
            })
    end;
version_stats(_, Req0, _State) ->
    Req0.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
