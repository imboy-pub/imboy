-module(app_version_logic).
%%%
% APP 版本升级降级业务逻辑模块
% 实现三级升级策略（force/recommend/silent）和灰度发布判断
%%%

-export([check/3]).
-export([sign_key/3]).
-export([e2ee_gate/2]).

-include("common.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

%% @doc 获取应用签名密钥
%% 委托至 app_version_ds:sign_key/3
%% @param DType 客户端操作系统类型（android/ios/macos）
%% @param Vsn 版本号
%% @param Pkg 包名
%% @return 签名密钥（binary），不存在时返回 <<>>
-spec sign_key(binary(), binary(), binary()) -> binary().
sign_key(DType, Vsn, Pkg) ->
    app_version_ds:sign_key(DType, Vsn, Pkg).

%% @doc 检查客户端版本，返回升级策略
%% @param ClientVsn 客户端当前版本号
%% @param Cos 客户端操作系统类型 <<"android">> | <<"ios">> | <<"web">>
%% @param DID 设备 ID，用于灰度判断
%% @return map() 包含 upgrade_type 等字段的版本信息
-spec check(binary(), binary(), binary()) -> map().
check(ClientVsn, Cos, DID) ->
    RegionCode = <<>>,
    check(ClientVsn, Cos, DID, RegionCode).

%% @doc 检查客户端版本（带地区参数）
-spec check(binary(), binary(), binary(), binary()) -> map().
check(ClientVsn, Cos, DID, RegionCode) ->
    %% 1. 查询最新版本
    VersionInfo = app_version_ds:find(Cos, RegionCode),
    case maps:size(VersionInfo) of
        0 ->
            %% 没有版本记录，返回无需更新
            #{<<"updatable">> => false, <<"upgrade_type">> => <<"none">>};
        _ ->
            %% 2. 查询全局策略
            Policy = app_version_policy_ds:find_by_type(Cos),
            %% 3. 计算升级策略
            determine_upgrade(ClientVsn, DID, VersionInfo, Policy)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

%% @doc E2EE 硬版本门（C12 / AC-25）
%% 旧端（低于生效最低版本）在 E2EE 端点被服务端拒绝，而不是仅提示升级。
%%
%% 与 check/3 共用同一套最低版本真源：全局策略 app_version_policy.min_vsn
%% 与版本级 app_version.min_supported_vsn 取较高者（同 calculate_upgrade_type
%% 的「检查 1」）。语义：
%%   - 未配置任何版本记录（app_version 表无该平台行）→ allow（未配置=无门，
%%     rollout 前默认全开，不改变现网行为）；
%%   - 生效最低版本为 0.0.0（未配置门）→ allow；
%%   - ClientVsn 缺失/为空 → 按 <<"0.0.0">> 参与比较：配置了门即拒绝
%%     （E2EE 端点上无法声明版本的客户端按旧端处理；生产链路 verify_sign
%%     已强制真实 vsn 头，此分支正常不可达，仅供 api_auth_switch=off 环境）；
%%   - ClientVsn < 生效最低版本 → {block, MinVsn}；
%%   - 其余 → allow。
%% @param ClientVsn 客户端声明版本（HTTP vsn 头）
%% @param Cos 客户端平台（HTTP cos 头，android/ios）；空平台查不到记录 → allow
%% @return allow | {block, MinVsn :: binary()}
-spec e2ee_gate(ClientVsn :: binary() | undefined, Cos :: binary()) ->
    allow | {block, binary()}.
e2ee_gate(ClientVsn, Cos) when is_binary(Cos), Cos =/= <<>> ->
    VersionInfo = app_version_ds:find(Cos, <<>>),
    case maps:size(VersionInfo) of
        0 ->
            %% 未配置任何版本记录：门默认全开（rollout 顺序见 runbook：
            %% 客户端先发布，管理员后配置 min_vsn，最后开 required E2EE）
            allow;
        _ ->
            Policy = app_version_policy_ds:find_by_type(Cos),
            GlobalMinVsn = maps:get(<<"min_vsn">>, Policy, <<"0.0.0">>),
            VsnMinVsn = maps:get(<<"min_supported_vsn">>, VersionInfo, <<"0.0.0">>),
            MinVsn = max_vsn(GlobalMinVsn, VsnMinVsn),
            Normalized = normalize_client_vsn(ClientVsn),
            case MinVsn =:= <<"0.0.0">> orelse not ec_semver:lt(Normalized, MinVsn) of
                true -> allow;
                false -> {block, MinVsn}
            end
    end;
e2ee_gate(_ClientVsn, _CosMissingOrEmpty) ->
    %% 平台未声明：查不到任何策略/记录，等价未配置（生产链路 verify_sign
    %% 已强制真实 cos 头，此分支正常不可达）
    allow.

%% @doc 客户端版本归一化：缺失/空版本按 0.0.0（无法声明版本=按最低版本处理）
-spec normalize_client_vsn(binary() | undefined) -> binary().
normalize_client_vsn(undefined) -> <<"0.0.0">>;
normalize_client_vsn(<<>>) -> <<"0.0.0">>;
normalize_client_vsn(Vsn) when is_binary(Vsn) -> Vsn.

%% @doc 判断升级策略
%% 优先级：全局最低版本 > 版本级最低版本 > 版本级 force_update > 灰度 > 版本级 upgrade_type
-spec determine_upgrade(binary(), binary(), map(), map()) -> map().
determine_upgrade(ClientVsn, DID, VersionInfo, Policy) ->
    LatestVsn = maps:get(<<"vsn">>, VersionInfo, <<"0.0.0">>),
    Updatable = ec_semver:lt(ClientVsn, LatestVsn),

    %% 基础返回信息（向后兼容旧客户端）
    BaseInfo = VersionInfo#{
        <<"updatable">> => Updatable,
        <<"check_interval_hours">> => maps:get(<<"check_interval_hours">>, Policy, 24)
    },

    case Updatable of
        false ->
            %% 已是最新版本
            BaseInfo#{<<"upgrade_type">> => <<"none">>};
        true ->
            %% 有新版本，判断升级类型
            UpgradeType = calculate_upgrade_type(ClientVsn, DID, VersionInfo, Policy),
            BaseInfo#{<<"upgrade_type">> => UpgradeType}
    end.

%% @doc 计算具体的升级类型
-spec calculate_upgrade_type(binary(), binary(), map(), map()) -> binary().
calculate_upgrade_type(ClientVsn, DID, VersionInfo, Policy) ->
    %% 全局最低版本（管理员设的兜底线）
    GlobalMinVsn = maps:get(<<"min_vsn">>, Policy, <<"0.0.0">>),
    %% 版本级最低版本
    VsnMinVsn = maps:get(<<"min_supported_vsn">>, VersionInfo, <<"0.0.0">>),
    %% 取两者中较高的作为最终最低版本
    MinVsn = max_vsn(GlobalMinVsn, VsnMinVsn),

    %% 检查 1：低于最低支持版本 → 强制升级
    case ec_semver:lt(ClientVsn, MinVsn) of
        true ->
            <<"force">>;
        false ->
            check_force_and_grayscale(ClientVsn, DID, VersionInfo, Policy)
    end.

%% @doc 检查强制标记和灰度
-spec check_force_and_grayscale(binary(), binary(), map(), map()) -> binary().
check_force_and_grayscale(_ClientVsn, DID, VersionInfo, Policy) ->
    %% force_update 为 boolean 列（Section 19 迁移后）
    ForceUpdate = maps:get(<<"force_update">>, VersionInfo, false),
    %% 新的 upgrade_type 字段
    ConfiguredType = maps:get(<<"upgrade_type">>, VersionInfo, <<"recommend">>),

    %% 检查 2：force_update=true 或 upgrade_type=force → 强制升级
    case ForceUpdate =:= true orelse ConfiguredType =:= <<"force">> of
        true ->
            <<"force">>;
        false ->
            %% 检查 3：灰度判断
            check_grayscale(DID, VersionInfo, Policy, ConfiguredType)
    end.

%% @doc 灰度判断
-spec check_grayscale(binary(), map(), map(), binary()) -> binary().
check_grayscale(DID, VersionInfo, Policy, ConfiguredType) ->
    GrayscaleEnabled = maps:get(<<"grayscale_enabled">>, Policy, false),
    GrayscalePercent = ec_cnv:to_integer(maps:get(<<"grayscale_percent">>, VersionInfo, 100)),

    case GrayscaleEnabled =:= true andalso GrayscalePercent < 100 of
        true ->
            %% 灰度启用且不是全量
            case is_in_grayscale(DID, GrayscalePercent) of
                true ->
                    %% 命中灰度，返回配置的升级类型
                    ConfiguredType;
                false ->
                    %% 未命中灰度，暂不推送
                    <<"none">>
            end;
        false ->
            %% 灰度未启用或已全量，直接返回配置的升级类型
            ConfiguredType
    end.

%% @doc 判断设备是否在灰度范围内
%% 使用设备 ID 的哈希值取模，保证同一设备结果固定
-spec is_in_grayscale(binary(), integer()) -> boolean().
is_in_grayscale(DID, GrayscalePercent) when is_binary(DID), GrayscalePercent > 0 ->
    Hash = erlang:phash2(DID, 100),
    Hash < GrayscalePercent;
is_in_grayscale(_, _) ->
    true.

%% @doc 取两个语义版本号中较高的一个
-spec max_vsn(binary(), binary()) -> binary().
max_vsn(Vsn1, Vsn2) ->
    case ec_semver:lt(Vsn1, Vsn2) of
        true -> Vsn2;
        false -> Vsn1
    end.

%% ===================================================================
%% EUnit tests.
%% ===================================================================
