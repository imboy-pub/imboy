-module(app_version_logic).
%%%
% APP 版本升级降级业务逻辑模块
% 实现三级升级策略（force/recommend/silent）和灰度发布判断
%%%

-export([check/3]).

-include("common.hrl").
-include("log.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

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
    VersionInfo = app_version_repo:find(Cos, RegionCode),
    case maps:size(VersionInfo) of
        0 ->
            %% 没有版本记录，返回无需更新
            #{<<"updatable">> => false, <<"upgrade_type">> => <<"none">>};
        _ ->
            %% 2. 查询全局策略
            Policy = app_version_policy_repo:find_by_type(Cos),
            %% 3. 计算升级策略
            determine_upgrade(ClientVsn, DID, VersionInfo, Policy)
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

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
    %% 兼容旧的 force_update 字段：1=强制
    ForceUpdate = ec_cnv:to_integer(maps:get(<<"force_update">>, VersionInfo, 2)),
    %% 新的 upgrade_type 字段
    ConfiguredType = maps:get(<<"upgrade_type">>, VersionInfo, <<"recommend">>),

    %% 检查 2：旧字段标记为强制 或 新字段标记为强制 → 强制升级
    case ForceUpdate =:= 1 orelse ConfiguredType =:= <<"force">> of
        true ->
            <<"force">>;
        false ->
            %% 检查 3：灰度判断
            check_grayscale(DID, VersionInfo, Policy, ConfiguredType)
    end.

%% @doc 灰度判断
-spec check_grayscale(binary(), map(), map(), binary()) -> binary().
check_grayscale(DID, VersionInfo, Policy, ConfiguredType) ->
    GrayscaleEnabled = ec_cnv:to_integer(maps:get(<<"grayscale_enabled">>, Policy, 0)),
    GrayscalePercent = ec_cnv:to_integer(maps:get(<<"grayscale_percent">>, VersionInfo, 100)),

    case GrayscaleEnabled =:= 1 andalso GrayscalePercent < 100 of
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
