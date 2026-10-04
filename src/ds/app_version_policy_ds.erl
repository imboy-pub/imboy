-module(app_version_policy_ds).
-moduledoc "客户端版本策略领域服务（G3 架构治理）。".
%%%
% app_version_policy_ds — G3 架构治理
% G3: thin DS wrapper for app version policy
%%%

-include("log.hrl").

%% ==================== API ====================
-export([find_by_type/1]).

%% @doc 查询版本策略（pass-through 到 repo）
-spec find_by_type(binary()) -> map().
find_by_type(Cos) ->
    app_version_policy_repo:find_by_type(Cos).
