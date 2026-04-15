-module(group_random_code_ds).
%%%
% group_random_code_ds — G3 架构治理：group_handler 不应直调 group_random_code_repo
% G3: thin DS wrapper for group random codes
%%%

-include("log.hrl").

%% ==================== API ====================
-export([find_by_gid/2]).

-spec find_by_gid(integer(), binary()) -> map().
find_by_gid(Gid, Column) -> group_random_code_repo:find_by_gid(Gid, Column).
