-module(app_upgrade_log_logic).

%%%
%% app_upgrade_log_logic — 客户端升级日志业务逻辑层
%% Client upgrade-log business logic layer.
%%
%% 职责 / Responsibility:
%%   作为 Handler 与 DS 之间的中间层，遵循 4 层架构
%%   Handler → Logic → DS → Repo 单向依赖规范。
%%%

-export([insert/1]).

-include("common.hrl").

-spec insert(map()) -> {ok, term()} | {error, term()}.
insert(Data) ->
    app_upgrade_log_ds:insert(Data).
