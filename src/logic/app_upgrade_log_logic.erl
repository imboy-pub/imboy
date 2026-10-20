-module(app_upgrade_log_logic).

-moduledoc "客户端升级日志业务逻辑层 / Client upgrade-log business logic layer。".
%%%
%% app_upgrade_log_logic — 客户端升级日志业务逻辑层
%% Client upgrade-log business logic layer.
%%
%% 职责 / Responsibility:
%%   作为 Handler 与 DS 之间的中间层，遵循 4 层架构
%%   Handler → Logic → DS → Repo 单向依赖规范。
%%%

-export([insert/1]).
-export([validate/1]).

-include("common.hrl").

%% 词表与表上的 CHECK 逐项对应——这里改一处，迁移必须同改，否则插入会在异步进程里失败：
%%   event / upgrade_type → priv/migrations/00000207_app_upgrade_log_check_alignment
%%   cos                  → priv/migrations/00000001_foundation（chk_app_upgrade_log_cos）
%% event 词表来自客户端实际上报点（imboyapp upgrade_page / app_upgrade_service /
%% app_upgrade_orchestrator）；upgrade_type 允许空串＝未指定（列 DEFAULT ''）。
-define(EVENTS, [
    <<"check">>,
    <<"prompted">>,
    <<"download_start">>,
    <<"download_done">>,
    <<"verify_ok">>,
    <<"verify_fail">>,
    <<"verify_exception">>,
    <<"install">>,
    <<"cancel">>,
    <<"error">>,
    <<"downgrade">>
]).
-define(UPGRADE_TYPES, [<<"force">>, <<"recommend">>, <<"silent">>, <<"none">>, <<>>]).
-define(COS, [<<"android">>, <<"ios">>, <<"web">>, <<"macos">>]).

-spec insert(map()) -> {ok, term()} | {error, term()}.
insert(Data) ->
    app_upgrade_log_ds:insert(Data).

%% @doc 入库前校验三个受 CHECK 约束的字段。失败原因可直接回给客户端，
%% 让词表漂移在 HTTP 400 上可见，而不是在异步插入里被静默丢弃。
-spec validate(map()) -> ok | {error, binary()}.
validate(Data) ->
    Checks = [
        {<<"event">>, maps:get(<<"event">>, Data, <<>>), ?EVENTS},
        {<<"upgrade_type">>, maps:get(<<"upgrade_type">>, Data, <<>>), ?UPGRADE_TYPES},
        {<<"cos">>, maps:get(<<"cos">>, Data, <<>>), ?COS}
    ],
    case [Field || {Field, Value, Allowed} <- Checks, not lists:member(Value, Allowed)] of
        [] -> ok;
        [Field | _] -> {error, <<"invalid ", Field/binary>>}
    end.
