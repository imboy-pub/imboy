%%% @doc `cs_org_lifecycle_port` 的测试替身（仅 EUnit；镜像 cs_fake_store 角色）。
%%%
%%% 恒返回 `{ok, active}`——纯单元套件（无 DB）经 `params/1` 注入本模块，
%%% 使 cs_org_lifecycle_gate 的端口解析不触库；archived 分支的运行时行为由
%%% 真库套件 cs_org_compat_tests（A04）覆盖。
-module(cs_fake_org_lifecycle).

-export([status/1, set_status/1, reset/0]).

%% 进程字典足够：单套件内串行使用，无并发裁决语义。
set_status(Status) when Status =:= active; Status =:= archived ->
    put(cs_fake_org_lifecycle_status, Status),
    ok.

reset() ->
    erase(cs_fake_org_lifecycle_status),
    ok.

%% cs_org_lifecycle_port callback（未显式 set 时恒 active）
status(_OrgId) ->
    case get(cs_fake_org_lifecycle_status) of
        archived -> {ok, archived};
        _ -> {ok, active}
    end.
