%% attachment_lifecycle_v5_obs — V5 运行期观测导出器（W3 逐场景证据流）。
%%
%% 套件场景函数在自己的 ?assert* 全部通过之后，把本场景**实际断言值**
%% （计数/版本/状态/相等性）写成一行 JSON 追加到 V5_OBSERVATIONS_FILE
%% 指向的文件；场景断言失败时 emit 不会执行 => 观测流缺该场景 => adapter
%% fail-closed（exit 30），不存在「套件绿了但场景没跑」的封印路径。
%%
%% V5_OBSERVATIONS_FILE 未设置时（普通 eunit / make eunit / dry-run 无观测
%% 采集）emit 是纯 no-op，不改变任何既有测试语义。JSON 编码用 jsone；
%% 事件值只允许 JSON 安全类型（binary / integer / boolean / list）。
-module(attachment_lifecycle_v5_obs).

-export([emit/1]).

emit(Event) ->
    case os:getenv("V5_OBSERVATIONS_FILE") of
        false ->
            ok;
        Path ->
            Line = jsone:encode(Event#{emitted_at_ms => os:system_time(millisecond)}),
            ok = file:write_file(Path, [Line, $\n], [append]),
            ok
    end.
