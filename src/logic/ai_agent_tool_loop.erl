-module(ai_agent_tool_loop).

-moduledoc "AGT-01 V1 No-Go 的 fail-closed 占位 —— 覆盖旧 beam，防止打包陈旧工具环代码。".
%% AGT-01 No-Go for V1. Keep this fail-closed module so incremental releases
%% overwrite any older runnable beam instead of packaging stale tool-loop code.

-export([run/5, applicable/2]).

-spec applicable(map(), module()) -> false.
applicable(_Agent, _ProviderMod) ->
    false.

-spec run(integer(), [map()], map(), module(), map()) -> {error, disabled_v1}.
run(_Uid, _Messages, _Opts, _ProviderMod, _ToolCtx) ->
    {error, disabled_v1}.
