-module(ai_agent_tool_loop).

%% AGT-01 No-Go for V1. Keep this fail-closed module so incremental releases
%% overwrite any older runnable beam instead of packaging stale tool-loop code.

-export([run/5, applicable/2]).

-spec applicable(map(), module()) -> false.
applicable(_Agent, _ProviderMod) ->
    false.

-spec run(integer(), [map()], map(), module(), map()) -> {error, disabled_v1}.
run(_Uid, _Messages, _Opts, _ProviderMod, _ToolCtx) ->
    {error, disabled_v1}.
