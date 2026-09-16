%%% @doc 扩展点：注入时钟（EB-02 冻结契约）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 铁律 4 要求 domain 不得依赖 `os:timestamp/0` / `calendar:universal_time/0`
%%% 等隐式环境依赖；时间必须由调用方作为参数传入。application 层统一经本端口
%%% 取「现在」再传给 domain 纯函数，使「注入时钟下的 retain_until / purge
%%% 资格」可被逐字复现（plan §2.1 #17）。
-module(eb_clock_port).

-export_type([unix_seconds/0]).

-type unix_seconds() :: integer().

%% @doc 当前时间（Unix 秒）。仅允许 application 层调用。
-callback now() -> unix_seconds().
