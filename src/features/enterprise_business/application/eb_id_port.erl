%%% @doc 扩展点：注入 ID 生成（EB-02 冻结契约）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 与时钟同理（铁律 4）：domain 不得自行生成 ID（不得依赖 `rand` / `uid:g()`
%%% 等隐式来源）。application 层经本端口取 ID 后作为参数传入 domain，使
%%% 「同输入同输出」的纯函数性质与注入时钟下的可复现性同时成立。
%%%
%%% 实现侧必须产出该部署的 canonical ID 形态（imboy 使用 64-bit TSID；
%%% JSON 传输按 EntityId 规则转 string），扩展点只冻结「生成并返回」这一动作。
-module(eb_id_port).

-moduledoc "扩展点：注入 ID 生成（EB-02 冻结契约）。".
-export_type([kind/0, id/0]).

-type kind() :: atom().
-type id() :: integer().

%% @doc 生成一个新的不透明 ID。`Kind` 用于实现侧按资源类型分域（如
%% `business_identity | enterprise_message | enterprise_asset | audit`）。
-callback new_id(Kind :: kind()) -> id().
