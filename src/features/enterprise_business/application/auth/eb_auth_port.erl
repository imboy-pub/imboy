%%% @doc 企业授权的事实扩展点（Port）——**只读**契约。
%%%
%%% 依据：plan v4.1 EB-04、EB-D10、EB-D11、`docs/architecture/feature-slice-rules.md`
%%% §0.3（扩展点 ≡ Port ≡ behaviour）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 设计要点（EB-04 的授权语义）：
%%%
%%%   * **逐请求加载**。授权判定所需的成员状态、identity assignment、治理角色与
%%%     权限必须**每个请求**从权威事实源重新读取；JWT 自报的 `member_status` /
%%%     `role` / `permissions` 一律不采信。这是「suspended 后旧 JWT 立即失效」
%%%     的实现前提——不依赖 token 过期时间，也不做跨请求缓存。
%%%   * **零写能力**（零副作用）。本扩展点**不声明任何写 callback**：授权路径在
%%%     契约层就没有 `insert/update/delete/append/advance` 等能力，越权判定不可能
%%%     产生副作用。任何写操作属于别的用例模块，不属于授权。
%%%   * **租户作用域显式**。返回值中的 `organization_id` 必须与调用方解析出的目标
%%%     Org 一致；不一致时 `eb_auth_app` fail-closed（`cross_org`），不降级为
%%%     「无租户条件」。
%%%
%%% 具体实现（PG/auth_adapter）由后续卡的 infrastructure 落地并由装配选择；
%%% 本卡只冻结契约，使「授权只读且逐请求」成为可静态判定的事实。
-module(eb_auth_port).

-export_type([port_module/0, facts/0, assignment/0]).

-type port_module() :: module().

%% @doc 授权事实（按 principal 类别取子集）。
%%
%% **payload 的键集合是契约的一部分，不是实现细节。** 只冻结 arity 而不管键
%% 会让实现与消费者各自 PASS、装配后却不兼容（两侧 arity 一致、键不同）——
%% 见 `board.defects.BUG-02`。下述键集合由两侧共同承担：
%%
%%   * 成员类：`#{organization_id, member, assignments, permissions}`
%%     - `organization_id`：与调用方解析出的目标 Org 一致（不一致 ⇒ fail-closed）
%%     - `member`：`#{user_id, role, status}`，`status` 含 `suspended`（事实照报）
%%     - `assignments`：`[assignment()]`
%%     - `permissions`：`[binary()]`——基于角色的**静态事实**，不是最终裁决
%%   * 访客/店铺类：`#{organization_id, contact_id, digest, expires_at, revoked}`
%%   * 平台类：`#{adm_user_id, permissions}`
-type facts() :: map().

%% @doc 单条经办关系（`facts()` 的 `assignments` 元素）。
%%
%% `user_id` 与 `organization_id` **必须逐条存在**：调用方用它们做归属过滤与
%% 跨 Org 判定（`eb_auth_app:active_assignments/3`）；缺键会被判为
%% `identity_assignment_missing`（fail-closed，不放行）。
-type assignment() ::
    #{
        business_identity_id := integer(),
        user_id := integer(),
        organization_id := integer(),
        function_key := binary(),
        status := active | suspended | removed | term(),
        version := integer()
    }.

%% @doc 逐请求加载该请求的授权事实。**不得缓存、不得跨请求复用**。
%% 未知 / 不可解析 / 无租户归属时必须返回 `{error, ...}`（fail-closed），
%% 绝不允许返回「默认放行」的空事实。
-callback load_request_facts(Request :: map()) -> {ok, facts()} | {error, term()}.
