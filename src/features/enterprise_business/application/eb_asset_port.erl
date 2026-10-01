%%% @doc 扩展点：企业私有对象读写 + 鉴权代理取流（EB-02 冻结契约；实现随 EB-07 落地）。
%%%
%%% 这是**真扩展点**：只有 `-callback` 声明，零实现、零 meck。
%%%
%%% 铁律级约束（plan EB-D06、§2.1 #13、§5.4）：
%%%
%%%   **本端口的任何 -callback 都不得把「存储句柄」交给调用方。**
%%%   调用方永远拿不到对象 key、bucket 名、Garage endpoint 或 presigned 读取链接；
%%%   附件下载只经 IMBoy 鉴权内容端点由后端从私有桶读取并流式转发。
%%%   因此：
%%%     * 传入的 `Descriptor` 使用**不透明存储引用**（`storage_ref`），
%%%       调用方不得解释、记录或回传；
%%%     * 返回的是**流句柄**（`content_stream()`），不是任何链接或 key；
%%%     * 短时 presigned PUT 只在 apply 层内部换取，绝不作为本端口的返回值。
%%%
%%%   铁律 6：前两个业务参数是 `organization_id` 与 `workspace_id`。
-module(eb_asset_port).

-export_type([descriptor/0, content_stream/0, asset_id/0, storage_ref/0]).

-type asset_id() :: integer().

%% 不透明存储引用：仅扩展点实现可解释，调用方与客户端均不可见。
-type storage_ref() :: term().

%% 上传/落库描述：只含哈希、MIME、大小与不透明引用，不含任何可下载链接。
-type descriptor() :: map().

%% 内容流句柄：调用方只能把它交给 HTTP 响应体，不得序列化或返回给客户端。
-type content_stream() :: {content_stream, term()}.

%% @doc 把未确认对象落为企业私有对象并登记元数据。
-callback put_private(OrgId :: integer(), WorkspaceId :: integer(), Descriptor :: descriptor()) ->
    {ok, map()} | {error, term()}.

%% @doc 鉴权代理取流：每次调用都必须由 application 重新校验 JWT / active member /
%% active assignment / resource ACL。返回流句柄，绝不返回链接或 key。
-callback stream_content(OrgId :: integer(), WorkspaceId :: integer(), AssetId :: asset_id()) ->
    {ok, content_stream()} | {error, term()}.

%% @doc 仅由 Org policy 或未确认对象回收触发；不由 uploader 决定，
%% 也不由离职流程触发。
-callback delete_private(OrgId :: integer(), WorkspaceId :: integer(), AssetId :: asset_id()) ->
    ok | {error, term()}.

%% ===================================================================
%% EB-03R 契约面补齐：asset **metadata 生命周期**（**只追加**，R0-4）
%% ===================================================================
%%
%% R0-4 的实测结论：`eb_asset_port` 原本只有 put/stream/delete 三件 object-store
%% 能力，**没有任何 metadata 生命周期**，于是 EB-07 会被迫再开一张契约卡
%% （重演 R0-1 的同一模式）。本段把状态机
%% `pending_confirm → active → deleted` 一并冻结。
%%
%% 与 object-store 三个 callback 的分工（铁律级约束不变）：
%%   * 本段只读写**元数据**（不含任何存储句柄、key、链接）；
%%   * `put_private/3` 负责把对象落到私有桶，`delete_private/3` 负责回收对象；
%%   * `cleanup_asset/3` 只推进**元数据**状态（对象回收由调用方按 policy 决定）。
%%
%% 状态值域与 `enterprise_asset.ck_enterprise_asset_status` 逐字对齐，禁止新增别名。

-type asset_status() :: pending_confirm | active | deleted.
-type asset_metadata() :: map().

-export_type([asset_status/0, asset_metadata/0]).

%% @doc 登记**未确认**资产元数据（status = `pending_confirm`）。
%% 只接受不透明描述（哈希 / MIME / 大小 / 归属），不接受任何可下载链接或对象 key 语义。
-callback insert_asset(OrgId :: integer(), WorkspaceId :: integer(), Descriptor :: descriptor()) ->
    {ok, asset_metadata()} | {error, conflict | term()}.

%% @doc 读取资产元数据（含 status 与归属）；跨 Org / 不存在一律 `{error, not_found}`。
-callback fetch_asset(OrgId :: integer(), WorkspaceId :: integer(), AssetId :: asset_id()) ->
    {ok, asset_metadata()} | {error, not_found | term()}.

%% @doc 确认资产（`pending_confirm` → `active`）。非 pending 状态返回 `{error, conflict}`
%% （幂等重放不算成功：状态机的每次跃迁都要能被审计）。
-callback confirm_asset(OrgId :: integer(), WorkspaceId :: integer(), AssetId :: asset_id()) ->
    {ok, asset_metadata()} | {error, conflict | not_found | term()}.

%% @doc 回收资产元数据（`pending_confirm`|`active` → `deleted`）。
%% 只改元数据状态，不代调用方决定对象回收与保留期。
-callback cleanup_asset(OrgId :: integer(), WorkspaceId :: integer(), AssetId :: asset_id()) ->
    ok | {error, conflict | not_found | term()}.

%% Atomically claim eligible pending metadata and durably retry object deletion.
-callback cleanup_pending_private(integer(), integer(), asset_id(), integer(), non_neg_integer()) ->
    ok | {error, term()}.
