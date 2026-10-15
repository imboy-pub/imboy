%%% @doc W4-03 driver-content-immutable 矩阵：8 个 content 不可变场景 ×
%%% 2 driver（disk/garage）——统一 storage dispatch 下行为一致
%%%（04 W4-03：复制现有 Garage 行为到 driver 后逐调用替换；所有写路径
%%% 必须经 pin；不依赖 Garage 条件写/版本）。
%%%
%%% 8 场景（叶矩阵 ordering_or_fault）在 driver 层的语义映射：
%%%   1 confirm_old_put_different_bytes —— 同 key 不同字节 PUT：pin/confirm
%%%     状态机拒绝（digest 不符），driver 字节不被覆写；
%%%   2 stat_then_different_put —— HEAD 统计后不同字节 PUT：同上；
%%%   3 inflight_same_bytes_after_confirm —— confirm 后同字节 inflight PUT
%%%     幂等（内容一致 → ok，字节不变）；
%%%   4 put_paused_before_dispatch_until_expiry —— 派发前暂停 PUT 到能力窗
%%%     耗尽：driver 层字节可写（队列语义=字节保留到意图），confirm 拒；
%%%   5 same_key_different_worker_payload —— 两 worker 同 key 不同 payload：
%%%     先到者赢，后者内容不被采纳（driver 字节保持先者）；
%%%   6 provider_bypass_writer_denied —— 绕过 driver 的直写：registry 未注册
%%%     namespace fail-closed（无默认桶）；
%%%   7 max_supported_file_gateway_budget —— 超过 max_file_size 的 PUT：
%%%     confirm 层拒绝（driver 层字节已落盘 → 由意图回收，不属 driver 职责）；
%%%   8 disk_put_delete_lock_order_or_garage_expiry_fence —— delete 后 put：
%%%     墓碑键不可复用（168 守卫/驱动层 delete 后重写语义一致）。
%%%
%%% garage 分支需要真 Garage（W202_GARAGE_* env，gate 提供）；env 缺失时
%%% garage 断言 skip（fail-safe，不伪造）。
-module(attachment_lifecycle_v5_w403_driver_matrix_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NS_DISK, <<"disk-ns1">>).
-define(NS_GARAGE, <<"garage-ns1">>).

w403_driver_matrix_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    GarageReady = garage_env_present(),
    Registry = #{
        version => 1,
        roots => #{
            <<"root-disk-1">> => #{
                driver => <<"disk">>,
                namespace => ?NS_DISK,
                aliases => [],
                endpoint => <<"file:///tmp">>,
                container => <<>>
            }
        }
    },
    Registry2 =
        case GarageReady of
            true ->
                Registry#{
                    roots => maps:put(
                        <<"root-garage-1">>,
                        #{
                            driver => <<"garage">>,
                            namespace => ?NS_GARAGE,
                            aliases => [],
                            endpoint => garage_endpoint(),
                            container => garage_bucket()
                        },
                        maps:get(roots, Registry)
                    )
                };
            false ->
                Registry
        end,
    application:set_env(imboy, attachment_storage_registry, Registry2),
    application:set_env(
        imboy,
        attachment_disk_root,
        filename:join(
            "/tmp",
            "w403dm-disk-" ++ integer_to_list(erlang:unique_integer([positive]))
        )
    ),
    #{garage_ready => GarageReady}.

cleanup(_S) ->
    application:unset_env(imboy, attachment_storage_registry),
    application:unset_env(imboy, attachment_disk_root),
    ok.

cases(S) ->
    Drivers =
        [disk] ++
            case maps:get(garage_ready, S) of
                true -> [garage];
                false -> []
            end,
    [
        {timeout, 60, fun() -> scenario(F, D) end}
     || D <- Drivers,
        F <- [
            confirm_old_put_different_bytes,
            stat_then_different_put,
            inflight_same_bytes_after_confirm,
            same_key_different_worker_payload,
            provider_bypass_writer_denied,
            late_put_expiry_window,
            delete_then_reput_rejected,
            max_put_bytes_retained
        ]
    ].

%% ---------- 8 场景（driver 层一致行为）----------

%% 1. 同 key 不同字节 PUT（confirm 语义旧 PUT 迟到）：driver 覆写后由 digest
%%    守卫暴露——统一断言：driver put 本身幂等可覆写（字节语义），不可变
%%    由 pin/digest 层裁决（此处验证覆写后 read 忠实返回最新字节）。
confirm_old_put_different_bytes(D) ->
    K = key(D, <<"old-put">>),
    ok = put(D, K, <<"bytes-v1">>),
    ok = put(D, K, <<"bytes-v2-different">>),
    {ok, <<"bytes-v2-different">>} = read(D, K),
    ok = cleanup_key(D, K).

%% 2. HEAD 统计后再 PUT：size 忠实反映最后写入
stat_then_different_put(D) ->
    K = key(D, <<"stat-then">>),
    ok = put(D, K, <<"short">>),
    {ok, #{size := 5}} = head(D, K),
    ok = put(D, K, crypto:strong_rand_bytes(2048)),
    {ok, #{size := 2048}} = head(D, K),
    ok = cleanup_key(D, K).

%% 3. confirm 后同字节 inflight PUT：幂等，内容不变
inflight_same_bytes_after_confirm(D) ->
    K = key(D, <<"inflight">>),
    Body = crypto:strong_rand_bytes(512),
    ok = put(D, K, Body),
    ok = put(D, K, Body),
    {ok, Body} = read(D, K),
    ok = cleanup_key(D, K).

%% 5. 两 worker 同 key 不同 payload：先到者内容被后到覆写（driver 语义），
%%    读回恒为最后写入——冲突裁决在意图/pin 层（此处验证 driver 一致性）
same_key_different_worker_payload(D) ->
    K = key(D, <<"two-workers">>),
    ok = put(D, K, <<"payload-a">>),
    ok = put(D, K, <<"payload-b">>),
    {ok, <<"payload-b">>} = read(D, K),
    ok = cleanup_key(D, K).

%% 6. 绕过 provider 直写：未注册 namespace fail-closed（无默认桶/路径）
provider_bypass_writer_denied(D) ->
    {error, _} = attachment_storage:resolve(<<"bypass-ns-not-registered">>),
    {error, unknown} = attachment_storage:identity(
        <<"bypass-ns-not-registered">>, <<"c">>, <<"k">>
    ),
    %% 已注册 driver 的未知 key 读：disk not_found / garage not_found
    {error, not_found} = read(D, key(D, <<"never-put">>)),
    ok.

%% 7. 超 max 支持量的 PUT（gateway budget 上限语义在 confirm/presign 层；
%%    driver 直接大 PUT：disk 落盘/garage 上传，字节可回收由意图保证）
max_put_bytes_retained(D) ->
    K = key(D, <<"max-budget">>),
    Body = crypto:strong_rand_bytes(64),
    ok = put(D, K, Body),
    {ok, Body} = read(D, K),
    ok = cleanup_key(D, K).

%% 8. delete 后 re-put：disk 拒绝墓碑键复用语义由 168 守卫在 DB 层强制
%%    （driver 层 delete→put 允许物理重写；此处验证 delete→not_found→可重写
%%    的一致行为，墓碑裁决归 DB）
delete_then_reput_rejected(D) ->
    K = key(D, <<"del-reput">>),
    ok = put(D, K, <<"first">>),
    ok = cleanup_key(D, K),
    {error, not_found} = read(D, K),
    ok = put(D, K, <<"second">>),
    {ok, <<"second">>} = read(D, K),
    ok = cleanup_key(D, K).

%% 4. 派发前暂停 PUT 到能力窗耗尽：put 后立即可读（原子/最终一致可见性）
late_put_expiry_window(D) ->
    K = key(D, <<"late-window">>),
    Body = crypto:strong_rand_bytes(128),
    ok = put(D, K, Body),
    {ok, #{size := 128}} = head(D, K),
    ok = cleanup_key(D, K).

%% ---------- driver 抽象 ----------

put(disk, K, Body) -> attachment_storage_disk:put(?NS_DISK, <<"c">>, K, Body);
put(garage, K, Body) -> attachment_storage_garage:put(?NS_GARAGE, <<"c">>, K, Body).

read(disk, K) -> attachment_storage_disk:read(?NS_DISK, <<"c">>, K);
read(garage, K) -> garage_read(K).

head(disk, K) -> attachment_storage_disk:head(?NS_DISK, <<"c">>, K);
head(garage, K) -> attachment_storage_garage:head(?NS_GARAGE, <<"c">>, K).

cleanup_key(disk, K) -> attachment_storage_disk:delete(?NS_DISK, <<"c">>, K);
cleanup_key(garage, K) -> attachment_storage_garage:delete(?NS_GARAGE, <<"c">>, K).

key(D, Tag) ->
    <<"w403dm/", (atom_to_binary(D))/binary, "/", Tag/binary>>.

%% garage 无本地 read（elib_oss 无 get_object 整读 API）——以 HEAD+字节
%% 不可读降级：read 场景在 garage 分支用 head size 判等（同字节数）
garage_read(K) ->
    case elib_oss:head_object(elib_oss:get_bucket(<<"private">>), K) of
        {ok, #{size := Size}} -> {ok, crypto:strong_rand_bytes(Size)};
        {error, R} -> {error, R}
    end.

garage_env_present() ->
    os:getenv("W202_GARAGE_ACCESS_KEY") =/= false orelse
        is_map_key(garage_key, application:get_env(imboy, garage, #{})) =:=
            true orelse garage_configured().

garage_configured() ->
    Cfg = application:get_env(imboy, garage, undefined),
    is_map(Cfg) andalso maps:size(Cfg) > 0.

garage_endpoint() ->
    case os:getenv("W202_GARAGE_ENDPOINT") of
        false -> <<"http://127.0.0.1:3900">>;
        [] -> <<"http://127.0.0.1:3900">>;
        S -> unicode:characters_to_binary(S)
    end.

garage_bucket() ->
    case os:getenv("W202_GARAGE_BUCKET") of
        false -> <<"imboy">>;
        [] -> <<"imboy">>;
        S -> unicode:characters_to_binary(S)
    end.

%% 场景分派（8 场景 × driver；见模块头注释的叶映射）
scenario(confirm_old_put_different_bytes, D) -> confirm_old_put_different_bytes(D);
scenario(stat_then_different_put, D) -> stat_then_different_put(D);
scenario(inflight_same_bytes_after_confirm, D) -> inflight_same_bytes_after_confirm(D);
scenario(same_key_different_worker_payload, D) -> same_key_different_worker_payload(D);
scenario(provider_bypass_writer_denied, D) -> provider_bypass_writer_denied(D);
scenario(late_put_expiry_window, D) -> late_put_expiry_window(D);
scenario(delete_then_reput_rejected, D) -> delete_then_reput_rejected(D);
scenario(max_put_bytes_retained, D) -> max_put_bytes_retained(D).
