-- 迁移 00000142: push_token 活动 token 唯一绑定（FULL-06 / plan-full §3.3、§7）。
--
-- 合同（plan-full §7 安全硬门）：「token 跨用户/设备不可复用」。
--   一台物理设备在生产上只对应一个推送 token（FCM token / JPush
--   RegistrationID）。同一 token 若同时挂在两个 user_id 上，就会出现：
--     user A 在设备 D1 注册 token T（status=1）→ user B 在同一台机器上登录，
--     其 device_id 与 A 不同，因此既有 uk_push_token_user_device
--     （user_id, device_id 上 WHERE status=1）**不会命中** → A 的行仍是活跃，
--     此后所有投给 A 的推送都会被投递到「已经登成 B」的那台设备上。
--   即：跨用户投递 + B 在自己设备上收到别人的推送。
--
-- 两层分工（本 phase 同时落地，缺一不可）：
--   * 应用层（同 phase 的 push_token_repo:upsert/5）：接管式 upsert——先按
--     token（而非仅按 user_id+device_id）把旧活跃行断电，再插入新行，给出
--     正确语义与 device_id 漂移下的自愈；
--   * DB 层（本迁移）：部分唯一索引兜底——任何写入方（含绕过 repo 的运维
--     SQL、未来新代码、并发竞态）都无法造出第二个活跃同 token 行。
--     没有这一层，两个并发的 upsert 仍能同时通过「先断电再插入」。
--
-- 升级兼容（plan-full §5 expand -> backfill -> switch 纪律）：建索引前先做数据
--   处置——同一 token 的历史多活跃行只保留 updated_at 最新的一条（并列取 id
--   大者），其余置 status=0。**不删除任何行**，只改 status；down 也不恢复
--   （token 是瞬时凭据，恢复只会重建违反合同的状态）。
--
-- 命名纪律（FULL-02 真库实测：同 schema 函数名全局唯一，CREATE OR REPLACE 会
--   静默改写他域同名守卫函数体）：本迁移**不新建任何函数/触发器**，只加一个
--   部分唯一索引，故无需域前缀；索引名 uq_push_token_active_token 属 push_token
--   表域、不与任何既有对象同名（§0 做存在性自证）。
--
-- 迁移契约：up=可重复执行（DROP INDEX IF EXISTS + 幂等的去重 UPDATE + CREATE）；
--   down=只 DROP 本迁移新增的索引，不动 push_token 既有列/约束/索引/触发器
--   （尤其不动 00000001 的 uk_push_token_user_device 与 00000136 的
--   chk_push_token_platform 值域）。禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

-- ============================================================
-- 0) 自证前置：目标索引名当前不属于「他人的既有对象」
-- ============================================================
-- 本项目此前没有同名索引（00000001/00000009 只建 i_push_token_user_id /
-- idx_push_token_uid / uk_push_token_user_device）。这里显式断言，避免
-- 未来有人在别处建了同名索引而被本迁移静默改写。
DO $$
BEGIN
    IF EXISTS (
        SELECT 1 FROM pg_class c
        JOIN pg_namespace n ON n.oid = c.relnamespace
        WHERE n.nspname = 'public'
          AND c.relname = 'uq_push_token_active_token'
          AND c.relkind = 'i'
          AND NOT EXISTS (
              SELECT 1 FROM pg_index i
              WHERE i.indexrelid = c.oid
                AND i.indrelid = 'public.push_token'::regclass
          )
    ) THEN
        RAISE EXCEPTION 'uq_push_token_active_token 已被非 push_token 对象占用';
    END IF;
END
$$;

-- ============================================================
-- 1) 数据处置：同一 token 只保留一条活跃行（其余 set status=0）
-- ============================================================
UPDATE public.push_token p
SET status = 0
WHERE
    p.status = 1
    AND EXISTS (
        SELECT 1
        FROM public.push_token q
        WHERE
            q.token = p.token
            AND q.status = 1
            AND (q.updated_at, q.id) > (p.updated_at, p.id)
    );

-- ============================================================
-- 2) 部分唯一索引：活跃行的 token 全局唯一
-- ============================================================
DROP INDEX IF EXISTS public.uq_push_token_active_token;
CREATE UNIQUE INDEX uq_push_token_active_token ON public.push_token USING btree (token) WHERE (status = 1);

COMMENT ON INDEX public.uq_push_token_active_token IS
    '活动推送 token 全局唯一（迁移 00000142 / FULL-06，plan-full §7）：一个 token（FCM token / JPush RegistrationID）同一时刻只能绑定一个 user_id+device_id；token 换主人或换设备时由 push_token_repo:upsert 先按 token 断电再插入';
