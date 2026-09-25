-- 迁移 00000148: 客服坐席 presence heartbeat lease 表（CS-BE-05 seat presence）。
--
-- 冻结决策（CS-DEC-02 / 计划 v1.1 CS-BE-05）：
--   * 运行态（online/away/busy/offline）是**派生值**（heartbeat 新鲜度 + 会话
--     负载 + 手动 away），不在 cs_seat 上新增任何治理列——治理开关 `enabled`
--     （运营属性）与运行态语义分离。本表只存派生所需的**事实输入**：
--     last_heartbeat_at（心跳 lease）与 manual_status（手动 away）。
--   * 心跳是持久/共享可见 lease：跨节点（多节点部署）共享可见，派生与派单
--     读同一份 DB 事实，不走节点内存（禁 Redis；如需缓存走 depcache，不属
--     本卡范围）。
--   * 派生在应用层纯函数完成（cs_presence），本表无函数/触发器；写入只有
--     两条路径：heartbeat upsert（刷新 last_heartbeat_at，可顺带 set/clear
--     manual_status）与手动状态显式 set。
--   * TTL（默认 90s 无心跳 → offline）与心跳间隔（默认 30s）是应用层常量，
--     时钟可注入——本表存的是 timestamptz 事实，派生时用注入的 Now 比较。
--
-- 命名纪律（FULL-02 / 00000147 同款）：不新建函数/触发器，只建表/约束/索引/
-- 注释；幂等用 IF NOT EXISTS；手动状态 CHECK 冻结枚举（当前仅 'away'——
-- manual online 无意义：非 away 的运行态由心跳/负载派生）。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

CREATE TABLE IF NOT EXISTS customer_service_seat_presence (
    organization_id      bigint                   NOT NULL,
    business_identity_id bigint                   NOT NULL,
    last_heartbeat_at    timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    manual_status        text,
    updated_at           timestamp with time zone DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT pk_customer_service_seat_presence
        PRIMARY KEY (organization_id, business_identity_id),
    CONSTRAINT ck_cssp_manual_status
        CHECK (manual_status IS NULL OR manual_status = 'away'),
    CONSTRAINT fk_cssp_seat FOREIGN KEY (organization_id, business_identity_id)
        REFERENCES customer_service_seat (organization_id, business_identity_id)
        ON DELETE CASCADE
);

COMMENT ON TABLE customer_service_seat_presence IS
    '客服坐席 presence 心跳 lease（CS-BE-05）：持久/共享可见的运行态事实输入；online/away/busy/offline 由 cs_presence 派生（TTL 90s + 手动 away 优先 + 容量满 busy），本表不存派生值、cs_seat 无新增治理列（CS-DEC-02）';
COMMENT ON COLUMN customer_service_seat_presence.last_heartbeat_at IS
    '最近一次心跳的服务端时钟（写路径注入 at，客户端不可报时）；NULL 视为从未上报（offline）';
COMMENT ON COLUMN customer_service_seat_presence.manual_status IS
    '手动运行态覆盖（当前冻结枚举仅 ''away''）：手动 away 优先于自动派生；NULL=无手动覆盖';
