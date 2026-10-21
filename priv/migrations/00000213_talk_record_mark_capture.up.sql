-- 00000213_talk_record_mark_capture.up.sql
-- 面聊记录 T07：talk_record_mark 采集绑定列扩展（主控制器裁决 2026-10-21）。
-- 契约：BACKEND-API.md §11.1（mark 绑定 capture_id+capture_offset_ms，有效音频时间轴）
--       §4 marks API（PATCH/DELETE /:id/marks/:mark_id + If-Match）
-- 迁移契约：禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹；down 成对可逆。
--
-- 裁决落实：
--   * capture_id bigint NULL + 复合 FK (record_id, capture_id)→capture(record_id, id)
--     ——API/写入路径以 capture_id+capture_offset_ms 为真源；
--   * capture_offset_ms bigint NOT NULL DEFAULT 0——有效音频时间轴位置（§11.1）；
--   * position_ms 保留为 V1 单 capture 投影（读时由 offset 投影，写时同时维护）。
--
-- 超出裁决的最小扩展（必要，报告已列）：
--   * revision bigint NOT NULL DEFAULT 1——契约 §3/§4 要求 marks PATCH/DELETE
--     必须 If-Match，mark 为可变资源需要自身 ETag/revision 域（§11.2），
--     00000211 表无版本列，If-Match 无从实现；故随本迁移补齐，down 一并回滚。
SET LOCAL lock_timeout = '5s';

ALTER TABLE talk_record_mark
    ADD COLUMN IF NOT EXISTS capture_id        bigint,
    ADD COLUMN IF NOT EXISTS capture_offset_ms bigint DEFAULT 0 NOT NULL,
    ADD COLUMN IF NOT EXISTS revision          bigint DEFAULT 1 NOT NULL;

-- 复合 FK：mark 绑定的 capture 必须与 mark 同记录（§8 防跨记录 ID 混用）；
-- MATCH SIMPLE 语义下 capture_id IS NULL 的行（无采集绑定的历史 mark）放行。
ALTER TABLE talk_record_mark DROP CONSTRAINT IF EXISTS fk_trm_capture;
ALTER TABLE talk_record_mark ADD CONSTRAINT fk_trm_capture
    FOREIGN KEY (record_id, capture_id)
    REFERENCES talk_record_capture(record_id, id) ON DELETE RESTRICT;

ALTER TABLE talk_record_mark DROP CONSTRAINT IF EXISTS ck_trm_capture_offset;
ALTER TABLE talk_record_mark ADD CONSTRAINT ck_trm_capture_offset
    CHECK (capture_offset_ms >= 0);
ALTER TABLE talk_record_mark DROP CONSTRAINT IF EXISTS ck_trm_revision;
ALTER TABLE talk_record_mark ADD CONSTRAINT ck_trm_revision
    CHECK (revision >= 1);

-- 采集维度检索（capture 反查其 marks；部分索引避开 NULL 行）
CREATE INDEX IF NOT EXISTS i_trm_capture ON talk_record_mark (capture_id)
    WHERE capture_id IS NOT NULL;

COMMENT ON COLUMN talk_record_mark.capture_id IS '绑定的采集（§11.1 mark 绑定 capture_id+capture_offset_ms；NULL=无采集绑定的历史/导入 mark）';
COMMENT ON COLUMN talk_record_mark.capture_offset_ms IS '有效音频时间轴位置（真源）；暂停期间不产生空段';
COMMENT ON COLUMN talk_record_mark.position_ms IS 'V1 单 capture 投影：= capture_offset_ms（多 capture 时由 offset 投影合成）';
COMMENT ON COLUMN talk_record_mark.revision IS 'mark 自身版本域（PATCH/DELETE If-Match；§11.2）';
