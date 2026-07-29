-- 00000050_attachment_cipher.up.sql
-- E2EE-061 Slice 5（后端字段语义）：附件密文判别位与哈希/大小语义变更。
--
-- 背景与人工拍板（2026-07-30，见 docs/guides/e2ee/v2/27-...design.md §6）：
--   ① confirm 只上报**密文**哈希 —— 明文 SHA-256 只进加密的 attachment_descriptor，
--      永不到达服务端。代价是服务端失去跨用户秒传/去重/已知违规文件识别能力
--      （与 ADR 18 合规边界直接相关，已由用户拍板接受）。
--   ② 历史明文附件**暂不回迁，但预留判别位** —— 即本迁移新增的 cipher 列。
--      旧行 cipher IS NULL 表示明文对象，语义完全不变；新的加密上传写入套件名。
--
-- 为什么是一列可空字符串而不是 boolean：
--   将来若换套件，boolean 无法表达「是哪一种」，而 attachment_descriptor 里
--   cipher 本就是一个具名字段（不做协商，当前只接受 AES-256-GCM）。
--   两侧用同一个概念，避免再造一个只有真假的影子字段。
--
-- ⚠️ 本迁移**不回填、不改动任何既有行**：旧行保持 NULL = 明文，
--   这正是拍板 ② 的落点。旧附件必须仍可读（Slice 9 的正向可用性）。
--
-- ⚠️ 引用计数触发器（user_collect.attach_file_hash256 <-> attachment.file_hash256
--   按值 JOIN，见迁移 000026）**不受影响**：收藏侧存的就是 attachment 里那一份
--   哈希字符串，同代数据仍然匹配。但请注意语义已变——加密附件的这个值是密文哈希，
--   同一明文用不同 content key 上传两次会得到两个不同的值，**跨用户去重就此不再成立**，
--   这是拍板 ① 的既定后果，不是缺陷。

ALTER TABLE public.attachment
    ADD COLUMN IF NOT EXISTS cipher character varying(32) DEFAULT NULL;

COMMENT ON COLUMN public.attachment.cipher IS
    '附件内容加密套件；NULL=明文对象（历史行与未加密上传）。非 NULL 时该行是 E2EE 附件，'
    'file_hash256 与 size 均指**密文**，服务端不持有 content key，也拿不到明文哈希。';

COMMENT ON COLUMN public.attachment.file_hash256 IS
    '附件文件哈希（SHA-256 hex）。cipher IS NULL 时为明文哈希（历史行可能是旧 MD5）；'
    'cipher 非 NULL 时为**密文**哈希——明文哈希只存在于客户端加密的 attachment_descriptor 内。'
    '仅作完整性参考，不作安全边界。';

COMMENT ON COLUMN public.attachment.size IS
    '对象字节数（服务端 HEAD 核实的真实值）。cipher 非 NULL 时为**密文**大小，'
    '明文大小只在 attachment_descriptor 内。';

-- 部分索引：用于日后盘点「还有多少明文对象」与分批回迁（拍板 ② 的"预留"）。
-- 只索引明文行，加密行不进索引，随加密普及索引会自然缩小。
CREATE INDEX IF NOT EXISTS idx_attachment_plaintext_backlog
    ON public.attachment (created_at)
    WHERE cipher IS NULL;
