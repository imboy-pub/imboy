-- 迁移 0000227: E2EE 收藏 v1 记录层（PLAN docs/design/2026-10-24-e2ee-collect/ §7.2/§7.3/§7.4，v7.1）
--
-- 背景：M1 收藏记录层客户端加密（info_sealed）需要行级版本元数据与
--   用户级密钥版本高水位，而现有 schema 无任何支撑列；且引用计数触发器
--   imboy_user_collect_fun 的 UPDATE 分支自迁移 26 起不做差分（原注释
--   "业务上不会有单独修改哈希的可能性"）——墓碑化/复活/编辑这类 UPDATE
--   变更 attach_file_hash256 的操作全部不会引起 referer_time 变化：
--   * 取消收藏不减计数（引用虚高，仅不误删）；
--   * 复活不加计数（引用虚低 → 附件可能被孤儿清理误删 → sealed 收藏
--     字节丢失，不可接受）。
--
-- 变更四件：
--   1) user_collect 新增 generation（记录修订号：一切持久化变更 +1，
--      CAS/防回放/跨设备同步检测唯一信号）与 key_version（NULL=legacy
--      行，sealed 判据）。存量行 generation=0 / key_version=NULL，
--      legacy 路径不读写新列，行为不变。
--   2) 新表 user_collect_key_state：用户级 key_version 高水位门
--      （§7.2）。一切 sealed 写路径在事务内先
--      INSERT ... ON CONFLICT (user_id) DO UPDATE ... RETURNING
--      取行锁串行化（等价按用户写锁），门判定提交值 >= 高水位；
--      高水位只升不降、不依赖 user_collect 行聚合——行被删/墓碑被清
--      不影响门。锁顺序约定：先 key_state 行，后 user_collect 行。
--   3) CREATE OR REPLACE imboy_user_collect_fun（函数替换惯例第六次，
--      179→185→196→212→220→226→227）：UPDATE 分支改为差集差分
--      （OLD−NEW 减、NEW−OLD 加、空串防护）；INSERT 分支对 v1 sealed 行
--      （key_version 非空）+1（v1 客户端只上送哈希，无法走应用层 save）；
--      legacy 行（NULL）INSERT 保持无操作、DELETE 分支与 26 终版一致。
--      v1 全生命周期计数规则（§7.4）：触发器为唯一权威，v1 不调 save。
--   4) 墓碑=UPDATE status=0 + generation+1 + attach_file_hash256=''
--      （触发器 −1 释放引用）；复活=复用原行 UPDATE（禁止 INSERT——
--      uk_user_collect_uid_kind UNIQUE(user_id,kind_id) 跨 status，
--      墓碑存在时 INSERT 必撞且现有 add 的 ON CONFLICT 目标不覆盖该冲突）。
--
-- 不改：user_collect 现有两处 UNIQUE/CHECK/触发器监听定义；
--   legacy 行为逐字节保持（SET 子句不含 attach_file_hash256 的 UPDATE
--   不触发触发器；含而值不变时差集为空，计数不动）。

-- 1) 行级版本元数据
ALTER TABLE public.user_collect
    ADD COLUMN generation bigint NOT NULL DEFAULT 0;
ALTER TABLE public.user_collect
    ADD COLUMN key_version integer;
COMMENT ON COLUMN public.user_collect.generation IS
    '记录修订号：一切持久化变更（创建/编辑/墓碑/复活/重包裹）都 +1；CAS 与防回放核心（迁移 227）';
COMMENT ON COLUMN public.user_collect.key_version IS
    'sealed 行的 CK 密钥版本；NULL=legacy 明文行（迁移 227）';

-- 2) 用户级 key_version 高水位（sealed 写入门的权威数据源，§7.2）
CREATE TABLE public.user_collect_key_state (
    user_id bigint PRIMARY KEY,
    max_key_version integer NOT NULL DEFAULT 0,
    updated_at timestamptz NOT NULL DEFAULT now(),
    CONSTRAINT chk_user_collect_key_state_nonneg CHECK (max_key_version >= 0)
);
COMMENT ON TABLE public.user_collect_key_state IS
    'E2EE 收藏用户级 key_version 高水位：只升不降，不随收藏行删除回落（迁移 227）';

-- 3) 引用计数触发器差分（DELETE 分支与迁移 26 终版一致）。
--    INSERT 分支按 NEW.key_version 分流：v1 sealed 行（key_version 非空）
--    由触发器 +1（v1 客户端只上送哈希、无 path/url 元数据，无法走 legacy
--    的应用层 save upsert+1）；legacy 行（NULL）保持无操作——+1 归应用层
--    save，行为逐字节不变。
CREATE OR REPLACE FUNCTION public.imboy_user_collect_fun() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
  IF (TG_OP = 'DELETE' OR TG_OP = 'TRUNCATE') THEN
    UPDATE public.attachment SET referer_time = referer_time - 1
      WHERE file_hash256 = any(string_to_array(OLD.attach_file_hash256, ','));
    RETURN OLD;
  ELSIF (TG_OP = 'UPDATE') THEN
    -- 迁移 227：E2EE 收藏 v1 引入编辑/墓碑/复活（UPDATE 变更哈希列表），
    -- 改为差集差分；值未变（同值重放）差集为空，计数不动。
    IF (OLD.attach_file_hash256 IS DISTINCT FROM NEW.attach_file_hash256) THEN
      UPDATE public.attachment SET referer_time = referer_time - 1
        WHERE file_hash256 <> ''
          AND file_hash256 = ANY(string_to_array(OLD.attach_file_hash256, ','))
          AND NOT (file_hash256 = ANY(string_to_array(NEW.attach_file_hash256, ',')));
      UPDATE public.attachment SET referer_time = referer_time + 1
        WHERE file_hash256 <> ''
          AND file_hash256 = ANY(string_to_array(NEW.attach_file_hash256, ','))
          AND NOT (file_hash256 = ANY(string_to_array(OLD.attach_file_hash256, ',')));
    END IF;
    RETURN NEW;
  ELSIF (TG_OP = 'INSERT') THEN
    -- 迁移 227：v1 sealed 行创建的引用 +1（见上方注释）；legacy 行不触发。
    IF (NEW.key_version IS NOT NULL AND NEW.attach_file_hash256 <> '') THEN
      UPDATE public.attachment SET referer_time = referer_time + 1
        WHERE file_hash256 <> ''
          AND file_hash256 = ANY(string_to_array(NEW.attach_file_hash256, ','));
    END IF;
    RETURN NEW;
  END IF;
end;
$$;
