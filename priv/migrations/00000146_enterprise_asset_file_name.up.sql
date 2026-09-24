-- 迁移 00000146: enterprise_asset 补 file_name 列（CS-BE-01 历史消息资产投影）。
--
-- 背景（CSX-01 断链 1 附带缺口）：冻结契约要求客服/enterprise 历史消息响应投影
-- assets:[{id,mime,size_bytes,file_name,status}]，而 enterprise_asset 建表迁移
-- （00000118）的列集没有 file_name——出站白名单 ASSET_VIEW_KEYS 同样无该键，
-- 后端即使加投影也无数据可填。
--
-- 本迁移只做一件事：给 enterprise_asset 加可空 file_name text。
--   * 可空（NULL = 上传方未声明文件名）：不回填、不默认——既有行与既有
--     上传链（不带 file_name 的 presign）行为逐字不变，纯增量。
--   * CHECK 约束与仓库既有文件名校验同口径（src/logic/enterprise_asset_logic.erl
--     valid_name/1：binary 1..256 字节）：非空时 1..256 字符。文件名是展示值，
--     不是路径——不参与 object_key 派生，也不得含 URL/存储语义（列上无从
--     约束 URL，安全性由投影白名单保证：file_name 只作为字符串透出）。
--   * 命名纪律（FULL-02 同款）：不新建函数/触发器，只加列/约束/注释；
--     幂等用 IF NOT EXISTS / DROP IF EXISTS。
--
-- 禁止 BEGIN/COMMIT——erlang_migrate 外层单事务包裹。

SET lock_timeout = '5s';
SET statement_timeout = '15min';

ALTER TABLE enterprise_asset
    ADD COLUMN IF NOT EXISTS file_name text;

ALTER TABLE enterprise_asset
    DROP CONSTRAINT IF EXISTS ck_enterprise_asset_file_name;

ALTER TABLE enterprise_asset
    ADD CONSTRAINT ck_enterprise_asset_file_name CHECK (
        file_name IS NULL OR (char_length(file_name) >= 1 AND char_length(file_name) <= 256));

COMMENT ON COLUMN enterprise_asset.file_name IS
    '上传方声明的展示文件名（CS-BE-01 冻结契约 assets[].file_name 的数据源；可空=未声明，不回填；1..256 字符）';
