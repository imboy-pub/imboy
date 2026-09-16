-- 迁移 00000115 回滚：企业客户与客户资料。
-- 顺序：子表在前（FK 依赖逆序）——note → contact_assignment → contact_identity → contact。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TABLE IF EXISTS enterprise_note;
DROP TABLE IF EXISTS enterprise_contact_assignment;
DROP TABLE IF EXISTS enterprise_contact_identity;
DROP TABLE IF EXISTS enterprise_contact;
