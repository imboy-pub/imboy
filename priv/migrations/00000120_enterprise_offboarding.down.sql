-- 迁移 00000120 回滚：企业离职交接。
-- 顺序：item 在前（复合 FK 依赖 case 与 identity），再 case；两表都无自定义触发器/函数。
-- 迁移契约：禁止 BEGIN/COMMIT（erlang_migrate 外层单事务包裹）。

DROP TABLE IF EXISTS enterprise_offboarding_item;
DROP TABLE IF EXISTS enterprise_offboarding_case;
