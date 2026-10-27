-- 迁移 231 回滚：删除资源创建受理回执表
-- （仅合成/开发库应用；生产启用另需授权，见计划 §10.4 混跑节点约束）

DROP TABLE IF EXISTS public.resource_create_receipt;
