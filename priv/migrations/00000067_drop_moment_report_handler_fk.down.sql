-- 回滚：恢复外键。用 NOT VALID 避免存量 adm 体系 id 使回滚失败；
-- 与 up 迁移前的原约束等价（仍由后续写入方保证引用有效）。
ALTER TABLE public.moment_report
    ADD CONSTRAINT fk_moment_report_handler
    FOREIGN KEY (handled_by) REFERENCES public."user"(id) ON DELETE SET NULL
    NOT VALID;
