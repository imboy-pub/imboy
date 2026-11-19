-- 00000281 down：移除转发请求稳定身份表（expand-only 的纯收缩；无数据回填、
-- 无既有结构改动）。真实发布前的存量请求需按保留/补偿演练（同 M-01 合同纪律）。
DROP TABLE IF EXISTS public.forward_request_identity;
