-- 孤儿附件查询复合索引
-- 加速 referer_time=0 AND created_at < NOW()-INTERVAL 的查询
CREATE INDEX IF NOT EXISTS i_attachment_referer_created
  ON public.attachment (referer_time, created_at)
  WHERE status = 1;
