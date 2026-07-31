-- 00000053_user_collect_info_recrypt.down.sql
-- 回滚：把明文 info 还原成 A-05 之前的 SQL 表达式字面值形态。
--
-- ⚠️ 只还原结构，不还原主密钥 —— 密钥位置写入固定占位串 'REVOKED_KEY'。
--    重新植入真实密钥属于把缺陷 #2 装回去，任何情况下都不做。
--    因此回滚后的行对 pgcrypto 无意义，仅供排查历史数据形态时比对。
--
-- 判据：只回滚"看起来像明文 JSON"的行（以 { 或 [ 开头），
--       不碰 A-05 之后写入的 aesg1_ 密文行。
-- 实践中不建议执行本回滚；正确的前滚路径是
-- scripts/recrypt_user_collect.escript --apply 把明文升级成 aesg1_ 密文。

UPDATE public.user_collect
SET info = 'encode(encrypt(''' ||
           replace(encode(convert_to(info, 'UTF8'), 'base64'), chr(10), '') ||
           ''', ''REVOKED_KEY'', ''aes-cbc/pad:pkcs''), ''base64'')'
WHERE info <> ''
  AND info NOT LIKE 'aesg1\_%'
  AND info NOT LIKE 'encode(encrypt(''%'
  AND (substr(info, 1, 1) = '{' OR substr(info, 1, 1) = '[');
