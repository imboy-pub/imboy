-- 00000053_user_collect_info_recrypt.up.sql
-- A-06：清洗 user_collect.info 中泄漏主密钥的历史脏数据（审计缺陷 #2）
--
-- 背景：
--   A-05 之前 elib_hasher:encoded_val/1 拼的是一段含真实 postgre_aes_key 的
--   pgcrypto SQL 表达式字符串。elib_pg_sql:unzip_map/1 只把 {raw, Sql} 元组拼进
--   SQL、普通 binary 一律走绑定参数，因此该字符串被当**字面值**写进了本列：
--     encode(encrypt('<base64(明文)>', '<主密钥>', 'aes-cbc/pad:pkcs'), 'base64')
--   双重后果：任意用户收藏一次 → SELECT info 即得全站主密钥；且加密从未生效。
--
-- 本迁移做什么：
--   把上述字面值就地还原成明文 JSON（内层 base64 解码），一条 UPDATE 抹掉密钥。
--   这是安全兜底 —— 随发布自动执行，保证密钥无论如何都不再留在数据列里。
--
-- 本迁移不做什么：
--   不负责把明文升级成新格式密文（SQL 做不了 AES-256-GCM）。该步由
--   scripts/recrypt_user_collect.escript 完成，支持 --dry-run，可在本迁移前后
--   任意顺序执行（两者都幂等）。读取侧 elib_hasher:decoded_val/1 三种形态全兼容。
--
-- 幂等性：WHERE 只命中 encode(encrypt(' 前缀行，重复执行为空操作。
-- 回滚：见 00000053_user_collect_info_recrypt.down.sql（明文可还原，密钥不还原）。

UPDATE public.user_collect
SET info = convert_from(
        decode(split_part(substr(info, 17), '''', 1), 'base64'),
        'UTF8'
    )
WHERE info LIKE 'encode(encrypt(''%'
  AND split_part(substr(info, 17), '''', 1) <> ''
  AND split_part(substr(info, 17), '''', 1) ~ '^[A-Za-z0-9+/]+={0,2}$';
