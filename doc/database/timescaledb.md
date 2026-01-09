
-- 创建超表

SELECT create_hypertable('msg_c2g', 'ts', chunk_time_interval => INTERVAL '14 day');

SELECT * FROM "_timescaledb_catalog"."dimension"
SELECT * FROM "_timescaledb_catalog"."dimension_slice"

select _timescaledb_functions.show_chunks("msg_c2g");

-- 其中chunk_time_interval表示分区时间间隔，以微秒作为单位， 86400000000 表示一天，意思是第二天的数据会自动生成新的分区，也可以将其换成 interval ‘1 day’。


-- 毫秒单位
-- 31536000000 = 365 * 86400000
-- SELECT create_hypertable('msg_c2g', 'created_at', chunk_time_interval => 31536000000);
-- SELECT create_hypertable('msg_c2g', 'created_at');



SELECT * FROM hypertable WHERE table_name = 'msg_c2g'

-- 计算过去一小时内，每分钟的消息数量
SELECT time_bucket('1 minute', ts) AS minute
, count(msg_id)
FROM public.msg_c2g
WHERE ts > NOW() - INTERVAL '1 hour'
GROUP BY minute;


```
-- 数据压缩
-- 首先需要设置允许超表进行数据压缩
ALTER TABLE stocks_real_time SET (
  timescaledb.compress,
  timescaledb.compress_orderby = 'time DESC',
  timescaledb.compress_segmentby = 'symbol'
);
-- 查看确认超表的压缩参数
SELECT * FROM timescaledb_information.compression_settings;

-- 自动压缩策略
-- 自动对两周之前的数据进行压缩，并创建循环规则
-- 被压缩的分区可以执行数据的插入，但是无法更新和删除
SELECT add_compression_policy('stocks_real_time', INTERVAL '2 weeks');
-- 查看压缩策略的细节
SELECT * FROM timescaledb_information.jobs;
-- 查看压缩策略的统计信息
SELECT * FROM timescaledb_information.job_stats;

-- 手动压缩策略
-- 对两周之前的数据进行压缩
-- 策略最好加入 if_not_compressed=>true 语句，否则如果对已经压缩过的分区进行压缩，数据库将会报错
SELECT compress_chunk(i, if_not_compressed=>true)
  FROM show_chunks('stocks_real_time', older_than => INTERVAL ' 2 weeks') i;

-- 查看压缩后数据的结果
SELECT pg_size_pretty(before_compression_total_bytes) as "before compression",
  pg_size_pretty(after_compression_total_bytes) as "after compression"
  FROM hypertable_compression_stats('stocks_real_time');

SELECT pg_size_pretty(before_compression_total_bytes) as "before compression",
  pg_size_pretty(after_compression_total_bytes) as "after compression"
  FROM hypertable_compression_stats('msg_c2g');

```


-- 保留策略
```
-- 自动保留策略
-- 自动删除 3周 之前的数据，并创建循环规则
SELECT add_retention_policy('stocks_real_time', INTERVAL '3 weeks');
-- 查询保留策略的相关细节
SELECT * FROM timescaledb_information.jobs;
-- 查询保留策略的统计信息
SELECT * FROM timescaledb_information.job_stats;


-- 手动保留策略
-- 手动删除 3周 之前的数据
SELECT drop_chunks('stocks_real_time', INTERVAL '3 weeks');
-- 手动删除 2周 之前，3周 之内的数据
SELECT drop_chunks(
  'stocks_real_time',
  older_than => INTERVAL '2 weeks',
  newer_than => INTERVAL '3 weeks'
);

-- 查询保留策略的信息
SELECT * FROM timescaledb_information.jobs;
SELECT * FROM timescaledb_information.job_stats;

SELECT remove_retention_policy('msg_c2g');
SELECT add_retention_policy('msg_c2g', INTERVAL '1 hours');

SELECT add_retention_policy('msg_c2g', INTERVAL '3 minutes');

SELECT alter_job(1005, schedule_interval => INTERVAL '3 minute');


https://docs.timescale.com/api/latest/hypertable/show_chunks/
SELECT show_chunks('msg_c2g');
-- 查询10天到20天的的块
SELECT show_chunks('msg_c2g', older_than => INTERVAL '10 days', newer_than => INTERVAL '20 days');

SELECT drop_chunks('msg_c2g', INTERVAL '3 minutes');
```

##  参考资料
* https://blog.csdn.net/zqf787351070/article/details/127498102  【基础】TimescaleDB 简单使用
* https://blog.csdn.net/yang_z_1/article/details/111560747 postgresql数据库 TimescaleDB 定时压缩超表 删除超表（块）
* http://www.pgsql.tech/article_0_10000020
* https://stackoverflow.com/questions/59480852/save-javascript-date-now-millisecond-timestamps-in-timescaledb
* 基于PostgreSQL的时序数据库TimescaleDB的基本用法和概念 https://www.jb51.net/database/290723xw7.htm#_label1
