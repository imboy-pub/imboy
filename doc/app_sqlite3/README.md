
# APP sqlite 数据结构规划说明

* 以 db_vsn_5.sql 为发布起点
* 计划每次执行更新APP的时候，提供from_vsn,to_vsn从网络读取需要更新的相关SQL语句

cp -rf /Users/leeyi/Library/Containers/8391348C-81E1-4F0F-ACFB-266F0E868B03/Data/Documents/imboy2_jp24wa.db ~/Downloads/

sqlite3 imboy_9d4nnk_5.db
>.output db_vsn_5.sql
>.dump
