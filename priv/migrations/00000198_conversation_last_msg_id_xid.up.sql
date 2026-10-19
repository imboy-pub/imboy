-- 00000198: conversation.last_msg_id 列类型对齐实际语义（TSID bigint → Xid varchar）
-- 背景: 该列自建表起无任何写入点（repo/触发器均无），恒为 DEFAULT 0，是死列；
--       列注释「最后消息ID (TSID bigint)」是 TSID 时代的遗留误导。
--       实际会话数据由 conversation_logic:list 从 msg_c2c/msg_c2g 实时聚合，
--       聚合返回的 last_msg_id 是客户端 Xid 字符串（msg_id varchar(40)）。
--       前端 ConversationModel.lastMsgId 旧声明为 int，parseModelInt 把 Xid
--       静默归零，BUG#119 判定（lastMsgId > 0）恒失效——本迁移与前端
--       lastMsgId int→String 修复同批落地，统一契约为 Xid 字符串。
-- 决策: 列类型对齐 msg_c2c.msg_id（varchar(40)）；历史值恒 0 归一为 NULL
--       （语义「无最后消息」）；DROP DEFAULT 0（0 对 varchar 无意义）。
--       列仍为死列（无写入点），本迁移只消除 schema 与语义的类型冲突，
--       为将来接线扫清障碍，不引入新读写路径。
ALTER TABLE public.conversation
    ALTER COLUMN last_msg_id TYPE character varying(40)
        USING CASE WHEN last_msg_id = 0 THEN NULL ELSE last_msg_id::text END,
    ALTER COLUMN last_msg_id DROP DEFAULT;

COMMENT ON COLUMN public.conversation.last_msg_id IS
    '最后消息ID (Xid 字符串，对应 msg_c2c/msg_c2g.msg_id；当前无写入点，保留列)';
