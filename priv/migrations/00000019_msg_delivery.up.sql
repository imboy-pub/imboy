-- 多端按设备送达标记表（T03/P0-1，根因 R2）
-- 语义：一行 = (msg_kind, msg_id, to_uid) 的消息已被设备 to_did 确认（CLIENT_ACK）。
-- 无行 = 该设备尚未确认。离线/新注册设备天然无行，因而仍可拉取未清理的主行。
-- 主行（msg_c2c/msg_s2c）在"全部活跃设备均已确认"后才删除，同时清除本表标记。
CREATE TABLE IF NOT EXISTS msg_delivery (
    msg_kind   varchar(8)  NOT NULL,   -- 'c2c' | 's2c'
    msg_id     varchar(40) NOT NULL,   -- 对应 msg_c2c.msg_id / msg_s2c.msg_id
    to_uid     bigint      NOT NULL,   -- 接收人 user id
    to_did     varchar(40) NOT NULL,   -- 已确认的设备 id（user_device.device_id）
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE msg_delivery ADD PRIMARY KEY (msg_kind, msg_id, to_uid, to_did);
CREATE INDEX idx_msg_delivery_uid_did ON msg_delivery(to_uid, to_did);
COMMENT ON TABLE msg_delivery IS '离线消息按设备送达确认标记（行存在=该设备已ACK）';
