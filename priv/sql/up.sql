
ALTER TABLE `dialog_msg` CHANGE `msg_md5` `msg_id` VARCHAR(40)  CHARACTER SET utf8mb4  COLLATE utf8mb4_general_ci  NOT NULL COMMENT '消息唯一标识 uuid.v4';
ALTER TABLE `dialog_msg` DROP INDEX `uk_MsgMd5`;
ALTER TABLE `dialog_msg` ADD UNIQUE INDEX `uk_MsgId` (`msg_id`);


ALTER TABLE `group_msg` CHANGE `msg_md5` `msg_id` VARCHAR(40)  CHARACTER SET utf8mb4  COLLATE utf8mb4_general_ci  NOT NULL COMMENT '消息唯一标识 uuid.v4';

ALTER TABLE `group_msg` ADD UNIQUE INDEX `uk_MsgId` (`msg_id`);

ALTER TABLE `dialog_msg` ADD `server_ts` BIGINT(13)  UNSIGNED  NOT NULL  DEFAULT '0'  COMMENT '消息服务器接受毫秒时间戳'  AFTER `created_at`;
