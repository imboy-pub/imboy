CREATE TABLE `config` (
  `tab` varchar(20) COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '配置选项，便于后台分类浏览',
  `key` varchar(40) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '主键',
  `value` varchar(2000) COLLATE utf8mb4_general_ci DEFAULT '',
  `title` varchar(40) COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '标题',
  `sort` bigint unsigned NOT NULL DEFAULT '20' COMMENT '排序 降序排序，大的值在前面',
  `remark` varchar(128) COLLATE utf8mb4_general_ci NOT NULL,
  `system` tinyint(1) NOT NULL DEFAULT '0' COMMENT '是否为系统配置，系统配置不可删除',
  `status` tinyint(1) NOT NULL DEFAULT '1' COMMENT '状态: -1 删除  0 禁用  1 启用',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  `updated_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '更新记录Unix时间戳毫秒单位',
  PRIMARY KEY (`key`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='系统配置';

CREATE TABLE `user` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `level_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '会员等级ID',
  `nickname` varchar(80) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '用户昵称',
  `password` varchar(800) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '',
  `account` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '会员账号',
  `mobile` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL,
  `email` varchar(80) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL,
  `region` varchar(80) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL DEFAULT '' COMMENT '地区：广东 深圳',
  `gender` tinyint unsigned NOT NULL DEFAULT '0' COMMENT '性别 1 男  2 女  3 保密',
  `experience` bigint unsigned NOT NULL DEFAULT '0' COMMENT '经验值',
  `avatar` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '头像',
  `sign` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT '' COMMENT '会员签名',
  `login_count` bigint unsigned NOT NULL DEFAULT '0' COMMENT '登陆次数',
  `last_login_ip` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '最后登陆IP',
  `last_login_at` bigint DEFAULT NULL COMMENT '最后登录UTC时间',
  `ref_user_id` bigint NOT NULL DEFAULT '0' COMMENT '推荐人ID，0表示无推荐人',
  `status` tinyint(1) NOT NULL DEFAULT '1' COMMENT '状态: -1 删除  0 禁用  1 启用',
  `created_at` bigint DEFAULT NULL COMMENT '创建记录UTC时间',
  `reg_ip` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '注册IP',
  `reg_cosv` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '客户端操作系统版本，例如： Linux 5.11.0-1018-gcp #20~20.04.2-Ubuntu SMP Fri Sep 3 01:01:37 UTC 2021 | "Windows 10 Pro" 10.0 (Build 19043)',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_email` (`email`),
  UNIQUE KEY `uk_mobile` (`mobile`),
  UNIQUE KEY `uk_account` (`account`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='会员表';

DROP TABLE IF EXISTS user_setting;
CREATE TABLE `user_setting` (
  `user_id` bigint unsigned NOT NULL COMMENT '主键 用户ID',
  `setting` json NOT NULL COMMENT '更多设置：json 数据，不同的业务不用的key( add_friend_type 加我方式： mobile 手机号; account 账号; qrcode 二维码; group 群聊; visit_card 名片)',
  `updated_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '更新记录Unix时间戳毫秒单位',
  PRIMARY KEY (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `user_device` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增长ID',
  `user_id` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '用户ID',
  `device_type` varchar(40) DEFAULT '' COMMENT '设备类型 web ios android macos windows',
  `device_id` varchar(40) DEFAULT NULL COMMENT '设备ID web设备留空',
  `device_vsn` varchar(680) DEFAULT NULL COMMENT '设备版本 {"baseOS":"HUAWEI/CLT-AL00/HWINE:8.1.0/HUAWEICLT-AL00/173(C00):user/release-keys","sdkInt":27,"release":"8.1.0","codename":"REL","incremental":"176(C00)","previewSdkInt":0,"securityPatch":"2018-10-01"}',
  `device_name` varchar(80) DEFAULT NULL COMMENT '设备名称（用户可修改的）',
  `login_count` bigint(20) unsigned NOT NULL DEFAULT '0' COMMENT '登陆次数',
  `last_login_ip` varchar(40) NOT NULL DEFAULT '' COMMENT '最后登陆IP',
  `last_login_at` bigint(20) DEFAULT NULL COMMENT '最后登录UTC时间',
  `last_active_at` bigint DEFAULT '0' COMMENT '最近活跃时间',
  `status` tinyint(1) NOT NULL DEFAULT '1' COMMENT '状态: -1 删除  0 禁用  1 启用',
  `created_at` bigint(20) DEFAULT NULL COMMENT '创建记录UTC时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_Status_UserID_DeviceID` (`status`,`user_id`,`device_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE `verification_code` (
  `id` varchar(80) COLLATE utf8mb4_general_ci NOT NULL COMMENT '唯一标示',
  `code` varchar(20) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '随机验证码',
  `validity_at` bigint(20) NOT NULL COMMENT '有效期截止时间 ',
  `created_at` bigint(20) NOT NULL COMMENT '创建记录UTC时间',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci;

CREATE TABLE `user_friend` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `from_user_id` bigint unsigned NOT NULL COMMENT '发起人',
  `to_user_id` bigint unsigned NOT NULL COMMENT '接受人',
  `status` tinyint(1) NOT NULL DEFAULT '1' COMMENT '状态: -1 删除  0 禁用  1 启用',
  `category_id` bigint DEFAULT '0' COMMENT '用户分组ID friend_category主键',
  `remark` varchar(80) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '备注名',
  `updated_at` bigint DEFAULT NULL COMMENT '记录更新时间',
  `created_at` bigint NOT NULL COMMENT '创建记录UTC时间',
  `setting` json NOT NULL COMMENT '朋友权限设置等信息',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci COMMENT='聊天朋友关系记录表（A请求B为朋友，B接受之后，系统要自动加入一条B请求A的记录并且A自动确认 user_id 是 user表的主键）';

ALTER TABLE `user_friend` ADD UNIQUE INDEX `uk_FromUID_ToUID` (`from_user_id`, `to_user_id`);
ALTER TABLE `user_friend` ADD INDEX `i_Status_FromUid_Cid` (`status`, `from_user_id`, `category_id`);


CREATE TABLE `user_friend_category` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `name` varchar(80) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '分组名称',
  `owner_user_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '分组所属用户ID',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='朋友分组表';

DROP TABLE IF EXISTS user_denylist;
CREATE TABLE `user_denylist` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `user_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '归属用户ID',
  `denied_user_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '被列入名单的用户ID',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_UserId_DeniedUserId` (`user_id`,`denied_user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='用户的拒绝聊天名单';

CREATE TABLE `tag` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `creator_user_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建人用户ID',
  `name` varchar(80) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '标签名称',
  `referer_time` int(1) NOT NULL DEFAULT '0' COMMENT '被引用次数',
  `status` tinyint(1) NOT NULL DEFAULT '1' COMMENT '状态: -1 删除  0 禁用  1 启用',
  `updated_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '更新记录Unix时间戳毫秒单位',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  PRIMARY KEY (`id`),
  KEY `i_CreatorUserId_Status` (`creator_user_id`,`status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='标签记录';

CREATE TABLE `tag_user_friend` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `user_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '记录所属用户ID',
  `tag_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '标签ID',
  `tag_user_id` bigint unsigned NOT NULL DEFAULT '0' COMMENT '被打标签用户ID',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  PRIMARY KEY (`id`),
  KEY `i_CreatorUserId_Status` (`creator_user_id`,`status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='标签朋友关联表';


DROP TABLE IF EXISTS `group`;
CREATE TABLE `group` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `type` tinyint(1) DEFAULT '0' COMMENT '类型: 1 公开群组  2 私有群组',
  `join_limit` tinyint(1) DEFAULT '0' COMMENT '加入限制: 1 不需审核  2 需要审核  3 只允许邀请加入',
  `content_limit` tinyint(1) DEFAULT '2' COMMENT '内部发布限制: 1 圈内不需审核  2 圈内需要审核  3 圈外需要审核',
  `owner_uid` bigint NOT NULL COMMENT '群组拥有者ID',
  `creater_uid` bigint NOT NULL COMMENT '群组创建者ID',
  `member_max` bigint NOT NULL DEFAULT '50' COMMENT '允许最大成员数量',
  `member_count` bigint NOT NULL DEFAULT '1' COMMENT '成员数量',
  `notification` varchar(800) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '公告',
  `introduction` varchar(2000) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '简介',
  `avatar` varchar(200) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '群组头像',
  `groupname` varchar(80) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '群组名称',
  `status` tinyint(1) NOT NULL DEFAULT '1' COMMENT '状态: -1 删除  0 禁用  1 启用',
  `updated_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '更新记录Unix时间戳毫秒单位',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='群组';

DROP TABLE IF EXISTS `group_member`;
CREATE TABLE `group_member` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `group_id` bigint NOT NULL COMMENT '群组ID',
  `user_id` bigint NOT NULL COMMENT '创建者用户ID',
  `alias` varchar(80) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '圈内别名',
  `description` varchar(800) COLLATE utf8mb4_general_ci DEFAULT '' COMMENT '成员描述',
  `role` tinyint unsigned NOT NULL DEFAULT '0' COMMENT '角色: 1 成员  2 嘉宾  3  管理员 4 群主',
  `status` tinyint(1) NOT NULL DEFAULT '0' COMMENT '状态: 1 有效  2 被踢出  3 主动退出',
  `updated_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '更新记录Unix时间戳毫秒单位',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='群组成员表（删除即表示成员被踢出群组，updated_at为被踢出的时间，同时有踢出记录）';

DROP TABLE IF EXISTS `group_member_log`;
CREATE TABLE `group_member_log` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT,
  `type` tinyint(1) DEFAULT '0' COMMENT '内容类型: 1 转让  2 被退出  3 主动退出',
  `group_member_id` bigint NOT NULL COMMENT 'group_member表ID',
  `group_id` bigint NOT NULL COMMENT '群组ID',
  `user_id` bigint NOT NULL COMMENT '群组“转让、被踢出、主动退出”用户ID',
  `option_uid` bigint NOT NULL DEFAULT '0' COMMENT '操作者用户ID（0 表示主动退出）',
  `remark` varchar(200) COLLATE utf8mb4_general_ci DEFAULT '' COMMENT '备注（踢出理由等）',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='群组成员关系变更记录（群组转让、主动退出、被踢出等等的时候记录到改表）';

CREATE TABLE `msg_c2c` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `from_id` bigint NOT NULL COMMENT '消息发送人user id',
  `to_id` bigint NOT NULL COMMENT '消息接收人user_id',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  `server_ts` bigint unsigned NOT NULL DEFAULT '0' COMMENT '消息服务器接受毫秒时间戳',
  `msg_id` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '消息唯一标识',
  `payload` text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '消息体json格式，数据结构参考文档',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_MsgId` (`msg_id`),
  KEY `i_ToId` (`to_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='单聊消息';
-- 消息只发送给接收方的最近15内登录过的设备按最近时间排序的最多10个设备ID；如果设备ID为空的话，就只给最近一次来登录的设备发离线消息
ALTER TABLE `msg_c2c` ADD `to_dids` varchar(648) NOT NULL DEFAULT '' COMMENT '接收人设备唯一ID列表: Id1,Id2' AFTER `msg_id`;

CREATE TABLE `msg_c2g` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `from_id` bigint NOT NULL COMMENT '消息发送人user id',
  `to_groupid` bigint NOT NULL COMMENT '消息接收群 group_id',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  `msg_id` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '消息唯一标识',
  `payload` text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '消息体json格式，数据结构参考文档',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_MsgId` (`msg_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='群聊消息';

CREATE TABLE `msg_c2g_timeline` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `to_groupid` bigint NOT NULL COMMENT '消息接收群 group_id',
  `to_id` bigint NOT NULL COMMENT '消息接收人user_id',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  `msg_id` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '消息唯一标识',
  PRIMARY KEY (`id`),
  KEY `i_ToId` (`to_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='群聊消息时间线';

CREATE TABLE `msg_s2c` (
  `id` bigint unsigned NOT NULL AUTO_INCREMENT COMMENT '主键',
  `from_id` bigint NOT NULL COMMENT '消息发送人user id',
  `to_id` bigint NOT NULL COMMENT '消息接收人user_id',
  `created_at` bigint unsigned NOT NULL DEFAULT '0' COMMENT '创建记录Unix时间戳毫秒单位',
  `server_ts` bigint unsigned NOT NULL DEFAULT '0' COMMENT '消息服务器接受毫秒时间戳',
  `msg_id` varchar(40) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '消息唯一标识',
  `to_dids` varchar(648) COLLATE utf8mb4_general_ci NOT NULL DEFAULT '' COMMENT '接收人设备唯一ID列表: Id1,Id2',
  `payload` text CHARACTER SET utf8mb4 COLLATE utf8mb4_bin NOT NULL COMMENT '消息体json格式，数据结构参考文档',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_MsgId` (`msg_id`),
  KEY `i_ToId` (`to_id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='服务端投递给用户的消息 添加朋友消息等其他系统消息';
