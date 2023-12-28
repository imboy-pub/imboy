PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
-- 联系人
CREATE TABLE IF NOT EXISTS contact (
        auto_id INTEGER,
        user_id varchar(40) NOT NULL,
        peer_id varchar(40) NOT NULL,
        nickname varchar(40) NOT NULL DEFAULT '',
        avatar varchar(255) NOT NULL DEFAULT '',
        gender int(4) NOT NULL DEFAULT 0,
        account varchar(40) NOT NULL DEFAULT '',
        status varchar(20) NOT NULL DEFAULT '',
        remark varchar(255) DEFAULT '',
        tag varchar(1600) DEFAULT '',
        region varchar(80) DEFAULT '',
        sign varchar(255) NOT NULL DEFAULT '',
        source varchar(40) NOT NULL DEFAULT '',
        update_at int(16) NOT NULL DEFAULT 0,
        is_friend int(4) NOT NULL DEFAULT 0,
        is_from int(4) NOT NULL DEFAULT 0,
        category_id int(20) NOT NULL DEFAULT 0,
        PRIMARY KEY("auto_id"),
        CONSTRAINT uk_FromTo UNIQUE (
            user_id,
            peer_id
        )
        );
-- 会话
CREATE TABLE IF NOT EXISTS conversation (
        `id` INTEGER,
        `user_id` varchar(40) NOT NULL,
        `peer_id` varchar(40) NOT NULL,
        `avatar` varchar(255) NOT NULL DEFAULT '',
        `title` varchar(40) NOT NULL DEFAULT '',
        `subtitle` varchar(255) DEFAULT '',
        `region` varchar(255) DEFAULT '',
        `sign` varchar(255) DEFAULT '',
        `unread_num` int NOT NULL DEFAULT 0,
        `type` varchar(40) NOT NULL,
        `msg_type` varchar(40) NOT NULL,
        `is_show` int NOT NULL DEFAULT 0,
        `last_time` int DEFAULT 0,
        `last_msg_id` varchar(40) NOT NULL,
        `last_msg_status` int DEFAULT 0,
        `payload` TEXT,
        PRIMARY KEY(id),
        CONSTRAINT uk_FromTo UNIQUE (
            user_id,
            peer_id
        )
        );
-- 消息
CREATE TABLE IF NOT EXISTS message (
        auto_id INTEGER,
        id varchar(40) NOT NULL,
        type VARCHAR (20),
        from_id VARCHAR (80),
        to_id VARCHAR (80),
        payload TEXT,
        created_at INTERGER,
        server_ts INTERGER,
        conversation_id int DEFAULT 0,
        status INTERGER,
        PRIMARY KEY(auto_id),
        CONSTRAINT uk_MsgId UNIQUE (
            id
        )
        );
-- 新朋友
CREATE TABLE IF NOT EXISTS new_friend (
        auto_id INTEGER,
        uid varchar(40) NOT NULL,
        from_id varchar(40) NOT NULL,
        to_id varchar(40) NOT NULL,
        nickname varchar(40) NOT NULL DEFAULT '',
        avatar varchar(255) NOT NULL DEFAULT '',
        msg varchar(255) NOT NULL DEFAULT '',
        status varchar(20) NOT NULL DEFAULT '',
        payload text DEFAULT '',
        update_at int(16) NOT NULL DEFAULT 0,
        create_at int(16) NOT NULL DEFAULT 0,
        PRIMARY KEY("auto_id"),
        CONSTRAINT uk_FromTo UNIQUE (
            from_id,
            to_id
        )
        );
-- 用户禁用名单
CREATE TABLE IF NOT EXISTS user_denylist (
        auto_id INTEGER,
        user_id varchar(40) NOT NULL,
        denied_user_id varchar(40) NOT NULL,
        nickname varchar(40) NOT NULL DEFAULT '',
        avatar varchar(255) NOT NULL DEFAULT '',
        gender int(4) NOT NULL DEFAULT 0,
        account varchar(40) NOT NULL DEFAULT '',
        region varchar(80) DEFAULT '',
        sign varchar(255) NOT NULL DEFAULT '',
        source varchar(40) NOT NULL DEFAULT '',
        remark varchar(255) DEFAULT '',
        created_at int(16) NOT NULL DEFAULT 0,
        PRIMARY KEY("auto_id"),
        CONSTRAINT i_Uid_DeniedUid UNIQUE (
            user_id,
            denied_user_id
        )
        );
-- 用户设备
CREATE TABLE IF NOT EXISTS user_device (
        auto_id INTEGER,
        user_id varchar(40) NOT NULL,
        device_id varchar(80) NOT NULL DEFAULT '',
        device_name varchar(255) NOT NULL DEFAULT '',
        device_type varchar(40) NOT NULL DEFAULT '',
        last_active_at int(16) NOT NULL DEFAULT 0,
        device_vsn varchar(255) DEFAULT '',
        PRIMARY KEY("auto_id"),
        CONSTRAINT i_Uid_DeviceId UNIQUE (
            user_id,
            device_id
        )
        );
-- 用户收藏
CREATE TABLE IF NOT EXISTS user_collect (
        auto_id INTEGER,
        user_id varchar(40) NOT NULL,
        kind int(16) NOT NULL DEFAULT '',
        kind_id varchar(40) NOT NULL DEFAULT '',
        source varchar(255) NOT NULL DEFAULT '',
        remark varchar(255) NOT NULL DEFAULT '',
        tag varchar(1600) NOT NULL DEFAULT '',
        updated_at int(16) NOT NULL DEFAULT 0,
        created_at int(16) NOT NULL DEFAULT 0,
        info text DEFAULT '',
        PRIMARY KEY("auto_id"),
        CONSTRAINT i_Uid_KindId UNIQUE (
            user_id,
            kind_id
        )
        );
-- 用户标签
CREATE TABLE IF NOT EXISTS user_tag (
        auto_id INTEGER,
        user_id varchar(40) NOT NULL,
        tag_id int(16) NOT NULL DEFAULT '',
        scene int(8) NOT NULL DEFAULT '',
        name varchar(255) NOT NULL DEFAULT '',
        subtitle varchar(800) NOT NULL DEFAULT '',
        referer_time int(16) NOT NULL DEFAULT 0,
        updated_at int(16) NOT NULL DEFAULT 0,
        created_at int(16) NOT NULL DEFAULT 0,
        PRIMARY KEY("auto_id"),
        CONSTRAINT i_Uid_Scene_Name UNIQUE (
            user_id,
            scene,
            name
        )
        );

CREATE INDEX IF NOT EXISTS i_UserId_IsFriend_UpdateTime
          ON contact
          (user_id, is_friend, update_at);
CREATE INDEX IF NOT EXISTS i_UserId_CategoryId
          ON contact
          (user_id, category_id);
CREATE INDEX IF NOT EXISTS i_Nickname
          ON contact
          (nickname);
CREATE INDEX IF NOT EXISTS i_Remark
          ON contact
          (remark);
CREATE INDEX IF NOT EXISTS i_Tag
          ON contact
          (tag);
CREATE INDEX IF NOT EXISTS i_UserId_IsShow_LastTime
          ON conversation
          (user_id,is_show, last_time);
CREATE INDEX IF NOT EXISTS i_ConversationId_CreatedAt
          ON message
          (conversation_id, created_at);
CREATE INDEX IF NOT EXISTS i_FromUid
          ON message
          (from_id);
CREATE INDEX IF NOT EXISTS i_ToUid
          ON message
          (to_id);
CREATE INDEX IF NOT EXISTS i_Source
          ON user_collect
          (source);
COMMIT;
