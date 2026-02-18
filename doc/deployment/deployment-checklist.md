# Imboy 部署检查清单

## 🎯 部署前检查

### 1. 数据库迁移

#### 必须执行的迁移脚本

```bash
# Week 1-2 单聊功能
psql -U imboy -d imboy_db -f priv/migrations/00000053_msg_forward.sql
psql -U imboy -d imboy_db -f priv/migrations/00000054_msg_reply.sql
psql -U imboy -d imboy_db -f priv/migrations/00000056_conversation_pin.sql
psql -U imboy -d imboy_db -f priv/migrations/00000064_conversation_delete.sql

# Week 3-4 群聊功能
psql -U imboy -d imboy_db -f priv/migrations/00000055_group_notice_enhancement.sql
psql -U imboy -d imboy_db -f priv/migrations/00000058_group_file.sql
psql -U imboy -d imboy_db -f priv/migrations/00000059_group_album.sql

# Week 5-7 增强功能
psql -U imboy -d imboy_db -f priv/migrations/00000055_msg_reaction.sql
psql -U imboy -d imboy_db -f priv/migrations/00000053_group_category.sql
```

### 2. 编译检查

```bash
make clean
make compile
make eunit
```

### 3. 性能优化

#### 数据库索引检查

```sql
SELECT indexname, tablename 
FROM pg_indexes 
WHERE tablename IN ('msg_forward', 'conversation_pin', 'msg_reaction', 'group_file');
```

### 4. 部署后验证

#### 功能验证清单

- [ ] **单聊功能**
  - [ ] 消息转发
  - [ ] 引用回复
  - [ ] 表情回应

- [ ] **群聊功能**
  - [ ] 群公告管理
  - [ ] 群文件上传/下载
  - [ ] 群相册管理
  - [ ] @提及功能

- [ ] **会话管理**
  - [ ] 会话置顶
  - [ ] 会话删除

- [ ] **搜索功能**
  - [ ] 高级筛选

- [ ] **群组增强**
  - [ ] 群分组
  - [ ] 群标签

**最后更新**: 2026-02-17
**维护者**: Imboy 运维团队

