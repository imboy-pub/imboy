# Imboy 前后端对接指南

## 概述

本文档为前端开发者提供完整的 API 对接指南，涵盖所有已实施的 IM 功能。

**基础信息**：
- Base URL: `https://api.imboy.pub/v1`
- 认证方式: JWT Token (Header: `Authorization: Bearer <token>`)
- 数据格式: JSON
- 字符编码: UTF-8
- ID 编码: HashID

---

## 一、认证相关

### 1.1 登录

```http
POST /v1/passport/signin
Content-Type: application/json

{
  "account": "user@example.com",
  "password": "encrypted_password",
  "device_id": "device_uuid"
}
```

**响应**：
```json
{
  "code": 0,
  "msg": "登录成功",
  "data": {
    "token": "jwt_token_string",
    "uid": "encoded_user_id",
    "nickname": "用户昵称",
    "avatar": "头像URL"
  }
}
```

---

## 二、单聊功能

### 2.1 发送消息（WebSocket）

**连接 WebSocket**：
```javascript
ws://api.imboy.pub/ws?token=jwt_token
```

**发送消息**：
```json
{
  "type": "C2C",
  "action": "message",
  "to": "encoded_to_uid",
  "msg_type": "text",
  "payload": {
    "content": "Hello, World!"
  }
}
```

**支持的消息类型**：
- `text` - 文本消息
- `image` - 图片消息
- `video` - 视频消息
- `audio` - 语音消息
- `file` - 文件消息
- `location` - 位置消息
- `forward` - 转发消息 ✨ 新增

### 2.2 引用回复 ✨ 新增

```json
{
  "type": "C2C",
  "action": "message",
  "to": "encoded_to_uid",
  "msg_type": "text",
  "payload": {
    "content": "回复内容",
    "reply_to": {
      "msg_id": "original_msg_id",
      "from_id": "original_from_uid"
    }
  }
}
```

### 2.3 消息转发 ✨ 新增

```http
POST /v1/msg/forward
Content-Type: application/json

{
  "msg_ids": ["msg_id_1", "msg_id_2"],
  "to": "encoded_uid_or_gid",
  "to_type": "c2c"
}
```

**to_type 值**：
- `c2c` - 转发给单聊
- `c2g` - 转发给群聊

### 2.4 表情回应 ✨ 新增

```http
POST /v1/msg/reaction/add
Content-Type: application/json

{
  "msg_id": "msg_123",
  "msg_type": "c2c",
  "emoji": "👍"
}
```

**支持的 Emoji**：
- 👍 点赞
- 👎 踩
- ❤️ 爱心
- 😄 微笑
- 🎉 庆祝
- 😢 难过
- 😮 惊讶
- 🔥 火焰

**查询表情列表**：
```http
GET /v1/msg/reaction/list?msg_id=msg_123&msg_type=c2c
```

**响应**：
```json
{
  "code": 0,
  "data": {
    "reactions": [
      {
        "emoji": "👍",
        "count": 5,
        "users": ["uid1", "uid2", "uid3"]
      }
    ]
  }
}
```

### 2.5 消息搜索（增强版）✨ 新增

```http
GET /v1/fts/msg?keyword=hello&start_date=2026-01-01&end_date=2026-02-17&msg_type=text&from_uid=encoded_uid&page=1&size=20
```

**参数说明**：
- `keyword` - 搜索关键词（必填）
- `start_date` - 开始日期（可选）
- `end_date` - 结束日期（可选）
- `msg_type` - 消息类型（可选：text/image/video/file）
- `from_uid` - 发送者ID（可选）
- `sort_by` - 排序方式（可选：relevance/time）

---

## 三、会话管理

### 3.1 会话列表

```http
GET /v1/conversation/mine?page=1&size=20
```

### 3.2 会话置顶 ✨ 新增

```http
POST /v1/conversation/pin
Content-Type: application/json

{
  "conversation_id": "encoded_uid_or_gid",
  "type": "c2c"
}
```

**type 值**：
- `c2c` - 单聊会话
- `c2g` - 群聊会话

### 3.3 取消置顶

```http
POST /v1/conversation/unpin
Content-Type: application/json

{
  "conversation_id": "encoded_uid_or_gid",
  "type": "c2c"
}
```

### 3.4 会话删除 ✨ 新增

```http
POST /v1/conversation/delete
Content-Type: application/json

{
  "conversation_id": "encoded_uid_or_gid",
  "type": "c2c"
}
```

### 3.5 恢复会话

```http
POST /v1/conversation/restore
Content-Type: application/json

{
  "conversation_id": "encoded_uid_or_gid",
  "type": "c2c"
}
```

---

## 四、群组管理

### 4.1 创建群组

```http
POST /v1/group/add
Content-Type: application/json

{
  "title": "群组名称",
  "avatar": "头像URL",
  "introduction": "群组简介",
  "member_ids": ["uid1", "uid2", "uid3"]
}
```

### 4.2 群公告管理 ✨ 新增

**发布公告**：
```http
POST /v1/group/notice/add
Content-Type: application/json

{
  "gid": "encoded_gid",
  "title": "公告标题",
  "body": "公告内容"
}
```

**查询公告列表**：
```http
GET /v1/group/notice/list?gid=encoded_gid&page=1&size=20
```

**置顶公告**：
```http
POST /v1/group/notice/pin
Content-Type: application/json

{
  "notice_id": "notice_123"
}
```

**删除公告**：
```http
POST /v1/group/notice/delete
Content-Type: application/json

{
  "notice_id": "notice_123"
}
```

### 4.3 群文件管理 ✨ 新增

**上传文件**：
```http
POST /v1/group/file/upload
Content-Type: multipart/form-data

gid=encoded_gid
file=<binary>
```

**响应**：
```json
{
  "code": 0,
  "data": {
    "file_id": "file_123",
    "file_name": "document.pdf",
    "file_size": 1048576,
    "file_type": "application/pdf",
    "file_category": "document",
    "file_url": "http://oss.example.com/files/xxx"
  }
}
```

**查询文件列表**：
```http
GET /v1/group/file/list?gid=encoded_gid&page=1&size=20&category=document
```

**category 值**：
- `document` - 文档
- `image` - 图片
- `video` - 视频
- `audio` - 音频
- `other` - 其他

**搜索文件**：
```http
GET /v1/group/file/search?gid=encoded_gid&keyword=report&page=1&size=20
```

### 4.4 群相册管理 ✨ 新增

**创建相册**：
```http
POST /v1/group/album/create
Content-Type: application/json

{
  "gid": "encoded_gid",
  "album_name": "团队活动"
}
```

**上传图片**：
```http
POST /v1/group/album/photo/upload
Content-Type: multipart/form-data

gid=encoded_gid
album_id=album_123
photo=<binary>
```

**查询图片列表**：
```http
GET /v1/group/album/photo/list?album_id=album_123&page=1&size=20
```

**点赞图片**：
```http
POST /v1/group/album/photo/like
Content-Type: application/json

{
  "photo_id": "photo_123"
}
```

**评论图片**：
```http
POST /v1/group/album/photo/comment
Content-Type: application/json

{
  "photo_id": "photo_123",
  "content": "这张照片拍得真不错！"
}
```

### 4.5 @提及功能 ✨ 新增

**发送带@的消息**：
```json
{
  "type": "C2G",
  "action": "message",
  "to": "encoded_gid",
  "msg_type": "text",
  "payload": {
    "content": "@用户A 你好！",
    "mentions": ["encoded_uid_1", "encoded_uid_2"]
  }
}
```

**查询@我的消息**：
```http
GET /v1/mention/list?page=1&size=20
```

**查询未读@消息**：
```http
GET /v1/mention/unread
```

**群成员建议列表**（用于@）：
```http
GET /v1/group/members/suggest?gid=encoded_gid&keyword=张
```

### 4.6 群分组功能 ✨ 新增

**创建分组**：
```http
POST /v1/group/category/create
Content-Type: application/json

{
  "category_name": "工作群"
}
```

**查询分组列表**：
```http
GET /v1/group/category/list
```

**移动群到分组**：
```http
POST /v1/group/category/move
Content-Type: application/json

{
  "gid": "encoded_gid",
  "category_id": 123
}
```

### 4.7 群标签功能 ✨ 新增

**添加标签**：
```http
POST /v1/group/tag/add
Content-Type: application/json

{
  "gid": "encoded_gid",
  "tag_name": "技术讨论"
}
```

**查询群的标签**：
```http
GET /v1/group/tag/list?gid=encoded_gid
```

**按标签搜索群**：
```http
GET /v1/group/tag/search?tag_name=技术讨论&page=1&size=20
```

**热门标签**：
```http
GET /v1/group/tag/hot?limit=10
```

---

## 五、WebSocket 消息格式

### 5.1 连接认证

```javascript
// 连接 WebSocket
const ws = new WebSocket('ws://api.imboy.pub/ws?token=jwt_token');

// 连接成功
ws.onopen = () => {
  console.log('WebSocket 已连接');
};

// 接收消息
ws.onmessage = (event) => {
  const data = JSON.parse(event.data);
  // 处理不同类型的消息
};
```

### 5.2 消息类型

#### 单聊消息
```json
{
  "type": "C2C",
  "action": "message",
  "id": "msg_123",
  "from": "encoded_from_uid",
  "to": "encoded_to_uid",
  "msg_type": "text",
  "payload": {
    "content": "消息内容"
  },
  "created_at": "2026-02-17T12:00:00Z",
  "server_ts": 1736141700000
}
```

#### 群聊消息
```json
{
  "type": "C2G",
  "action": "message",
  "id": "msg_123",
  "from": "encoded_from_uid",
  "to": "encoded_gid",
  "msg_type": "text",
  "payload": {
    "content": "@用户A 你好！",
    "mentions": ["encoded_uid_1"]
  },
  "created_at": "2026-02-17T12:00:00Z"
}
```

#### 表情回应通知
```json
{
  "type": "C2C",
  "action": "message_reaction",
  "from": "encoded_from_uid",
  "to": "encoded_to_uid",
  "payload": {
    "msg_id": "msg_123",
    "emoji": "👍",
    "action": "add"
  },
  "server_ts": 1736141700000
}
```

---

## 六、错误码说明

### 通用错误码
- `0` - 成功
- `1` - 未知错误
- `2` - 参数错误
- `3` - 未授权
- `4` - 禁止访问
- `5` - 资源不存在
- `6` - 已存在

### 业务错误码
- `100-199` - 用户相关
- `200-299` - 好友相关
- `300-399` - 群组相关
- `400-499` - 消息相关
- `500-599` - 会话相关
- `952-956` - 群文件相关
- `960-968` - 群相册相关

---

## 七、前端集成示例

### 7.1 Flutter 示例

```dart
import 'package:web_socket_channel/web_socket_channel.dart';

class ImboyService {
  final WebSocketChannel channel;
  
  ImboyService(String token) 
    : channel = WebSocketChannel.connect(
        Uri.parse('ws://api.imboy.pub/ws?token=$token')
      );
  
  // 发送消息
  void sendMessage(String to, String content) {
    final message = {
      'type': 'C2C',
      'action': 'message',
      'to': to,
      'msg_type': 'text',
      'payload': {'content': content}
    };
    channel.sink.add(jsonEncode(message));
  }
  
  // 添加表情回应
  void addReaction(String msgId, String emoji) async {
    final response = await http.post(
      Uri.parse('https://api.imboy.pub/v1/msg/reaction/add'),
      headers: {'Authorization': 'Bearer $token'},
      body: jsonEncode({
        'msg_id': msgId,
        'msg_type': 'c2c',
        'emoji': emoji
      })
    );
  }
  
  // 监听消息
  Stream<dynamic> get messages => channel.stream;
}
```

### 7.2 React 示例

```javascript
import React, { useEffect, useState } from 'react';

function ChatComponent({ token }) {
  const [messages, setMessages] = useState([]);
  const [ws, setWs] = useState(null);

  useEffect(() => {
    const websocket = new WebSocket(`ws://api.imboy.pub/ws?token=${token}`);
    
    websocket.onmessage = (event) => {
      const data = JSON.parse(event.data);
      setMessages(prev => [...prev, data]);
    };
    
    setWs(websocket);
    
    return () => websocket.close();
  }, [token]);

  const sendMessage = (to, content) => {
    ws.send(JSON.stringify({
      type: 'C2C',
      action: 'message',
      to: to,
      msg_type: 'text',
      payload: { content }
    }));
  };

  const addReaction = async (msgId, emoji) => {
    await fetch('https://api.imboy.pub/v1/msg/reaction/add', {
      method: 'POST',
      headers: {
        'Authorization': `Bearer ${token}`,
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({
        msg_id: msgId,
        msg_type: 'c2c',
        emoji: emoji
      })
    });
  };

  return (
    <div>
      {/* 消息列表 */}
      {messages.map(msg => (
        <div key={msg.id}>
          <p>{msg.payload.content}</p>
          <button onClick={() => addReaction(msg.id, '👍')}>👍</button>
        </div>
      ))}
    </div>
  );
}
```

---

## 八、性能优化建议

### 8.1 连接管理
- 使用 WebSocket 心跳保持连接
- 实现断线重连机制
- 合理管理连接池

### 8.2 消息缓存
- 本地缓存历史消息
- 使用分页加载
- 实现增量同步

### 8.3 图片优化
- 使用缩略图显示列表
- 懒加载原图
- 实现图片预加载

### 8.4 搜索优化
- 防抖处理搜索输入
- 缓存搜索结果
- 实现本地搜索

---

## 九、测试建议

### 9.1 功能测试
- 测试所有 API 端点
- 验证参数校验
- 测试错误处理

### 9.2 性能测试
- 测试大量消息加载
- 测试文件上传下载
- 测试并发连接

### 9.3 兼容性测试
- 测试不同浏览器
- 测试移动设备
- 测试网络切换

---

**文档版本**: v1.1
**最后更新**: 2026-02-17
**维护者**: Imboy 开发团队

---

## 十、群分组管理

### 10.1 创建分组
```http
POST /v1/group/category/create
Content-Type: application/json

{
  "name": "工作群"
}
```

**响应**:
```json
{
  "code": 0,
  "data": {
    "category_id": 1
  }
}
```

### 10.2 获取分组列表
```http
GET /v1/group/category/list
```

### 10.3 重命名分组
```http
POST /v1/group/category/rename
Content-Type: application/json

{
  "category_id": 1,
  "name": "新名称"
}
```

### 10.4 删除分组
```http
POST /v1/group/category/delete
Content-Type: application/json

{
  "category_id": 1
}
```

### 10.5 移动群组到分类
```http
POST /v1/group/category/move_group
Content-Type: application/json

{
  "group_id": "encoded_group_id",
  "category_id": 1
}
```

### 10.6 排序分组
```http
POST /v1/group/category/sort
Content-Type: application/json

{
  "sort_orders": [
    {"category_id": 1, "sort_order": 0},
    {"category_id": 2, "sort_order": 1}
  ]
}
```

---

## 十一、群标签管理

### 11.1 添加标签
```http
POST /v1/group/tag/add
Content-Type: application/json

{
  "group_id": "encoded_group_id",
  "tag_name": "重要"
}
```

### 11.2 移除标签
```http
POST /v1/group/tag/remove
Content-Type: application/json

{
  "group_id": "encoded_group_id",
  "tag_id": 1
}
```

### 11.3 获取群标签列表
```http
GET /v1/group/tag/list?group_id=<encoded_group_id>
```

### 11.4 搜索标签
```http
GET /v1/group/tag/search?keyword=重
```

### 11.5 热门标签
```http
GET /v1/group/tag/hot?limit=10
```

---

## 十二、群投票管理

### 12.1 创建投票
```http
POST /v1/group/vote/create
Content-Type: application/json

{
  "group_id": "encoded_group_id",
  "title": "周末去哪里聚餐？",
  "description": "请大家投票选择",
  "options": [
    {"option_text": "火锅", "sort_order": 1},
    {"option_text": "烧烤", "sort_order": 2}
  ],
  "vote_type": 1,
  "is_anonymous": false,
  "end_at": "2026-02-20T18:00:00Z"
}
```

| 参数 | 类型 | 说明 |
|------|------|------|
| vote_type | int | 1=单选, 2=多选 |
| is_anonymous | bool | 是否匿名投票 |

### 12.2 获取投票列表
```http
GET /v1/group/vote/list?group_id=<encoded_group_id>&page=1&size=20
```

### 12.3 获取投票详情
```http
GET /v1/group/vote/detail?vote_id=vote_xxx
```

### 12.4 投票
```http
POST /v1/group/vote/cast
Content-Type: application/json

{
  "vote_id": "vote_xxx",
  "option_ids": ["opt_1"]
}
```

### 12.5 修改投票
```http
POST /v1/group/vote/update
Content-Type: application/json

{
  "vote_id": "vote_xxx",
  "option_ids": ["opt_2"]
}
```

### 12.6 取消投票
```http
POST /v1/group/vote/cancel
Content-Type: application/json

{
  "vote_id": "vote_xxx"
}
```

### 12.7 结束投票（创建者）
```http
POST /v1/group/vote/close
Content-Type: application/json

{
  "vote_id": "vote_xxx"
}
```

### 12.8 获取我的投票
```http
GET /v1/group/vote/my_vote?vote_id=vote_xxx
```

---

## 十三、群日程管理

### 13.1 创建日程
```http
POST /v1/group_schedule/create
Content-Type: application/json

{
  "group_id": "encoded_group_id",
  "title": "项目评审会议",
  "description": "讨论第一阶段成果",
  "location": "会议室A",
  "start_time": "2026-02-20T14:00:00Z",
  "end_time": "2026-02-20T16:00:00Z",
  "remind_type": 1,
  "participants": [101, 102]
}
```

| remind_type | 说明 |
|-------------|------|
| 0 | 不提醒 |
| 1 | 开始前15分钟 |
| 2 | 开始前30分钟 |
| 3 | 开始前1小时 |

### 13.2 更新日程
```http
POST /v1/group_schedule/update
Content-Type: application/json

{
  "schedule_id": "sched_xxx",
  "title": "更新后的标题"
}
```

### 13.3 取消日程
```http
POST /v1/group_schedule/cancel
Content-Type: application/json

{
  "schedule_id": "sched_xxx"
}
```

### 13.4 获取日程详情
```http
GET /v1/group_schedule/detail?schedule_id=sched_xxx
```

### 13.5 获取群日程列表
```http
GET /v1/group_schedule/list?group_id=<encoded_group_id>&page=1&size=20
```

### 13.6 获取我的日程
```http
GET /v1/group_schedule/my_list?page=1&size=20
```

### 13.7 确认参与
```http
POST /v1/group_schedule/confirm
Content-Type: application/json

{
  "schedule_id": "sched_xxx",
  "status": 1
}
```

| status | 说明 |
|--------|------|
| 0 | 待确认 |
| 1 | 已确认参加 |
| 2 | 已拒绝 |

---

## 十四、群作业管理

### 14.1 创建作业
```http
POST /v1/group/task/create
Content-Type: application/json

{
  "group_id": "encoded_group_id",
  "title": "完成第一章练习",
  "description": "完成课本P20-P25的所有习题",
  "deadline": "2026-02-25T23:59:59Z"
}
```

### 14.2 更新作业
```http
POST /v1/group/task/update
Content-Type: application/json

{
  "task_id": 1001,
  "title": "更新后的标题"
}
```

### 14.3 分配作业
```http
POST /v1/group/task/assign
Content-Type: application/json

{
  "task_id": 1001,
  "user_ids": [101, 102]
}
```

### 14.4 提交作业
```http
POST /v1/group/task/submit
Content-Type: application/json

{
  "task_id": "task_xxx",
  "content": "作业已完成",
  "attachment": "https://example.com/my-homework.pdf"
}
```

### 14.5 批改作业
```http
POST /v1/group/task/review
Content-Type: application/json

{
  "assignment_id": 2001,
  "score": 95,
  "comment": "完成得很好！"
}
```

### 14.6 获取作业列表
```http
GET /v1/group/task/list?group_id=<encoded_group_id>&page=1&size=20
```

### 14.7 获取作业详情
```http
GET /v1/group/task/detail?task_id=1001
```

### 14.8 获取我的作业
```http
GET /v1/group/task/my?page=1&size=20
```

| status | 说明 |
|--------|------|
| 0 | 待完成 |
| 1 | 进行中 |
| 2 | 已提交 |
| 3 | 已批改 |

### 14.9 获取待批改作业
```http
GET /v1/group/task/pending?task_id=task_xxx&page=1&size=20
```

---

## 十五、@提及功能

### 15.1 获取@我的消息列表
```http
GET /mention/list?page=1&size=20
```

**响应**:
```json
{
  "code": 0,
  "data": {
    "total": 10,
    "list": [
      {
        "msg_id": "msg_xxx",
        "group_id": "encoded_group_id",
        "group_name": "技术交流群",
        "from_uid": 101,
        "from_nickname": "张三",
        "payload": "@你 请查看这个方案",
        "created_at": "2026-02-17T10:00:00Z",
        "is_read": false
      }
    ]
  }
}
```

### 15.2 获取未读@数量
```http
GET /mention/unread
```

### 15.3 标记@消息已读
```http
POST /mention/mark_read
Content-Type: application/json

{
  "msg_id": "msg_xxx"
}
```

### 15.4 获取@成员建议列表
```http
GET /mention/suggest?gid=<encoded_group_id>&keyword=张
```

### 15.5 WebSocket 发送带@的群消息
```json
{
  "id": "msg_xxx",
  "type": "C2G",
  "to": "encoded_group_id",
  "from": "encoded_uid",
  "msg_type": "text",
  "payload": "@张三 @李四 请查看",
  "mentions": [101, 102],
  "created_at": 1739788800000
}
```

---

## 十六、数据库迁移

执行以下迁移文件（按顺序）：

```bash
# 群分组
psql -d imboy -f priv/migrations/00000053_group_category.sql

# 群标签
psql -d imboy -f priv/migrations/00000053_group_tag.sql

# @提及
psql -d imboy -f priv/migrations/00000053_msg_mentions.sql

# 群投票
psql -d imboy -f priv/migrations/00000053_group_vote.sql

# 群日程
psql -d imboy -f priv/migrations/00000053_group_schedule.sql

# 群作业
psql -d imboy -f priv/migrations/00000060_group_task.sql
```

---

## 十七、联调检查清单

- [ ] 创建群分组
- [ ] 移动群组到分组
- [ ] 添加/移除群标签
- [ ] 创建群投票
- [ ] 参与投票
- [ ] 创建群日程
- [ ] 确认参与日程
- [ ] 创建群作业
- [ ] 提交作业
- [ ] 批改作业
- [ ] 发送带@的群消息
- [ ] 获取@我的消息列表

