# C11 多设备路径复核

状态：SOURCE_TRACE_ONLY；product_pass=false；未使用网络或真机。

客户端 chat_network_service.dart 会构造对端设备及发送者其他设备的密文副本。后端普通 C2C 实时路径在 msg_c2c_logic.erl:422 调用 encode_and_send(ToId, ...)，辅助模块原样传递目标账号；message_ds:send_next_loop/6 根据 ToUid 查找在线成员。该调用链不能证明发送者其他设备收到回显。

这不是对整个后端不存在其他同步路径的结论。下一步必须追踪独立同步及离线恢复路径，并用行为测试分别观察收件账号与发送者其他设备的投递。若没有独立路径，应修复生产投递，再扩展验收组件；不能仅靠放宽组件校验声称完成。

注册绑定组件目前要求 devices.length == 1，也不能覆盖生产多设备信封。完整目标集合、显式 skipped_devices、回显接收及 ACK 归属仍需实现和验证。97 个原验收项保持未关闭。

源码版本与 SHA256 见同目录 c11-multidevice-source-trace.json。
