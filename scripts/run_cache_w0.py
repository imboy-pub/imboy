#!/usr/bin/env python3
import argparse
import hashlib
import json
import os
import platform
import re
import secrets
import signal
import subprocess
import tempfile
from datetime import datetime, timezone
from pathlib import Path


def command(arguments, **options):
    return subprocess.run(arguments, text=True, capture_output=True, **options)


def measure(arguments, env, cwd):
    process = subprocess.Popen(arguments, env=env, cwd=cwd, text=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE, start_new_session=True)
    try:
        stdout, stderr = process.communicate(timeout=300)
        return process.returncode, stdout + stderr
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGTERM)
        try:
            stdout, stderr = process.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            stdout, stderr = process.communicate()
        return "TIMEOUT", stdout + stderr


# 缓存/同步机制自身文件：它们的 imboy_cache:/depcache: 调用是机制自用，
# 不构成业务 namespace 使用者（与 docs/architecture/imboy-distributed-cache-v2.md §2 模块图一致）。
INFRA_PATHS = frozenset({
    "src/lib/imboy_cache.erl",
    "src/lib/imboy_cache_sync.erl",
    "src/lib/imboy_cache_sync_apply.erl",
    "src/lib/imboy_cache_sync_proto.erl",
    "src/lib/imboy_cache_sync_recover.erl",
    "src/lib/imboy_cache_sync_snapshot.erl",
    "src/lib/imboy_cache_policy.erl",
})


def census(root):
    calls = []
    evidence = []
    pattern = re.compile(r"\b(imboy_cache|depcache)\s*:\s*(\w+)\s*\(")
    function_pattern = re.compile(r"^([a-zA-Z_][a-zA-Z0-9_]*)\s*\(")
    for path in sorted(root.rglob("*.erl")):
        relative = path.relative_to(root)
        if any(part in {".git", "deps", "_build", "_rel", ".elp"} for part in relative.parts):
            continue
        source = path.read_text()
        production = relative.parts[0] == "src"
        lines = source.splitlines()
        for number, line in enumerate(lines, 1):
            code = line.split("%", 1)[0]
            for match in pattern.finditer(code):
                calls.append(dict(path=str(relative), line=number,
                                  module=match[1], operation=match[2],
                                  function=enclosing_function(lines, number),
                                  source=code.strip(), production=production))
        if production and (pattern.search(source) or "adm_user_logic:find(" in source):
            evidence.append(dict(path=str(relative), sha256=hashlib.sha256(source.encode()).hexdigest(),
                                 source=source))
    assert pattern.search("imboy_cache:memo(Fun, Key, 60)")
    assert not pattern.search("ordinary:call(Key)")
    return calls, evidence


def enclosing_function(lines, line_number):
    """词法回溯最近的外层函数子句；仅提供清单线索，不冒充 AST 证明。
    已知词法限制：子句头与调用同行（foo() -> imboy_cache:get(k)）时归属 unknown。"""
    for number in range(line_number - 1, 0, -1):
        stripped = lines[number - 1].strip()
        if not stripped or stripped.startswith(("-", "%%", "%")):
            continue
        match = re.match(r"^([a-zA-Z_][a-zA-Z0-9_]*)\s*\(", lines[number - 1])
        if match:
            return match[1]
    return "unknown"


def registry_rows():
    database = "PostgreSQL；缓存非权威"
    rows = [
        # G-01（e001fb8e）后移除 local_cache/account_list：ID 池改由
        # imboy_account_allocator 进程内存持有，该缓存键已不存在于 src
        #（census 断言 owner 必须有缓存调用，键消失属合法结果）。
        ("is_friend2", "{is_friend2,FromUid,ToUid}", "invalidate", database, "300", True),
        ("is_friend_fields2", "{is_friend_fields2,FromUid,ToUid,Fields}", "invalidate", database, "300", True),
        ("friend_rel_dep", "{friend_rel_dep,FromUid,ToUid}", "invalidate", "依赖标签；不存业务值", "随依赖", False),
        ("check_relationship3", "{check_relationship3,FromUid,ToUid}", "invalidate", database, "300", True),
        ("is_friend", "{is_friend,FromUid,ToUid}", "invalidate", "legacy 失效标签；未发现生产写入", "无写入", True),
        ("in_denylist", "{in_denylist,Uid,DeniedUid}", "invalidate", database, "864000", True),
        ("auth_session_epoch", "{auth_session_epoch,Uid}", "invalidate", database, "60", True),
        ("adm_session_epoch", "{adm_session_epoch,AdmId}", "invalidate", database, "60", True),
        ("user_device_active", "{user_device_active,Uid,DID}", "invalidate", database, "60", True),
        ("user_device_name", "{user_device_name,2,Uid,DID}", "invalidate", database, "864000", True),
        ("group", "{group,Gid}", "invalidate", database, "3600", True),
        ("group_e2ee_mode", "{group_e2ee_mode,Gid}", "invalidate", database, "60", True),
        ("channel", "{channel,Cid}", "invalidate", "失效标签；未发现生产填充", "无写入", False),
        ("channel_subs", "{channel_subs,Cid}", "invalidate", database, "3600", True),
        ("config5", "{config5,K}", "invalidate", "配置文件/application env/PG；按子项审查", "864000", True),
        ("conv_deleted", "{conv_deleted,Uid,ConversationId,Type}", "invalidate", database, "300", True),
        ("moment_acl", "{moment_acl,PostId}", "invalidate", database, "300", True),
        ("msg_reaction", "{msg_reaction,MsgId,MsgType}", "invalidate", "仅删除标签；未发现填充", "无写入", False),
        ("user_dnd_rule", "{user_dnd_rule,Uid}", "invalidate", database, "3600", True),
        ("billing_meter", "{billing_meter,active_sub,0}", "invalidate", database, "300", True),
        ("moderation_sensitive_words", "{moderation_sensitive_words}", "invalidate", database, "60", True),
        ("cluster_node_info", "{cluster_node_info,SourceNode}", "local", "节点运行时；复制候选需字段投影", "300", False),
        ("apns_jwt", "{apns_jwt,KeyId}", "local", "本地签名凭据", "3000", True),
        ("fcm_access_token_dyn", "{fcm_access_token_dyn}", "local", "OAuth 服务", "3000", True),
        ("qianfan_access_token", "qianfan_access_token", "local", "OAuth 服务", "2591997", True),
        ("qr_login", "{qr_login,SessionToken}", "G-QR_BLOCKED", "现有缓存是权威；需 PG 原子消费", "60", True),
        ("qr_login_qr", "{qr_login_qr,QRToken}", "G-QR_BLOCKED", "现有缓存映射；须与 QR authority 绑定", "60", True),
        ("login_attempt", "{login_attempt,Identifier,Ip}", "local", "login_attempt_ds 自有 ETS；login_security_logic 读 depcache 无同源写入", "自有 ETS 窗口；无 depcache 写入", True),
        ("login_attempt_ip_limit", "{login_attempt_ip_limit,Ip}", "local", "当前缓存计数；读改写非原子", "3600", True),
        ("bind_mail", "{bind_mail,Mail,Ts}", "local", "当前缓存一次性标志；不可复制模拟领取", "86400", True),
        ("csrf", "Csrf 随机 binary（无 namespace）", "local", "当前管理员 CSRF 缓存", "默认3600", True),
        ("tag_subtitle_2", "implode(tag_subtitle_2,TagId)", "invalidate", database, "86400", True),
        ("tag_subtitle_3", "implode(tag_subtitle_3,TagId)", "invalidate", "legacy 删除标签", "无写入", True),
    ]
    for namespace in ["adm_user_permission", "adm_user_admin_permission", "adm_user_account",
                      "adm_user_sample", "adm_user_current", "adm_user_rbac", "adm_user_role_ids",
                      "adm_user_role_permission"]:
        rows.append((namespace, "{" + namespace + ",AdmId}", "invalidate", database, "7200", True))
    for namespace in ["adm_user_group_permission", "adm_user_moment_permission",
                      "adm_user_report_permission", "adm_user_feedback_permission", "adm_user_stats_permission"]:
        rows.append((namespace, "{" + namespace + ",AdmId}", "invalidate", "仅失效标签；未发现填充", "无写入", True))
    return rows


def hook_status_rows():
    """每命名空间的失效钩子追踪结论（W0 调查；不是 W1 修复验收）。
    状态语义：
    TRACED_COMMIT_AFTER_OK —— 逐调用点核验：失效位于 DB 写返回/with_tx 提交之后
        （含单语句自动提交）；回填竞态栅栏仍属 W1/W2 设计，不是已修复。
    TRACED_IN_TX_W1 —— 已证实存在事务内缓存变更，W1-03 修复对象。
    MIXED_TX_BOUNDARY_W1 —— 部分路径 COMMIT 后失效正确、部分事务内失效。
    W1_KNOWN_DEFECT —— 已证实缺陷（读改写非原子 / 填充与失效键失配 / QR 阻塞）。
    NO_FLUSH_HOOK_TTL_ONLY —— 全仓未发现失效调用，仅 TTL 兜底；W1 审查可否接受。
    LEGACY_TAG_NO_WRITER —— 未发现生产写入方，仅失效标签。
    DEP_TAG_CASCADE —— 依赖标签，随宿主失效级联，无独立值。
    LOCAL_SELF_MANAGED —— 本地自管（自有 ETS/本地生成），无 DB mutation 钩子。
    LOCAL_SELF_MANAGED_BROADCAST_W1 —— 本地自管但 legacy set 会在 dsync 下全节点广播。
    SECRET_SUBITEM_REVIEW_W1 —— 钩子存在但值含解密配置，敏感子项须逐项审查。
    """
    commit_after = "逐调用点核验：失效位于 DB 写返回/with_tx 提交之后（含单语句自动提交）；回填竞态栅栏属 W1/W2 设计"
    tag = dict(status="LEGACY_TAG_NO_WRITER", reason="未发现生产写入方，仅 flush/delete 标签")
    no_hook = dict(status="NO_FLUSH_HOOK_TTL_ONLY",
                    reason="全仓未发现失效调用，仅 TTL 兜底；W1 审查 TTL 窗口可否接受或补钩子")
    statuses = {
        "local_cache/account_list": dict(
            status="W1_KNOWN_DEFECT",
            reason="get_wait 后 set 读改写非原子（G-01 原子分配先修）；失效即写同 Key，钩子本身闭环"),
        "is_friend2": dict(status="TRACED_COMMIT_AFTER_OK",
                            reason="friend_ds 7 个调用点（:300/312/437/443/449/477/491）均为 repo 返回后 invalidate_cache；" + commit_after),
        "is_friend_fields2": dict(status="TRACED_COMMIT_AFTER_OK",
                                   reason="经 friend_rel_dep 标签随 invalidate_cache/2 级联；调用点核验同 is_friend2；" + commit_after),
        "friend_rel_dep": dict(status="DEP_TAG_CASCADE",
                                reason="依赖标签，随 invalidate_cache/2 级联 flush，无独立业务值"),
        "check_relationship3": dict(status="TRACED_COMMIT_AFTER_OK",
                                     reason="随 invalidate_cache/2 双向 flush；" + commit_after),
        "is_friend": tag,
        "in_denylist": dict(status="TRACED_COMMIT_AFTER_OK",
                             reason="user_denylist_logic add/remove（:63-67/:89-92）DS 写返回后 flush 并级联 friend_ds:invalidate_cache；" + commit_after),
        "auth_session_epoch": dict(
            status="MIXED_TX_BOUNDARY_W1",
            reason="bump/1 为 with_tx COMMIT 后 evict（正确时序）；bump_in_tx/2 事务内 evict，W1-03 移到 COMMIT 后"),
        "adm_session_epoch": dict(status="TRACED_COMMIT_AFTER_OK",
                                   reason="bump/1（:67-76）with_tx COMMIT 后 evict，无事务内变体；" + commit_after),
        "user_device_active": dict(status="TRACED_COMMIT_AFTER_OK",
                                    reason="user_device_logic delete（:107-118）DS 写返回后 flush 并 try 包裹广播；" + commit_after),
        "user_device_name": dict(status="TRACED_COMMIT_AFTER_OK",
                                  reason="change_name（:69-76）/delete（:107-109）DS 写返回后 flush；" + commit_after),
        "group": dict(
            status="TRACED_IN_TX_W1",
            reason="group_member_ds 事务内调 group_ds:join → imboy_cache:set 位于事务内（W1-03）；"
                   "且 join/leave（:264-296）为成员列表缓存读改写，与 account_list 同类非原子缺陷"),
        "group_e2ee_mode": dict(status="TRACED_COMMIT_AFTER_OK",
                                 reason="group_logic do_set_e2ee_mode（:563-568）update_by_id {ok,_} 后 flush_e2ee_mode；" + commit_after),
        "channel": tag,
        "channel_subs": dict(status="TRACED_COMMIT_AFTER_OK",
                              reason="channel_ds subscribe/unsubscribe（:203-215/:252-262）with_tx 返回后 case Result flush；"
                                     "channel_logic_subscription remove_subscriber（:270-300）同序；" + commit_after),
        "config5": dict(
            status="SECRET_SUBITEM_REVIEW_W1",
            reason="config_ds:set 写库前 flush（:135）+ 写库后再 flush（:167）双重失效；仍存在 7.1 回填竞态窗口"
                   "（提交前读→第二次 flush 后回填旧值，TTL 864000s 放大）；解密后配置值经 imboy_cache:set "
                   "在 dsync_enabled=true 时全节点广播，敏感子项逐项审查（G-04）"),
        "conv_deleted": dict(status="TRACED_COMMIT_AFTER_OK",
                              reason="delete_conversation/restore（:31-39/:44-50）repo 返回后 flush；" + commit_after),
        "moment_acl": dict(status="NO_FLUSH_HOOK_TTL_ONLY",
                            reason="全仓仅 moment_ds:377-384 memo 填充（TTL 300s），未发现任何失效调用；"
                                   "ACL 属权限相关，W1 审查 TTL 兜底可否接受或补钩子"),
        "msg_reaction": tag,
        "user_dnd_rule": dict(status="TRACED_COMMIT_AFTER_OK",
                               reason="save/delete（:52-56/:59-63）repo 返回后 delete；" + commit_after),
        "billing_meter": dict(status="NO_FLUSH_HOOK_TTL_ONLY",
                               reason="仅 billing_meter:74-79 get/set 读透填充（TTL 300s），无失效调用；订阅变更最长 5 分钟旧值"),
        "moderation_sensitive_words": dict(status="NO_FLUSH_HOOK_TTL_ONLY",
                                            reason="仅 moderation_policy:113 memo（TTL 60s），无失效调用；词表更新最长 60 秒旧值"),
        "cluster_node_info": dict(status="LOCAL_SELF_MANAGED",
                                   reason="imboy_cluster 周期 set 本节点运行时信息；无 DB mutation 钩子"),
        "apns_jwt": dict(
            status="LOCAL_SELF_MANAGED_BROADCAST_W1",
            reason="签名 JWT 经 imboy_cache:set 在 dsync_enabled=true 时全节点广播；"
                   "legacy 行为与 local 策略矩阵不一致（G-04/W1-05）"),
        "fcm_access_token_dyn": dict(status="LOCAL_SELF_MANAGED_BROADCAST_W1",
                                      reason="OAuth token 经 imboy_cache:set 全节点广播（同 apns_jwt）"),
        "qianfan_access_token": dict(
            status="LOCAL_SELF_MANAGED",
            reason="memo 填充直走 depcache:set，不经 imboy_cache:set，不广播；本地可重取"),
        "qr_login": dict(
            status="W1_KNOWN_DEFECT",
            reason="G-QR 阻塞：status get→delete 非原子消费；且会话值经 set 全节点广播"),
        "qr_login_qr": dict(status="W1_KNOWN_DEFECT",
                             reason="G-QR 阻塞：QRToken→SessionToken 映射经 set 全节点广播"),
        "login_attempt": dict(status="LOCAL_SELF_MANAGED",
                               reason="login_attempt_ds 自有 ETS 权威；depcache 侧仅 login_security_logic:160 读取，"
                                      "无同源写入。注意：login_attempt_ds:181/186 的 set 实际写入 "
                                      "?IP_RATE_LIMIT_CACHE_KEY → {login_attempt_ip_limit,Ip}（宏），"
                                      "write_sites 中的该两行为文件粒度词法候选，不构成 login_attempt 的写入"),
        "login_attempt_ip_limit": dict(
            status="W1_KNOWN_DEFECT",
            reason="get→set 读改写非原子，且每次失败登录都经 set 全节点广播（W1-05）"),
        "bind_mail": dict(status="LOCAL_SELF_MANAGED_BROADCAST_W1",
                           reason="一次性标志经 set 广播（含邮箱地址）；不可复制模拟领取"),
        "csrf": dict(status="LOCAL_SELF_MANAGED_BROADCAST_W1",
                      reason="随机 binary 键经 set 广播；动态键无法静态枚举"),
        "tag_subtitle_2": dict(
            status="W1_KNOWN_DEFECT",
            reason="失效时序本身正确（user_tag_ds:88/143 均在 with_tx 闭包结束后调 flush_subtitle）；"
                   "缺陷在填充键 implode(tag_subtitle_2,TagId) 与失效键 tag_subtitle_3 失配，"
                   "副标题变更后最长 1 天内读旧值"),
        "tag_subtitle_3": tag,
    }
    for namespace in ["adm_user_permission", "adm_user_admin_permission", "adm_user_account",
                      "adm_user_sample", "adm_user_current", "adm_user_rbac", "adm_user_role_ids",
                      "adm_user_role_permission"]:
        statuses[namespace] = dict(
            status="TRACED_COMMIT_AFTER_OK",
            reason="写入方 memo 于 adm_user_logic:find/3；失效为 adm_admin_handler flush 束（:596，"
                   "assign_roles/update_status 写返回后调用 :311-313/:817-819）与 adm_acl:111；"
                   + commit_after)
    for namespace in ["adm_user_group_permission", "adm_user_moment_permission",
                      "adm_user_report_permission", "adm_user_feedback_permission",
                      "adm_user_stats_permission"]:
        statuses[namespace] = dict(status="LEGACY_TAG_NO_WRITER",
                                    reason="仅失效标签；未发现生产填充调用")
    return statuses


def hook_sites(calls, namespace):
    sites = [dict(path=call["path"], line=call["line"], function=call.get("function", "unknown"),
                  operation=call["operation"])
             for call in calls
             if call["path"] not in INFRA_PATHS
             and call["operation"] in {"flush", "delete", "broadcast"}
             and namespace in call.get("registry_candidates", [])]
    return sorted(sites, key=lambda item: (item["path"], item["line"]))


def write_sites(calls, namespace):
    sites = [dict(path=call["path"], line=call["line"], function=call.get("function", "unknown"),
                  operation=call["operation"])
             for call in calls
             if call["path"] not in INFRA_PATHS
             and call["operation"] in {"set", "memo", "get_wait"}
             and namespace in call.get("registry_candidates", [])]
    return sorted(sites, key=lambda item: (item["path"], item["line"]))


def registry(evidence, calls):
    result = []
    statuses = hook_status_rows()
    for namespace, key, policy, authority, ttl, sensitive in registry_rows():
        term = namespace.split("/")[-1]
        locations = [dict(path=item["path"], line=number, source=line.strip())
                     for item in evidence for number, line in enumerate(item["source"].splitlines(), 1)
                     if term.lower() in line.lower() and not line.lstrip().startswith("%")]
        owner = sorted({item["path"] for item in locations})
        call_candidates = [call for call in calls if call["path"] in owner]
        forwarded = any("adm_user_logic:find(" in item["source"]
                        for item in evidence if item["path"] in owner)
        assert call_candidates or forwarded, \
            f"{namespace} 的 owner 无任何缓存调用或 find/3 转发"
        hook = statuses[namespace]
        result.append(dict(namespace=namespace, key_pattern=key, policy=policy,
                           authority=authority, ttl_sec=ttl, sensitive=sensitive,
                           owner=owner, evidence=locations,
                           max_value_bytes="尚待业务样本冻结；复制默认拒绝", evictable=policy != "G-QR_BLOCKED",
                           mutation_hook=hook["status"], mutation_hook_reason=hook["reason"],
                           recovery="local 重建；invalidate 冷启动并重新读权威；QR 禁止自动迁移",
                           failure_policy="安全决策不得默认允许；读权威失败拒绝，普通读按原接口降级",
                           replication_allowed=False))
    assert len({item["namespace"] for item in result}) == len(result)
    assert not any(item["replication_allowed"] for item in result)
    assert all(item["mutation_hook"] for item in result)
    return result


def store_inventory(root):
    pattern = re.compile(r"\b(ets|persistent_term|ack_retry_cache)\s*:\s*(\w+)\s*\(")
    stores = []
    for path in sorted((root / "src").rglob("*.erl")):
        operations = [dict(line=number, module=match[1], operation=match[2], source=line.strip())
                      for number, line in enumerate(path.read_text().splitlines(), 1)
                      for match in pattern.finditer(line.split("%", 1)[0])]
        if operations:
            stores.append(dict(path=str(path.relative_to(root)), calls=operations,
                               disposition="显式排除通用 depcache 迁移；节点本地 store/registry，禁止全值复制"))
    stores.append(dict(path="src/ds/push_notification_ds.erl", key="{gun_conn,Host}",
                       disposition="进程字典连接句柄；禁止复制 PID，非 depcache"))
    return stores


def unresolved_forms(root, calls):
    """动态/不可静态解析形态清单：扫描器不静默遗漏，逐项显式记录。"""
    forms = []
    find_callers = []
    find_pattern = re.compile(r"adm_user_logic\s*:\s*find\s*\(")
    for path in sorted((root / "src").rglob("*.erl")):
        for number, line in enumerate(path.read_text().splitlines(), 1):
            if find_pattern.search(line.split("%", 1)[0]):
                find_callers.append(dict(path=str(path.relative_to(root)), line=number,
                                         source=line.strip()))
    forms.append(dict(
        form="forwarded_key",
        detail="adm_user_logic:find/3 第三参由调用方构造缓存 Key，键形态随调用方变化",
        callers=find_callers,
        disposition="按调用方 namespace 记录；禁止依赖键名猜策略，迁移时须显式 contract"))
    forms.append(dict(
        form="derived_string_key",
        detail="elib_cnv:implode 派生字符串键（tag_subtitle_2/3）；填充键与失效键失配已登记 W1",
        disposition="字符串键无 namespace 前缀；迁移时改结构化键或登记 legacy 例外"))
    forms.append(dict(
        form="random_binary_key",
        detail="adm_passport_handler Csrf 键为运行时随机 binary，无静态前缀",
        disposition="local 策略；随机键只存在/失效于本节点，禁止依赖键名匹配"))
    forms.append(dict(
        form="runtime_keys_via_syn",
        detail="imboy_cache_sync 接收的 Key 来自 syn 消息，任意运行时形态；接收端无格式/大小/策略校验",
        disposition="legacy 协议现状记录；V2 接收端校验属 W3，不在本轮实施"))
    broadcast_namespaces = sorted({namespace for call in calls
                                   if call["module"] == "imboy_cache" and call["operation"] == "set"
                                   for namespace in call.get("registry_candidates", [])
                                   if namespace != "local_cache/account_list"})
    forms.append(dict(
        form="legacy_set_broadcast",
        detail="dsync_enabled=true 时 imboy_cache:set 对一切非 {local_cache,_} 键全节点广播完整值",
        broadcast_namespaces=broadcast_namespaces,
        attribution_note="namespace 归属为词法文件粒度：login_attempt_ds 内 set 的真实键为 "
                         "{login_attempt_ip_limit,Ip}（宏展开），login_attempt 仅因同文件共享而出现；"
                         "其余条目待 W1 数据流核验逐项确认",
        disposition="legacy 行为调查记录（G-04/W1-05 依据）；包含 apns_jwt/fcm/qr/csrf/bind_mail/"
                    "config5/login_attempt_ip_limit 等敏感或噪声广播，策略矩阵为迁移目标而非现状"))
    memo_one = [dict(path=call["path"], line=call["line"])
                for call in calls if call["operation"] == "memo" and call["module"] == "imboy_cache"
                and call["path"] != "src/lib/imboy_cache.erl"]
    forms.append(dict(
        form="anonymous_memo_mfa",
        detail="imboy_cache:memo/1 与 {M,F,A} 形态的键由 depcache memo_key 从函数派生，词法扫描不可枚举",
        production_memo_wrapper_calls=memo_one,
        disposition="生产 src 内未发现 memo/1 包装调用则清单为空；若未来引入须先登记 namespace"))
    return forms


def scan_artifacts(root, output):
    calls, evidence = census(root)
    keys = registry(evidence, calls)
    (output / "key-registry.json").write_text(json.dumps(keys, ensure_ascii=False, indent=2))
    matrix = ["# W0 Key Registry（迁移决策；不是已实现的运行时策略）", "",
              "复制白名单为空；所有未闭合 mutation hook/大小预算明确阻塞后续切换。", "",
              "| Namespace | Key | 策略 | 权威 | TTL 秒 | 敏感 | Owner |",
              "|---|---|---|---|---|---|---|"]
    matrix += ["| " + " | ".join(str(item[field]).replace("|", "/") for field in
               ["namespace", "key_pattern", "policy", "authority", "ttl_sec", "sensitive", "owner"]) + " |"
               for item in keys]
    (output / "key-registry.md").write_text("\n".join(matrix) + "\n")
    for call in calls:
        call["registry_candidates"] = [item["namespace"] for item in keys if call["path"] in item["owner"]]
        if call["path"] == "src/logic/adm_user_logic.erl":
            call["registry_candidates"] = [item["namespace"] for item in keys
                                            if item["namespace"].startswith("adm_user_")]
            call["forwarded_key_contract"] = "外部 find/3 调用者传入 Key；调用者证据见 source-evidence"
        call["disposition"] = ("infrastructure" if call["path"] in INFRA_PATHS
                               else "candidate_namespaces_require_dataflow_review" if call["registry_candidates"]
                               else "non_production" if not call["production"] else "EXCEPTION_UNRESOLVED")
    for item in keys:
        item["mutation_hook_sites"] = hook_sites(calls, item["namespace"])
        item["write_sites"] = write_sites(calls, item["namespace"])
    (output / "key-registry.json").write_text(json.dumps(keys, ensure_ascii=False, indent=2))
    (output / "unresolved-forms.json").write_text(
        json.dumps(unresolved_forms(root, calls), ensure_ascii=False, indent=2))
    (output / "other-stores.json").write_text(json.dumps(store_inventory(root), ensure_ascii=False, indent=2))
    (output / "calls.json").write_text(json.dumps(calls, ensure_ascii=False, indent=2))
    (output / "source-evidence.json").write_text(json.dumps(evidence, ensure_ascii=False, indent=2))
    assert not any(call["disposition"] == "EXCEPTION_UNRESOLVED" for call in calls if call["production"]), \
        "生产调用存在未归类项，须人工归类后重跑"
    return calls


def baseline_snapshot(root, operations, calls):
    baseline = dict(started_at=datetime.now(timezone.utc).isoformat(),
                    head=command(["git", "rev-parse", "HEAD"], cwd=root).stdout.strip(),
                    working_tree_status=command(["git", "status", "--porcelain=v1"], cwd=root).stdout,
                    platform=platform.platform(), cpu_count=os.cpu_count(),
                    load=os.getloadavg(), operations=operations,
                    production_calls=sum(item["production"] for item in calls),
                    census_scope="所有非 vendored Erlang 源文件；排除生成、release、ELP 镜像",
                    census_limit="词法调用清单，不等于数据流证明；动态 apply/匿名 Key 需人工审查",
                    measurement_scope="真实 depcache/syn，合成值，loopback；非生产业务吞吐",
                    dependency_apps={name: (root / "deps" / name / "ebin" / (name + ".app")).read_text()
                                     for name in ["depcache", "syn", "jsone"]})
    baseline["dependency_hashes"] = {
        str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
        for name in ["depcache", "syn", "jsone"]
        for folder, suffix in [("src", "*.erl"), ("ebin", "*.beam")]
        for path in sorted((root / "deps" / name / folder).glob(suffix))}
    baseline["script_hashes"] = {name: hashlib.sha256((root / "scripts" / name).read_bytes()).hexdigest()
                                 for name in ["run_cache_w0.py", "cache_w0_bench.erl"]}
    return baseline


def run_benchmark(root, output, operations, baseline):
    with tempfile.TemporaryDirectory(prefix="cache-w0-beam-") as build:
        compile_result = command(["erlc", "-Werror", "-o", build,
                                  str(root / "scripts/cache_w0_bench.erl")])
        (output / "compile.log").write_text(compile_result.stdout + compile_result.stderr)
        baseline["compile_exit"] = compile_result.returncode
        if compile_result.returncode == 0:
            env = dict(os.environ, CACHE_W0_OUTPUT=str(output.resolve() / "performance.json"),
                       CACHE_W0_OPERATIONS=str(operations))
            node = "cache_w0_" + secrets.token_hex(6) + "@127.0.0.1"
            paths = [build] + [str(root / "deps" / name / "ebin") for name in ["depcache", "syn", "jsone"]]
            run_exit, raw = measure(["erl", "-noshell", "-name", node, "-setcookie", secrets.token_hex(24),
                                    "-kernel", "inet_dist_use_interface", "{127,0,0,1}",
                                    "-pa", *paths, "-s", "cache_w0_bench", "run"], env, output.resolve())
            baseline["run_exit"] = run_exit
            (output / "raw.log").write_text(raw)
        baseline["status"] = "W0_BLOCKED"
        baseline["blockers"] = ["尚无实际业务峰值/绝对 SLO 授权冻结；loopback 数据不能代替目标硬件",
                                "Registry 的动态调用与失效钩子、值大小/资源预算仍需逐项关闭"]
        baseline["future_gates_not_w0_blockers"] = ["QR 原子迁移独立 G-QR；V2 故障/分区/重启恢复尚未实施"]
    return baseline


def main():
    parser = argparse.ArgumentParser(description="仅运行隔离的 W0；不启动 IMBoy/数据库")
    parser.add_argument("--operations", type=int, default=1000)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if not 1 <= args.operations <= 100000:
        parser.error("operations 必须在 1..100000")
    root = Path(__file__).resolve().parents[1]
    assert command(["git", "rev-parse", "--show-toplevel"], cwd=root).stdout.strip() == str(root)
    output = args.output or Path(tempfile.mkdtemp(prefix="imboy-cache-w0-"))
    output.mkdir(parents=True, exist_ok=True)
    if list(output.iterdir()):
        parser.error("输出目录必须为空，避免覆盖历史证据")
    calls = scan_artifacts(root, output)
    baseline = run_benchmark(root, output, args.operations, baseline_snapshot(root, args.operations, calls))
    (output / "baseline.json").write_text(json.dumps(baseline, ensure_ascii=False, indent=2))
    hashes = {path.name: hashlib.sha256(path.read_bytes()).hexdigest()
              for path in output.iterdir() if path.is_file()}
    (output / "sha256.json").write_text(json.dumps(hashes, indent=2))
    print(json.dumps(dict(output=str(output), status=baseline["status"],
                         compile_exit=baseline["compile_exit"], run_exit=baseline.get("run_exit")), ensure_ascii=False))
    return 2


if __name__ == "__main__":
    raise SystemExit(main())
