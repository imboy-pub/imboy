%%%
% 错误码宏定义
%
% 设计原则：
% - 0: 成功（API 响应成功标记）
% - 4xx: 客户端错误（参数、认证、资源等）
% - 5xx: 服务端错误（服务器问题）
% - 9xx: 业务特定错误（IM 业务专用）
%
% 错误码语义参考 HTTP 状态码，便于理解。
% 重要：所有中文字符串必须使用 /utf8 后缀
%%%

%% ===================================================================
%% 成功 (0)
%% ===================================================================

-define(MSG_SUCCESS, <<"success."/utf8>>).
-define(ERR_OK, 0).                      % 成功

%% ===================================================================
%% 通用错误 (1)
%% ===================================================================

%% elib_response:error/2（不带 Code 的那个重载）reply 的就是 1 —— 它是全站
%% 最常用的通用错误码，此前却从未在本文件定义（C-20 发现）。
%% 后果：错误码生成器产出的 Dart 文件缺 ERROR 常量，前端只能手工往"由脚本
%% 自动生成、请勿手动修改"的产物里补一行，于是没人再敢跑生成器 —— 生成器
%% 因此漂移了 7 个月。真源补全后手改即可撤销。
-define(ERR_ERROR, 1).                   % 通用错误（未细分）

%% ===================================================================
%% 4xx 客户端错误（参考 HTTP 4xx）
%% ===================================================================

%% 400 Bad Request - 请求参数错误
-define(ERR_BAD_REQUEST, 400).                   % 请求参数错误
-define(ERR_INVALID_PARAM, 400).                  % 参数错误
-define(ERR_INVALID_FORMAT, 400).                 % 参数格式错误
-define(ERR_PARAM_TOO_LONG, 400).                 % 参数过长
%% 401 Unauthorized - 未认证
-define(ERR_UNAUTHORIZED, 401).                   % 未认证
-define(ERR_TOKEN_MISSING, 401).                  % Token 缺失
-define(ERR_TOKEN_INVALID, 401).                  % Token 无效
-define(ERR_TOKEN_EXPIRED, 401).                  % Token 已过期
%% 402 Payment Required - 保留（付费功能）
-define(ERR_PAYMENT_REQUIRED, 402).               % 需要付费
%% 403 Forbidden - 已认证但无权限
-define(ERR_FORBIDDEN, 403).                      % 无权限访问
-define(ERR_ACCESS_DENIED, 403).                  % 拒绝访问
%% 404 Not Found - 资源不存在
-define(ERR_NOT_FOUND, 404).                      % 资源不存在
-define(ERR_USER_NOT_FOUND, 404).                 % 用户不存在
-define(ERR_FRIEND_NOT_FOUND, 404).               % 好友不存在
-define(ERR_GROUP_NOT_FOUND, 404).                % 群组不存在
-define(ERR_MESSAGE_NOT_FOUND, 404).              % 消息不存在
%% 405 Method Not Allowed - 请求方法不允许
-define(ERR_METHOD_NOT_ALLOWED, 405).             % 请求方法不允许
%% 406 Not Acceptable - 内容格式不支持
-define(ERR_NOT_ACCEPTABLE, 406).                 % 内容格式不支持
%% 408 Request Timeout - 请求超时
-define(ERR_REQUEST_TIMEOUT, 408).                % 请求超时
%% 409 Conflict - 资源冲突
-define(ERR_CONFLICT, 409).                       % 资源冲突
-define(ERR_RESOURCE_EXISTS, 409).                % 资源已存在
-define(ERR_ALREADY_FRIENDS, 409).                % 已经是好友
-define(ERR_ALREADY_IN_GROUP, 409).               % 已在群组中
%% 410 Gone - 资源已删除
-define(ERR_GONE, 410).                           % 资源已删除
%% 412 Precondition Failed - 前置条件失败
-define(ERR_PRECONDITION_FAILED, 412).            % 前置条件失败
%% 413 Payload Too Large - 请求体过大
-define(ERR_PAYLOAD_TOO_LARGE, 413).              % 请求体过大
-define(ERR_FILE_SIZE_EXCEEDED, 413).             % 文件大小超出限制
%% 415 Unsupported Media Type - 不支持的媒体类型
-define(ERR_UNSUPPORTED_MEDIA_TYPE, 415).         % 不支持的媒体类型
-define(ERR_FILE_TYPE_INVALID, 415).              % 文件类型无效
%% 422 Unprocessable Entity - 语义错误
-define(ERR_UNPROCESSABLE_ENTITY, 422).           % 请求语义错误
-define(ERR_MISSING_PARAM, 422).                  % 缺少必填参数
-define(ERR_PARAM_INVALID, 422).                  % 参数值无效
%% 423 Locked - 资源被锁定
-define(ERR_LOCKED, 423).                         % 资源被锁定
-define(ERR_ACCOUNT_LOCKED, 423).                 % 账号已锁定
-define(ERR_REVOKE_TIMEOUT, 409).                 % 消息撤回超时
%% 429 Too Many Requests - 请求过于频繁
-define(ERR_TOO_MANY_REQUESTS, 429).              % 请求过于频繁
-define(ERR_OPERATION_TOO_FREQUENT, 429).         % 操作过于频繁

%% ===================================================================
%% 5xx 服务端错误（参考 HTTP 5xx）
%% ===================================================================

%% 500 Internal Server Error - 服务器内部错误
-define(ERR_INTERNAL_SERVER_ERROR, 500).          % 服务器内部错误
-define(ERR_SERVER_ERROR, 500).                   % 服务器错误
-define(ERR_BUSINESS_FAILED, 500).                % 业务逻辑失败
-define(ERR_OPERATION_FAILED, 500).               % 操作失败
%% 501 Not Implemented - 功能未实现
-define(ERR_NOT_IMPLEMENTED, 501).                % 功能未实现
%% 502 Bad Gateway - 网关错误
-define(ERR_BAD_GATEWAY, 502).                    % 网关错误
%% 503 Service Unavailable - 服务不可用
-define(ERR_SERVICE_UNAVAILABLE, 503).            % 服务不可用
-define(ERR_NODE_OFFLINE, 503).                   % 节点离线
-define(ERR_CLUSTER_ERROR, 503).                  % 集群错误
%% 504 Gateway Timeout - 网关超时
-define(ERR_GATEWAY_TIMEOUT, 504).                % 网关超时
-define(ERR_TIMEOUT, 504).                        % 请求超时
%% 507 Insufficient Storage - 存储空间不足
-define(ERR_INSUFFICIENT_STORAGE, 507).           % 存储空间不足

%% ===================================================================
%% 9xx 业务特定错误（IM 业务专用）
%% ===================================================================

%% 认证相关（避开 HTTP 标准码）
-define(ERR_TOKEN_REFRESH_NOT_ALLOWED, 901).      % 不支持刷新 Token
-define(ERR_SIGNATURE_INVALID, 902).              % 签名验证失败
-define(ERR_CSRF_TOKEN_ERROR, 903).               % CSRF Token 错误
-define(ERR_VERIFICATION_CODE_ERROR, 904).        % 验证码错误
-define(ERR_VERIFICATION_CODE_EXPIRED, 905).      % 验证码过期
-define(ERR_PASSWORD_WRONG, 906).                 % 密码错误
-define(ERR_ACCOUNT_DISABLED, 907).               % 账号已禁用
-define(ERR_ACCOUNT_NOT_EXIST, 908).              % 账号不存在
-define(ERR_ACCOUNT_ALREADY_EXISTS, 909).         % 账号已存在
-define(ERR_LOGIN_ELSEWHERE, 910).                % 在其他设备登录
%% 首启初始化相关（911-919）
-define(ERR_SETUP_ALREADY_COMPLETED, 911).         % 系统已完成首启初始化
-define(ERR_SETUP_INVALID_PARAMS, 912).            % 首启参数无效
%% 好友相关
-define(ERR_NOT_FRIENDS, 920).                    % 不是好友
-define(ERR_FRIEND_REQUEST_PENDING, 921).         % 好友请求待确认
-define(ERR_FRIEND_REQUEST_REJECTED, 922).        % 好友请求被拒绝
-define(ERR_FRIEND_EXISTS, 923).                  % 好友关系已存在
%% 群组相关
-define(ERR_NOT_GROUP_MEMBER, 930).               % 非群组成员
-define(ERR_NOT_GROUP_ADMIN, 931).                % 非群管理员
-define(ERR_NOT_GROUP_OWNER, 932).                % 非群主
-define(ERR_GROUP_PERMISSION_DENIED, 933).        % 群组权限不足
-define(ERR_GROUP_MEMBER_FULL, 934).              % 群成员已满
-define(ERR_GROUP_CREATE_FAILED, 935).            % 创建群组失败
%% 消息相关
-define(ERR_USER_OFFLINE, 940).                   % 用户离线
-define(ERR_MSG_SEND_FAILED, 941).                % 消息发送失败
-define(ERR_MSG_NOT_FOUND, 942).                  % 消息不存在
%% 文件相关
-define(ERR_FILE_UPLOAD_FAILED, 950).             % 文件上传失败
-define(ERR_FILE_DOWNLOAD_FAILED, 951).           % 文件下载失败
-define(ERR_FILE_NOT_FOUND, 952).                 % 文件不存在
-define(ERR_FILE_TYPE_NOT_ALLOWED, 953).          % 不允许的文件类型
-define(ERR_FILE_DELETE_FAILED, 955).             % 文件删除失败
-define(ERR_FILE_STORAGE_FULL, 956).              % 群文件存储空间已满

%% ===================================================================
%% 相册相关错误（960-969）
%% ===================================================================

-define(ERR_ALBUM_NOT_FOUND, 960).                % 相册不存在
-define(ERR_ALBUM_NAME_INVALID, 961).             % 相册名称无效
-define(ERR_ALBUM_ALREADY_EXISTS, 962).           % 相册已存在
-define(ERR_PHOTO_NOT_FOUND, 963).                % 图片不存在
-define(ERR_PHOTO_UPLOAD_FAILED, 964).            % 图片上传失败
-define(ERR_PHOTO_TYPE_INVALID, 965).             % 图片类型无效
-define(ERR_PHOTO_SIZE_EXCEEDED, 966).            % 图片大小超出限制
-define(ERR_ALBUM_PERMISSION_DENIED, 967).        % 相册权限不足
-define(ERR_PHOTO_ALREADY_LIKED, 968).            % 已点赞该图片

%% ===================================================================
%% E2EE 密钥恢复相关错误（5000-5099）
%% ===================================================================

%% 设备传输相关（5000-5019）
-define(ERR_E2EE_TRANSFER_INVALID_SESSION, 5000).  % 无效的传输会话
-define(ERR_E2EE_TRANSFER_SESSION_EXPIRED, 5001).  % 传输会话已过期
-define(ERR_E2EE_TRANSFER_SESSION_NOT_FOUND, 5002). % 传输会话不存在
-define(ERR_E2EE_TRANSFER_INVALID_DEVICE, 5003).  % 无效的设备
-define(ERR_E2EE_TRANSFER_ALREADY_ACCEPTED, 5004). % 传输会话已被接受
-define(ERR_E2EE_TRANSFER_CANNOT_CONFIRM, 5005).  % 无法确认传输会话
-define(ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH, 5006). % 发送方用户 ID 不匹配
-define(ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH, 5007). % 接收方用户 ID 不匹配
-define(ERR_E2EE_TRANSFER_CONCURRENT, 5008).       % 存在并发传输
-define(ERR_E2EE_TRANSFER_ALREADY_CANCELLED, 5009). % 会话已取消
-define(ERR_E2EE_TRANSFER_STATUS_INVALID, 5010).   % 无效状态转换

%% 社交恢复相关（5020-5039）
-define(ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND, 5020). % 可信联系人不存在
-define(ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS, 5021). % 可信联系人已存在
-define(ERR_E2EE_SOCIAL_CONTACT_IS_SELF, 5022).   % 不能添加自己为可信联系人
-define(ERR_E2EE_SOCIAL_CONTACT_NOT_TRUSTED, 5023). % 该联系人不在可信列表中
-define(ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES, 5024). % 密钥分片数量不足
-define(ERR_E2EE_SOCIAL_SHARE_ALREADY_CREATED, 5025). % 密钥分片已创建
-define(ERR_E2EE_SOCIAL_SHARE_NOT_FOUND, 5026).   % 密钥分片不存在
-define(ERR_E2EE_SOCIAL_INVALID_THRESHOLD, 5027).  % 无效的恢复阈值
-define(ERR_E2EE_SOCIAL_RECOVER_FAILED, 5028).    % 密钥恢复失败
-define(ERR_E2EE_SOCIAL_TRUSTEE_LIMIT_EXCEEDED, 5029). % 受托人数量超过限制

%% 本地备份相关（5040-5049）
-define(ERR_E2EE_BACKUP_INVALID_PASSWORD, 5040).   % 备份密码错误
-define(ERR_E2EE_BACKUP_FILE_CORRUPTED, 5041).     % 备份文件已损坏
-define(ERR_E2EE_BACKUP_VERSION_MISMATCH, 5042).   % 备份版本不匹配
-define(ERR_E2EE_BACKUP_CHECKSUM_MISMATCH, 5043). % 备份校验和不匹配
-define(ERR_E2EE_BACKUP_FILE_TOO_LARGE, 5044).     % 备份文件过大
-define(ERR_E2EE_BACKUP_INVALID_FORMAT, 5045).     % 备份文件格式无效

%% 通用错误（5050-5099）
-define(ERR_E2EE_INVALID_KEY_FORMAT, 5050).        % 无效的密钥格式
-define(ERR_E2EE_KEY_DERIVATION_FAILED, 5051).     % 密钥派生失败
-define(ERR_E2EE_ENCRYPTION_FAILED, 5052).         % 加密失败
-define(ERR_E2EE_DECRYPTION_FAILED, 5053).         % 解密失败
-define(ERR_E2EE_KEY_NOT_FOUND, 5054).              % 密钥不存在
-define(ERR_E2EE_OPERATION_NOT_SUPPORTED, 5055).    % 不支持的操作
-define(ERR_E2EE_BACKUP_PASSWORD_TOO_WEAK, 5056).  % 备份密码强度不足

%% 自动恢复相关（5060-5079）
-define(ERR_E2EE_RECOVERY_NO_OPTIONS, 5060).       % 无可用恢复方式
-define(ERR_E2EE_RECOVERY_IN_PROGRESS, 5061).      % 恢复进行中
-define(ERR_E2EE_RECOVERY_FAILED, 5062).           % 恢复失败
-define(ERR_E2EE_RECOVERY_TIMEOUT, 5063).          % 恢复超时
-define(ERR_E2EE_RECOVERY_KEY_MISMATCH, 5064).     % 密钥不匹配

%% ===================================================================
%% 设备会话管理相关错误（5100-5199）
%% ===================================================================

-define(ERR_DEVICE_SESSION_INVALID, 5100).         % 设备会话无效
-define(ERR_DEVICE_SESSION_KICKED, 5101).          % 设备会话已被踢出
-define(ERR_DEVICE_TYPE_CONFLICT, 5102).           % 同类型设备已登录
-define(ERR_DEVICE_NOT_FOUND, 5103).               % 设备不存在
-define(ERR_DEVICE_SESSION_EXPIRED, 5104).         % 设备会话已过期

%% ===================================================================
%% 功能开关相关错误（5190-5199）
%% ===================================================================

-define(ERR_FEATURE_DISABLED, 5190).               % 功能未启用

%% ===================================================================
%% 群作业相关错误（5300-5399）
%% ===================================================================

-define(ERR_TASK_NOT_FOUND, 5300).                 % 作业不存在
-define(ERR_TASK_TITLE_REQUIRED, 5301).            % 作业标题必填
-define(ERR_TASK_ALREADY_SUBMITTED, 5302).         % 作业已提交
-define(ERR_TASK_ASSIGNMENT_NOT_FOUND, 5303).      % 作业分配不存在
-define(ERR_TASK_ALREADY_REVIEWED, 5304).          % 作业已批改
-define(ERR_TASK_DEADLINE_PASSED, 5305).           % 作业已过期
-define(ERR_TASK_PERMISSION_DENIED, 5306).         % 作业权限不足

%% ===================================================================
%% QR 码登录相关错误（5200-5219）
%% ===================================================================

-define(ERR_INVALID_QR_TOKEN, 5200).               % 无效的二维码
-define(ERR_QR_LOGIN_EXPIRED, 5201).               % 二维码已过期
-define(ERR_QR_LOGIN_CANCELLED, 5202).             % 登录已取消
-define(ERR_QR_LOGIN_ALREADY_USED, 5203).          % 二维码已使用
-define(ERR_QR_LOGIN_NOT_SCANNED, 5204).           % 二维码未被扫描
-define(ERR_QR_LOGIN_DEVICE_LIMIT, 5205).          % 设备数量达到上限

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 定义错误码到消息的映射表
-define(ERROR_MSG_MAP, #{
    0 => <<"成功"/utf8>>,
    1 => <<"操作失败"/utf8>>,
    400 => <<"请求参数错误"/utf8>>,
    401 => <<"未认证，请先登录"/utf8>>,
    402 => <<"需要付费"/utf8>>,
    403 => <<"无权限访问"/utf8>>,
    404 => <<"资源不存在"/utf8>>,
    405 => <<"方法不允许"/utf8>>,
    406 => <<"内容格式不支持"/utf8>>,
    408 => <<"请求超时"/utf8>>,
    409 => <<"资源冲突"/utf8>>,
    410 => <<"资源已删除"/utf8>>,
    412 => <<"前置条件失败"/utf8>>,
    413 => <<"请求体过大"/utf8>>,
    415 => <<"不支持的媒体类型"/utf8>>,
    422 => <<"请求语义错误"/utf8>>,
    423 => <<"资源被锁定"/utf8>>,
    429 => <<"请求过于频繁，请稍后重试"/utf8>>,
    500 => <<"服务器内部错误"/utf8>>,
    501 => <<"功能未实现"/utf8>>,
    502 => <<"网关错误"/utf8>>,
    503 => <<"服务不可用"/utf8>>,
    504 => <<"网关超时"/utf8>>,
    507 => <<"存储空间不足"/utf8>>,
    901 => <<"不支持刷新 Token"/utf8>>,
    902 => <<"签名验证失败"/utf8>>,
    903 => <<"CSRF Token 错误"/utf8>>,
    904 => <<"验证码错误"/utf8>>,
    905 => <<"验证码已过期"/utf8>>,
    906 => <<"密码错误"/utf8>>,
    907 => <<"账号已禁用"/utf8>>,
    908 => <<"账号不存在"/utf8>>,
    909 => <<"账号已存在"/utf8>>,
    910 => <<"您的账号已在其他设备登录"/utf8>>,
    911 => <<"系统已完成首启初始化"/utf8>>,
    912 => <<"首启参数无效"/utf8>>,
    920 => <<"还不是好友"/utf8>>,
    921 => <<"好友请求待确认"/utf8>>,
    922 => <<"好友请求被拒绝"/utf8>>,
    923 => <<"好友关系已存在"/utf8>>,
    930 => <<"非群组成员"/utf8>>,
    931 => <<"非群管理员"/utf8>>,
    932 => <<"非群主"/utf8>>,
    933 => <<"群组权限不足"/utf8>>,
    934 => <<"群成员已满"/utf8>>,
    935 => <<"创建群组失败"/utf8>>,
    940 => <<"用户离线，消息已存储"/utf8>>,
    941 => <<"消息发送失败"/utf8>>,
    942 => <<"消息不存在"/utf8>>,
    950 => <<"文件上传失败"/utf8>>,
    951 => <<"文件下载失败"/utf8>>,
    952 => <<"文件不存在"/utf8>>,
    953 => <<"不允许的文件类型"/utf8>>,
    954 => <<"文件大小超出限制"/utf8>>,
    955 => <<"文件删除失败"/utf8>>,
    956 => <<"群文件存储空间已满"/utf8>>,
    960 => <<"相册不存在"/utf8>>,
    961 => <<"相册名称无效"/utf8>>,
    962 => <<"相册已存在"/utf8>>,
    963 => <<"图片不存在"/utf8>>,
    964 => <<"图片上传失败"/utf8>>,
    965 => <<"图片类型无效"/utf8>>,
    966 => <<"图片大小超出限制"/utf8>>,
    967 => <<"相册权限不足"/utf8>>,
    968 => <<"已点赞该图片"/utf8>>,
    5000 => <<"无效的传输会话"/utf8>>,
    5001 => <<"传输会话已过期"/utf8>>,
    5002 => <<"传输会话不存在"/utf8>>,
    5003 => <<"无效的设备"/utf8>>,
    5004 => <<"传输会话已被接受"/utf8>>,
    5005 => <<"无法确认传输会话"/utf8>>,
    5006 => <<"发送方用户 ID 不匹配"/utf8>>,
    5007 => <<"接收方用户 ID 不匹配"/utf8>>,
    5008 => <<"存在进行中的传输会话"/utf8>>,
    5009 => <<"传输会话已取消"/utf8>>,
    5010 => <<"无效的会话状态转换"/utf8>>,
    5020 => <<"可信联系人不存在"/utf8>>,
    5021 => <<"可信联系人已存在"/utf8>>,
    5022 => <<"不能添加自己为可信联系人"/utf8>>,
    5023 => <<"该联系人不在可信列表中"/utf8>>,
    5024 => <<"密钥分片数量不足"/utf8>>,
    5025 => <<"密钥分片已创建"/utf8>>,
    5026 => <<"密钥分片不存在"/utf8>>,
    5027 => <<"无效的恢复阈值"/utf8>>,
    5028 => <<"密钥恢复失败"/utf8>>,
    5029 => <<"受托人数量超过限制"/utf8>>,
    5040 => <<"备份密码错误"/utf8>>,
    5041 => <<"备份文件已损坏"/utf8>>,
    5042 => <<"备份版本不匹配"/utf8>>,
    5043 => <<"备份校验和不匹配"/utf8>>,
    5044 => <<"备份文件过大"/utf8>>,
    5045 => <<"备份文件格式无效"/utf8>>,
    5050 => <<"无效的密钥格式"/utf8>>,
    5051 => <<"密钥派生失败"/utf8>>,
    5052 => <<"加密失败"/utf8>>,
    5053 => <<"解密失败"/utf8>>,
    5054 => <<"密钥不存在"/utf8>>,
    5055 => <<"不支持的操作"/utf8>>,
    5056 => <<"备份密码强度不足"/utf8>>,
    5060 => <<"无可用恢复方式"/utf8>>,
    5061 => <<"恢复进行中，请稍后"/utf8>>,
    5062 => <<"密钥恢复失败"/utf8>>,
    5063 => <<"恢复操作超时"/utf8>>,
    5064 => <<"密钥不匹配，请重新获取"/utf8>>,
    5100 => <<"设备会话无效"/utf8>>,
    5101 => <<"设备会话已被踢出，请重新登录"/utf8>>,
    5102 => <<"同类型设备已登录，请确认是否踢出旧设备"/utf8>>,
    5103 => <<"设备不存在"/utf8>>,
    5104 => <<"设备会话已过期"/utf8>>,
    5190 => <<"功能未启用"/utf8>>,
    5300 => <<"作业不存在"/utf8>>,
    5301 => <<"作业标题必填"/utf8>>,
    5302 => <<"作业已提交，无法修改"/utf8>>,
    5303 => <<"作业分配不存在"/utf8>>,
    5304 => <<"作业已批改，无法修改"/utf8>>,
    5305 => <<"作业已过期，无法提交"/utf8>>,
    5306 => <<"无权限操作此作业"/utf8>>
}).
