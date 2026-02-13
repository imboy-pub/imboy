-module(e2ee_error_code_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc E2EE 错误码单元测试
%%%
%%% 测试目标：
%%% - 验证 E2EE 错误码定义是否正确
%%% - 验证错误消息映射是否正确
%%% - 确保错误码范围不冲突
%%%===================================================================

%% ===================================================================
%% 设备传输相关错误码测试 (5000-5019)
%% ===================================================================

e2ee_transfer_error_codes_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证设备传输错误码定义
        ?assertEqual(5000, ?ERR_E2EE_TRANSFER_INVALID_SESSION),
        ?assertEqual(5001, ?ERR_E2EE_TRANSFER_SESSION_EXPIRED),
        ?assertEqual(5002, ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND),
        ?assertEqual(5003, ?ERR_E2EE_TRANSFER_INVALID_DEVICE),
        ?assertEqual(5004, ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED),
        ?assertEqual(5005, ?ERR_E2EE_TRANSFER_CANNOT_CONFIRM),
        ?assertEqual(5006, ?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH),
        ?assertEqual(5007, ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH)
    end).

%% ===================================================================
%% 社交恢复相关错误码测试 (5020-5039)
%% ===================================================================

e2ee_social_error_codes_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证社交恢复错误码定义
        ?assertEqual(5020, ?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND),
        ?assertEqual(5021, ?ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS),
        ?assertEqual(5022, ?ERR_E2EE_SOCIAL_CONTACT_IS_SELF),
        ?assertEqual(5023, ?ERR_E2EE_SOCIAL_CONTACT_NOT_TRUSTED),
        ?assertEqual(5024, ?ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES),
        ?assertEqual(5025, ?ERR_E2EE_SOCIAL_SHARE_ALREADY_CREATED),
        ?assertEqual(5026, ?ERR_E2EE_SOCIAL_SHARE_NOT_FOUND),
        ?assertEqual(5027, ?ERR_E2EE_SOCIAL_INVALID_THRESHOLD),
        ?assertEqual(5028, ?ERR_E2EE_SOCIAL_RECOVER_FAILED),
        ?assertEqual(5029, ?ERR_E2EE_SOCIAL_TRUSTEE_LIMIT_EXCEEDED)
    end).

%% ===================================================================
%% 本地备份相关错误码测试 (5040-5049)
%% ===================================================================

e2ee_backup_error_codes_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证本地备份错误码定义
        ?assertEqual(5040, ?ERR_E2EE_BACKUP_INVALID_PASSWORD),
        ?assertEqual(5041, ?ERR_E2EE_BACKUP_FILE_CORRUPTED),
        ?assertEqual(5042, ?ERR_E2EE_BACKUP_VERSION_MISMATCH),
        ?assertEqual(5043, ?ERR_E2EE_BACKUP_CHECKSUM_MISMATCH)
    end).

%% ===================================================================
%% E2EE 通用错误码测试 (5050-5099)
%% ===================================================================

e2ee_general_error_codes_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证通用错误码定义
        ?assertEqual(5050, ?ERR_E2EE_INVALID_KEY_FORMAT),
        ?assertEqual(5051, ?ERR_E2EE_KEY_DERIVATION_FAILED),
        ?assertEqual(5052, ?ERR_E2EE_ENCRYPTION_FAILED),
        ?assertEqual(5053, ?ERR_E2EE_DECRYPTION_FAILED),
        ?assertEqual(5054, ?ERR_E2EE_KEY_NOT_FOUND),
        ?assertEqual(5055, ?ERR_E2EE_OPERATION_NOT_SUPPORTED)
    end).

%% ===================================================================
%% 错误消息映射测试
%% ===================================================================

e2ee_error_messages_transfer_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证设备传输错误消息
        ?assertEqual(
            <<"无效的传输会话"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_INVALID_SESSION)
        ),
        ?assertEqual(
            <<"传输会话已过期"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_SESSION_EXPIRED)
        ),
        ?assertEqual(
            <<"传输会话不存在"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND)
        ),
        ?assertEqual(
            <<"无效的设备"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_INVALID_DEVICE)
        ),
        ?assertEqual(
            <<"传输会话已被接受"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED)
        ),
        ?assertEqual(
            <<"无法确认传输会话"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_CANNOT_CONFIRM)
        ),
        ?assertEqual(
            <<"发送方用户 ID 不匹配"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH)
        ),
        ?assertEqual(
            <<"接收方用户 ID 不匹配"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH)
        )
    end).

e2ee_error_messages_social_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证社交恢复错误消息
        ?assertEqual(
            <<"可信联系人不存在"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND)
        ),
        ?assertEqual(
            <<"可信联系人已存在"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS)
        ),
        ?assertEqual(
            <<"不能添加自己为可信联系人"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_CONTACT_IS_SELF)
        ),
        ?assertEqual(
            <<"该联系人不在可信列表中"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_CONTACT_NOT_TRUSTED)
        ),
        ?assertEqual(
            <<"密钥分片数量不足"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES)
        ),
        ?assertEqual(
            <<"密钥分片已创建"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_SHARE_ALREADY_CREATED)
        ),
        ?assertEqual(
            <<"密钥分片不存在"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_SHARE_NOT_FOUND)
        ),
        ?assertEqual(
            <<"无效的恢复阈值"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_INVALID_THRESHOLD)
        ),
        ?assertEqual(
            <<"密钥恢复失败"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_RECOVER_FAILED)
        ),
        ?assertEqual(
            <<"受托人数量超过限制"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_SOCIAL_TRUSTEE_LIMIT_EXCEEDED)
        )
    end).

e2ee_error_messages_backup_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证本地备份错误消息
        ?assertEqual(
            <<"备份密码错误"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_BACKUP_INVALID_PASSWORD)
        ),
        ?assertEqual(
            <<"备份文件已损坏"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_BACKUP_FILE_CORRUPTED)
        ),
        ?assertEqual(
            <<"备份版本不匹配"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_BACKUP_VERSION_MISMATCH)
        ),
        ?assertEqual(
            <<"备份校验和不匹配"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_BACKUP_CHECKSUM_MISMATCH)
        )
    end).

e2ee_error_messages_general_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证通用错误消息
        ?assertEqual(
            <<"无效的密钥格式"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_INVALID_KEY_FORMAT)
        ),
        ?assertEqual(
            <<"密钥派生失败"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_KEY_DERIVATION_FAILED)
        ),
        ?assertEqual(
            <<"加密失败"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_ENCRYPTION_FAILED)
        ),
        ?assertEqual(
            <<"解密失败"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_DECRYPTION_FAILED)
        ),
        ?assertEqual(
            <<"密钥不存在"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_KEY_NOT_FOUND)
        ),
        ?assertEqual(
            <<"不支持的操作"/utf8>>,
            imboy_error:error_msg(?ERR_E2EE_OPERATION_NOT_SUPPORTED)
        )
    end).

%% ===================================================================
%% 错误码范围测试
%% ===================================================================

e2ee_error_code_range_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证错误码在 5000-5099 范围内
        ?assert(?ERR_E2EE_TRANSFER_INVALID_SESSION >= 5000),
        ?assert(?ERR_E2EE_TRANSFER_INVALID_SESSION =< 5099),

        ?assert(?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND >= 5000),
        ?assert(?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND =< 5099),

        ?assert(?ERR_E2EE_BACKUP_INVALID_PASSWORD >= 5000),
        ?assert(?ERR_E2EE_BACKUP_INVALID_PASSWORD =< 5099),

        ?assert(?ERR_E2EE_INVALID_KEY_FORMAT >= 5000),
        ?assert(?ERR_E2EE_INVALID_KEY_FORMAT =< 5099)
    end).

%% ===================================================================
%% 错误码唯一性测试
%% ===================================================================

e2ee_error_code_uniqueness_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 收集所有 E2EE 错误码
        ErrorCodes = [
            ?ERR_E2EE_TRANSFER_INVALID_SESSION,
            ?ERR_E2EE_TRANSFER_SESSION_EXPIRED,
            ?ERR_E2EE_TRANSFER_SESSION_NOT_FOUND,
            ?ERR_E2EE_TRANSFER_INVALID_DEVICE,
            ?ERR_E2EE_TRANSFER_ALREADY_ACCEPTED,
            ?ERR_E2EE_TRANSFER_CANNOT_CONFIRM,
            ?ERR_E2EE_TRANSFER_FROM_UID_NOT_MATCH,
            ?ERR_E2EE_TRANSFER_TO_UID_NOT_MATCH,
            ?ERR_E2EE_SOCIAL_CONTACT_NOT_FOUND,
            ?ERR_E2EE_SOCIAL_CONTACT_ALREADY_EXISTS,
            ?ERR_E2EE_SOCIAL_CONTACT_IS_SELF,
            ?ERR_E2EE_SOCIAL_CONTACT_NOT_TRUSTED,
            ?ERR_E2EE_SOCIAL_NOT_ENOUGH_SHARES,
            ?ERR_E2EE_SOCIAL_SHARE_ALREADY_CREATED,
            ?ERR_E2EE_SOCIAL_SHARE_NOT_FOUND,
            ?ERR_E2EE_SOCIAL_INVALID_THRESHOLD,
            ?ERR_E2EE_SOCIAL_RECOVER_FAILED,
            ?ERR_E2EE_SOCIAL_TRUSTEE_LIMIT_EXCEEDED,
            ?ERR_E2EE_BACKUP_INVALID_PASSWORD,
            ?ERR_E2EE_BACKUP_FILE_CORRUPTED,
            ?ERR_E2EE_BACKUP_VERSION_MISMATCH,
            ?ERR_E2EE_BACKUP_CHECKSUM_MISMATCH,
            ?ERR_E2EE_INVALID_KEY_FORMAT,
            ?ERR_E2EE_KEY_DERIVATION_FAILED,
            ?ERR_E2EE_ENCRYPTION_FAILED,
            ?ERR_E2EE_DECRYPTION_FAILED,
            ?ERR_E2EE_KEY_NOT_FOUND,
            ?ERR_E2EE_OPERATION_NOT_SUPPORTED
        ],

        % 检查唯一性
        UniqueCodes = lists:usort(ErrorCodes),
        ?assertEqual(length(ErrorCodes), length(UniqueCodes))
    end).
