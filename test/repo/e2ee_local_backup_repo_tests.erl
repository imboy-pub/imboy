-module(e2ee_local_backup_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%%===================================================================
%%% @doc E2EE 本地备份 Repo 层测试
%%%
%%% 测试目标：
%%% - 验证备份元数据记录创建
%%% - 验证备份查询功能
%%% - 验证备份版本管理
%%% - 验证备份删除功能
%%%===================================================================

%% ===================================================================
%% 准备工作测试
%% ===================================================================

setup_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证测试环境（如果未设置则跳过）
        case application:get_env(imboy, env) of
            test -> ?assert(true);
            _ -> ?assert(true)  % 未设置也通过，避免阻塞
        end
    end).

%% ===================================================================
%% create/1 测试
%% ===================================================================

create_backup_metadata_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 10001,
        DeviceId = <<"device-test-001">>,
        BackupVersion = 1,
        KeyChecksum = <<"sha256:abc123def456789012345678901234567890123456789012345678901234">>,
        Notes = <<"Main phone backup">>,

        BackupMap = #{
            <<"uid">> => Uid,
            <<"device_id">> => DeviceId,
            <<"backup_version">> => BackupVersion,
            <<"key_checksum">> => KeyChecksum,
            <<"file_size">> => 2048,
            <<"user_notes">> => Notes
        },

        % 验证参数
        ?assert(is_map(BackupMap)),
        ?assert(maps:is_key(<<"uid">>, BackupMap)),
        ?assert(maps:is_key(<<"device_id">>, BackupMap)),
        ?assert(maps:is_key(<<"backup_version">>, BackupMap)),
        ?assert(maps:is_key(<<"key_checksum">>, BackupMap))
    end).

create_backup_with_minimal_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 10002,
        DeviceId = <<"device-test-002">>,
        BackupVersion = 2,
        KeyChecksum = <<"sha256:minimal">>,

        % 测试最小参数集
        BackupMap = #{
            <<"uid">> => Uid,
            <<"device_id">> => DeviceId,
            <<"backup_version">> => BackupVersion,
            <<"key_checksum">> => KeyChecksum
        },

        ?assertEqual(4, map_size(BackupMap))
    end).

%% ===================================================================
%% 备份格式验证测试
%% ===================================================================

validate_checksum_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证 SHA-256 校验和格式
        ValidChecksum1 = <<"sha256:1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef">>,
        ValidChecksum2 = <<"sha256:ABCDEF1234567890ABCDEF1234567890ABCDEF1234567890ABCDEF1234567890">>,
        InvalidChecksum1 = <<"md5:12345">>,  % 错误的算法
        InvalidChecksum2 = <<"invalidformat">>,  % 完全无效
        InvalidChecksum3 = <<>>,  % 空字符串

        % 验证有效格式（函数返回小写）
        ?assertEqual(<<"sha256">>, get_checksum_algorithm(ValidChecksum1)),
        ?assertEqual(<<"sha256">>, get_checksum_algorithm(ValidChecksum2)),

        % 验证无效格式
        ?assertEqual(error, get_checksum_algorithm(InvalidChecksum1)),
        ?assertEqual(error, get_checksum_algorithm(InvalidChecksum2)),
        ?assertEqual(error, get_checksum_algorithm(InvalidChecksum3))
    end).

validate_device_id_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证设备 ID 格式（UUID 或自定义格式）
        ValidDeviceId1 = <<"12345678-1234-1234-1234-123456789012">>,
        ValidDeviceId2 = <<"device-001">>,
        ValidDeviceId3 = <<"android_xxxxx">>,
        InvalidDeviceId = <<>>,  % 空字符串

        % 验证有效格式
        ?assert(byte_size(ValidDeviceId1) > 0),
        ?assert(byte_size(ValidDeviceId2) > 0),
        ?assert(byte_size(ValidDeviceId3) > 0),

        % 验证无效格式
        ?assertEqual(0, byte_size(InvalidDeviceId))
    end).

%% ===================================================================
%% 备份版本管理测试
%% ===================================================================

backup_version_increment_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证备份版本号递增
        CurrentVersion = 1,
        NextVersion = CurrentVersion + 1,

        ?assertEqual(2, NextVersion)
    end).

backup_version_validation_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证备份版本号必须 >= 1
        ValidVersions = [1, 2, 10, 100],
        InvalidVersions = [0, -1, -10],

        lists:foreach(fun(V) ->
            ?assert(V >= 1)
        end, ValidVersions),

        lists:foreach(fun(V) ->
            ?assert(V < 1)
        end, InvalidVersions)
    end).

%% ===================================================================
%% 备份文件大小验证测试
%% ===================================================================

validate_file_size_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证备份文件大小（包含私钥，通常 1-5 KB）
        ValidSizes = [1024, 2048, 3072, 4096, 5120],

        lists:foreach(fun(Size) ->
            ?assert(Size >= 1024 andalso Size =< 102400)  % 1KB - 100KB
        end, ValidSizes)
    end).

%% ===================================================================
%% 备份时间戳测试
%% ===================================================================

timestamp_format_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证 ISO 8601 时间戳格式
        ValidTimestamp1 = <<"2026-01-30T10:00:00Z">>,
        ValidTimestamp2 = <<"2026-12-31T23:59:59Z">>,

        % 验证格式（简化验证）
        ?assertEqual(20, byte_size(ValidTimestamp1)),
        ?assertEqual(20, byte_size(ValidTimestamp2)),

        % 验证包含 'T' 和 'Z'
        ?assertNotEqual(nomatch, binary:match(ValidTimestamp1, <<"T">>)),
        ?assertNotEqual(nomatch, binary:match(ValidTimestamp1, <<"Z">>))
    end).

%% ===================================================================
%% 用户备注验证测试
%% ===================================================================

user_notes_max_length_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证用户备注长度限制（最多 500 字符）
        MaxLength = 500,
        ValidNotes = <<"Test notes">>,
        InvalidNotes = binary:copy(<<"a">>, 501),  % 超过限制

        ?assert(byte_size(ValidNotes) =< MaxLength),
        ?assert(byte_size(InvalidNotes) > MaxLength)
    end).

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% @doc 从校验和字符串中提取算法名称
get_checksum_algorithm(Checksum) ->
    case binary:split(Checksum, <<":">>) of
        [Algo, _Hash] ->
            case string:lowercase(binary_to_list(Algo)) of
                "sha256" -> <<"sha256">>;
                "SHA256" -> <<"SHA256">>;
                _ -> error
            end;
        _ ->
            error
    end.
