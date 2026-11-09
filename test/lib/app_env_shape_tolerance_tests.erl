%%% 夜间 full-eunit 连坐回归钉子（2026-11-09 取证）。
%%%
%%% == 背景 ==
%%%
%%% Backend Nightly 连续 8 晚以完全相同的 6 个失败收尾（11-07/11-08 两晚
%%% 逐字一致）：
%%%   * e2ee_presign_mime_binding_tests ×5 —— elib_s3_sign:presign_url_at/8
%%%     读 garage env 得 {ok, Map}，maps:get 直接 badmap；
%%%   * attach_direct_logic_pg_tests:declare_size_cap_test ×1 ——
%%%     attachment_storage:registry/0 读 attachment_storage_registry 得
%%%     {ok, Map}，同样 badmap；
%%%   * 另有 eb_seat_send_protocol_tests 整组 context setup 失败被取消。
%%%
%%% 机制：同 VM 全量跑中某测试夹具把 application:get_env 的返回元组
%%% {ok, Map} 原样写回 app env（fixture restore 的历史脏手已在 9e4fc46a0 /
%%% e5a6737e2 修过数处，eb_direct_purge_mixed_pg_tests:environment_restore_test_
%%% 是当时的回归钉子），后续模块读到元组形态即崩。
%%%
%%% == 本文件守护什么 ==
%%%
%%% 读侧对 {ok, Map} 形态容错（attachment_storage:registry/0 与
%%% elib_s3_sign:garage_config/0）：只解包、不改语义——非法形态仍回落
%%% 缺省（registry 回空根 fail-closed，garage 回空 map）。真凶夹具的根治
%%% 另行追查（写侧不恢复 env 的模块清单见 PR 描述），这里先把夜间批量的
%%% 确定性红钉死成绿。
-module(app_env_shape_tolerance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(KEY_REGISTRY, attachment_storage_registry).
-define(KEY_GARAGE, garage).

%% 与夜间 badmap 现场同构的合成 garage 配置（attach_logic_tests 同款值）。
garage_fixture() ->
    #{
        endpoint => <<"http://127.0.0.1:3900">>,
        bucket => <<"imboy">>,
        public_bucket => <<"imboy-public">>,
        public_base_url => <<"https://s3.imboy.pub">>,
        access_key => <<"GKtest">>,
        secret_key => <<"testsecret">>
    }.

registry_fixture() ->
    #{
        version => 1,
        roots => #{
            <<"shape-root">> => #{
                driver => <<"garage">>,
                namespace => <<"shape-ns">>,
                endpoint => <<"http://127.0.0.1:3900">>,
                container => <<"imboy">>
            }
        },
        upload_namespaces => #{<<"shape">> => <<"shape-ns">>}
    }.

%% 保存→污染→断言→恢复；恢复自身也吃 {ok, V} / undefined 两形态。
with_env(Key, Value, Fun) ->
    Prev = application:get_env(imboy, Key),
    try
        ok = application:set_env(imboy, Key, Value),
        Fun()
    after
        case Prev of
            undefined -> application:unset_env(imboy, Key);
            {ok, V} -> application:set_env(imboy, Key, V)
        end
    end.

%% ===================================================================
%% attachment_storage:registry/0
%% ===================================================================

registry_unwraps_ok_tuple_test() ->
    Reg = registry_fixture(),
    with_env(?KEY_REGISTRY, {ok, Reg}, fun() ->
        ?assertEqual(Reg, attachment_storage:registry())
    end).

registry_accepts_plain_map_test() ->
    Reg = registry_fixture(),
    with_env(?KEY_REGISTRY, Reg, fun() ->
        ?assertEqual(Reg, attachment_storage:registry())
    end).

registry_invalid_shape_falls_back_test() ->
    %% 非法形态（含 {ok, 非 map}）一律回落缺省空 registry：fail-closed 不变。
    [
        with_env(?KEY_REGISTRY, {ok, not_a_map}, fun() ->
            ?assertEqual(#{version => 1, roots => #{}}, attachment_storage:registry())
        end),
        with_env(?KEY_REGISTRY, <<"junk">>, fun() ->
            ?assertEqual(#{version => 1, roots => #{}}, attachment_storage:registry())
        end)
    ].

%% ===================================================================
%% elib_s3_sign:garage_config/0（经导出面 presign_put/5 验证）
%% ===================================================================

%% 夜间 5 连红的直接复刻：garage env 为 {ok, Map} 时 presign 仍产出带签名的 URL。
presign_survives_ok_tuple_garage_test() ->
    with_env(?KEY_GARAGE, {ok, garage_fixture()}, fun() ->
        Url = elib_s3_sign:presign_put(
            <<"http://127.0.0.1:3900">>,
            <<"imboy">>,
            <<"u1/shape/a.bin">>,
            <<"application/octet-stream">>,
            600
        ),
        ?assertNotEqual(nomatch, binary:match(Url, <<"X-Amz-Signature=">>))
    end).

presign_survives_plain_map_garage_test() ->
    with_env(?KEY_GARAGE, garage_fixture(), fun() ->
        Url = elib_s3_sign:presign_put(
            <<"http://127.0.0.1:3900">>,
            <<"imboy">>,
            <<"u1/shape/b.bin">>,
            <<"image/jpeg">>,
            600
        ),
        ?assertNotEqual(nomatch, binary:match(Url, <<"X-Amz-Signature=">>))
    end).
