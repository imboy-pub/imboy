-module(attach_direct_avatar_tests).
-moduledoc "step-13：Admin AI 头像 direct（application 主体全链 + publish 公开复制，T28/T35 语义）".
%%%
% 前置 fixture：registry('g')；样例 application id='990910' org='990911'。
% 用例：
%   * application 主体 register→declare→grant→finalize 全链 CONFIRMED
%     （guard application 分支：attachment.scope_ref='org:O/app:A'）；
%   * 身份域隔离：creator_user_id=0、key 前缀 app<id>/avatar/、subject
%     kind=application（不借 Human 凭据，T28）；
%   * 匿名可见性（T35/I13）：publish 前公开桶无写操作；publish 才产生公开
%     复制；put_object 失败 → publish_failed 可重试（失败无错误头像引用）；
%   * 重复 register 幂等（T02）。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(APP, <<"990910">>).
-define(ORG, <<"990911">>).

avatar_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun app_subject_full_chain_confirmed/0,
        fun publish_copies_to_public_and_recovers/0,
        fun register_idempotent_same_declaration/0,
        fun human_session_rejected_on_admin_face/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    EP = <<"http://127.0.0.1:39999">>,
    ok = application:set_env(
        imboy,
        garage,
        #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}
    ),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    Uploads = maps:get(upload_namespaces, Registry, #{}),
    Ns = <<"dv1av-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{
            Ns => #{
                driver => <<"garage">>,
                namespace => Ns,
                endpoint => EP,
                container => <<"b">>
            }
        },
        upload_namespaces => Uploads#{<<"public">> => Ns}
    }),
    #{booted => Booted, before_env => Before}.

cleanup(#{booted := Booted, before_env := Before}) ->
    try
        elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<"SET LOCAL session_replication_role = replica">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM attach_direct_attempt WHERE object_key LIKE 'app990910/%'">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attachment WHERE creator_user_id = 0"
                    " AND path LIKE 'app990910/%'"
                >>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attach_pending WHERE creator_user_id = 0"
                    " AND object_key LIKE 'app990910/%'"
                >>,
                []
            ),
            ok
        end),
        eunit_runner:eunit_cleanup(Booted)
    catch
        _:_ -> eunit_runner:eunit_cleanup(Booted)
    after
        [
            case Value of
                undefined -> application:unset_env(imboy, K);
                {ok, V} -> application:set_env(imboy, K, V)
            end
         || {K, Value} <- Before
        ]
    end.

%% ---------------------------------------------------------------- helpers

hex64(N) -> iolist_to_binary(io_lib:format("~64.16.0b", [N])).

with_storage_mock(CopyFun, HeadFun, ReadFun, PutFun, Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(attachment_storage_garage, copy_dispatch, 4, CopyFun),
    ok = meck:expect(attachment_storage_garage, head_bounded, 3, HeadFun),
    ok = meck:expect(attachment_storage_garage, read_snapshot, 1, ReadFun),
    ok = meck:new(elib_oss, [passthrough, no_link]),
    ok = meck:expect(elib_oss, put_object, 4, PutFun),
    try
        Inner()
    after
        meck:unload(attachment_storage_garage),
        meck:unload(elib_oss)
    end.

%% register/declare/grant 真实链（无网络面）；finalize 由调用方在
%% storage mock 内执行（copy/head 是网络面）。
prepare_to_granted() ->
    {ok, #{object_key := Key}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, undefined, <<"avatar.png">>
    ),
    ok = attach_direct_logic:declare(Key, hex64(16#AA), 10, <<"image/png">>),
    Grant = attach_direct_logic:grant(Key, hex64(16#AA)),
    {Key, Grant}.

finalize_in_mock(Key, No) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        copy_dispatch,
        4,
        fun(_N, _C, _S, _T) -> {ok, #{}} end
    ),
    ok = meck:expect(
        attachment_storage_garage,
        head_bounded,
        3,
        fun(_N, _C, _T) -> {ok, #{size => 10}} end
    ),
    R = attach_direct_logic:finalize(Key, No),
    meck:unload(attachment_storage_garage),
    R.

%% ---------------------------------------------------------------- cases

app_subject_full_chain_confirmed() ->
    {Key, {ok, #{grant := G, attempt_id := No}}} = prepare_to_granted(),
    %% 身份域隔离（T28）：key 前缀 app/、creator=0、subject kind=application
    ?assertMatch(<<"app990910/avatar/", _/binary>>, Key),
    ?assertEqual(<<"s3_put">>, maps:get(transport, G)),
    {ok, #{state := <<"CONFIRMED">>}} = finalize_in_mock(Key, No),
    {ok, [Att]} = elib_pg:query(
        <<
            "SELECT creator_user_id, scope, scope_ref, storage_physical_key FROM attachment"
            " WHERE path=$1"
        >>,
        [Key]
    ),
    %% creator=0（application 主体）；guard 要求 scope_ref='org:O/app:A'
    ?assertEqual(0, maps:get(<<"creator_user_id">>, Att)),
    ?assertEqual(<<"public">>, maps:get(<<"scope">>, Att)),
    ?assertEqual(<<"org:990911/app:990910">>, maps:get(<<"scope_ref">>, Att)),
    %% pending 侧 subject 也为 application（不借 Human 凭据）
    {ok, [Pending]} = elib_pg:query(
        <<"SELECT upload_registration FROM attach_pending WHERE object_key=$1">>, [Key]
    ),
    Reg = jsone:decode(maps:get(<<"upload_registration">>, Pending)),
    Subject = maps:get(<<"subject">>, Reg),
    ?assertEqual(<<"application">>, maps:get(<<"kind">>, Subject)),
    ok.

publish_copies_to_public_and_recovers() ->
    {Key, {ok, #{attempt_id := No}}} = prepare_to_granted(),
    {ok, #{state := <<"CONFIRMED">>}} = finalize_in_mock(Key, No),
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        read_snapshot,
        1,
        fun(_I) -> {ok, <<"avatar-bytes">>} end
    ),
    ok = meck:new(elib_oss, [passthrough, no_link]),
    try
        %% 发布前：公开桶零写操作（T35/I13：验证与发布许可前匿名不可读）
        ?assertEqual(0, meck:num_calls(elib_oss, put_object, '_')),
        %% publish 成功：恰好一次公开复制
        ok = meck:expect(elib_oss, put_object, 4, fun(_B, _K, _Bytes, _M) -> ok end),
        {ok, Pub} = attach_direct_logic:publish_avatar(Key, <<"agent-1">>, <<>>),
        ?assertMatch(
            <<"https://s3.imboy.pub/app990910/avatar/", _/binary>>,
            maps:get(public_url, Pub)
        ),
        ?assertEqual(1, meck:num_calls(elib_oss, put_object, '_'))
    after
        meck:unload(elib_oss),
        meck:unload(attachment_storage_garage)
    end,
    %% 公开复制失败：可恢复错误（winner 未动；重试语义）
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        read_snapshot,
        1,
        fun(_I) -> {ok, <<"avatar-bytes">>} end
    ),
    ok = meck:new(elib_oss, [passthrough, no_link]),
    ok = meck:expect(
        elib_oss,
        put_object,
        4,
        fun(_B, _K, _Bytes, _M) -> {error, storage_down} end
    ),
    try
        ?assertMatch(
            {error, {publish_failed, _}},
            attach_direct_logic:publish_avatar(Key, <<"agent-1">>, <<>>)
        )
    after
        meck:unload(elib_oss),
        meck:unload(attachment_storage_garage)
    end,
    %% 未 confirm 会话不可 publish
    {ok, #{object_key := Key2}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, undefined, <<"avatar2.png">>
    ),
    ?assertEqual(
        {error, not_confirmed},
        attach_direct_logic:publish_avatar(Key2, <<"agent-1">>, <<>>)
    ),
    ok.

register_idempotent_same_declaration() ->
    %% T02 幂等键 = client_upload_id（undefined 无幂等，key 随机）
    Cid = <<"5f0e9c1e-1111-4222-8333-444455556666">>,
    {ok, #{object_key := Key1}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, Cid, <<"same.png">>
    ),
    {ok, #{object_key := Key2}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, Cid, <<"same.png">>
    ),
    ?assertEqual(Key1, Key2),
    ok.

human_session_rejected_on_admin_face() ->
    %% review P1 修复验证：Human 用户会话绝不能经 Admin 面操作——
    %% require_application_session / publish_avatar 对 user 主体一律拒绝，
    %% 且拒绝路径对公开桶零写（Admin ≠ 读任意用户附件）。
    Uid = 990935,
    {ok, #{object_key := Key}} = attach_direct_logic:register(
        Uid, <<"private">>, undefined, undefined, <<"human-secret.png">>
    ),
    ok = attach_direct_logic:declare(Key, hex64(16#BB), 10, <<"image/png">>),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, hex64(16#BB)),
    {ok, #{state := <<"CONFIRMED">>}} = finalize_in_mock(Key, No),
    %% application 主体断言：user 会话拒绝 / application 会话通过
    ?assertEqual(
        {error, not_application_session},
        attach_direct_logic:require_application_session(Key)
    ),
    {ok, #{object_key := AppKey}} = attach_direct_logic:register_application(
        ?APP, ?ORG, <<"public">>, undefined, <<"app-ok.png">>
    ),
    ?assertEqual(ok, attach_direct_logic:require_application_session(AppKey)),
    %% publish 对 user 会话拒绝且不产生任何公开复制
    ok = meck:new(elib_oss, [passthrough, no_link]),
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    try
        ?assertEqual(
            {error, not_application_session},
            attach_direct_logic:publish_avatar(Key, <<"agent-1">>, <<>>)
        ),
        ?assertEqual(0, meck:num_calls(elib_oss, put_object, '_')),
        ?assertEqual(0, meck:num_calls(attachment_storage_garage, read_snapshot, '_'))
    after
        meck:unload(elib_oss),
        meck:unload(attachment_storage_garage)
    end,
    %% 自清理（Human 会话行不在本套件 cleanup 范围）
    elib_pg:with_tx(fun(Conn) ->
        {ok, _} = elib_pg:query(
            Conn,
            <<"SET LOCAL session_replication_role = replica">>,
            []
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attachment WHERE path=$1">>,
            [Key]
        ),
        {ok, _} = elib_pg:query(
            Conn,
            <<"DELETE FROM attach_pending WHERE object_key=$1">>,
            [Key]
        ),
        ok
    end),
    ok.

%% put_object 调用计数器（meck num_calls 包装）
list_to_putter() ->
    fun() -> meck:num_calls(elib_oss, put_object, '_') end.
