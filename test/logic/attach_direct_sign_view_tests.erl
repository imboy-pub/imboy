-module(attach_direct_sign_view_tests).
-moduledoc "step-10：direct 物理键读链（sign_view/download）+ 逐请求撤权 + 有限授权窗口（T23/T26 本地语义）".
%%%
% 协议 §6/§7：
%   * direct 行（storage_physical_key 非 NULL）经 sign_view/download_target
%     必须命中**物理 winner 键**（step-4 reader 兼容自动贯通——本套件钉住该行为）；
%   * 历史行（NULL）回落逻辑键（兼容不变）；
%   * ticket download 每请求复核 ACL（非成员/篡改 ticket 下一请求拒绝）；
%   * 有限授权窗口数值固定：GET 签发 600s、ticket TTL 600s、响应
%     cache-control private,no-store——旧 URL 到期自然排空（无召回）。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

sign_view_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun direct_row_signs_physical_key/0,
        fun legacy_row_falls_back_to_logical_key/0,
        fun ticket_download_per_request_acl/0,
        fun window_values_frozen/0
    ]}.

setup() ->
    Before = [
        {K, application:get_env(imboy, K)}
     || K <- [garage, attachment_storage_registry]
    ],
    Booted = eunit_runner:eunit_setup(),
    %% 测试行用 namespace 'g'/container 'b'：注册对应 root（否则 resolve
    %% fail-closed → frozen_location forbidden）
    EP = <<"http://127.0.0.1:39999">>,
    ok = application:set_env(
        imboy,
        garage,
        #{endpoint => EP, bucket => <<"b">>, access_key => <<"ak">>, secret_key => <<"sk">>}
    ),
    Registry = attachment_storage:registry(),
    Roots = maps:get(roots, Registry, #{}),
    ok = application:set_env(imboy, attachment_storage_registry, Registry#{
        roots => Roots#{
            <<"g">> => #{
                driver => <<"garage">>,
                namespace => <<"g">>,
                endpoint => EP,
                container => <<"b">>,
                root_id => <<"g">>
            }
        }
    }),
    #{booted => Booted, before_env => Before}.

cleanup(#{booted := Booted, before_env := Before}) ->
    %% 测试行清理绕开 v5 delete guard（guard 要求 delete_intent；本套件行是
    %% 本任务自建的测试夹具，非业务数据）
    try
        elib_pg:with_tx(fun(Conn) ->
            {ok, _} = elib_pg:query(
                Conn,
                <<"SET LOCAL session_replication_role = replica">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<"DELETE FROM attachment WHERE creator_user_id IN (990801,990802,990803)">>,
                []
            ),
            ok
        end),
        eunit_runner:eunit_cleanup(Booted)
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

insert_attachment(Id, Path, Creator, Scope, ScopeRef, PhysicalKey) ->
    %% locator 必须等于 registry root('g') 的 canonical digest（location_row 校验）
    Locator = attachment_storage:locator_digest(<<"g">>, #{
        driver => <<"garage">>,
        root_id => <<"g">>,
        endpoint => <<"http://127.0.0.1:39999">>,
        container => <<"b">>
    }),
    Sql =
        <<
            "INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url, size, info,"
            " referer_time, creator_user_id, scope, scope_ref, cipher, status, updated_at, created_at,"
            " storage_driver, storage_namespace, storage_container, storage_locator_sha256,"
            " storage_physical_key)"
            " VALUES ($1,$2,'image/png','.png','a.png',$3,$3,10,'{}',1,$4,$5,$6,null,1,NOW(),NOW(),"
            " 'garage','g','b',$7,$8)"
        >>,
    {ok, _} = elib_pg:query(Sql, [
        Id,
        hex64(Id),
        Path,
        Creator,
        Scope,
        ScopeRef,
        Locator,
        PhysicalKey
    ]),
    ok.

with_presign_capture(Inner) ->
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        presign_snapshot,
        2,
        fun(#{key := K}, Ttl) ->
            put(captured_key, {K, Ttl}),
            {ok, <<"https://s3.example/get">>}
        end
    ),
    try
        Result = Inner(),
        {ok, get(captured_key), Result}
    after
        erase(captured_key),
        meck:unload(attachment_storage_garage)
    end.

captured_key() ->
    {K, _Ttl} = get(captured_key),
    K.

%% ---------------------------------------------------------------- cases

direct_row_signs_physical_key() ->
    PhysicalKey = <<"direct-c/u990801/file/x/abc">>,
    LogicalKey = <<"u990801/file/x">>,
    ok = insert_attachment(990801001, LogicalKey, 990801, <<"private">>, <<>>, PhysicalKey),
    {ok, Rec} = attachment_repo:find_by_path(LogicalKey),
    %% 导出面 view_url（逐请求 authorize → authorized_view → sign_view）
    {ok, {CapturedKey, Ttl}, _} = with_presign_capture(fun() ->
        attach_logic:view_url(990801, LogicalKey)
    end),
    ?assertEqual(PhysicalKey, CapturedKey),
    ?assertEqual(600, Ttl),
    ok.

legacy_row_falls_back_to_logical_key() ->
    LogicalKey = <<"u990802/file/legacy">>,
    ok = insert_attachment(990802001, LogicalKey, 990802, <<"private">>, <<>>, null),
    {ok, {CapturedKey, Ttl}, _} = with_presign_capture(fun() ->
        attach_logic:view_url(990802, LogicalKey)
    end),
    ?assertEqual(LogicalKey, CapturedKey, '历史行必须回落逻辑键'),
    ?assertEqual(600, Ttl),
    ok.

ticket_download_per_request_acl() ->
    %% c2c 走有限授权窗口短 GET（协议 §7）；authorize(c2c) 每请求解析成员
    ScopeRef = <<"c2c:11:22">>,
    Path = <<"u990803/c2c/x/file.png">>,
    ok = insert_attachment(990803001, Path, 990801, <<"c2c">>, ScopeRef, null),
    with_presign_capture(fun() ->
        %% 成员 11：每请求复核通过，签出短 GET
        ?assertMatch({ok, _}, attach_logic:view_url(11, Path)),
        %% 非成员 33：下一请求即拒绝（撤权窗口 = 下一请求，T23 本地语义）
        ?assertEqual({error, forbidden}, attach_logic:view_url(33, Path)),
        ok
    end),
    %% ticket download 链：篡改 ticket 拒绝（verify 失败关闭）
    ?assertEqual(
        {error, forbidden},
        attach_logic:download(<<"tampered-ticket">>)
    ),
    ok.

window_values_frozen() ->
    %% 有限授权窗口数值固定（协议 §7；旧 URL 到期排空，无召回语义）
    GetExpires = list_to_integer(
        os:cmd(
            "grep -m1 'define(GET_EXPIRES' src/logic/attach_logic.erl | "
            "sed -E 's/.*,[[:space:]]*([0-9]+)\\).*/\\1/' | tr -d '\\n'"
        )
    ),
    ?assertEqual(600, GetExpires),
    TicketTtl = list_to_integer(
        os:cmd(
            "grep -m1 'define(TTL' src/lib/attachment_download_ticket.erl | "
            "sed -E 's/.*,[[:space:]]*([0-9]+)\\).*/\\1/' | tr -d '\\n'"
        )
    ),
    ?assertEqual(600, TicketTtl),
    %% 响应缓存策略：download handler 必须 private,no-store（缓存不跨身份）
    HandlerSrc = os:cmd("grep -c 'private, no-store' src/api/attachment_download_handler.erl"),
    ?assertEqual(1, list_to_integer(string:trim(HandlerSrc))),
    ok.
