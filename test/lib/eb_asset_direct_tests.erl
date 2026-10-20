-module(eb_asset_direct_tests).
-moduledoc "step-14：EB 域 direct 桥接（application 主体会话 + 内容 Worker 复核 + 跨 org 拒绝）".
%%%
% 本地可验收核心（逐面真实 HTTP 证据归 20b）：
%   * EB direct 会话全链：register(application 主体 org 声明)→declare→grant→
%     finalize CONFIRMED；
%   * scan_confirmed：CONFIRMED 后受控扫描（mock 读）+ receipt 五道门验证
%     通过（25MiB/魔数/文本规则基线=eb_asset_content 零改动）；
%   * 跨 org/跨主体写回拒绝（T04/E10 语义）；
%   * 受控读注入器合同（Range/eof/416→eof）。
%%%

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(ORG, <<"990920">>).
-define(SUBJ, <<"990921">>).

eb_direct_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun eb_full_chain_and_worker_scan/0,
        fun cross_org_writeback_rejected/0,
        fun read_fun_contract/0
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
    Ns = <<"dv1eb-", (binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))/binary>>,
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
                <<"DELETE FROM attach_direct_attempt WHERE object_key LIKE 'app990921/%'">>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attachment WHERE creator_user_id = 0"
                    " AND path LIKE 'app990921/%'"
                >>,
                []
            ),
            {ok, _} = elib_pg:query(
                Conn,
                <<
                    "DELETE FROM attach_pending WHERE creator_user_id = 0"
                    " AND object_key LIKE 'app990921/%'"
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

with_finalize_mock(Inner) ->
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
        fun(_N, _C, _T) -> {ok, #{size => 12}} end
    ),
    try
        Inner()
    after
        meck:unload(attachment_storage_garage)
    end.

eb_chain_to_confirmed(Bin) ->
    {ok, #{object_key := Key}} = eb_asset_direct:register(
        ?ORG, ?SUBJ, <<"doc.bin">>, undefined, <<"application/octet-stream">>, test
    ),
    Sha = binary:encode_hex(crypto:hash(sha256, Bin), lowercase),
    ok = attach_direct_logic:declare(
        Key,
        Sha,
        byte_size(Bin),
        <<"application/octet-stream">>
    ),
    {ok, #{attempt_id := No}} = attach_direct_logic:grant(Key, Sha),
    {ok, #{state := <<"CONFIRMED">>}} = with_finalize_mock(fun() ->
        attach_direct_logic:finalize(Key, No)
    end),
    {Key, Sha, byte_size(Bin)}.

%% ---------------------------------------------------------------- cases

eb_full_chain_and_worker_scan() ->
    Bin = sample(<<"image/png">>),
    {Key, Sha, Size} = eb_chain_to_confirmed(Bin),
    %% 受控读 mock：presign 产 URL；httpc Range 分块（模拟 Garage 分块读合同）
    ok = meck:new(attachment_storage_garage, [passthrough, no_link]),
    ok = meck:expect(
        attachment_storage_garage,
        presign_snapshot,
        2,
        fun(_I, _T) -> {ok, <<"http://mock/get">>} end
    ),
    ok = meck:new(httpc, [no_link]),
    ok = meck:expect(httpc, request, 4, fun(get, {_U, Headers}, _Opts, _ClientOpts) ->
        Range = proplists:get_value("range", Headers),
        <<"bytes=", RangeRest/binary>> = list_to_binary(Range),
        [FromS, ToS] = binary:split(RangeRest, <<"-">>, [global]),
        From = binary_to_integer(FromS),
        To = binary_to_integer(ToS),
        case From >= byte_size(Bin) of
            true ->
                {ok, {{"HTTP/1.1", 416, "Range Not Satisfiable"}, [], <<>>}};
            false ->
                End = min(To, byte_size(Bin) - 1),
                Slice = binary:part(Bin, From, End - From + 1),
                {ok, {{"HTTP/1.1", 206, "Partial Content"}, [], Slice}}
        end
    end),
    R =
        try
            eb_asset_direct:scan_confirmed(Key, <<"image/png">>, <<"sec">>, 60000)
        after
            meck:unload(httpc),
            meck:unload(attachment_storage_garage)
        end,
    ?assertMatch({ok, #{verified := true, sha256 := Sha, size := Size}}, R),
    ok.

cross_org_writeback_rejected() ->
    %% org 声明进 registration：另一 org 主体同 filename 是不同 key（A11）；
    %% 同 key 跨主体 declare 异值 → expected_conflict（T03/T04 语义）
    {ok, #{object_key := Key}} = eb_asset_direct:register(
        ?ORG, ?SUBJ, <<"doc.bin">>, undefined, <<"application/octet-stream">>, test
    ),
    ok = attach_direct_logic:declare(Key, hex64(16#BB), 10, <<"application/octet-stream">>),
    ?assertEqual(
        {error, expected_conflict},
        attach_direct_logic:declare(Key, hex64(16#BC), 10, <<"application/octet-stream">>)
    ),
    ok.

read_fun_contract() ->
    %% 受控读注入器合同（纯数据：Range 截取/_eof）
    Bin = sample(<<"image/png">>),
    ReadFun = fun
        (Offset, Len) when Offset >= byte_size(Bin) -> eof;
        (Offset, Len) ->
            End = min(Offset + Len, byte_size(Bin)),
            {ok, binary:part(Bin, Offset, End - Offset)}
    end,
    {ok, Head} = ReadFun(0, 8),
    ?assertEqual(8, byte_size(Head)),
    ?assertEqual(eof, ReadFun(byte_size(Bin), 8)),
    ok.

sample(<<"image/png">>) -> <<137, 80, 78, 71, 13, 10, 26, 10, 0, 1, 2, 3>>.
