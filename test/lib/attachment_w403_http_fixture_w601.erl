%%% W6-01 C 组（W2-01/W2-02/W2-03）追加入口，自 attachment_w403_http_fixture
%%% 按功能域拆出（主文件超 800 行上限）。签名与行为与拆分前逐字一致，仅模块
%%% 迁移；共享证据表/listener/认证由主 fixture（attachment_w403_http_fixture）
%%% 的 setup/execute 拥有，本模块只通过其导出入口复用。
-module(attachment_w403_http_fixture_w601).

-include_lib("eunit/include/eunit.hrl").
-define(TABLE, attachment_w403_http_evidence).
-define(F, attachment_w403_http_fixture).

-export([
    presign_op/4,
    status/1,
    renew/1,
    multipart_register/2,
    multipart_put/3,
    confirm_with/3,
    confirm_legacy/1,
    expired_put/2,
    bare_put/2,
    q/2
]).

%% V5 恢复 API：operation=status|renew 打到各 surface 既有的 presign 端点。
%% Extra 覆盖请求身份字段（如 client_upload_id/asset_id/upload_ref）。
presign_op(_S0, T, Op, Extra) ->
    S = maps:get(fixture, T),
    Id = #{
        operation => Op,
        client_upload_id => maps:get(client_upload_id, T),
        upload_ref => upload_ref_of(T),
        asset_id => asset_id_of(T)
    },
    {Method, Path, Body} =
        case maps:get(surface, T) of
            ordinary ->
                Q = uri_string:compose_query(
                    [{<<"operation">>, Op}, {<<"client_upload_id">>, maps:get(client_upload_id, T)}]
                ),
                {get, <<"/api/v1/attachment/presign?", Q/binary>>, <<>>};
            enterprise_internal ->
                {post, <<"/api/internal/v1/files/presign">>,
                    merge(maps:with([operation, client_upload_id], Id), Extra)};
            enterprise_seat ->
                %% seat 面会话定位（eb_asset_app locate_session/with_authorized_
                %% session 经 eb_tenant_handler:90 → eb_enterprise_http:workspace_id）
                %% 必须有 workspace_id：缺参 422 missing_workspace_id（第二跑
                %% 实证 seat 面 status/renew 全败）——与 register 的
                %% registration_request seat 分支同源取值。
                {post, ?F:seat_path(S, <<"presign">>),
                    merge(
                        Id#{workspace_id => maps:get(workspace_id, maps:get(scope, S))},
                        Extra
                    )};
            cs_widget ->
                {post, ?F:widget_path(S, <<"presign">>),
                    merge(Id#{installation_id => maps:get(installation_id, S)}, Extra)}
        end,
    case Method of
        get -> ?F:request(S, get, Path, <<>>, maps:get(headers, T), <<"application/json">>);
        post -> ?F:request(S, post, Path, Body, maps:get(headers, T), <<"application/json">>)
    end.

status(T) -> presign_op(maps:get(fixture, T), T, <<"status">>, #{}).
renew(T) -> presign_op(maps:get(fixture, T), T, <<"renew">>, #{}).

%% ordinary multipart 登记入口：GET presign 带 method=multipart，响应额外
%% 携带指向本端点的 upload_url（multipart/form-data POST 目标）。
multipart_register(S0, _Bytes) ->
    S = S0,
    Cid = elib_uuid:gen_v7(),
    Registry = attachment_storage:registry(),
    Ns = maps:get(multipart_ns, S0, <<"w403-disk">>),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => #{<<"private">> => Ns, <<"enterprise">> => Ns}}
    ),
    Q = uri_string:compose_query([
        {<<"filename">>, <<"w403.txt">>},
        {<<"mime_type">>, <<"text/plain">>},
        {<<"client_upload_id">>, Cid},
        {<<"method">>, <<"multipart">>}
    ]),
    Headers = maps:get(ordinary_headers, S),
    Path = <<"/api/v1/attachment/presign?", Q/binary>>,
    NoAuth = maps:without([<<"authorization">>], Headers),
    ?assertNot(?F:success(?F:request(S, get, Path, <<>>, NoAuth, <<"application/json">>))),
    Registered = ?F:request(S, get, Path, <<>>, Headers, <<"application/json">>),
    ?assert(?F:success(Registered)),
    Data = ?F:payload(Registered),
    Key = maps:get(<<"object_key">>, Data),
    {ok, Location} = attachment_storage:session_location(Key),
    ?assert(Location =/= legacy_unknown),
    true = ets:insert(?TABLE, {{object, Key}, Location}),
    #{
        fixture => S,
        leaf => maps:get(leaf, S, elib_uuid:gen_v7()),
        surface => ordinary,
        driver => maps:get(multipart_driver, S0, disk),
        key => Key,
        location => Location,
        upload_url => maps:get(<<"put_url">>, Data),
        multipart_url => maps:get(<<"upload_url">>, Data),
        headers => Headers,
        confirm_body => #{object_key => Key, scope => <<"private">>},
        registration => Data,
        client_upload_id => Cid
    }.

%% multipart/form-data POST（文件字段名 file）：真实 loopback HTTP，
%% 上传 URL 由 presign 以请求 Host 派生（同 listener），保留 evidence 观察。
multipart_put(S, T, Bytes) ->
    Url = maps:get(multipart_url, T),
    Boundary = <<"w403multipart7f3a">>,
    Body =
        <<
            "--",
            Boundary/binary,
            "\r\n",
            "Content-Disposition: form-data; name=\"file\"; filename=\"w403.txt\"\r\n",
            "Content-Type: text/plain\r\n\r\n",
            Bytes/binary,
            "\r\n--",
            Boundary/binary,
            "--\r\n"
        >>,
    Mime = <<"multipart/form-data; boundary=", Boundary/binary>>,
    absolute_request(S, post, Url, Body, maps:get(headers, T), Mime).

%% confirm 带附加字段（cipher / anchor_msg_id / md5 等旧字段）。
confirm_with(_S0, T, Extra) ->
    T2 = T#{confirm_body => merge(maps:get(confirm_body, T), Extra)},
    ?F:confirm(T2).

%% 旧客户端 confirm：无 client_upload_id，声明型旧字段（file_hash256/md5）。
%% 服务端 V5 链路以真实对象字节裁决，客户端自报 hash 只进 legacy 兼容分支。
confirm_legacy(T) ->
    Key = maps:get(key, T),
    Sha = binary:encode_hex(crypto:hash(sha256, <<"legacy client declared bytes">>), lowercase),
    case maps:get(surface, T) of
        ordinary ->
            confirm_with(maps:get(fixture, T), T, #{
                file_hash256 => Sha,
                mime_type => <<"text/plain">>,
                size => byte_size(<<"legacy client declared bytes">>),
                md5 => <<>>
            });
        enterprise_internal ->
            confirm_with(maps:get(fixture, T), T, #{
                object_key => Key,
                file_hash256 => Sha
            });
        _ ->
            ?F:confirm(T)
    end.

%% 过期能力 PUT：ordinary/internal 用真实 JWT 签发器构造已过期能力并替换
%% gateway URL 的 capability 参数；seat/widget 的不透明 upload_ref 无法替换，
%% 过期场景由套件正常登记后将服务端测试时钟推进至凭证到期时刻，
%% 不走本函数，也不由客户端指定 TTL。
expired_put(T, Bytes) ->
    Key = maps:get(key, T),
    {ok, Expired} =
        imboy_jwt:issue_upload_capability(Key, <<"text/plain">>, erlang:system_time(second) - 1),
    Stale = replace_capability(maps:get(upload_url, T), Expired),
    S = maps:get(fixture, T),
    Headers =
        case maps:get(surface, T) of
            enterprise_seat -> maps:get(headers, T);
            _ -> #{}
        end,
    ?F:request(S, put, Stale, Bytes, Headers, <<"text/plain">>).

%% 裸 PUT：剥离 capability 参数的 gateway URL（旧客户端直传形态）。
bare_put(T, Bytes) ->
    Stale = strip_capability(maps:get(upload_url, T)),
    S = maps:get(fixture, T),
    Headers =
        case maps:get(surface, T) of
            enterprise_seat -> maps:get(headers, T);
            _ -> #{}
        end,
    ?F:request(S, put, Stale, Bytes, Headers, <<"text/plain">>).

%% SQL 直通（套件内持久状态断言用；只读查询为主）。
q(Sql, Args) -> elib_pg:query(Sql, Args).

replace_capability(Url, NewCap) ->
    case binary:split(Url, <<"capability=">>) of
        [Prefix, Rest] ->
            Suffix =
                case binary:split(Rest, <<"&">>) of
                    [_, S] -> <<"&", S/binary>>;
                    _ -> <<>>
                end,
            <<Prefix/binary, "capability=", NewCap/binary, Suffix/binary>>;
        _ ->
            erlang:error(no_capability_in_url)
    end.

strip_capability(Url) ->
    case binary:split(Url, <<"capability=">>) of
        [Prefix, Rest] ->
            Suffix =
                case binary:split(Rest, <<"&">>) of
                    [_, S] -> <<"&", S/binary>>;
                    _ -> <<>>
                end,
            <<Prefix/binary, "capability=", Suffix/binary>>;
        _ ->
            erlang:error(no_capability_in_url)
    end.

upload_ref_of(T) ->
    Body = maps:get(confirm_body, T),
    maps:get(upload_ref, Body, undefined).

asset_id_of(T) ->
    Body = maps:get(confirm_body, T),
    maps:get(asset_id, Body, undefined).

merge(A, B) when is_map(A), is_map(B) ->
    maps:merge(A, maps:filter(fun(_, V) -> V =/= undefined end, B)).

%% multipart 上传 URL 是 presign 以请求 Host 派生的绝对 loopback 地址，
%% 不经 w403.invalid 改写；直连同 listener 并保留 evidence 观察。
absolute_request(S, Method, Url, Body, Headers0, Mime) ->
    ok = assert_loopback_url(Url),
    Id = <<"w403-", (elib_uuid:gen_v7())/binary>>,
    Headers = Headers0#{<<"x-w403-request-id">> => Id, <<"connection">> => <<"close">>},
    H = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- maps:to_list(Headers)],
    Req = {binary_to_list(Url), H, binary_to_list(Mime), Body},
    Started = erlang:monotonic_time(millisecond),
    {ok, {{_, Status, _}, _, ResponseBody}} = isolated_request(Method, Req),
    %% 证据行回读 Cowboy 实际观察（同一 listener 的 execute/2 已记录真实
    %% handler/server_process/connection_id）；{observed,Id} 缺失即请求未命中
    %% 本 fixture 拥有的 listener，直接失败——证据闭环不允许手写假值兜底。
    [{{observed, Id}, Observed}] = ets:lookup(?TABLE, {observed, Id}),
    true = ets:insert(
        ?TABLE,
        {{request, maps:get(leaf, S), Id}, Observed#{
            status => Status, elapsed_ms => erlang:monotonic_time(millisecond) - Started
        }}
    ),
    #{status => Status, body => ResponseBody, request_id => Id}.

%% multipart 直传必须落在本 fixture 拥有的 loopback listener 上（presign 以
%% 请求 Host 派生上传 URL）。非 loopback host（如对外网关域名）或非绝对 URL
%% 一律失败——旁路目标不得进入证据。uri_string:parse/1 直接返回 map
%% （非 {ok, Map}——第二跑实证 badmatch 值为 map 本体）。
assert_loopback_url(Url) ->
    #{host := Host} = uri_string:parse(Url),
    true = lists:member(Host, [<<"127.0.0.1">>, <<"localhost">>]),
    ok.

%% 与主 fixture request/6 同款独立连接（每请求独立 httpc profile）。
isolated_request(Method, Req) ->
    Slot = claim_profile([w403_http_1, w403_http_2, w403_http_3, w403_http_4]),
    try
        {ok, Pid} = inets:start(httpc, [{profile, Slot}], stand_alone),
        try
            httpc:request(
                Method,
                Req,
                [{timeout, 120000}, {autoredirect, false}],
                [{body_format, binary}],
                Pid
            )
        after
            unlink(Pid),
            gen_server:stop(Pid, shutdown, 5000)
        end
    after
        ets:delete(?TABLE, {http_profile, Slot})
    end.

claim_profile([]) ->
    erlang:error(w403_http_concurrency_exceeded);
claim_profile([Slot | Rest]) ->
    case ets:insert_new(?TABLE, {{http_profile, Slot}, self()}) of
        true -> Slot;
        false -> claim_profile(Rest)
    end.
