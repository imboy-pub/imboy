%%% W6-01 C 组（W2-01/W2-02/W2-03）追加入口，自 attachment_w403_http_fixture
%%% 按功能域拆出（主文件超 800 行上限）。签名与行为与拆分前逐字一致，仅模块
%%% 迁移；共享证据表/listener/认证由主 fixture（attachment_w403_http_fixture）
%%% 的 setup/execute 拥有，本模块只通过其导出入口复用。
-module(attachment_w403_http_fixture_w601).
%% put/2 分派入口遮蔽 erlang:put/2（与主 fixture 同款显式声明）。
-compile({no_auto_import, [put/2]}).

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
    bare_put/2,
    q/2,
    %% C10 S3-only 合同的面分派登记/字节上传（w601_w2 三套件专用）：
    register/4,
    put/2,
    upload_put/3
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
    %% V3-20：disk 驱动退役，multipart 默认命名空间随驱动改挂 garage 根。
    Ns = maps:get(multipart_ns, S0, <<"w403-garage">>),
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
        %% V3-20：disk 驱动退役，默认 driver 改为 garage。
        driver => maps:get(multipart_driver, S0, garage),
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

%% ------------------------------------------------------------------
%% C10 S3-only 合同的面分派登记 / 字节上传
%% ------------------------------------------------------------------

%% 面分派登记：ordinary/enterprise_internal 原样走主 fixture（put_url 是
%% 真 Garage 签名直传 URL）；enterprise_seat/cs_widget 走 register_eb/4——
%% C10 后 presign 响应为 opaque token descriptor（presign_view rule=
%% opaque_token_no_url_no_object_key，upload.url 已随 seat/widget 字节代理
%% 链物理退役），登记本身仍是真实 HTTP presign，会话 map 不再读取任何
%% URL 字段。
register(S, Surface, Driver, Bytes) when
    Surface =:= enterprise_seat; Surface =:= cs_widget
->
    register_eb(S, Surface, Driver, Bytes);
register(S, Surface, Driver, Bytes) ->
    ?F:register(S, Surface, Driver, Bytes).

register_eb(S0, Surface, Driver, Bytes) ->
    Leaf = maps:get(leaf, S0, elib_uuid:gen_v7()),
    S = S0#{leaf => Leaf},
    Ns = <<"w403-garage">>,
    Registry = attachment_storage:registry(),
    application:set_env(
        imboy,
        attachment_storage_registry,
        Registry#{upload_namespaces => #{<<"private">> => Ns, <<"enterprise">> => Ns}}
    ),
    Cid = maps:get(registration_cid, S, elib_uuid:gen_v7()),
    {Path, Body, Headers} = eb_registration_request(S, Surface, Bytes, Cid),
    %% 每叶先证入口拒绝无主体请求（与主 fixture register/4 同款）。
    Denied =
        ?F:request(
            S,
            post,
            Path,
            Body,
            maps:without([<<"authorization">>, <<"x-cs-visit-token">>], Headers),
            <<"application/json">>
        ),
    ?assertNot(?F:success(Denied)),
    Registered = ?F:request(S, post, Path, Body, Headers, <<"application/json">>),
    ?assert(?F:success(Registered)),
    eb_session(S, Surface, Driver, Leaf, Cid, ?F:payload(Registered), Registered).

%% seat/widget 的真实 HTTP presign 请求形状（与主 fixture
%% registration_request/4 及套件 raw_register/5 逐字同源）。
eb_registration_request(S, Surface, Bytes, Cid) ->
    Base = #{
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes),
        object_hash => eb_asset_content:sha256_hex(Bytes),
        file_name => <<"w403.txt">>,
        client_upload_id => Cid
    },
    Scope = maps:get(scope, S),
    case Surface of
        enterprise_seat ->
            {
                ?F:seat_path(S, <<"presign">>),
                Base#{
                    workspace_id => maps:get(workspace_id, Scope),
                    conversation_id => maps:get(conversation_id, Scope)
                },
                maps:get(seat_headers, S)
            };
        cs_widget ->
            {
                ?F:widget_path(S, <<"presign">>),
                Base#{installation_id => maps:get(installation_id, S)},
                maps:get(widget_headers, S)
            }
    end.

%% C10 会话 map：asset_id 从响应取，object_key/冻结位置从服务端行取
%% （与主 fixture register/4 的 seat/widget 分支同源，仅去掉 upload.url）。
eb_session(S, Surface, Driver, Leaf, Cid, Data, Registered) ->
    Id = int_of(maps:get(<<"asset_id">>, Data)),
    Scope = maps:get(scope, S),
    {ok, Row} = eb_pg_asset_meta:fetch_asset(
        maps:get(org_id, Scope), maps:get(workspace_id, Scope), Id
    ),
    K = maps:get(object_key, Row),
    Frozen = maps:from_list([
        {atom_to_binary(F), maps:get(F, Row)}
     || F <-
            [storage_driver, storage_namespace, storage_container, storage_locator_sha256]
    ]),
    {ok, Location} = attachment_storage:frozen_location(K, Frozen),
    ?assertEqual(Driver, maps:get(driver, Location)),
    true = ets:insert(?TABLE, {{object, K}, Location}),
    ok = ?F:bind_request(S, Registered, K, presign),
    UploadRef = maps:get(<<"upload_ref">>, Data),
    Base = #{upload_ref => UploadRef},
    CB =
        case Surface of
            enterprise_seat -> Base#{workspace_id => maps:get(workspace_id, Scope)};
            cs_widget -> Base#{installation_id => maps:get(installation_id, S)}
        end,
    #{
        fixture => S,
        leaf => Leaf,
        surface => Surface,
        driver => Driver,
        key => K,
        location => Location,
        %% C10 opaque token 合同：无上传 URL；字节上传走 put/2 应用链。
        upload_url => undefined,
        direct_put => false,
        headers => eb_headers(S, Surface),
        confirm_body => CB#{asset_id => Id},
        registration => Data,
        registration_request_id => maps:get(request_id, Registered),
        client_upload_id => Cid
    }.

int_of(I) when is_integer(I) -> I;
int_of(B) when is_binary(B) -> binary_to_integer(B).

%% confirm 用的面请求头（与主 fixture register/4 的 T.headers 同源）。
eb_headers(S, enterprise_seat) -> maps:get(seat_headers, S);
eb_headers(S, cs_widget) -> maps:get(widget_headers, S).

%% 面分派字节上传：seat/widget 走唯一存留的生产字节链——应用层
%% eb_asset_app:put_object（upload_ref open → hash/size/sniff 复核 →
%% fn_v5_pin_eb_content pin COMMIT → driver put_snapshot；退役前的
%% seat/widget HTTP 字节代理在服务端走的正是这一链）；其余面主 fixture
%% put/2 原样（ordinary/internal 为 S3 直传）。
put(T, Bytes) ->
    case maps:get(surface, T) of
        Ep when Ep =:= enterprise_seat; Ep =:= cs_widget -> put_eb(T, Bytes);
        _ -> ?F:put(T, Bytes)
    end.

put_eb(T, Bytes) ->
    S = maps:get(fixture, T),
    Scope = maps:get(scope, S),
    UploadRef = maps:get(upload_ref, maps:get(confirm_body, T), undefined),
    {ActorKey, ActorId} =
        case maps:get(surface, T) of
            enterprise_seat -> {actor_user_id, maps:get(actor_user_id, Scope)};
            cs_widget -> {actor_contact_id, maps:get(contact_id, Scope)}
        end,
    Started = erlang:monotonic_time(millisecond),
    %% 崩溃归一为 500 响应（退役前 HTTP 代理面由 Cowboy 兜底成 5xx；
    %% pin 已 COMMIT 的 provider 崩溃按同形 fail-closed 回给调用方）。
    Result =
        try
            enterprise_business_facade:put_object(
                maps:get(org_id, Scope),
                #{
                    workspace_id => maps:get(workspace_id, Scope),
                    upload_ref => UploadRef,
                    payload => Bytes,
                    ActorKey => ActorId
                }
            )
        catch
            Class:Reason ->
                {error, {put_crash, Class, Reason}}
        end,
    app_put_response(
        S,
        T,
        eb_put_status(Result),
        eb_put_body(Result),
        erlang:monotonic_time(millisecond) - Started
    ).

%% 应用链无 Cowboy 观察（execute/2 不经手）——按 direct_put_request 先例
%% 手工落最小证据行（不含 object_key/action——bind_request 契约），再绑定。
app_put_response(S, T, Status, Body, Elapsed) ->
    Id = <<"w403-", (elib_uuid:gen_v7())/binary>>,
    true = ets:insert(
        ?TABLE,
        {{request, maps:get(leaf, S), Id}, #{
            request_id => Id,
            route => <<"<eb-put-object>">>,
            method => <<"PUT">>,
            handler => <<"eb_asset_app">>,
            server_process => <<"<facade-call>">>,
            connection_id => <<"<app-call>">>,
            observed_at_us => erlang:monotonic_time(microsecond),
            status => Status,
            elapsed_ms => Elapsed,
            diagnostic_untrusted_locations => []
        }}
    ),
    ok = ?F:bind_request(S, #{request_id => Id}, maps:get(key, T), put),
    #{status => Status, body => Body, request_id => Id}.

%% eb 面 put_object 错误 → HTTP 状态映射：与退役前 eb_enterprise_http /
%% cs_http 的 classify 同口径（F-LAY-02 换体 hash 裁决 409、DF-2 过期
%% upload_ref 409、越权 403、形状 422，其余 fail-safe 500）。
eb_put_status({ok, _}) -> 200;
eb_put_status({error, {hash_mismatch, _, _}}) -> 409;
eb_put_status({error, {size_mismatch, _, _}}) -> 409;
eb_put_status({error, expired_upload_ref}) -> 409;
eb_put_status({error, conflict}) -> 409;
eb_put_status({error, session_not_renewable}) -> 409;
eb_put_status({error, {forbidden, _}}) -> 403;
eb_put_status({error, {invalid_argument, _}}) -> 422;
eb_put_status({error, _}) -> 500.

eb_put_body({ok, Summary}) when is_map(Summary) -> jsone:encode(Summary);
eb_put_body({error, Reason}) ->
    jsone:encode(#{<<"error">> => list_to_binary(io_lib:format("~0tp", [Reason]))});
eb_put_body(_) ->
    <<"{}">>.

%% ordinary 面的生产字节链（C10 后唯一带 pin 事务边界的 ordinary 入口）：
%% POST /api/v1/attachment/upload（multipart/form-data）——服务端
%% attachment_upload_logic:pin_and_put 先 COMMIT register_content_pin、
%% 再派发 provider PUT（冻结签名窗=会话 expiry）。T 可来自任一 ordinary
%% 登记（presign 或 multipart presign；端点按 object_key 定位会话）。
upload_put(S, T, Bytes) ->
    Boundary = <<"w601multipart7f3a">>,
    Q = uri_string:compose_query([
        {<<"object_key">>, maps:get(key, T)},
        {<<"mime_type">>, <<"text/plain">>}
    ]),
    Response =
        ?F:request(
            S,
            post,
            <<"/api/v1/attachment/upload?", Q/binary>>,
            multipart_body(Boundary, Bytes),
            maps:get(headers, T),
            <<"multipart/form-data; boundary=", Boundary/binary>>
        ),
    ok = ?F:bind_request(S, Response, maps:get(key, T), put),
    Response.

multipart_body(Boundary, Bytes) ->
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
    >>.

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

%% 裸 PUT（C10 S3-only）：剥离签名查询串的直传 URL——S3 侧认证缺失拒绝
%% （400/401/403，F11）；gateway/capability 通道已物理退役。
bare_put(T, Bytes) ->
    Stale = unsigned_url(maps:get(upload_url, T)),
    S = maps:get(fixture, T),
    Headers =
        case maps:get(surface, T) of
            enterprise_seat -> maps:get(headers, T);
            _ -> #{}
        end,
    ?F:request(S, put, Stale, Bytes, Headers, <<"text/plain">>).

unsigned_url(Url) ->
    case binary:split(Url, <<"?">>) of
        [Path, _Query] -> Path;
        _ -> Url
    end.

%% SQL 直通（套件内持久状态断言用；只读查询为主）。
q(Sql, Args) -> elib_pg:query(Sql, Args).

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
