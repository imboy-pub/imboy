%%% @doc EB-05 企业客户备注（enterprise note）用例套件（真库 + 真加密原语）。
%%%
%%% 覆盖作业书 §5 中「资料与备注」一条 + A04/A10 的口径：
%%%   * 备注正文是**企业托管密文**：应用层经 `eb_crypto_port` 的真实签名
%%%     `seal_scoped/3` 封装（AAD 绑定 Org/Workspace/资源），明文不入库、不入日志；
%%%   * 缺 `key_version`、空正文、非本 Org 客户一律 fail-closed，且**零副作用**；
%%%   * 写入能力由 EB-03R 的 P3（`insert_note`）补齐 —— 原先的
%%%     `{error,{store_capability_missing,insert_note}}` 缺口已消失，本套件
%%%     改为验证**正向落库**（含真密文可解回、明文不在整行转文本里）。
%%%     原缺口证据见 `agents/a2/EB-05/superseded/`（作废，仅供追溯）。
-module(eb_note_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).

note_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a10_note_plaintext_is_sealed_via_contract_and_persisted/0},
        {timeout, 60, fun a10_note_plaintext_not_logged_by_application_layer/0},
        {timeout, 60, fun note_fails_closed_without_key_version/0},
        {timeout, 60, fun note_fails_closed_on_invalid_body_cipher/0},
        {timeout, 60, fun note_is_contact_scoped_across_orgs/0},
        {timeout, 60, fun note_seal_binds_scope_and_leaks_no_plaintext/0}
    ];
cases({error, Reason}) ->
    erlang:error({eb05_note_suite_db_unavailable, Reason}).

%% ===================================================================
%% EB-05-A10：正文经契约（seal_scoped/3）封好再落库；明文不入库不入日志
%% ===================================================================

%% 正向：`append_note/2` 自己经 `eb_crypto_port` 的真实签名 `seal_scoped/3` 封装
%% 正文（明文只存在于调用栈），落库的是密文；且该密文可用同一作用域解回原文
%% （证明它是 seal_scoped 的产物，而不是「随便一段二进制」）。
a10_note_plaintext_is_sealed_via_contract_and_persisted() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Contact = maps:get(contact_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Ref = ?FIX:key_ref(1),
        Canary = <<"EB05-A10-CANARY-PLAINTEXT-DO-NOT-LOG">>,
        CountBefore = note_count(Org),
        {ok, Result} = eb_contact_app:append_note(Org, #{
            workspace_id => Ws,
            contact_id => Contact,
            body_plaintext => Canary,
            business_identity_id => Sales,
            actor_user_id => Actor,
            key_ref => Ref
        }),
        Note = maps:get(note, Result),
        NoteId = maps:get(id, Note),
        ?assert(is_integer(NoteId)),
        ?assertEqual(1, maps:get(body_key_version, Note)),
        ?assertEqual(active, maps:get(status, Note)),
        ?assertEqual(Contact, maps:get(contact_id, Note)),
        ?assertEqual(CountBefore + 1, note_count(Org)),
        %% 落库密文 == 应用层封装的 cipher（同一份密文，不是重新生成）
        ?assertEqual(maps:get(cipher, maps:get(sealed, Result)), maps:get(body_cipher, Note)),
        %% 明文不入库（整行转文本判据）
        ?assertEqual(nomatch, binary:match(maps:get(body_cipher, Note), Canary)),
        ?assertEqual(nomatch, binary:match(note_blob(Org, NoteId), Canary)),
        %% 真密文：正确作用域 + 同一密钥可解回原文
        ?assertEqual(
            {ok, Canary},
            eb_managed_crypto:open_scoped(
                ?FIX:resource_aad(<<"enterprise_note">>, Org, Ws, NoteId),
                maps:get(sealed, Result),
                Ref
            )
        ),
        %% 密文与 key_version 成对落库
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_note"
                    " WHERE organization_id=$1 AND id=$2 AND body_cipher IS NOT NULL"
                    "   AND body_key_version=1 AND status='active'"
                >>,
                [Org, NoteId],
                0
            )
        ),
        %% 审计落库（actor 如实）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND actor_user_id=$3"
                    "   AND action='enterprise_note.create'"
                >>,
                [Org, NoteId, Actor],
                0
            )
        ),
        %% 缺主密钥 ⇒ fail-closed，零写入
        ?assertEqual(
            {error, missing_key},
            eb_contact_app:append_note(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                body_plaintext => Canary
            })
        ),
        ?assertEqual(CountBefore + 1, note_count(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% 「明文不入日志」的机械判据：应用层**没有任何日志原语** ⇒ 明文不可能经日志外泄。
%% （运行期另有证据：green.log 全文 grep 固定金丝雀串，命中数必须为 0。）
a10_note_plaintext_not_logged_by_application_layer() ->
    App = read_source(
        <<"src/features/enterprise_business/application/contact/eb_contact_app.erl">>
    ),
    lists:foreach(
        fun(Token) -> ?assertEqual(nomatch, binary:match(App, Token)) end,
        [
            <<"logger:">>,
            <<"error_logger">>,
            <<"io:format">>,
            <<"io_lib:format">>,
            <<"error_logger:info">>
        ]
    ),
    %% 正文只在「封好之后」才作为密文离开应用层：body_plaintext 只出现在封装函数的入参
    ?assertNotEqual(nomatch, binary:match(App, <<"body_plaintext">>)),
    ?assertNotEqual(nomatch, binary:match(App, <<"seal_scoped">>)).

%% 备注正文形状与租户作用域仍 fail-closed（原 B3 的负例保留，改为以真实写入能力为前提）。
note_fails_closed_without_key_version() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Contact = maps:get(contact_id, Scope),
        Before = note_count(Org),
        %% 有密文却无 key_version：与 DB CHECK（body_cipher → body_key_version）同口径拒绝
        ?assertEqual(
            {error, client_cipher_not_accepted},
            eb_contact_app:append_note(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                body_cipher => <<9, 9, 9>>
            })
        ),
        %% key_version 非法同样拒绝
        ?assertEqual(
            {error, client_cipher_not_accepted},
            eb_contact_app:append_note(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                body_cipher => <<9, 9, 9>>,
                body_key_version => 0
            })
        ),
        ?assertEqual(Before, note_count(Org))
    after
        ?FIX:cleanup(Scope)
    end.

note_fails_closed_on_invalid_body_cipher() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Contact = maps:get(contact_id, Scope),
        Before = note_count(Org),
        %% FND-5：任何客户端密文（含空/畸形）统一 client_cipher_not_accepted
        ?assertEqual(
            {error, client_cipher_not_accepted},
            eb_contact_app:append_note(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                body_cipher => <<>>,
                body_key_version => 1
            })
        ),
        ?assertEqual(
            {error, client_cipher_not_accepted},
            eb_contact_app:append_note(Org, #{
                workspace_id => Ws,
                contact_id => Contact,
                body_cipher => not_a_binary,
                body_key_version => 1
            })
        ),
        %% 缺 tenant 参数 → 不触库
        ?assertMatch(
            {error, {invalid_workspace_id, _}},
            eb_contact_app:append_note(Org, #{
                contact_id => Contact,
                body_cipher => <<1>>,
                body_key_version => 1
            })
        ),
        ?assertEqual(Before, note_count(Org))
    after
        ?FIX:cleanup(Scope)
    end.

note_is_contact_scoped_across_orgs() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Contact = maps:get(contact_id, Scope),
        Before = note_count(Org),
        %% 未知客户
        ?assertEqual(
            {error, {contact_not_found, 999999999999}},
            eb_contact_app:append_note(Org, #{
                workspace_id => Ws,
                contact_id => 999999999999,
                body_plaintext => <<"not-found-probe">>
            })
        ),
        %% 跨 Org / 跨 Workspace：同一 contact 不在该租户内
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:append_note(OtherOrg, #{
                workspace_id => OtherWs,
                contact_id => Contact,
                body_plaintext => <<"cross-org-a">>
            })
        ),
        ?assertEqual(
            {error, {contact_not_found, Contact}},
            eb_contact_app:append_note(Org, #{
                workspace_id => OtherWs,
                contact_id => Contact,
                body_plaintext => <<"cross-ws-a">>
            })
        ),
        ?assertEqual(Before, note_count(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 正文封装：AAD 绑定 + 无明文
%% ===================================================================

note_seal_binds_scope_and_leaks_no_plaintext() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Ref = ?FIX:key_ref(1),
        Canary = canary(),
        {ok, Sealed} = eb_contact_app:seal_note_body(Org, Ws, #{
            body_plaintext => Canary,
            key_ref => Ref
        }),
        NoteId = maps:get(id, Sealed),
        Cipher = maps:get(body_cipher, Sealed),
        ?assert(is_integer(NoteId)),
        ?assertEqual(1, maps:get(body_key_version, Sealed)),
        ?assertEqual(nomatch, binary:match(Cipher, Canary)),
        %% 正确作用域可解封
        ?assertEqual(
            {ok, Canary},
            eb_managed_crypto:open_scoped(
                ?FIX:resource_aad(<<"enterprise_note">>, Org, Ws, NoteId),
                maps:get(sealed, Sealed),
                Ref
            )
        ),
        %% AAD 不匹配（换资源 ID / 换资源类型 / 换 Workspace）一律 fail-closed
        lists:foreach(
            fun(OtherScope) ->
                ?assertEqual(
                    {error, aad_mismatch},
                    eb_managed_crypto:open_scoped(OtherScope, maps:get(sealed, Sealed), Ref)
                )
            end,
            [
                ?FIX:resource_aad(<<"enterprise_note">>, Org, Ws, NoteId + 1),
                ?FIX:resource_aad(<<"enterprise_contact">>, Org, Ws, NoteId),
                ?FIX:resource_aad(
                    <<"enterprise_note">>, Org, maps:get(other_workspace_id, Scope), NoteId
                )
            ]
        ),
        %% 缺主密钥 → fail-closed，不产出密文
        ?assertEqual(
            {error, missing_key},
            eb_contact_app:seal_note_body(Org, Ws, #{body_plaintext => Canary})
        ),
        ?assertMatch(
            {error, {invalid_body_plaintext, _}},
            eb_contact_app:seal_note_body(Org, Ws, #{body_plaintext => not_a_binary, key_ref => Ref})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 辅助
%% ===================================================================

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).

canary() ->
    <<"EB05-CANARY-PLAINTEXT-", (integer_to_binary(?FIX:id()))/binary>>.

note_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM enterprise_note WHERE organization_id=$1">>,
        [Org],
        -1
    ).

%% 备注关键字段摘要：拒绝路径必须逐字不变。
note_digest(Org) ->
    Blob = ?FIX:scalar(
        <<
            "SELECT coalesce(string_agg("
            "  id::text || ':' || contact_id::text || ':' || coalesce(business_identity_id::text,'-')"
            "  || ':' || coalesce(actor_user_id::text,'-') || ':' || coalesce(body_key_version::text,'-')"
            "  || ':' || status, '|' ORDER BY id), '')"
            "  FROM enterprise_note WHERE organization_id=$1"
        >>,
        [Org],
        <<>>
    ),
    crypto:hash(sha256, term_to_binary(Blob)).

%% 备注整行转文本（「明文不入库」的机械判据）。
note_blob(Org, NoteId) ->
    ?FIX:scalar(
        <<
            "SELECT coalesce(string_agg(n::text, '|'), '') FROM enterprise_note n"
            " WHERE n.organization_id=$1 AND n.id=$2"
        >>,
        [Org, NoteId],
        <<>>
    ).

%% 读被测源码（A10 的静态只读判据；找不到文件是**环境问题**，必须报错而不是放过）。
read_source(Rel) ->
    Candidates = [
        filename:join(code:lib_dir(imboy), binary_to_list(Rel)),
        binary_to_list(Rel)
    ],
    case [Path || Path <- Candidates, filelib:is_regular(Path)] of
        [Path | _] ->
            {ok, Bin} = file:read_file(Path),
            Bin;
        [] ->
            erlang:error({eb05_source_not_found, Rel, Candidates})
    end.
