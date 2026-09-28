%%% @doc CS-BE-02（队列摘要与等待时长）：坐席会话列表 `last_message.preview`
%%% 的服务端读面解密 + 截断投影。
%%%
%%% 方案权衡（最小侵入，零 migration、零 Facade 改动、零架构门变更）：
%%%   * 客服消息正文是服务端托管加密（enterprise_message.body_cipher，密钥
%%%     经 `imboy.eb_enterprise_keyring` 装配）。preview **不**新增明文摘要列
%%%     （migration lease 属 A0），也不引用 eb 内层加密模块（铁律 5：跨
%%%     Feature 只能引用 facade——pre-commit arch-check 硬门），而是复用
%%%     `enterprise_business_facade:list_messages/2` 的**既有读面解密**契约
%%%     （CSB-02S D5：keyring 可用时服务端解出明文体）；
%%%   * 精确取末条：`after_id => LastId − 1, limit => 1`。键集语义
%%%     （`id > after_id`，ASC，LIMIT 1）下，末条 id 是该会话最大 id，
%%%     `> LastId − 1` 的最小 id 恰为 LastId（整数域内 (LastId−1, LastId)
%%%     无其他 id；即便读取间隙新消息落库，ASC 首行仍是 LastId）；末条
%%%     已被 bounded purge 时首行 id ≠ LastId，按「消息已消失」降级占位；
%%%   * keyring 可用 ⇒ 解出明文，按 Unicode 码点截断前 64 个出站；
%%%   * keyring 不可用（env 未装配）⇒ facade 维持**密文投影**（无 body 键）
%%%     ⇒ preview 为 null——不报错、不吐半解密内容（D5 降级口径）；
%%%   * 解密失败（密文被篡改/AAD 不符——D5 以 erlang:error fail-closed）
%%%     ⇒ 本模块捕获并转 `{error, {preview_open_failed, Id, Reason}}`，
%%%     调用方整页 fail-closed，绝不夹带未验证内容；
%%%   * 基础设施不可用（exit 类：读池未起/进程崩溃）⇒ preview 降级 null
%%%     （主列表路径不受拖累；密文未解出，零内容泄漏）；
%%%   * 密文只在 enterprise 侧内存中转，customer_service 代码与坐席视图行
%%%     （cs_session_app:seat_session_view 白名单）均不接触密文材料——
%%%     出站只有截断后的明文 preview。
%%%
%%% 占位语义（preview = null）：
%%%   * 会话无消息（last_message_id 缺失）；
%%%   * 末条消息是撤回/隐藏行（visibility='hidden'——facade 行携带该列，
%%%     此处显式拦截，撤回内容零透出）；
%%%   * 附件-only 消息（明文为空二进制——BE-PATCH-01：附件消息 = 空正文
%%%     + asset_ids）；
%%%   * 末条已被 purge（facade 首行 id ≠ 末条 id）；
%%%   * keyring 不可用。
%%%
%%% 代价说明：每队列行一次 facade 单行读（PK/复合索引范围扫描，页上限 50
%%% ⇒ 每页 ≤ 50 次毫秒级查询）。批量投影需要 Facade 新面（升级触发器），
%%% 不在本卡范围。
-module(cs_message_preview).

-export([
    preview/2,
    truncate/1,
    max_chars/0
]).

-define(PREVIEW_MAX_CHARS, 64).

%% @doc 行级 preview 投影。
%%
%% `Row` 是 store 分页行（含 organization_id / workspace_id / conversation_id /
%% last_message_id）；`Params` 的 `key_ref` 是服务端/测试显式注入面（HTTP 面
%% 提交即 422，cs_http 的密钥材料键守卫），缺省由 enterprise 侧解析 env
%% keyring。
-spec preview(map(), map()) -> {ok, binary() | undefined} | {error, term()}.
preview(Row, Params) when is_map(Row), is_map(Params) ->
    case maps:get(last_message_id, Row, undefined) of
        undefined ->
            {ok, undefined};
        Id when is_integer(Id), Id > 0 ->
            preview_of(Id, Row, Params)
    end;
preview(_Row, _Params) ->
    {error, invalid_preview_row}.

preview_of(Id, Row, Params) ->
    case fetch_last_decrypted(Id, Row, Params) of
        {ok, undefined} ->
            {ok, undefined};
        {ok, Body} ->
            preview_body(Id, Body);
        {error, _} = Err ->
            Err
    end.

%% D5 的解密失败是 erlang:error（进程级 fail-closed）；队列读面把它收敛成
%% 业务错误元组，让整页走 {error,_} 通道（不夹带未验证内容、不裸崩）。
%% 基础设施不可用（exit 类：PG 池未起/进程崩溃——如零 DB 测试直驱）时
%% preview 降级 null：主列表路径不受拖累，密文未解出、零内容泄漏。
fetch_last_decrypted(Id, Row, Params) ->
    OrgId = maps:get(organization_id, Row, undefined),
    WorkspaceId = maps:get(workspace_id, Row, undefined),
    ConversationId = maps:get(conversation_id, Row, undefined),
    Query = #{
        workspace_id => WorkspaceId,
        conversation_id => ConversationId,
        after_id => Id - 1,
        limit => 1
    },
    WithKey = cs_optional_key(Query, key_ref, Params),
    try enterprise_business_facade:list_messages(OrgId, WithKey) of
        {ok, [#{id := Id, visibility := Visible, body := Body}]} when
            Visible =:= visible; Visible =:= <<"visible">>
        ->
            {ok, Body};
        {ok, [#{id := Id}]} ->
            %% hidden（撤回）或密文投影（keyring 不可用，无 body 键）。
            {ok, undefined};
        {ok, _PurgedOrRaced} ->
            %% 首行 id ≠ 末条 id：末条已被 bounded purge——按消息消失降级。
            {ok, undefined};
        {error, _} = Err ->
            Err
    catch
        error:{body_open_failed, BadId, Reason} ->
            {error, {preview_open_failed, BadId, Reason}};
        exit:_InfrastructureUnavailable ->
            {ok, undefined}
    end.

preview_body(_Id, <<>>) ->
    %% 附件-only：空正文——占位 null。
    {ok, undefined};
preview_body(_Id, Body) when is_binary(Body), byte_size(Body) > 0 ->
    {ok, truncate(Body)};
preview_body(_Id, _Other) ->
    {ok, undefined}.

%% 键存在才并入（key_ref => undefined 会污染 facade 参数面）。
cs_optional_key(Map, Key, Params) ->
    case maps:get(Key, Params, undefined) of
        undefined -> Map;
        Value -> Map#{Key => Value}
    end.

%% @doc Unicode 码点截断（前 ?PREVIEW_MAX_CHARS 个码点，UTF-8 安全——
%% 不会在多字节字符中间切断；截断策略在本函数冻结，测试逐字断言）。
%% 明文来自 HTTP/JSON 面（jsone 解码保证 UTF-8）；理论上不可解码的残留由
%% 守卫回落占位（不崩溃、不产出坏 UTF-8）。
-spec truncate(binary()) -> binary() | undefined.
truncate(Plain) when is_binary(Plain) ->
    case unicode:characters_to_list(Plain, utf8) of
        Chars when is_list(Chars) ->
            unicode:characters_to_binary(lists:sublist(Chars, ?PREVIEW_MAX_CHARS), utf8);
        _Undecodable ->
            undefined
    end;
truncate(_Other) ->
    undefined.

%% @doc 截断上限（供契约测试逐字核对）。
-spec max_chars() -> pos_integer().
max_chars() ->
    ?PREVIEW_MAX_CHARS.
