-module(bot_ds).

%%%
% Bot 数据服务 / Bot data service
%
% 职责：Bot 账号编排（建 user 行 + 标 account_type=3 + 绑 bot 元数据）、
%       账号类型判定、token 认证。
% 边界：屏蔽 repo 存储细节；账号类型常量集中在此。
% 事务：create/1 三步写收进 elib_pg:with_tx 单事务（TX-01，镜像
%       ai_agent_ds:create），任一步失败整体回滚，无孤儿 user 行；
%       username 唯一约束在事务内拦截重复注册（并发恰一实体）。
%%%

-export([create/1]).
-export([is_bot/1]).
-export([find_by_token/1]).

-include("log.hrl").

%% account_type 枚举（对齐迁移 00000070 注释：0=human 1=agent 2=system_bot 3=bot）
-define(ACCOUNT_TYPE_BOT, 3).

%% ===================================================================
%% API
%% ===================================================================

%% @doc 创建 Bot：单事务【建 user 行 → 标 account_type=3 → 绑 bot 元数据】
%% Data 键：name(必填), username(必填), owner_uid(必填), description, avatar,
%%          webhook_url, api_token, verify_token, commands, permissions, events, is_public
%% TX-01 事务收敛：三步全部走 elib_pg:with_tx 单连接（镜像 ai_agent_ds:create），
%%   任一步失败整体回滚，杜绝"account_type=3 但无 bot 行"的孤儿 user
%%   （原先三步 auto-commit，绑定失败留孤儿靠管理端 upsert 重试修复）。
%%   username 唯一约束（bot_username_key）在事务内拦截：同 username 并发/重复
%%   注册恰一成功，失败方整体回滚零孤儿。
-spec create(map()) -> {ok, map()} | {error, binary()}.
create(#{name := Name, username := Username, owner_uid := OwnerUid} = Data) ->
    case validate(Data) of
        ok ->
            %% user 表主键必须用 user 命名空间生成器（与 channel_webhook_ds 同款）：
            %% 独立生成器在同节点同毫秒可与 user 生成器产出相同值 → user.id 主键冲突；
            %% 且未注册的生成器名会直接 crash（elib_tsid_generator_not_registered）。
            Uid = elib_tsid:generate(user),
            Account = <<"bot_", (ec_cnv:to_binary(Uid))/binary>>,
            BotData = #{
                user_id => Uid,
                name => Name,
                username => Username,
                owner_uid => OwnerUid,
                description => maps:get(description, Data, <<>>),
                avatar => maps:get(avatar, Data, <<>>),
                webhook_url => maps:get(webhook_url, Data, <<>>),
                api_token => maps:get(api_token, Data, <<>>),
                verify_token => maps:get(verify_token, Data, <<>>),
                commands => maps:get(commands, Data, <<"[]">>),
                permissions => maps:get(permissions, Data, <<"[]">>),
                events => maps:get(events, Data, <<"[]">>),
                is_public => maps:get(is_public, Data, false),
                status => 1
            },
            case
                elib_pg:with_tx(fun(Conn) ->
                    ok = workspace_guard:abort_on_error(
                        create_bot_user_tx(Conn, Uid, Name, Account)
                    ),
                    ok = workspace_guard:abort_on_error(
                        bind_bot_tx(Conn, BotData)
                    ),
                    #{<<"user_id">> => Uid}
                end)
            of
                %% with_tx(reraise) 成功返回裸 Fun 结果；失败归一 {error, Reason}
                #{<<"user_id">> := _} ->
                    {ok, #{<<"user_id">> => Uid}};
                {error, {bot_user, Reason}} ->
                    ?ERROR_LOG("bot_ds:create user error ~p~n", [Reason]),
                    {error, <<"创建 Bot 账号失败"/utf8>>};
                {error, {bot_bind, {unique_violation, _}}} ->
                    %% username 唯一冲突：并发/重复注册的稳定业务错误
                    {error, <<"Bot 调用名已被占用"/utf8>>};
                {error, {bot_bind, Reason}} ->
                    ?ERROR_LOG("bot_ds:create bind error ~p~n", [Reason]),
                    {error, <<"绑定 Bot 元数据失败"/utf8>>};
                {error, Reason} ->
                    ?ERROR_LOG("bot_ds:create error ~p~n", [Reason]),
                    {error, <<"创建 Bot 失败"/utf8>>}
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc 检查 user_id 是否为 Bot（account_type=3）
-spec is_bot(integer()) -> boolean().
is_bot(UserId) when UserId > 0 ->
    case user_repo:find_by_id(UserId, <<"account_type">>) of
        #{<<"account_type">> := ?ACCOUNT_TYPE_BOT} -> true;
        _ -> false
    end;
is_bot(_) ->
    false.

%% @doc 按 api_token 查找 Bot（Bot 调用 API 时认证）
-spec find_by_token(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_token(Token) ->
    case bot_repo:find_by_token(Token) of
        {ok, Row} -> {ok, Row};
        {error, notfound} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @doc 事务内创建 Bot 用户行并标记 account_type=3（TX-01）
%% create_tx 尊重传入 id（user 命名空间生成器），返回落库 id 与传入不一致
%% 即回滚；update_tx 影响 0 行同样回滚——两道 id 脱钩防御。
%% 错误统一包装 {bot_user, Reason}（镜像 ai_agent_ds:create_agent_user_tx），
%% DS create 的 case 据此映射稳定业务错误。
-spec create_bot_user_tx(any(), integer(), binary(), binary()) ->
    ok | {error, term()}.
create_bot_user_tx(Conn, BotUid, Nickname, Account) when BotUid > 0 ->
    case user_repo:create_tx(Conn, #{id => BotUid, nickname => Nickname, account => Account}) of
        {ok, BotUid} ->
            case user_repo:update_tx(Conn, BotUid, #{account_type => ?ACCOUNT_TYPE_BOT}) of
                {ok, 1} ->
                    ok;
                {ok, Other} ->
                    {error, {bot_user, {account_type_rows, Other}}};
                {error, Reason} ->
                    {error, {bot_user, Reason}}
            end;
        {ok, Other} ->
            {error, {bot_user, {id_mismatch, Other}}};
        {error, Reason} ->
            {error, {bot_user, Reason}}
    end;
create_bot_user_tx(_, _, _, _) ->
    {error, {bot_user, invalid_bot_uid}}.

%% @doc 事务内绑定 bot 元数据（第三步；失败与建号同事务回滚）。
%% username 唯一约束（bot_username_key）冲突时错误归一为
%% {bot_bind, {unique_violation, _}}（epgsql equery 失败返回 {error, #error{}}
%% 的 record 展开形态，同 user_tag_ds/auth_oidc_logic 惯例），
%% DS create 的 case 据此映射稳定业务错误"Bot 调用名已被占用"。
-spec bind_bot_tx(any(), map()) -> ok | {error, term()}.
bind_bot_tx(Conn, BotData) ->
    case bot_repo:create_tx(Conn, BotData) of
        {ok, _} ->
            ok;
        {error, {error, _Severity, <<"23505">>, unique_violation, _Msg, _Extra}} ->
            {error, {bot_bind, {unique_violation, bot_username_key}}};
        {error, Reason} ->
            {error, {bot_bind, Reason}}
    end.

%% @doc 验证 Bot 创建参数
-spec validate(map()) -> ok | {error, binary()}.
validate(Data) ->
    Name = maps:get(name, Data, <<>>),
    Username = maps:get(username, Data, <<>>),
    OwnerUid = maps:get(owner_uid, Data, 0),
    case Name of
        <<>> ->
            {error, <<"Bot 名称不能为空"/utf8>>};
        _ when byte_size(Name) > 80 -> {error, <<"Bot 名称过长"/utf8>>};
        _ ->
            case Username of
                <<>> ->
                    {error, <<"Bot 调用名不能为空"/utf8>>};
                _ when byte_size(Username) > 80 -> {error, <<"Bot 调用名过长"/utf8>>};
                _ ->
                    case OwnerUid of
                        N when is_integer(N), N > 0 -> ok;
                        _ -> {error, <<"开发者不能为空"/utf8>>}
                    end
            end
    end.
