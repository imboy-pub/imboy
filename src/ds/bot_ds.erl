-module(bot_ds).

%%%
% Bot 数据服务 / Bot data service
%
% 职责：Bot 账号编排（建 user 行 + 标 account_type=3 + 绑 bot 元数据）、
%       账号类型判定、token 认证。
% 边界：屏蔽 repo 存储细节；账号类型常量集中在此。
% 范式：建 bot 账号镜像 channel_webhook_ds:create_bot_user（建 user 行 + 标 account_type）。
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

%% @doc 创建 Bot：新建 user 账号 → 标 account_type=3 → 绑定 bot 元数据
%% Data 键：name(必填), username(必填), owner_uid(必填), description, avatar,
%%          webhook_url, api_token, verify_token, commands, permissions, events, is_public
%% ponytail: 三步非单事务（channel_webhook_ds 同款取舍）；bot 绑定失败会留下
%%   account_type=3 但无 bot 行的孤儿 user，管理端可 upsert 重试修复。
-spec create(map()) -> {ok, map()} | {error, binary()}.
create(#{name := Name, username := Username, owner_uid := OwnerUid} = Data) ->
    case validate(Data) of
        ok ->
            %% user 表主键必须用 user 命名空间生成器（与 channel_webhook_ds 同款）：
            %% 独立生成器在同节点同毫秒可与 user 生成器产出相同值 → user.id 主键冲突；
            %% 且未注册的生成器名会直接 crash（elib_tsid_generator_not_registered）。
            Uid = elib_tsid:generate(user),
            Account = <<"bot_", (ec_cnv:to_binary(Uid))/binary>>,
            case create_bot_user(Uid, Name, Account) of
                ok ->
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
                    case bot_repo:create(BotData) of
                        {ok, _} ->
                            {ok, #{<<"user_id">> => Uid}};
                        {error, Reason} ->
                            ?ERROR_LOG("bot_ds:create bind error ~p~n", [Reason]),
                            {error, <<"绑定 Bot 元数据失败"/utf8>>}
                    end;
                {error, Reason} ->
                    ?ERROR_LOG("bot_ds:create user error ~p~n", [Reason]),
                    {error, <<"创建 Bot 账号失败"/utf8>>}
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

%% @doc 创建 Bot 用户行并标记 account_type=3
-spec create_bot_user(integer(), binary(), binary()) -> ok | {error, term()}.
create_bot_user(BotUid, Nickname, Account) when BotUid > 0 ->
    case user_repo:create(#{id => BotUid, nickname => Nickname, account => Account}) of
        ok ->
            case user_repo:update(BotUid, #{account_type => ?ACCOUNT_TYPE_BOT}) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
create_bot_user(_, _, _) ->
    {error, invalid_bot_uid}.

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
