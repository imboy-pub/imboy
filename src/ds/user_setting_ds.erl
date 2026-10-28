-module(user_setting_ds).

-moduledoc "用户设置领域服务 / user setting domain service。".
%%%
% user_setting_ds 是 user_setting domain service 缩写
%%%
-export([find_by_uid/1]).
-export([chat_state_hide/1]).
-export([batch_chat_state_hide/1]).
-export([save/3]).
-export([search/1]).
-export([manual_pending_map/1]).
-export([set_manual_pending/3]).
-export([set_manual_pending/4]).
-export([bump_conversation_version/2]).
-export([bump_conversation_version_tx/3]).
-export([conversation_uk3/3]).

-include("log.hrl").

%% @doc 搜索用户设置
%%
%% 根据账号信息搜索用户设置，目前为占位实现
%%
%% @param Account 用户账号
%% @returns ok 占位返回值
-spec search(binary() | integer()) -> ok.
search(_Account) ->
    ok.

%% @doc 根据用户ID查找用户设置
%%
%% 从数据库中查找指定用户的设置信息，包括允许搜索状态等
%%
%% @param Uid 用户ID，可以是整数或base64编码的二进制
%% @returns map() 用户设置信息映射
% user_setting_ds:find_by_uid(1).
-spec find_by_uid(binary() | integer()) -> map().
find_by_uid(Uid) when is_binary(Uid) ->
    find_by_uid(ec_cnv:to_integer(Uid));
find_by_uid(Uid) ->
    Column = <<"setting">>,
    S =
        case user_setting_repo:find_by_uid(Uid, Column) of
            #{<<"setting">> := Setting} when Setting =/= <<>> ->
                try jsone:decode(Setting, [{object_format, map}]) of
                    Res ->
                        Res
                catch
                    _:_ ->
                        #{}
                end;
            _ ->
                #{}
        end,
    S#{<<"allow_search">> => fts_user_repo:allow_search(Uid)}.

%% @doc 检查用户是否隐藏在线状态
%%
%% 检查用户设置中的聊天状态是否为隐藏
%%
%% @param Uid 用户ID
%% @returns true | false true表示隐藏状态，false表示显示状态
%% ===================================================================
%% W09.a（channel-group-ux-v2.2 §6.7/J17.c）：人工待处理元数据
%% ===================================================================

%% @doc 读取 manual_pending 子对象（uk3 => #{value, version, updated_at}）。
%% 真实未读水位不依赖本元数据；旧 unreadNum 不回填历史人工意图。
-spec manual_pending_map(integer()) -> map().
manual_pending_map(Uid) ->
    case user_setting_repo:find_by_uid(Uid, <<"setting">>) of
        #{<<"setting">> := Bin} when is_binary(Bin), Bin =/= <<>> ->
            case safe_decode_setting(Bin) of
                #{<<"manual_pending">> := M} when is_map(M) -> M;
                _ -> #{}
            end;
        _ ->
            #{}
    end.

%% @doc 显式目标 true/false 设置某会话人工待处理（版本化，J17.c/d）。
%%
%% 同一业务事务：行锁（SELECT ... FOR UPDATE）→ 读 → 应用明确目标值 →
%% 推进该会话版本。无变化的同目标重试返回当前状态、不重复副作用
%% （版本不变）；目标变化才推进版本。
-spec set_manual_pending(integer(), binary(), boolean()) ->
    {ok, map()} | {conflict, map()} | {error, term()}.
set_manual_pending(Uid, Uk3, Target) ->
    %% 无期望版本（旧端语义）→ 直接应用并推进版本（J17.e）。
    set_manual_pending(Uid, Uk3, Target, undefined).

%% @doc W09.c（J17.d）：带 expected_version 的 CAS 写。
%% 期望版本与当前不符 → {conflict, CurrentEntry}（调用方回读后重试
%% 原目标，保留 desired_value，不自动取反）；无期望版本（旧端）直接
%% 应用并推进版本（J17.e 同业务入口推进版本）。
-spec set_manual_pending(
    integer(),
    binary(),
    boolean(),
    undefined | non_neg_integer()
) ->
    {ok, map()} | {conflict, map()} | {error, term()}.
set_manual_pending(Uid, Uk3, Target, ExpectedVersion) when
    is_boolean(Target), Uk3 =/= <<>>
->
    elib_pg:with_tx(fun(Conn) ->
        case user_setting_repo:find_setting_for_update_tx(Conn, Uid) of
            {ok, Setting0} ->
                Setting =
                    case Setting0 of
                        M when is_map(M) -> M;
                        _ -> #{}
                    end,
                Mp0 = maps:get(<<"manual_pending">>, Setting, #{}),
                Mp0b =
                    case is_map(Mp0) of
                        true -> Mp0;
                        _ -> #{}
                    end,
                Now = elib_dt:now(),
                Current = maps:get(Uk3, Mp0b, undefined),
                %% CAS：期望版本存在且与当前不符 → 冲突（可安全回读）。
                case {ExpectedVersion, Current} of
                    {V, Entry} when is_integer(V), is_map(Entry) ->
                        CurV = maps:get(<<"version">>, Entry, 0),
                        case CurV =:= V of
                            false ->
                                {conflict, Entry};
                            true ->
                                apply_manual_pending(
                                    Conn, Uid, Setting, Mp0b, Uk3, Target, Entry, Now
                                )
                        end;
                    {V, _Other} when is_integer(V) ->
                        %% 期望有版本但本地无条目 → 版本视 0，不符即冲突。
                        case V of
                            0 ->
                                apply_manual_pending(
                                    Conn, Uid, Setting, Mp0b, Uk3, Target, undefined, Now
                                );
                            _ ->
                                {conflict, #{<<"value">> => false, <<"version">> => 0}}
                        end;
                    _ ->
                        apply_manual_pending(
                            Conn, Uid, Setting, Mp0b, Uk3, Target, Current, Now
                        )
                end;
            {error, _} = Err ->
                Err
        end
    end);
set_manual_pending(_Uid, _Uk3, _Target, _Expected) ->
    {error, <<"人工待处理参数无效"/utf8>>}.

%% 同目标重试返回当前状态不推进版本；目标变化才推进（§6.7）。
apply_manual_pending(Conn, Uid, Setting, Mp0b, Uk3, Target, Current, Now) ->
    case Current of
        #{<<"value">> := Target} = Entry when is_map(Entry) ->
            {ok, Entry};
        Entry when is_map(Entry) ->
            V = erlang:max(0, maps:get(<<"version">>, Entry, 0)) + 1,
            write_manual_pending(Conn, Uid, Setting, Mp0b, Uk3, Target, V, Now);
        _ ->
            write_manual_pending(Conn, Uid, Setting, Mp0b, Uk3, Target, 1, Now)
    end.

write_manual_pending(Conn, Uid, Setting, Mp0b, Uk3, Target, V, Now) ->
    NewEntry = #{
        <<"value">> => Target,
        <<"version">> => V,
        <<"updated_at">> => Now
    },
    ok = user_setting_repo:upsert_setting_tx(
        Conn,
        Uid,
        Setting#{<<"manual_pending">> => maps:put(Uk3, NewEntry, Mp0b)}
    ),
    {ok, NewEntry}.

%% @doc W09.c（J17.e/D7）：旧写动作（pin/unpin/delete/restore，无版本参数）
%% 与版本推进同一业务事务推进该会话版本——新端凭 stale expected_version
%% 可检测旧写冲突。条目不存在则以 value=false、version=1 建立。
-spec bump_conversation_version(integer(), binary()) -> {ok, map()} | {error, term()}.
bump_conversation_version(Uid, Uk3) when Uk3 =/= <<>> ->
    elib_pg:with_tx(fun(Conn) ->
        bump_conversation_version_tx(Conn, Uid, Uk3)
    end);
bump_conversation_version(_Uid, _Uk3) ->
    {error, <<"人工待处理参数无效"/utf8>>}.

%% W09.c（J17.e/D7）：版本推进的同事务内核——调用方已持有业务事务 Conn
%% 时（旧写 pin/unpin/delete/restore 与版本推进原子化），在该 Conn 上
%% 行锁（SELECT ... FOR UPDATE）→ 读 → 推进该会话版本；条目不存在则以
%% value=false、version=1 建立。返回 {error,_} 令外层 with_tx 回滚。
-spec bump_conversation_version_tx(pid(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
bump_conversation_version_tx(Conn, Uid, Uk3) when Uk3 =/= <<>> ->
    case user_setting_repo:find_setting_for_update_tx(Conn, Uid) of
        {ok, Setting0} ->
            Setting =
                case Setting0 of
                    M when is_map(M) -> M;
                    _ -> #{}
                end,
            Mp0 = maps:get(<<"manual_pending">>, Setting, #{}),
            Mp0b =
                case is_map(Mp0) of
                    true -> Mp0;
                    _ -> #{}
                end,
            Now = elib_dt:now(),
            case maps:get(Uk3, Mp0b, undefined) of
                Entry when is_map(Entry) ->
                    V = erlang:max(0, maps:get(<<"version">>, Entry, 0)) + 1,
                    Target = maps:get(<<"value">>, Entry, false),
                    write_manual_pending(
                        Conn, Uid, Setting, Mp0b, Uk3, Target, V, Now
                    );
                _ ->
                    write_manual_pending(
                        Conn, Uid, Setting, Mp0b, Uk3, false, 1, Now
                    )
            end;
        {error, _} = Err ->
            Err
    end;
bump_conversation_version_tx(_Conn, _Uid, _Uk3) ->
    {error, <<"人工待处理参数无效"/utf8>>}.

%% @doc 会话 uk3 构造（与 App ConversationUk3Generator 约定一致）：
%% C2C_小id_大id；C2G_当前用户id_群id。
-spec conversation_uk3(binary(), integer(), integer()) -> binary().
conversation_uk3(<<"c2c">>, Uid, PeerId) ->
    [A, B] = lists:sort([
        elib_cnv:safe_to_binary(Uid),
        elib_cnv:safe_to_binary(PeerId)
    ]),
    <<"C2C_", A/binary, "_", B/binary>>;
conversation_uk3(<<"c2g">>, Uid, GroupId) ->
    <<"C2G_", (elib_cnv:safe_to_binary(Uid))/binary, "_",
        (elib_cnv:safe_to_binary(GroupId))/binary>>;
conversation_uk3(_Type, _Uid, _Id) ->
    <<>>.

safe_decode_setting(Bin) ->
    try
        case jsone:decode(Bin, [{object_format, map}]) of
            M when is_map(M) -> M;
            _ -> #{}
        end
    catch
        _:_ -> #{}
    end.

%% user_setting_ds:chat_state_hide(1).
-spec chat_state_hide(integer()) -> true | false.
chat_state_hide(Uid) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    case maps:get(<<"chat_state">>, Setting, <<"hide">>) of
        <<"online">> ->
            false;
        _ ->
            true
    end.

%% @doc 批量检查多个用户是否隐藏在线状态
%%
%% 【优化】批量查询用户聊天状态设置，避免 N+1 查询问题
%% 使用单次 SQL 查询获取所有用户的设置，返回 Uid => IsHide 的映射
%%
%% @param Uids 用户ID列表
%% @returns map() #{Uid => true/false} true表示隐藏状态，false表示显示状态
%% user_setting_ds:batch_chat_state_hide([1, 2, 3]).
-spec batch_chat_state_hide([integer()]) -> map().
batch_chat_state_hide([]) ->
    #{};
batch_chat_state_hide(Uids) when is_list(Uids) ->
    % 使用安全的参数化查询，一次查询获取所有用户设置
    Tb = user_setting_repo:tablename(),
    InClause = iolist_to_binary(
        lists:join(
            <<",">>,
            [<<"$", (integer_to_binary(I))/binary>> || I <- lists:seq(1, length(Uids))]
        )
    ),
    Sql =
        <<"SELECT user_id, setting FROM ", Tb/binary, " WHERE user_id IN (", InClause/binary, ")">>,

    Default = maps:from_list([{Uid, true} || Uid <- Uids]),
    case elib_pg:query(Sql, Uids) of
        {ok, Rows} ->
            lists:foldl(
                fun(Row, Acc) ->
                    Uid = maps:get(<<"user_id">>, Row),
                    SettingBin = maps:get(<<"setting">>, Row, <<>>),
                    maps:put(Uid, chat_state_hide_from_setting(SettingBin), Acc)
                end,
                Default,
                Rows
            );
        {error, _Reason} ->
            % 查询失败时按隐私安全默认值处理，所有用户均不展示在线状态
            Default
    end.

chat_state_hide_from_setting(<<>>) ->
    true;
chat_state_hide_from_setting(SettingBin) ->
    try jsone:decode(SettingBin, [{object_format, map}]) of
        Setting ->
            maps:get(<<"chat_state">>, Setting, <<"hide">>) =/= <<"online">>
    catch
        _:_ ->
            true
    end.

%% @doc 保存用户设置
%%
%% 支持多种用户设置项的保存，包括加好友方式、附近的人可见性、字体大小、聊天状态等
%%
%% 加我方式：
%%           mobile 手机号;
%%           account 账号;
%%           qrcode 二维码;
%%           group 群聊;
%%           visit_card 名片;
%%           people_nearby 附近的人
%%           recently_user 新注册的朋友
%%
%% @param Uid 用户ID
%% @param Key 设置项名称
%% @param Val 设置值
%% @returns ok 表示操作成功
% user_setting_ds:save(1, <<"add_friend_type">>, [<<"qrcode">>, <<"visit_card">>, <<"people_nearby">>]).
-spec save(binary() | integer(), binary(), term()) -> ok.
save(Uid, <<"add_friend_type">>, TypeLi) ->
    priv_save(Uid, <<"add_friend_type">>, TypeLi);
%% 设置附近的人是否可见
%% user_setting_ds:save(1, <<"people_nearby_visible">>, false).
%% user_setting_ds:save(1, <<"people_nearby_visible">>, true).
save(Uid, <<"people_nearby_visible">>, Visible) ->
    priv_save(Uid, <<"people_nearby_visible">>, Visible);
save(Uid, <<"font_size">>, State) ->
    priv_save(Uid, <<"font_size">>, State);
%% 设置聊天状态
%% State: hide online offline
%% user_setting_ds:save(CurrentUid, <<"chat_state">>, ChatState),
save(Uid, <<"chat_state">>, State) ->
    priv_save(Uid, <<"chat_state">>, State);
%% 隐私布尔开关（QA #19）：是否显示在线状态 / 允许手机号加友 / 允许二维码加友
save(Uid, <<"show_online_status">>, Visible) when is_boolean(Visible) ->
    priv_save(Uid, <<"show_online_status">>, Visible);
save(Uid, <<"allow_add_by_phone">>, Allow) when is_boolean(Allow) ->
    priv_save(Uid, <<"allow_add_by_phone">>, Allow);
save(Uid, <<"allow_add_by_qr">>, Allow) when is_boolean(Allow) ->
    priv_save(Uid, <<"allow_add_by_qr">>, Allow);
save(Uid, <<"alipay">>, State) ->
    priv_save(Uid, <<"alipay">>, State).

%% @doc 内部函数：保存用户设置
%%
%% 实际执行用户设置的更新操作
%%
%% @param Uid 用户ID
%% @param Key 设置项键名
%% @param State 设置值
%% @returns ok 表示操作成功
-spec priv_save(binary() | integer(), binary(), term()) -> ok.
priv_save(Uid, Key, State) ->
    Setting = user_setting_ds:find_by_uid(Uid),
    Setting2 = Setting#{Key => State},
    % ?DEBUG_LOG(Setting2),
    user_setting_repo:update(Uid, Setting2),
    ok.
