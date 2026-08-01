-module(attach_pending_repo).
%%%
% 已签发 presigned PUT 但尚未 confirm 的对象登记表（迁移 00000054）
%
% 存在意义见 #20：attach_logic:presign/5 原来签发后不落任何库行，
% 而 attachment_repo:orphan_list_for_delete/1 只扫 attachment 表。
% 于是"PUT 上去但从不 confirm"的对象在库里根本不存在，清理器看不见它，
% 占用的空间永远收不回来。本表补上这条登记。
%
% 生命周期：presign 写入 → confirm 删除 → 超龄未删 = 从未 confirm，由
% attach_cleanup_logic 连同 S3 对象一并回收。
%%%

-export([add/4]).
-export([remove/1]).
-export([list_expired/1]).
-export([delete_by_keys/1]).

-include("log.hrl").

tablename() ->
    elib_pg_sql:public_tablename(<<"attach_pending">>).

%% @doc 登记一次 presign 签发。
%%
%% ON CONFLICT DO UPDATE 而非 DO NOTHING：同一 object_key 被重新签发时
%% （客户端重试 / 换 MIME 重来），过期计时必须从最后一次签发算起，
%% 否则会按第一次的时间把正在上传的对象提前删掉。
-spec add(binary(), binary(), binary(), integer()) -> ok | {error, term()}.
add(ObjectKey, Bucket, Scope, Uid) ->
    Tb = tablename(),
    Sql = [
        <<"INSERT INTO ">>,
        Tb,
        <<
            " (object_key, bucket, scope, creator_user_id, created_at)"
            " VALUES ($1, $2, $3, $4, NOW())"
            " ON CONFLICT (object_key) DO UPDATE SET"
            " bucket = EXCLUDED.bucket,"
            " scope = EXCLUDED.scope,"
            " creator_user_id = EXCLUDED.creator_user_id,"
            " created_at = NOW()"
        >>
    ],
    normalize(elib_pg:execute(Sql, [ObjectKey, Bucket, Scope, Uid])).

%% @doc confirm 成功后销账。对象已登记进 attachment 表，不再是待确认状态。
-spec remove(binary()) -> ok | {error, term()}.
remove(ObjectKey) ->
    Tb = tablename(),
    Sql = [<<"DELETE FROM ">>, Tb, <<" WHERE object_key = $1">>],
    normalize(elib_pg:execute(Sql, [ObjectKey])).

%% @doc 列出超龄仍未 confirm 的登记（单批上限 500，与 orphan 清理一致）。
%%
%% 单位是**小时**不是天：presign 有效期只有分钟级，超过若干小时还没 confirm
%% 就是垃圾。attachment 的 orphan 清理用 30 天 + 7 天下限，那是保护"已确认但
%% 暂未被引用"的正常附件，两者语义不同不可混用。
-spec list_expired(integer()) -> {ok, [map()]} | {error, term()}.
list_expired(AgeHours) ->
    Tb = tablename(),
    AttachTb = elib_pg_sql:public_tablename(<<"attachment">>),
    Sql = [
        <<"SELECT p.object_key, p.bucket FROM ">>,
        Tb,
        <<" p WHERE p.created_at < NOW() - ($1 * INTERVAL '1 hour')">>,
        %% 关键守卫：已经登记进 attachment 表的对象一律不碰。
        %% confirm 成功后销账（pending_remove）如果失败，pending 行会残留，
        %% 少了这个条件就会把一个**已转正的附件**连同 S3 对象删掉 —— 那是
        %% 真正的数据丢失，比"收不回垃圾"严重得多。宁可漏收，不可误删。
        <<" AND NOT EXISTS (SELECT 1 FROM ">>,
        AttachTb,
        <<" a WHERE a.path = p.object_key)">>,
        <<" ORDER BY p.created_at ASC LIMIT 500">>
    ],
    elib_pg:query(Sql, [AgeHours]).

%% @doc 按 object_key 批量删除登记行（S3 对象删除成功后调用）
-spec delete_by_keys([binary()]) -> ok | {error, term()}.
delete_by_keys([]) ->
    ok;
delete_by_keys(Keys) ->
    Tb = tablename(),
    Placeholders = iolist_to_binary(
        lists:join(
            <<",">>,
            [[<<"$">>, integer_to_binary(I)] || I <- lists:seq(1, length(Keys))]
        )
    ),
    Sql = [
        <<"DELETE FROM ">>,
        Tb,
        <<" WHERE object_key IN (">>,
        Placeholders,
        <<")">>
    ],
    normalize(elib_pg:execute(Sql, Keys)).

%% elib_pg:execute/2 的成功返回有 {ok, N} 与 {ok, N, Rows} 两种形态
normalize({ok, _}) -> ok;
normalize({ok, _, _}) -> ok;
normalize({error, R}) -> {error, R}.
