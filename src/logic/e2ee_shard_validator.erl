%%%-------------------------------------------------------------------
%%% @doc E2EE 分片传输审计日志
%%% 记录分片传输日志，用于零信任架构下的审计和异常检测。
%%%
%%% 契约以调用方实际语义为准：log_shard_transmission(Event, ShardId, Meta)
%%%   - Event: shard_created | shard_sent | shard_stored |
%%%            shard_decrypted | shard_recovered
%%%   - Meta 必含 <<"uid">>、<<"proxy_uid">>；可含 <<"key_version">> 及
%%%     其他键（shard_index/total_shards 等，收进 metadata jsonb）
%%%
%%% 历史教训：本模块曾同时存在 src/lib/（stub 空实现）与 src/logic/
%%% 两份同名源文件，后编译覆盖先编译，且调用方按 stub 签名传参，
%%% 导致审计日志从未写入过一条正确记录。合并后以此文件为唯一来源，
%%% CI 有同名模块静态检查兜底。
%%% @end
%%%-------------------------------------------------------------------
-module(e2ee_shard_validator).

%% API
-export([log_shard_transmission/3]).

-include("log.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc 记录分片传输审计日志
%% 日志写入失败不影响主流程，仅记录错误。
-spec log_shard_transmission(atom(), binary() | integer(), map()) -> ok.
log_shard_transmission(Event, ShardId, Meta) when is_atom(Event), is_map(Meta) ->
    Extra = maps:without([<<"uid">>, <<"proxy_uid">>, <<"key_version">>], Meta),
    Data = #{
        shard_id => ec_cnv:to_binary(ShardId),
        key_version => maps:get(<<"key_version">>, Meta, <<"latest">>),
        uid => ec_cnv:to_integer(maps:get(<<"uid">>, Meta)),
        proxy_uid => ec_cnv:to_integer(maps:get(<<"proxy_uid">>, Meta)),
        action => atom_to_binary(Event, utf8),
        direction => direction_for(Event),
        metadata => jsone:encode(Extra, [native_utf8]),
        created_at => elib_dt:millisecond()
    },
    case e2ee_shard_transmission_log_ds:insert(Data) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?ERROR_LOG([log_shard_transmission_failed, Event, ShardId, Reason]),
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc 事件 → 传输方向
%% 分片回传给用户（解密/恢复）为 proxy_to_user，其余为 server_to_proxy
-spec direction_for(atom()) -> binary().
direction_for(shard_decrypted) -> <<"proxy_to_user">>;
direction_for(shard_recovered) -> <<"proxy_to_user">>;
direction_for(_) -> <<"server_to_proxy">>.
