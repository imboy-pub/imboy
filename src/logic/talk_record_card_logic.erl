-module(talk_record_card_logic).
-moduledoc "面聊记录分享卡片逻辑（T17）：受控定位数据生成。".
%%%
% share-card 域业务逻辑（§4 POST /:id/share-card + §11.3 最小暴露）。
%
%   * 权限：owner 或可读者（visible 谓词——editor/viewer 均可为读者
%     生成定位；无权限/不存在统一 404）；
%   * 只生成受控 talk_record_card 定位数据：记录 ID + 最小标题策略，
%     不额外授权（无 grant 写入）、不发消息（无任何消息通道调用——
%     发送行为由用户在 IM 确认，§4）；
%   * 最小标题策略：严格模式（默认，config
%     {imboy, talk_record_share_card_strict_title} 可关）只发「面聊记录」
%     定位——卡片正文/转发不泄漏记录标题（§4「严格模式只发『面聊记录』
%     定位」+「发卡片后不能靠卡片正文泄露完整摘要」）；非严格模式回
%     记录标题（部署策略显式放开才生效，能力关闭不能仅 UI 禁用）；
%   * 接收者打开卡片仍需 ACL（受控路由 /talk-records/:id 为未来 App
%     路由；未登录先登录再回原定位，§4）——本端点不产生任何访问凭证。
%%%

-export([share_card/2, strict_title_enabled/0]).

-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

-define(DEFAULT_STRICT_TITLE, true).

%% ===================================================================
%% POST /:id/share-card
%% ===================================================================

%% @doc 生成受控定位数据。返回 {ok, Payload} | {error, Reason}。
%% Payload：{card_type, record_id, title, route}——title 按严格/最小
%% 标题策略；route 为受控定位路由（不含任何票据/凭证）。
-spec share_card(integer(), integer()) -> {ok, map()} | {error, term()}.
share_card(Uid, RecordId) ->
    case talk_record_content_common:visible_or_404(Uid, RecordId) of
        {ok, Row} ->
            Payload = card_payload(RecordId, Row),
            %% 审计：定位数据签发留痕（metadata 只含策略位，不含标题）
            _ = talk_record_repo:insert_audit(
                RecordId,
                Uid,
                <<"share_card_issue">>,
                null,
                #{<<"strict_title">> => strict_title_enabled()}
            ),
            {ok, Payload};
        {error, Reason} ->
            {error, Reason}
    end.

-spec card_payload(integer(), map()) -> map().
card_payload(RecordId, Row) ->
    #{
        <<"card_type">> => <<"talk_record_card">>,
        <<"record_id">> => RecordId,
        <<"title">> => card_title(Row),
        <<"route">> => <<"/talk-records/", (integer_to_binary(RecordId))/binary>>
    }.

%% @private 严格模式只发「面聊记录」定位（§4）；非严格回记录标题。
-spec card_title(map()) -> binary().
card_title(Row) ->
    case strict_title_enabled() of
        true -> <<"面聊记录"/utf8>>;
        false -> title_or_generic(Row)
    end.

-spec title_or_generic(map()) -> binary().
title_or_generic(#{<<"title">> := Title}) when is_binary(Title), Title =/= <<>> ->
    Title;
title_or_generic(_) ->
    <<"面聊记录"/utf8>>.

%% @doc 严格标题开关（默认 true；config 覆写须显式传 boolean false）。
-spec strict_title_enabled() -> boolean().
strict_title_enabled() ->
    case config_ds:env(imboy, talk_record_share_card_strict_title, ?DEFAULT_STRICT_TITLE) of
        false -> false;
        _ -> true
    end.
