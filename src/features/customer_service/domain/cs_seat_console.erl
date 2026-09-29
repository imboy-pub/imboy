%%% @doc 坐席控制台嵌入的领域纯函数（seat-console-embed SC-BE；application
%%% `cs_seat_console_app` 的判定真源）。
%%%
%%% 纯净性（铁律 4）：无 I/O、无进程、无隐式时间/随机源。Origin 判定**复用**
%%% domain `cs_widget:normalize_origin/1`（scheme+host+port 归一、六禁形状门
%%% 的唯一实现点——本模块不复制任何 origin 规则，只做数量/字节上限与保序
%%% 去重的编排）。
%%%
%%% 冻结的判定：
%%%   * origin 白名单上限：单条归一前 ≤255 字节、总条数 ≤10、总字节 ≤1024——
%%%     防 CSP 头注入膨胀与配置爆炸；非法形状 fail-closed（`{error,
%%%     {invalid_origin, V}}` 原样上抛，不做容错截断）；
%%%   * public_seat_console_id 形状门：1..128 字节、字符集 [A-Za-z0-9_-]——
%%%     路径绑定不可信，控制字符/引号/空白在进 store 前即拒；
%%%   * 管理面投影白名单：逐字冻结（`public_view/1`）——created_by_user_id /
%%%     revoked_at 等白名单外键不出 application 层；
%%%   * 嵌入面（/seat/:id）投影只含公开 id 与归一 origin 名单（`frame_view/1`）
%%%     ——零 org/workspace/secret（HTML 壳合同）。
-module(cs_seat_console).

-export([
    normalize_and_dedupe_origins/1,
    valid_public_seat_console_id/1,
    public_view/1,
    frame_view/1
]).

%% origin 白名单上限（CSP frame-ancestors 头长度的服务端上界）。
-define(MAX_ORIGINS, 10).
-define(MAX_ORIGIN_BYTES, 255).
-define(MAX_TOTAL_BYTES, 1024).

%% 管理面投影白名单：**逐字**（行里其余键一律不出 application 层）。
-define(PUBLIC_PROJECTION, [
    id,
    organization_id,
    workspace_id,
    public_seat_console_id,
    allowed_origins,
    status,
    version,
    created_at,
    updated_at
]).

%% ===================================================================
%% Origin 归一化 + 保序去重 + 上限门
%% ===================================================================

%% @doc 归一化 origin 列表（复用 `cs_widget:normalize_origin/1` 六禁形状门），
%% 保序去重（首次出现位置保留），并强制三条上限：
%%   * 单条归一前 >255 字节 → `{error, {origin_too_long, V}}`；
%%   * 总条数 >10 → `{error, {too_many_origins, N}}`；
%%   * 归一后总字节 >1024 → `{error, {origins_total_too_large, N}}`。
%%
%% 输入非 list → `{error, {invalid_argument, allowed_origins}}`（与
%% cs_widget_app:normalize_origins 同口径；空列表由调用方按用例语义裁决）。
-spec normalize_and_dedupe_origins(term()) -> {ok, [binary()]} | {error, term()}.
normalize_and_dedupe_origins(Origins) when is_list(Origins) ->
    case too_long_entry(Origins) of
        {error, _} = Err ->
            Err;
        ok ->
            normalize_loop(Origins, [])
    end;
normalize_and_dedupe_origins(_Origins) ->
    {error, {invalid_argument, allowed_origins}}.

too_long_entry([O | Rest]) when is_binary(O) ->
    case byte_size(O) > ?MAX_ORIGIN_BYTES of
        true -> {error, {origin_too_long, O}};
        false -> too_long_entry(Rest)
    end;
too_long_entry([O | _Rest]) ->
    {error, {invalid_origin, O}};
too_long_entry([]) ->
    ok.

normalize_loop(Origins, _Acc) when length(Origins) > ?MAX_ORIGINS ->
    {error, {too_many_origins, length(Origins)}};
normalize_loop([], Acc) ->
    Deduped = dedupe(lists:reverse(Acc), []),
    case byte_size_of(Deduped, 0) of
        Total when Total > ?MAX_TOTAL_BYTES ->
            {error, {origins_total_too_large, Total}};
        _ ->
            {ok, Deduped}
    end;
normalize_loop([Origin | Rest], Acc) ->
    case cs_widget:normalize_origin(Origin) of
        {ok, Normalized} -> normalize_loop(Rest, [Normalized | Acc]);
        {error, _} = Err -> Err
    end.

%% 保序去重：首次出现位置保留（`lists:usort` 会破坏顺序，不适用——CSP 头的
%% origin 顺序是配置可读性的一部分，稳定输出是合同面）。
dedupe([], Acc) ->
    lists:reverse(Acc);
dedupe([O | Rest], Acc) ->
    case lists:member(O, Acc) of
        true -> dedupe(Rest, Acc);
        false -> dedupe(Rest, [O | Acc])
    end.

byte_size_of([], N) ->
    N;
byte_size_of([O | Rest], N) ->
    byte_size_of(Rest, N + byte_size(O)).

%% ===================================================================
%% public_seat_console_id 形状门
%% ===================================================================

%% @doc 公开控制台 ID 形状门：非空、≤128 字节、字符集 [A-Za-z0-9_-]。
%% 路径绑定不可信——控制字符/引号/空白/路径穿越形态在进 store 前即拒。
-spec valid_public_seat_console_id(term()) -> boolean().
valid_public_seat_console_id(PublicId) when is_binary(PublicId) ->
    byte_size(PublicId) > 0 andalso
        byte_size(PublicId) =< 128 andalso
        lists:all(fun public_id_char/1, binary_to_list(PublicId));
valid_public_seat_console_id(_PublicId) ->
    false.

public_id_char(C) when C >= $a, C =< $z -> true;
public_id_char(C) when C >= $A, C =< $Z -> true;
public_id_char(C) when C >= $0, C =< $9 -> true;
public_id_char($_) -> true;
public_id_char($-) -> true;
public_id_char(_) -> false.

%% ===================================================================
%% 投影白名单
%% ===================================================================

%% @doc 管理面投影（白名单逐字冻结）：id / organization_id / workspace_id /
%% public_seat_console_id / allowed_origins / status / version / created_at /
%% updated_at。created_by_user_id / revoked_at 等白名单外键不出本层。
-spec public_view(map()) -> map().
public_view(Row) when is_map(Row) ->
    maps:with(?PUBLIC_PROJECTION, Row);
public_view(_Other) ->
    #{}.

%% @doc 嵌入面（/seat/:id）投影：只含公开 id 与归一 origin 名单——零
%% organization_id / workspace_id / secret（HTML 壳合同：壳内不得出现租户键）。
%% 行的 status 门由 application 裁决后才进本函数（此处不再判 status）。
-spec frame_view(map()) -> map().
frame_view(Row) when is_map(Row) ->
    #{
        public_seat_console_id => maps:get(public_seat_console_id, Row, <<>>),
        allowed_origins => normalized_allowed_origins(maps:get(allowed_origins, Row, []))
    };
frame_view(_Other) ->
    #{}.

%% 存量行的 origin 二次归一（写入侧已归一；此处是读出面的纵深防御——
%% 归一失败的存量条目 fail-closed 丢弃，绝不进 CSP 头）。
normalized_allowed_origins(Origins) when is_list(Origins) ->
    lists:filtermap(
        fun(Raw) ->
            case cs_widget:normalize_origin(Raw) of
                {ok, Norm} -> {true, Norm};
                {error, _} -> false
            end
        end,
        Origins
    );
normalized_allowed_origins(_Other) ->
    [].
