%%% @doc V5 统一存储边界 driver behaviour（W4-03）。
%%% 每个回调的 Namespace 先经 attachment_storage:resolve/1 归一（未知
%%% namespace fail-closed）；locator digest 由 attachment_storage:identity/3
%%% 统一计算（driver 不自行拼接定位语义）。
-module(attachment_storage_driver).

-callback put(
    Namespace :: binary(),
    Container :: binary(),
    Key :: binary(),
    Body :: binary() | {file, file:filename_all()}
        | {upload_spool, pid(), non_neg_integer(), binary()}
) ->
    ok | {error, term()}.

-callback head(Namespace :: binary(), Container :: binary(), Key :: binary()) ->
    {ok, #{size := non_neg_integer(), content_type := binary()}}
    | {error, not_found | term()}.

-callback delete(
    Namespace :: binary(),
    Container :: binary(),
    Key :: binary()
) ->
    ok | {error, not_found | term()}.

-callback presign_put(
    Namespace :: binary(),
    Container :: binary(),
    Key :: binary(),
    ExpiresSeconds :: pos_integer()
) ->
    {ok, binary()} | {error, term()}.

-callback presign_get(
    Namespace :: binary(),
    Container :: binary(),
    Key :: binary(),
    ExpiresSeconds :: pos_integer()
) ->
    {ok, binary()} | {error, term()}.

-callback read(
    Namespace :: binary(),
    Container :: binary(),
    Key :: binary()
) ->
    {ok, binary()} | {error, not_found | term()}.

-export_type([namespace/0, container/0]).
-type namespace() :: binary().
-type container() :: binary().
