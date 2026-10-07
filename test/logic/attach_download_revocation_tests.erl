-module(attach_download_revocation_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% Q06（AC13-R1/R3）：附件撤权矩阵与票据时钟边界
%%%
%%% 覆盖设计文档 G13-1/G13-2：
%%% - 撤权后已签发 ticket 的真实验证：download_authorized 对 group/channel
%%%   每次 HTTP（含 stream_start 后）复跑 authorize 的机制本体在
%%%   attach_logic:download/1——被移出群后，撤权前签发的旧 ticket 必须
%%%   403 语义（{error, forbidden}），新签发也必须拒绝。
%%% - ticket TTL=600s 时钟边界：手工构造"已过期但结构合法"的 ticket
%%%   （iat/exp 均在过去，差值 <= TTL），verify 必须拒绝（不静默放行）。
%%% - 附件 status<0（撤权下架）：view_url 与 download 双拒绝。
%%%
%%% 真实对象字节级 oracle（AC13-R2，SHA256 对比）与 stream_start 后二次
%%% 校验的 HTTP 集成需要真 Garage + 真节点，由 AC04 boot 验证线承接，
%%% 本文件钉住 logic 层撤权语义。
%%%===================================================================

-define(TICKET_KEY(Secret),
    crypto:mac(
        hmac, sha256, Secret, <<"imboy:attachment-download:v1">>
    )
).

-define(SECRET, <<"q06-revocation-matrix-secret">>).
-define(OBJ_KEY, <<"u1/g66/a.png">>).
-define(MEMBER_UID, 7).
-define(OUTSIDER_UID, 999).

-define(GROUP_REC, #{
    <<"scope">> => <<"group">>,
    <<"scope_ref">> => <<"66">>,
    <<"path">> => ?OBJ_KEY,
    <<"creator_user_id">> => 1,
    <<"status">> => 0
}).

%% 撤销 literal update warning：status 可控的 group 记录
group_rec(Status) ->
    #{
        <<"scope">> => <<"group">>,
        <<"scope_ref">> => <<"66">>,
        <<"path">> => ?OBJ_KEY,
        <<"creator_user_id">> => 1,
        <<"status">> => Status
    }.

%% @doc 在测试进程内设置/恢复 jwt_key 与 base_url（同 attach_logic_tests
%% 的 assert_gateway_url 范式，config_ds:env 读 application env）
with_env(Fun) ->
    Previous = [{Key, application:get_env(imboy, Key)} || Key <- [jwt_key, base_url]],
    try
        application:set_env(imboy, jwt_key, ?SECRET),
        application:set_env(imboy, base_url, <<"http://127.0.0.1:9800">>),
        Fun()
    after
        lists:foreach(
            fun
                ({Key, {ok, Value}}) -> application:set_env(imboy, Key, Value);
                ({Key, undefined}) -> application:unset_env(imboy, Key)
            end,
            Previous
        )
    end.

%% 从 view_url 产物中提取 ticket 参数
extract_ticket(Url) ->
    #{query := Query} = uri_string:parse(Url),
    proplists:get_value(<<"ticket">>, uri_string:dissect_query(Query)).

%% 动态授权开关：authorized 返回值由进程字典驱动，模拟"入群→撤权"时序
authorize_mock() ->
    fun(_ObjectKey, Uid) ->
        case get({group_access, Uid}) of
            true -> true;
            _ -> false
        end
    end.

%% ===================================================================
%% AC13-R1 撤权矩阵
%% ===================================================================

%% @doc 矩阵核心：B 在群内时经 view_url 拿到合法 ticket URL；被移出群后
%% （1）旧 ticket 走 attach_logic:download（每次读重验）必须拒绝；
%% （2）重新 view_url 请求也必须拒绝（不签新票据）。
revoked_member_old_ticket_and_new_request_denied_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_) -> {ok, ?GROUP_REC} end},
                {'authorize_group_access', 2, authorize_mock()}
            ]}
        ],
        fun() ->
            with_env(fun() ->
                %% 时序 1：成员态——签发成功
                put({group_access, ?MEMBER_UID}, true),
                {ok, Url} = attach_logic:view_url(?MEMBER_UID, ?OBJ_KEY),
                Ticket = extract_ticket(Url),
                ?assertEqual(
                    {ok, ?MEMBER_UID, ?OBJ_KEY},
                    attachment_download_ticket:verify(Ticket, ?SECRET)
                ),

                %% 时序 2：撤权（被移出群）——旧 ticket 必须失效
                put({group_access, ?MEMBER_UID}, false),
                ?assertEqual({error, forbidden}, attach_logic:download(Ticket)),

                %% 时序 3：撤权后新签发请求必须拒绝
                ?assertEqual({error, forbidden}, attach_logic:view_url(?MEMBER_UID, ?OBJ_KEY))
            end),
            erase({group_access, ?MEMBER_UID})
        end
    ).

%% @doc 矩阵分支：从未入群的用户 C——任何途径（view_url/download）都拒绝
never_member_all_paths_denied_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_) -> {ok, ?GROUP_REC} end},
                {'authorize_group_access', 2, fun(_, _) -> false end}
            ]}
        ],
        fun() ->
            with_env(fun() ->
                ?assertEqual({error, forbidden}, attach_logic:view_url(?OUTSIDER_UID, ?OBJ_KEY))
            end)
        end
    ).

%% @doc 矩阵分支：跨组织用户 D 复用成员签发链路——ticket 绑定签发时 uid，
%% 撤权该 uid 后 ticket 即失效（ticket 不可转交性由
%% attachment_download_ticket_tests 钉住；本用例聚焦撤权时序）。
outsider_cannot_use_member_ticket_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_) -> {ok, ?GROUP_REC} end},
                {'authorize_group_access', 2, authorize_mock()}
            ]}
        ],
        fun() ->
            with_env(fun() ->
                %% 成员 7 在群内签发 ticket
                put({group_access, ?MEMBER_UID}, true),
                {ok, Url} = attach_logic:view_url(?MEMBER_UID, ?OBJ_KEY),
                Ticket = extract_ticket(Url),
                %% 撤权 7（成员离开），此时 ticket 下载必须拒绝
                put({group_access, ?MEMBER_UID}, false),
                ?assertEqual({error, forbidden}, attach_logic:download(Ticket))
            end),
            erase({group_access, ?MEMBER_UID})
        end
    ).

%% @doc 矩阵分支：附件 status<0（撤权下架）——即使 authorize_group_access
%% 误放行，authorize/2 头部 status guard 也必须拒绝（纵深防御）；
%% view_url 与 download 双路径都拒绝
status_negative_revoked_all_denied_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_) ->
                    {ok, group_rec(-1)}
                end},
                {'authorize_group_access', 2, fun(_, _) -> true end}
            ]}
        ],
        fun() ->
            with_env(fun() ->
                %% 签发侧拒绝（不产生新票据）
                ?assertEqual({error, forbidden}, attach_logic:view_url(?MEMBER_UID, ?OBJ_KEY)),
                %% 下载侧拒绝（构造成员 uid 的结构合法 ticket，走
                %% download_authorized → authorize status guard）
                Ticket = valid_ticket(?MEMBER_UID),
                ?assertEqual({error, forbidden}, attach_logic:download(Ticket))
            end)
        end
    ).

%% ===================================================================
%% AC13-R3 票据时钟边界
%% ===================================================================

%% @doc 结构合法但已过期的 ticket（iat=now-1200, exp=now-600，差值=TTL
%% 上限内）必须被 verify 拒绝——到期即失效，无宽限期
expired_ticket_rejected_by_download_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_) -> {ok, ?GROUP_REC} end},
                {'authorize_group_access', 2, fun(_, _) -> true end}
            ]}
        ],
        fun() ->
            with_env(fun() ->
                Ticket = expired_ticket(?MEMBER_UID),
                %% verify 层拒绝（exp 在过去）
                ?assertEqual(
                    {error, invalid}, attachment_download_ticket:verify(Ticket, ?SECRET)
                ),
                %% download 层同样拒绝（403 语义）
                ?assertEqual({error, forbidden}, attach_logic:download(Ticket))
            end)
        end
    ).

%% @doc 篡改 exp 超过 TTL 上限（exp - iat > 600）的 ticket 必须拒绝
%% （TTL 上界钳制，防签发方失误造成超长时效能力）
stretched_exp_ticket_rejected_test_() ->
    ?WITH_MECKS(
        [
            {attachment_ds, [
                {'find_by_path', 1, fun(_) -> {ok, ?GROUP_REC} end},
                {'authorize_group_access', 2, fun(_, _) -> true end}
            ]}
        ],
        fun() ->
            with_env(fun() ->
                Now = erlang:system_time(second),
                Ticket = imboy_jwt:sign(
                    #{
                        <<"sub">> => <<"attachment_download">>,
                        <<"uid">> => ?MEMBER_UID,
                        <<"object_key">> => ?OBJ_KEY,
                        <<"iat">> => Now,
                        <<"exp">> => Now + 3600
                    },
                    ?TICKET_KEY(?SECRET)
                ),
                ?assertEqual(
                    {error, invalid}, attachment_download_ticket:verify(Ticket, ?SECRET)
                )
            end)
        end
    ).

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% @doc 构造结构合法且在有效期内的 ticket（与 issue/3 同款 claims 形态、
%% purpose-derived key），供 download_authorized 路径测试直接使用
valid_ticket(Uid) ->
    Now = erlang:system_time(second),
    imboy_jwt:sign(
        #{
            <<"sub">> => <<"attachment_download">>,
            <<"uid">> => Uid,
            <<"object_key">> => ?OBJ_KEY,
            <<"iat">> => Now,
            <<"exp">> => Now + 600
        },
        ?TICKET_KEY(?SECRET)
    ).

%% @doc 构造结构合法但已过期的 ticket（与 issue/3 同款 claims 形态、
%% purpose-derived key；exp 在过去且 exp-iat <= TTL）
expired_ticket(Uid) ->
    Now = erlang:system_time(second),
    imboy_jwt:sign(
        #{
            <<"sub">> => <<"attachment_download">>,
            <<"uid">> => Uid,
            <<"object_key">> => ?OBJ_KEY,
            <<"iat">> => Now - 1200,
            <<"exp">> => Now - 600
        },
        ?TICKET_KEY(?SECRET)
    ).
