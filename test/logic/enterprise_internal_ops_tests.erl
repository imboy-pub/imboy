-module(enterprise_internal_ops_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% @doc
%%% enterprise_internal_ops 纯单测（mock repo，不触库）。
%%%
%%% C1（brownfield 复审切片 C）：generate_secret/0 的 base64 padding 剥除
%%% 错剥了一个数据字符——base64:encode(32B) 产 44 字符（43 数据 + 1 '='），
%%% binary:part(_, 0, size-2) 剥 2 后剩 42 字符 = 252 bit 熵，低于注释声称
%%% 的「43 字符，熵 ≥ 256 bit」。合同：生成式签发的 secret 必须 43 字符
%%% urlsafe（与 sso_logic:generate_code/0、enterprise_oa_login_logic:b64/1
%%% 同款剥全 padding 惯例）。
%%%===================================================================

issue_credential_secret_entropy_test_() ->
    {setup, fun setup_repo_mock/0, fun cleanup_repo_mock/1, fun(_) ->
        ?_test(begin
            {ok, #{credential := Cred}} =
                enterprise_internal_ops:issue_credential_tx(fake_conn, 42, 7, undefined),
            [Prefix, Secret] = binary:split(Cred, <<".">>),
            %% prefix 形态（ib_int_<tsid>）与点分结构不变
            ?assertMatch(<<"ib_int_", _/binary>>, Prefix),
            %% C1 合同：43 字符（≥256bit）；错误剥法产出 42
            ?assertEqual(43, byte_size(Secret)),
            %% 全串 urlsafe 字符集
            ?assert(
                lists:all(
                    fun(C) ->
                        (C >= $a andalso C =< $z) orelse
                            (C >= $A andalso C =< $Z) orelse
                            (C >= $0 andalso C =< $9) orelse
                            C =:= $- orelse C =:= $_
                    end,
                    binary_to_list(Secret)
                )
            )
        end)
    end}.

setup_repo_mock() ->
    %% prefix 生成经 next_id -> elib_tsid:generate（同 EPGZ05 夹具惯例，幂等）
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    meck:new(enterprise_application_credential_repo, [passthrough]),
    meck:expect(
        enterprise_application_credential_repo,
        create_tx,
        fun(_Conn, _Org, _App, _Prefix, _Secret, _ExpiresAt) ->
            {ok, #{<<"id">> => 12345}}
        end
    ),
    ok.

cleanup_repo_mock(_) ->
    catch meck:unload(enterprise_application_credential_repo),
    ok.
