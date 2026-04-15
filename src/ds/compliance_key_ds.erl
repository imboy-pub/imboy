-module(compliance_key_ds).
%%%
%% compliance_key_ds — 合规密钥数据服务层
%% Compliance-key data service layer (thin pass-through to repo).
%%
%% 创建动机 / Motivation:
%%   G2-a 治理：`e2ee_handler` 原本直调 `compliance_key_repo:find_active/0`。
%%   YAGNI：仅包装目前所需的查询接口；管理类接口（create / revoke / list_all）
%%   待 admin handler 下沉时再扩展。
%%%

-export([find_active/0]).
-export([list_all/0]).
-export([create/4]).
-export([revoke/2]).

-spec find_active() -> {ok, map()} | {error, term()}.
find_active() ->
    compliance_key_repo:find_active().

%% G3: adm_admin_handler 不应直调 compliance_key_repo
-spec list_all() -> {ok, list(map())} | {error, term()}.
list_all() -> compliance_key_repo:list_all().

-spec create(binary(), binary(), binary(), integer()) -> {ok, binary()} | {error, term()}.
create(KeyId, PublicKey, PrivateKeyEncrypted, AdmUserId) ->
    compliance_key_repo:create(KeyId, PublicKey, PrivateKeyEncrypted, AdmUserId).

-spec revoke(binary(), integer()) -> {ok, integer()} | {error, term()}.
revoke(KeyId, AdmUserId) -> compliance_key_repo:revoke(KeyId, AdmUserId).
