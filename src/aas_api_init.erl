-module(aas_api_init).
%%%
% aas_api_init 是 api_init application assembler 缩写
%%%
-export([data/0]).


%%%
%%%
data() ->
    [{<<"login_pwd_rsa_encrypt">>,
      logic_config:get("login_pwd_rsa_encrypt")},
     {<<"login_rsa_pub_key">>, logic_config:get("login_rsa_pub_key")}].
