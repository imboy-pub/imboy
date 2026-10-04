%%% @doc 扩展点：客服 ID 生成（TSID；镜像 `eb_id_port` 的形状）。
%%%
%%% 真扩展点：只有 `-callback` 声明，零实现。domain 不自行生成 ID；
%%% application 经本端口注入（生产实现按装配选择）。
-module(cs_id_port).

-moduledoc "扩展点：客服 ID 生成（TSID，镜像 eb_id_port 形状）。".
-export_type([kind/0]).

%% 命名生成器按资源类型分域（不同 Kind 在不同表，主键不冲突）。
-type kind() :: cs_session | cs_shop_key | cs_visit_token | cs_widget_installation | cs_event.

-callback new_id(Kind :: kind()) -> integer().
