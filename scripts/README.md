
export IMBOY_CTL_NODE=imboy_dev@127.0.0.1
export IMBOY_CTL_COOKIE=imboycookie
escript scripts/imboy_ctl db migrate



```

bash ./script/deploy.sh 10.0.0.10 1.0.0-rc.1 001
bash ./script/deploy.sh -v 10.0.0.10 1.0.0-rc.1 002

# 示例 / Examples:
#   bash scripts/imboy-deploy.sh all
#   bash scripts/imboy-deploy.sh api
#   bash scripts/imboy-deploy.sh admin
#   bash scripts/imboy-deploy.sh migrate
#   bash scripts/imboy-deploy.sh rollback
```
