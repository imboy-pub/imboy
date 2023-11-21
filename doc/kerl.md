## kerl
```
// 列表可安装的版本号
kerl list releases


CPP=cpp EGREP=egrep kerl build 25.3.2.7

kerl delete build 24.3.3

kerl list builds

kerl install 25.3.2.7 ~/kerl/25.3.2.7

. /Users/leeyi/kerl/25.3.2.7/activate

Later on, you can leave the installation typing:
kerl_deactivate

Anytime you can check which installation, if any, is currently active with:
kerl active
```
