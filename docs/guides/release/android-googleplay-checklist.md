# Google Play 首发与内测轨清单 / Google Play First Release & Internal Testing

> 核查日期 / Checked: 2026-11-05；Flutter GitHub main: `9f5d36e5b852f0a3dc20c94a0997fde82d9852d2`。本清单记录代码证据和待验证项，并非上架通过证明。
> Source review only: signing material, actual release AAB, device behavior and Play Console have not been verified.

## 1. 当前状态 / Current status

| 项目 / Item | 代码现状 / Evidence | 状态 / Status |
|---|---|---|
| 包名 / Package | `standard` 为 `pub.imboy.app`；`gzCustomer` 默认追加 `.gzcustomer`，供客户白标使用 | 首发固定 standard；Console 包名待核对 |
| 版本 / Version | `pubspec.yaml` 为 `1.0.0-alpha.18`，没有 `+build`；Gradle 从 Flutter 读取 versionName/versionCode | 必须显式指定未使用且递增的正整数 versionCode；不得沿用旧清单的 1 或 3 |
| SDK | `minSdkVersion(24)` 后覆盖前面的 Flutter 默认；`targetSdk = flutter.targetSdkVersion`；`compileSdk = maxOf(flutter.compileSdkVersion, 37)` | 本机 Flutter 3.47.8 与 CI 固定的 3.47.5 标签源码均默认 target 36；最终 AAB 的实际值仍须检查 |
| 签名 / Signing | `android/key.properties` 优先，四个字段分别回退至 `android/local.properties`；release 使用 release signingConfig | 有配置机制不代表真实密钥就绪；不得使用 CI 的 debug key 发版 |
| 优化 / Shrinking | release 开启 R8 minify、资源压缩及现有 `proguard-rules.pro` | 构建和真机验证待做；不要盲目追加大范围 keep/dontwarn |
| CI | 2026-11-04 [CI run](https://github.com/imboy-pub/imboy-flutter/actions/runs/41825708990) 对上述 SHA 成功；Android job 使用 debug keystore 构建 `standard` release APK | 不证明正式签名、AAB 或 Play 可接受 |

2026-08-31 起普通手机新应用和更新需 target API 36 或更高，参见 [Google 官方要求](https://developer.android.com/google/play/requirements/target-sdk)。`compileSdk 37` 不等于 target 37；最终以 AAB 中 manifest 及提交时政策为准。

## 2. 签名准备 / Signing preparation

- [ ] 确认 Console 中 `pub.imboy.app` 的归属、Play App Signing 方案和上传证书 SHA-256；尚未创建时，由负责人决定首次注册与签名方案。
- [ ] 在安全环境准备上传密钥，备份 keystore、密码和 alias；不提交、不打印、不把真实材料放入构建日志或普通 CI artifact。`android/.gitignore` 忽略 `key.properties`、`*.jks`、`*.keystore`，仍须审查实际存放位置。
- [ ] 在 `imboyapp/android/key.properties` 私下配置 `storeFile`、`storePassword`、`keyAlias`、`keyPassword`。`storeFile` 建议使用 keystore 绝对路径；相对路径由 `android/app` 模块解析。每个字段可回退至 `local.properties`，要防止混用旧值。不要用 debug keystore。
- [ ] Play 分发 APK 使用 **app signing key**，AAB 上传使用 **upload key**；二者可不同。上传密钥丢失可按 Console 流程申请重置，不等于应用签名密钥可随意替换。跨商店升级和极光/Google 等第三方服务须核对包名及最终分发证书指纹。[Android 官方签名说明](https://developer.android.com/studio/publish/app-signing)

## 3. 正式 AAB 构建 / Build the release AAB

以下在 Flutter 仓库根目录执行；先准备项目所需的 Flutter/Java/Android SDK、Rust 原生依赖、产品 features 生成物和目标环境配置。CI 固定 Flutter 3.47.5、JDK 17；正式构建记录实际工具版本及源码 SHA。请先在 Play Console 检查所有轨道曾使用的 versionCode，再分配一个更大的值；以下变量是占位符，不能直接照填。

```bash
cd /Users/leeyi/project/imboy.pub/imboyapp
git rev-parse HEAD
flutter --version
flutter pub get
python3 scripts/check_sdk_compatibility.py
python3 ../imboy/scripts/generate_product_features.py --check --require-profile full-selected

BUILD_NAME='1.0.0-alpha.18'       # 发布负责人确认
BUILD_NUMBER='<未使用且递增的正整数>'
SYMBOL_DIR="/安全保存目录/imboy-android-${BUILD_NUMBER}"
mkdir -p "$SYMBOL_DIR"
flutter build appbundle --release --flavor standard \
  --build-name="$BUILD_NAME" --build-number="$BUILD_NUMBER" \
  --obfuscate --split-debug-info="$SYMBOL_DIR" \
  --dart-define=ALIPAY_APP_ID="${ALIPAY_APP_ID:-2021004142626807}"
```

将额外业务 `--dart-define` 与实际发布配置一起核对；不可把密钥打入客户端。Dart 混淆是本清单建议的发布选项，须在相同选项的包上真机回归。本地 Flutter 仓库已更新 `scripts/build_play_aab.sh <build_number> [build_name]`：校验版本码、指定 standard、启用 Dart 混淆并输出符号；可通过 `PLAY_SYMBOL_DIR` 指定符号目录。预检脚本使用 standardRelease 路径；AAB 缺失仍为 WARN，其成功退出不能代替签名或发布验收。上述直接命令可用于自行控制构建参数，使用脚本时按 Flutter 仓库文档核对。

构建前清理旧产物或隔离输出后，以 Flutter 最终输出为准；常见路径为 `build/app/outputs/bundle/standardRelease/app-standard-release.aab`。若不一致，查找并核对时间、SHA 和 variant，避免误用旧包：

```bash
find build/app/outputs/bundle -name '*.aab' -print
AAB='build/app/outputs/bundle/standardRelease/app-standard-release.aab' # 按实际输出调整
shasum -a 256 "$AAB"
```

## 4. 产物验证 / Verify the artifact

- [ ] 用 Android SDK 的 `bundletool` 检查 AAB 有效性及实际 manifest。这里 `BUNDLETOOL_JAR` 是本机已安装的 bundletool jar 绝对路径：

```bash
java -jar "$BUNDLETOOL_JAR" validate --bundle="$AAB"
java -jar "$BUNDLETOOL_JAR" dump manifest --bundle="$AAB" --module=base > /tmp/imboy-play-manifest.xml
rg 'package=|versionCode=|versionName=|minSdkVersion=|targetSdkVersion=' /tmp/imboy-play-manifest.xml
```

核对 `pub.imboy.app`、版本名、分配的 versionCode、minSdk、targetSdk ≥ 36；检查最终 merged manifest 权限、导出组件和 SDK 配置，并用 bundletool 或 zip 检查 arm64-v8a 等 ABI。`scripts/check_play_release.sh` 可辅助检查禁用权限和链接，但 AAB 不存在只产生 WARN，且不验证签名，成功退出不等于通过发布验收。

- [ ] 使用 JDK 工具验证 AAB 的 JAR 签名完整性和证书指纹；不得用 `apksigner` 验 AAB：

```bash
jarsigner -verify -verbose -certs "$AAB"
keytool -printcert -jarfile "$AAB"
```

确认输出为 `jar verified`、所有需要签名的条目均已签名且没有完整性错误；自签名证书的信任链告警需结合输出判断，不能只看退出码。将 SHA-256 与独立可信记录中的上传证书比对；已有 Play App Signing 时与 Console 的 **upload certificate** 比对。检查证书有效期并排除 debug 证书。首次建立应用、Console 尚未显示证书时先与独立保管的预期上传证书比对，注册后再复核。
- [ ] 存档 AAB、SHA-256、提交 SHA、工具链、构建参数、版本和证书指纹。保存对应构建的 R8 `mapping.txt`（常见于 `build/app/outputs/mapping/standardRelease/`）、每 ABI Dart `*.symbols`，以及可用的原生符号；安全关联版本，不提交仓库。R8 mapping 与 Dart symbols 是不同材料；按需在 Console/崩溃平台上传相应符号，记录是否实际成功。`libvodozemac_bindings_dart.so` 配置跳过 strip，并不自动证明符号完整；需验证原生符号可用于还原。
- [ ] 使用与发布版本相同的优化及配置做 Android **真机**冒烟：启动、登录、聊天、附件、通知、音视频、支付/登录等实际启用能力；重点检查 R8 反射/JNI 和插件。不要根据旧清单推断必须加入某组 keep 规则。

APK 是可直接安装的包；AAB 是上传包，Play 从中生成设备专用 APK 并以应用签名密钥签名。普通 APK 成功不证明 AAB、签名或 Play 分发可用；如用 bundletool 生成 APK 做预检，其签名和 Play 分发签名可能不同，最终应使用 Play 内测分发包复验。

## 5. 最小 CI 改进 / Minimal CI change

本地 Flutter 仓库的 CI `build` job 已复用临时 debug 签名，在 APK 构建后追加以下 AAB 冒烟检查（尚待推送后运行）：

```yaml
- name: Build standard AAB (debug-signed CI smoke only)
  run: flutter build appbundle --release --flavor standard
```

构建失败即 job 失败；标明它是 **debug-signed CI smoke，绝不可上传 Play**。若存档，使用明确的 `ci-debug-signed-aab` 名称并按实际 `standardRelease` 路径校验；不需要把上传密钥注入常规 PR/CI。该工作流尚未执行；它只验证 AAB 构建，不验证正式签名、版本分配、符号归档或真实功能。正式发布若日后自动化，另设受保护、手动触发的发布流程，隔离密钥、验证证书及版本、存档 AAB 和对应符号；不能使用 `GITHUB_RUN_NUMBER` 代替与 Play 历史记录核对。

## 6. Console 与合规待办 / Play Console and policy

- [ ] 核对开发者账号验证、应用归属、Play App Signing、上传证书、已用 versionCode、目标地区、内测人员和当前发布资格；账户类型可能有额外测试要求，以 Console 提示为准。
- [ ] 填写应用名称、默认语言及实际支持语言的商店文案、图标、特色图和截图；验证显示内容与实际应用一致。
- [ ] 核查隐私政策、账号删除入口、数据安全、广告、应用访问说明、内容分级、目标受众及敏感权限声明；依据实际代码、SDK、服务端和数据流逐项填写，不预设联系人、位置、加密或 SDK 的收集结论。
- [ ] 审核最终 merged manifest 的权限及用途，确认推送、位置和媒体访问与各地区版本一致；不要仅因依赖 `permission_handler` 就增加联系人权限。
- [ ] 在获授权后由负责人上传已核验 AAB 到内测轨、核对 Play 处理结果和设备目录，再由测试员通过 Play 安装验证。正式审核和上线需另行决定；本次未上传、未提交审核。

参考 / References: [Flutter Android release](https://docs.flutter.dev/deployment/android)、[Dart obfuscation and symbols](https://docs.flutter.dev/deployment/obfuscate)、[Android app signing](https://developer.android.com/studio/publish/app-signing)。
