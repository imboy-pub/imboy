# STEP-03 commands — UX 原型与品牌资产

> 工作目录：/Users/leeyi/project/imboy.pub/imboy/docs/plans/evidence/moya-calligraphy-ai-review/STEP-03（除标注外）。
> 执行者：Agent A（PRODUCT-COMPLIANCE-UX）。日期：2026-09-09。

## 头像盘点（全部只读，未修改/移动/删除 moya 仓文件）

| 命令 | 工作目录 | 退出码 | 摘要 |
|---|---|---|---|
| `ls -l moya/moyalogo_144X144.png moya/moyalogo_256X256.png` | /Users/leeyi/project/imboy.pub | 0 | 21,972 B / 68,291 B |
| `sips -g pixelWidth -g pixelHeight -g format <两个PNG>` | 同上 | 0 | 144×144 PNG、256×256 PNG |
| Read 两 PNG（视觉输入）+ AI 视觉模型评估 | — | 0 | 纸白底/墨笔画/两片绿芽/田字格/红章/无文字均符合；风险：红章贴圆裁切边、田字格 144 下几乎不可见 |

## 候选生成与验证

| 命令 | 退出码 | 摘要 |
|---|---|---|
| `which qlmanage rsvg-convert inkscape` | 0 | qlmanage=/usr/bin、rsvg-convert=/opt/homebrew/bin（可用）；inkscape 无 |
| `python3 -c "import cairosvg"` | 1 | cairosvg 不可用（未用） |
| `python3 -c "import PIL"` | 0 | PIL 11.3.0 可用 |
| 手写 3 个 SVG → `STEP-03/assets/moya-candidate-{a,b,c}.svg` | 0 | Write 工具创建 |
| `rsvg-convert -w 144 -h 144 -o …_144.png <svg>` ×3 | 0 | a=3,471B b=2,570B c=2,737B |
| `rsvg-convert -w 256 -h 256 -o …_256.png <svg>` ×3 | 0 | 高清源 a=6,814B b=4,773B c=5,399B |
| `sips -g pixelWidth -g pixelHeight …_144.png` ×3 | 0 | 均为 144×144 |
| PIL 圆形蒙版脚本 → `…_144_circle_preview.png` ×3 | 0 | 模拟微信头像圆形裁切 |
| Read 3 个圆形预览 + AI 视觉模型评估 | 0 | A=8/10（元素完整无裁切）、B=7.5/10、C=7/10（右上红章贴边观感）；均无文字 |

## 介绍文案字数核验

| 命令 | 退出码 | 摘要 |
|---|---|---|
| `python3 len()` 统计 | 0 | 计划 §8.4 基线稿 73 字；推荐稿 89 字；均 < 120（120 为第三方一致口径，官方口径待人工核验，见 STEP-02） |

## 写入（owned paths 内）

- imboy/docs/plans/2026-09-09-moya-ux-spec.md（UX 交付文档）
- STEP-03/commands.md、STEP-03/notes.md
- STEP-03/assets/：3 SVG + 3×(_144.png/_256.png/_144_circle_preview.png) = 12 个资产文件

## 边界遵守声明

- 未执行任何 git 写操作；moya 仓两个 PNG 仅 `ls -l`/`sips`/Read 只读盘点；未读取 .env；未使用真实 AppID/儿童数据；未对外发布任何品牌资产；未上传微信平台。
