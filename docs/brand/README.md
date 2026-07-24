# IMBoy 品牌资源

官方 logo、图标与设计 token。未经授权不得修改 logo 比例、配色或用于暗示 IMBoy 官方背书的第三方产品。

## 目录

```
brand/
├── logo/
│   ├── imboy-logo-primary.png   # 主色 logo（#2474E5，浅色背景）
│   └── imboy-logo-mono.png      # 单色 logo（深色背景 / 单色印刷）
├── icon/
│   └── imboy-icon-1024.png      # 应用图标母版 1024×1024
├── tokens.json                  # W3C Design Tokens 规范的设计令牌
└── README.md
```

## 颜色

| 名称 | HEX | 用途 |
|---|---|---|
| Brand Primary | `#2474E5` | logo、主按钮、主链接 |
| Brand Primary Hover | `#1E63C8` | 主按钮 hover/active |
| Brand Primary Subtle | `#E8F1FD` | 低饱和背景、tag |
| Ink | `#0F172A` | 正文 |
| Ink Muted | `#475569` | 次要文字 |
| Surface | `#FFFFFF` | 卡片背景 |
| Surface Subtle | `#F8FAFC` | 页面背景 |

完整 token（含语义色、字体、间距、圆角）见 `tokens.json`，符合 [W3C Design Tokens](https://design-tokens.github.io/community-group/format/) 草案。

## 使用指南

### 允许

- 在介绍 IMBoy 的博客、演讲、文档中使用官方 logo
- 在自托管 IMBoy 的 footer 中展示 "Powered by IMBoy"
- 作为第三方集成示例中的参考

### 禁止

- 修改 logo 的颜色、比例、描边或添加特效
- 在暗示官方背书的商业材料中使用（如"IMBoy 认证合作伙伴"）
- 将 logo 作为自己产品主标识的一部分
- 使用相近的蓝色 `#2474E5` 作为仿冒品的主色

## 最小使用空间

logo 周围至少保留相当于 logo 图标高度 `1/2` 的留白。不要将 logo 贴近边缘或嵌入干扰性图案。

## 图标规格

| 平台 | 尺寸 | 说明 |
|---|---|---|
| iOS App Store | 1024×1024 | 由 `icon/imboy-icon-1024.png` 下采样 |
| Android Play | 512×512 | 同上 |
| Web favicon | 32×32, 180×180 (apple-touch) | 使用 `imagemagick` 或 `sharp` 下采样 |
| 社交分享 (OG image) | 1200×630 | 以 primary logo 居中 + 纯色 `#2474E5` 背景 |

> 本目录只提供**母版文件**，下采样与跨平台打包由构建工具生成，不入库。

## 合规与许可

品牌资源受 [MulanPSL-2.0](../../LICENSE) 覆盖，但 **"IMBoy" 商标与 logo 的使用权限独立于代码许可证**。商用授权请联系维护者。

## 变更

| 日期 | 变更 |
|---|---|
| 2026-04-11 | 初版（S1 落地），从根 `imboy_logo*.png` 与 iOS App Icon 迁移而来 |
