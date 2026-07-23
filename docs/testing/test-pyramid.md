# 测试金字塔（Test Pyramid）

> 定义各层比例、职责边界与反模式 · 配合 `testing-strategy.md`

---

## 目标形态

```
        /\        L3 E2E / maestro / Playwright   ~5%    慢·少·高价值
       /  \       (关键用户流,真机/真后端)
      /____\      L2 契约 / 集成 (真 PG · 真帧)     ~25%
     /      \     (Repo/handler/协议边界)
    /________\    L1 组件/widget                    ~20%
   /          \
  /____________\  L0 单元 (纯函数·逻辑·毫秒级)      ~50%
```

## 各层职责与边界

| 层 | 测什么 | 依赖 | 速度 | 现有资产 |
|---|---|---|---|---|
| L0 单元 | 纯函数、编解码、状态机、校验逻辑 | 全 mock/内存 | ms | 后端 logic/lib 测试、Flutter unit/service |
| L1 组件/widget | 单模块行为、UI 渲染 | 少量 mock | 10-100ms | Flutter widget/page、后端 ds |
| L2 集成/契约 | Repo+真PG、handler+真链路、三端协议 | 真 PG/真帧 | 100ms-s | 后端 CT suite、test/integration、repo 层 |
| L3 E2E | 全链路用户流 | 真后端+真机/浏览器 | s-min | maestro 14 流、admin Playwright 9 |

## 反模式（评审已实证）

- **冰淇淋筒**：admin E2E 存在但不进 CI、Flutter 重 integration 轻单元隔离 → E2E 慢且 flaky,单元不足以定位。
- **mock 掉边界的假金字塔**：404 后端单测 mock 掉协议/存储,5 个真 bug 全靠真 PG 的 CT 抓出 → L0 数量虚高但 L2 信号缺失。
- **死测试充数**：dead-tests-census B 类 7 个死测试(含 3 个性能测试)使金字塔底座虚胖。

## 层间平衡准则

- 一个 bug 应在**最低能复现它的层**被测(契约错误进 L2 而非靠 L3 兜)。
- L0 覆盖逻辑分支,L2 覆盖集成契约,L3 只覆盖"用户能感知的关键流",不重复 L0/L2。
- 新功能 TDD:先 L0 红→绿,边界行为补 L2,关键流补 L3。

## 验收标准

- [ ] L0:L2:L3 比例趋近 50:25:5(其余 L1)
- [ ] 无 mock 协议/存储边界的 L0 冒充 L2
- [ ] 每个已知生产 bug 有对应最低层回归测试
- [ ] 死测试清零,底座真实
