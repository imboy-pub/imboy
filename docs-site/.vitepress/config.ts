import { defineConfig } from 'vitepress';

export default defineConfig({
  title: 'IMBoy Docs',
  description: 'IMBoy 私有化即时通讯平台 — 部署、集成与开发文档',
  lang: 'zh-CN',
  cleanUrls: true,
  lastUpdated: true,
  srcDir: 'content',
  outDir: '.vitepress/dist',
  ignoreDeadLinks: true,

  themeConfig: {
    logo: '/imboy-logo.svg',
    nav: [
      { text: '教程', link: '/tutorials/' },
      { text: '操作指南', link: '/guides/' },
      { text: '参考', link: '/reference/' },
      { text: '设计解析', link: '/explanation/' },
      { text: '合规', link: '/compliance/e2ee-policy' },
    ],

    sidebar: {
      '/tutorials/': [
        {
          text: '快速上手',
          items: [
            { text: '本地跑通后端', link: '/tutorials/quickstart-backend' },
          ],
        },
      ],

      '/guides/': [
        {
          text: 'E2EE 端到端加密',
          items: [
            { text: '密钥轮换策略', link: '/guides/e2ee/e2ee-key-rotation-policy' },
            { text: 'V2 总览', link: '/guides/e2ee/v2/01-overview' },
            { text: '协议设计', link: '/guides/e2ee/v2/02-protocol' },
            { text: '设备身份', link: '/guides/e2ee/v2/03-device-identity' },
            { text: '能力协商', link: '/guides/e2ee/v2/04-capability-negotiation' },
            { text: '设备信任', link: '/guides/e2ee/v2/06-device-trust' },
            { text: '存储加密', link: '/guides/e2ee/v2/07-storage' },
            { text: '威胁模型', link: '/guides/e2ee/v2/08-threat-model' },
          ],
        },
        {
          text: '运维部署',
          items: [
            { text: 'Day1 快速部署', link: '/guides/operations/deployment/day1-quickstart' },
            { text: '生产架构', link: '/guides/operations/deployment/production-architecture' },
            { text: '监控', link: '/guides/operations/deployment/monitoring' },
            { text: '备份恢复', link: '/guides/operations/deployment/backup-restore' },
            { text: '集群', link: '/guides/operations/clustering' },
            { text: '可观测性', link: '/guides/operations/observability' },
            { text: '升级手册', link: '/guides/operations/upgrade-runbook' },
          ],
        },
        {
          text: '测试',
          items: [
            { text: '测试策略', link: '/guides/testing/testing-strategy' },
            { text: 'E2EE 测试', link: '/guides/testing/e2ee-testing' },
            { text: 'WebSocket 测试', link: '/guides/testing/websocket-testing' },
            { text: '安全测试', link: '/guides/testing/security-testing' },
          ],
        },
        {
          text: '发布',
          items: [
            { text: '发布流程', link: '/guides/release/RELEASE' },
            { text: 'Google Play 清单', link: '/guides/release/android-googleplay-checklist' },
            { text: 'App Store 清单', link: '/guides/release/ios-appstore-checklist' },
          ],
        },
      ],

      '/reference/': [
        {
          text: 'API',
          items: [
            { text: 'REST API 目录', link: '/reference/rest-api-v1-catalog' },
            { text: 'API 格式规范', link: '/reference/api-format' },
            { text: '错误码', link: '/reference/error-codes' },
            { text: 'WebSocket 协议', link: '/reference/ws-protocol-contract' },
          ],
        },
        {
          text: 'E2EE',
          items: [
            { text: '协议规范', link: '/reference/e2ee-protocol-specification' },
          ],
        },
        {
          text: '工程',
          items: [
            { text: '工程总览', link: '/reference/engineering/engineering-overview' },
            { text: '配置说明', link: '/reference/engineering/configuration-notes' },
            { text: '依赖说明', link: '/reference/engineering/dependency-notes' },
            { text: 'CI 笔记', link: '/reference/engineering/ci-notes' },
            { text: '迁移命名', link: '/reference/engineering/migration-naming' },
          ],
        },
        {
          text: '约定',
          items: [
            { text: 'TSID 字段规范', link: '/reference/tsid-field-convention' },
            { text: 'UTF-8 编码', link: '/reference/utf8-encoding' },
          ],
        },
      ],

      '/explanation/': [
        {
          text: '设计解析',
          items: [
            { text: '模块分类', link: '/explanation/current-module-classification' },
            { text: '产品 Profile 与插件注册', link: '/explanation/product-profile-and-plugin-registry-design' },
          ],
        },
      ],

      '/compliance/': [
        {
          text: '合规',
          items: [
            { text: 'E2EE 策略与密钥托管披露', link: '/compliance/e2ee-policy' },
            { text: '等保 2.0 清单', link: '/compliance/mlps2-checklist' },
          ],
        },
      ],
    },

    search: { provider: 'local' },

    editLink: {
      pattern: 'https://github.com/imboy-pub/imboy/edit/main/docs/:path',
      text: '在 GitHub 上编辑此页',
    },

    footer: {
      message: 'IMBoy — 企业私有化即时通讯平台',
      copyright: 'Copyright 2020-present imboy.pub',
    },
  },
});
