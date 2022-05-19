
# 框架详述
参考 [【DDD】领域驱动设计实践 —— 框架实现](https://www.cnblogs.com/daoqidelv/p/7499662.html)，有细节调整
## User Interface层
门面层，对外以各种协议提供服务，该层需要明确定义支持的服务协议、契约等。包含：

### api/dto
包括request和response两部分，通过它定义入参和出参的契约，在dto层可以使用基础设施层的validation组件完成入参格式校验；

### api/handler
支持不同访问协议的控制器实现，比如：http/restful风格、tcp/二进制流协议、mq消息/json对象等等。

handler使用基础设施层公共组件完成许多通用的工作：

* 调用checklogin完成登录态/权限校验；
* 调用logging组件完成日志记录；
* 调用message-resource组件完成错误信息转义，支持I18N；

## application层
### service
应用服务层，组合domain层的领域对象和基础设施层的公共组件，根据业务需要包装出多变的服务，以适应多变的业务服务需求。

应用服务层主要访问domain领域对象，完成服务逻辑的包装。

应用服务层也会访问基础设施层的公共组件，如rabbitmq，完成领域消息的生产等。

### assembler
组装器，负责将多个domain领域对象组装为需要的dto对象，比如查询帖子列表，需要从Post（帖子）领域对象中获取帖子的详情，还需要从User（用户）领域对象中获取用户的基本信息。

组装器中不应当有业务逻辑在里面，主要负责格式转换、字段映射等职责。

## domain层
业务领域层，是我们最应当关心的一层，也是最多变的一层，需要保证这一层是高内聚的。确保所有的业务逻辑都留在这一层，而不会遗漏到其他层。按照ddd（domain driven design）理论，主要有如下概念构成：

### domain entity
领域实体。有唯一标识，可变的业务实体对象，它有着自己的生命周期。比如社区这一业务领域中，‘帖子’就是一个业务实体，它需要有一个唯一性业务标识表征，同时他的状态和内容可以不断发生变化。

### domain value object
领域值对象。可以没有唯一性业务标识，且一旦定义，他是不可变的，它通常是短暂的。这和java中的值对象（基本类型和String类型）类似。比如社区业务领域中，‘帖子的置顶信息’可以理解为是一个值对象，不需要为这一值对象定义独立的业务唯一性标识，直接使用‘帖子id‘便可表征，同时，它只有’置顶状态‘和’置顶位置‘，一旦其中一个属性需要发生变化，则重建值对象并赋值给’帖子‘实体的引用，不会对领域带来任何负面影响。

### domain factory
领域对象工厂。用于复杂领域对象的创建/重建。重建是指通过respostory加载持久化对象后，重建领域对象。

### domain service
领域服务。区别于应用服务，他属于业务领域层。

可以认为，如果某种行为无法归类给任何实体/值对象，则就为这些行为建立相应的领域服务即可。比如：转账服务（transferService），需要操作借方/贷方两个账户实体。

传统意义上的util static方法中，涉及到业务逻辑的部分，都可以考虑归入domain service。

### domain event
领域事件。领域中产生的一些消息事件，通过事件通知/订阅的方式，可以在性能和解耦层面得到好处。

### repository
仓库。我们将仓库的接口定义归类在domain层，因为他和domain entity联系紧密。仓库用户和基础实施的持久化层交互，完成领域对应的增删改查操作。

仓库的实际实现根据不同的存储介质而不同，可以是redis、oracle、mongodb等。

鉴于现在社区服务的存储介质有三套：oracle、redis、mongodb，且各个存储介质的字段属性名不一致，因此需要使用translator来做翻译，将持久化层的对象翻译为统一的领域对象。

### translator
翻译器。将持久化层的对象翻译为统一的领域对象。

翻译器中不应当有业务逻辑在里面，主要负责格式转换、字段映射等职责。

## infrastructure层
基础设施层提供公共功能组件，供controller、service、domain层调用。

### repository impl
对domain层repository接口的实现，对应每种存储介质有其特定实现，如oracle的mapper，mongodb的dao等等。repository impl会调用mybatis、mongo client、redis client完成实际的存储层操作。

### checkLogin
权限校验器，判定客户端是否有访问该资源的权限。提供给User Interface层的Controller调用。

### exception
异常分类及定义，同时提供公共的异常处理逻辑，具体由ExceptionHandler实现。

### transport
transport完成和第三方服务的交互，可以有多种协议形式的实现，如http+json、tcp+自定义协议等，配套使用的还有Resolver解析器，用于对第三方服务的请求和响应进行适配，提供一个防腐层（AnticorruptionLayer，DDD原书P255）的作用。

### transcation
提供事务管理，交给Spring管理。

### logging
日志模块，记录trace日志，使用log4j完成。
