/**
 * `WebsocketHeartbeatJs` constructor.
 *
 * @param {Object} opts
 * {
 *  url                  websocket链接地址
 *  pingTimeout 未收到消息多少秒之后发送ping请求，默认15000毫秒
    pongTimeout  发送ping之后，未收到消息超时时间，默认10000毫秒
    reconnectTimeout
    pingMsg
 * }
 * @api public
 *
 * @link https://github.com/zimv/websocket-heartbeat-js/blob/master/README-zh.md
 */
function WebsocketHeartbeatJs({
    url,
    pingTimeout = 15000,
    pongTimeout = 10000,
    reconnectTimeout = 2000,
    pingMsg = 'ping',
    subprotocols = ['text'],
    repeatLimit = 10
}){
    this.opts = {
        url: url,
        pingTimeout: pingTimeout,
        pongTimeout: pongTimeout,
        reconnectTimeout: reconnectTimeout,
        pingMsg: pingMsg,
        subprotocols: subprotocols,
        repeatLimit: repeatLimit
    };
    this.ws = null;//websocket实例
    this.repeat = 0;

    //override hook function
    this.onclose = () => {};
    this.onerror = () => {};
    this.onopen = () => {};
    this.onmessage = () => {};
    this.onreconnect = () => {};

    this.createWebSocket();
}
WebsocketHeartbeatJs.prototype.createWebSocket = function(callback, args) {
    try {
        var url = this.opts.url + '?authorization=' + get_cookie('authorization').replace("+", "%2B")
        console.log('createWebSocket url ', url)
        if (this.opts.subprotocols) {
            this.ws = new WebSocket(url, this.opts.subprotocols);
        } else {
            this.ws = new WebSocket(url);
        }
        this.initEventHandle();
        if (callback && args) {
            callback(args)
        }
    } catch (e) {
        throw e;
    }
};

WebsocketHeartbeatJs.prototype.initEventHandle = function(){
    this.ws.onclose = () => {
        this.onclose();
        this.reconnect();
    };
    this.ws.onerror = () => {
        this.onerror();
        this.onclose();
        this.reconnect();
    };
    this.ws.onopen = () => {
        this.repeat = 0;
        this.onopen();
        //心跳检测重置
        this.heartCheck();
    };
    this.ws.onmessage = (event) => {
        console.log('ws.onmessage event: ', event, this.onmessage)
        var that = this;
        setTimeout(function() {
            that.onmessage(event);
        }, 160),
        // this.onmessage(event);
        //如果获取到消息，心跳检测重置
        //拿到任何消息都说明当前连接是正常的
        this.heartCheck();
    };
};

WebsocketHeartbeatJs.prototype.reconnect = function(){
    if(this.opts.repeatLimit>0 && this.opts.repeatLimit <= this.repeat) {
        //limit repeat the number
        return;
    }
    if(this.lockReconnect || this.forbidReconnect) {
        return;
    }
    this.lockReconnect = true;
    this.repeat++;//必须在lockReconnect之后，避免进行无效计数
    this.onreconnect();
    var reconnectTimeout = this.opts.reconnectTimeout + 2000 * this.repeat
    if (this.repeat > (this.repeatLimit / 2)) {
        reconnectTimeout = reconnectTimeout - 1000 * this.repeat
    }
    if (reconnectTimeout < this.opts.reconnectTimeout) {
        reconnectTimeout = this.opts.reconnectTimeout
    }
    //没连接上会一直重连，设置延迟避免请求过多
    setTimeout(() => {
        this.createWebSocket();
        this.lockReconnect = false;
    }, reconnectTimeout)
};
WebsocketHeartbeatJs.prototype.send = function(msg){
    if (this.ws.readyState === WebSocket.OPEN) {
        this.ws.send(msg)
    } else if (this.ws.readyState === WebSocket.CONNECTING) {
        setTimeout(function() {
            this.ws.send(msg)
        }, 100)
    } else if (this.ws.readyState === WebSocket.CLOSING) {
        setTimeout(function() {
            this.createWebSocket()
            setTimeout(function() {
                this.ws.send(msg)
            }, 100)
        }, 100)
    } else { // WebSocket.CLOSED
        this.createWebSocket()
        setTimeout(function() {
            this.ws.send(msg)
        }, 100)
    }
};
//心跳检测
WebsocketHeartbeatJs.prototype.heartCheck = function(){
    this.heartReset();
    this.heartStart();
};
WebsocketHeartbeatJs.prototype.heartStart = function(){
    if(this.forbidReconnect) return;//不再重连就不再执行心跳
    this.pingTimeoutId = setTimeout(() => {
        //这里发送一个心跳，后端收到后，返回一个心跳消息，
        //onmessage拿到返回的心跳就说明连接正常
        this.ws.send(this.opts.pingMsg);
        //如果超过一定时间还没重置，说明后端主动断开了
        this.pongTimeoutId = setTimeout(() => {
            //如果onclose会执行reconnect，我们执行ws.close()就行了.如果直接执行reconnect 会触发onclose导致重连两次
            this.ws.close();
        }, this.opts.pongTimeout);
    }, this.opts.pingTimeout);
};
WebsocketHeartbeatJs.prototype.heartReset = function(){
    clearTimeout(this.pingTimeoutId);
    clearTimeout(this.pongTimeoutId);
};
WebsocketHeartbeatJs.prototype.close = function(){
    //如果手动关闭连接，不再重连
    this.forbidReconnect = true;
    this.heartReset();
    this.ws.close();
};

//
var friend_helper = {
    friends : {}
    , group_users: {}
    , init : function(cachedata) {
        layui.each(cachedata.friend, function(i, group) {
            layui.each(group.list, function(index, friend) {
                if (friend && friend.id) {
                    friend_helper.friends[friend.id] = friend
                }
            })
        })
    }
    , userinfo : function(Uid) {
        // {id, account, avatar, sign, status}
        if (!(friend_helper.friends && friend_helper.friends[Uid])) {
            friend_helper.init(layui.layim.cache())
        }
        return friend_helper.friends[Uid]
    }
    , group_userinfo(Gid, Uid) {
        // {id, account, avatar, sign, status}
        if (friend_helper.group_users && friend_helper.group_users[Uid]) {
            return friend_helper.group_users[Uid]
        }
        api_ajax(apiurl.group_member, 'get', {'id': Gid}, function(res) {
            if (res.code == 0 && res.payload && res.payload.list) {
                layui.each(res.payload.list, function(index, user) {
                    if (user && user.id) {
                        friend_helper.group_users[user.id] = {
                            "account": user.account,
                            "avatar": user.avatar
                        }
                    }
                })
            }
        }, function(res) {
        }, false)
        return friend_helper.group_users[Uid]
    }
}

layui.define(['jquery', 'layer', 'layim', 'contextmenu', 'form'], function (exports) {
    var contextmenu = layui.contextmenu
    var $ = layui.jquery
    var layer = layui.layer
    var form = layui.form
    var layim = layui.layim
    // 这里需要用 layui.layim.cache() 的cache
    var cachedata = layim.cache()
    var conf = {
        uid: 0, //
        key: '', //
        layim: layim,
        token: null,
    }

    var chat = {
        config: function (options) {
            //把layim继承出去，方便在register中使用
            conf = $.extend(conf, options)
            this.register()
            im.init(options.user, options.pwd)
        },
        register: function () {
            if (!conf.layim) {
                return false
            }
            //监听在线状态的切换事件
            conf.layim.on('online', function (state) {
                api_ajax(apiurl.change_state, 'put', {state: state}, function(res) {
                }, default_error_callback)
            })
            //监听签名修改
            conf.layim.on('sign', function (value) {
                api_ajax(apiurl.change_sign, 'put', {sign: value}, function(res) {
                }, default_error_callback)
            })
            //监听自定义工具栏点击，以添加代码为例
            layim.on('tool(code)', function(insert){
                layer.prompt({
                    title: '插入代码'
                    ,formType: 2
                    ,shade: 0
                }, function(text, index){
                    layer.close(index);
                    insert('[pre class=layui-code]' + text + '[/pre]'); //将内容插入到编辑器
                });
            });
            //监听layim建立就绪
            conf.layim.on('ready', function (res) {
                // console.log('cachedata.mine.msgbox', cachedata.mine.msgbox)
                if (cachedata.mine.msgbox>0) {
                    conf.layim.msgbox(cachedata.mine.msgbox) //消息盒子有新消息
                }
                im.contextmenu()
            })

            //监听查看群员
            conf.layim.on('members', function (data) {
            })

            $('body').on('click', '*[socket-event]', function(e) {//自定义事件
                var othis = $(this), methid = othis.attr('socket-event')
                im[methid] ? im[methid].call(this, othis, e) : ''
            })
            //监听聊天窗口的切换
            conf.layim.on('chatChange', function (res) {
                im.closeAllGroupList()
                console.log('chatChange >> ', res)
                var type = res.data.type.toUpperCase();
                if (type === 'C2C') {
                    //模拟标注好友状态
                    // im.userStatus({
                    //     id: res.payload.id
                    // })
                } else if (type === 'GROUP') {
                    var _time = (new Date()).valueOf()//当前时间
                    if (res.payload && 'gagTime' in res.payload && parseInt(res.payload.gagTime) > _time) {
                        im.setGag({
                            groupid: res.payload.id,
                            type: 'set',
                            user: cachedata.mine.id,
                            gagTime: ''
                        })
                    }
                }
            })
            conf.layim.on('sendMessage', function (data) { //监听发送消息
                console.log('sendMessage data ', data.to.type, data)
                data.to.cmd = 0;
                if (data.to.type == 'friend') {
                    data.to.type = 'C2C'
                }
                console.log('data.to.type ', data.to.type)
                if (['C2C', 'SYSTEM', 'GROUP', 'friend'].includes(data.to.type)) {
                    im.sendMsg(data)
                } else {
                    var _time = (new Date()).valueOf()//当前时间
                    var gagTime = parseInt(layui.layim.thisChat().data.gagTime)
                    console.log('sendMessage not friend ', _time, gagTime, gagTime < _time)
                    if (gagTime < _time) {
                        im.sendMsg(data)
                    } else {
                        im.popMsg(data,'当前为禁言状态，消息未发送成功！')
                        return false;
                    }
                }
            })
        }
    }

    var im = {
        init: function (user, pwd) {
            setTimeout(function() {
                friend_helper.init(cachedata)
                console.log(' >>>> on friend_helper ', friend_helper)
                cachedata = layui.layim.cache()
                // console.log('cachedata.mine ', cachedata.mine)
            }, 100)
            //初始化事件监听
            WSHB.onmessage = function(message) {
                var data = {}
                try {
                    if (message.data == "pong") {
                        return true
                    }
                    data = JSON.parse(message.data)
                    console.log('onmessage data: ', data)
                    var message = {
                        token: token,
                    }
                    var type = data['type'].toUpperCase();
                    switch(type) {
                        // 服务端ping客户端
                        case 'SYSTEM':
                            if (data.payload.msg_type == 706) {
                                layer.open({
                                    type: 1
                                    , btn: '登录'
                                    , yes: function(index, layero) {
                                        window.location = '/passport/login.html'
                                    }
                                    , content: '<br/><center>请重新登录</center>'
                                })
                            } else if(data.payload.msg_type == 705) {
                                refreshtoken(function(res) {
                                    WSHB.createWebSocket()
                                })
                            } else if(data.payload.msg_type == 786) {
                                // 如果手动关闭连接，不再重连
                                WSHB.close()
                                layer.open({
                                    type: 0,
                                    area: ['300px', 'auto'],
                                    content: data.msg
                                })
                            } else if(data.payload.msg_type == 500) {
                                // 用户在线状态： online offline hide
                                conf.layim.setFriendStatus(data.from, data.payload.content)
                            } else {
                                var content = data.payload && data.payload.content ? data.payload.content : '未知消息'
                                layer.open({
                                    type: 0,
                                    area: ['300px', 'auto'],
                                    content: content
                                })
                            }
                            break
                        // 检测聊天数据
                        case 'GROUP':
                            var from = friend_helper.group_userinfo(data.to, data.from)
                            console.log('from ', from, 'friend_helper ', friend_helper.group_users)
                            if (from) {
                                Object.assign(data, from)
                            }
                            data['id'] = data.to
                            // console.log('group data ', data)
                            conf.layim.getMessage(data)
                            break
                        case 'C2C':
                            var from = friend_helper.userinfo(data.from)
                            // console.log('from ', from, 'friend_helper ', friend_helper.friends)
                            if (from) {
                                Object.assign(data, from)
                            }
                            if (!data.content && data.payload.content) {
                                data.content = data.payload.content
                            }
                            if (data.payload.msg_type == 'text' && data.payload.text) {
                                data.content = data.payload.text
                            // } elseif (data.payload.msg_type == 'file') {
                            //     data.content = 'file'
                            } else {
                            }

                            data.timestamp = data.payload.server_ts
                            console.log('data ', data)
                            conf.layim.getMessage(data)
                            break

                        // 下面的功能没实现，以后会放到
                        // 离线消息推送
                        case 'OFFLINE_MSG':
                            setTimeout(function() {layim.getMessage(data.data)}, 3000)
                            break
                        // 添加好友
                        case 'ADDFRIEND':
                            console.log('addFriend', data.data)
                            conf.layim.addList(data.data)
                            break
                        //加入黑名单
                        case 'BLACK':
                            conf.layim.removeList({
                                type: 'chat'
                                , id: data.data.id //好友或者群组ID
                            })
                            break
                        //删除好友
                        case 'DEL_FRIEND':
                            console.log(data.data)
                            conf.layim.removeList({
                                type: 'C2C'
                                , id: data.data.id //好友或者群组ID
                            })
                            break
                        // 添加 分组信息
                        case 'ADD_GROUP':
                            console.log(data.data)
                            conf.layim.addList(data.data)
                            break
                        // 申请加入群组
                        case 'APPLY_GROUP':
                            console.log(data.data)
                            //询问框
                            var index = layer.confirm(
                                data.data.joinname + ' 申请加入 ' + data.data.groupname + "<br/> 附加信息： " + data.data.remark , {
                                btn: ['同意', '拒绝'], //按钮
                                title: '加群申请',
                                closeBtn: 0,
                                icon: 3
                            }, function() {
                                $.post(join_group_url,
                                    {
                                        'user_id': data.data.joinid,
                                        'user_name': data.data.joinname,
                                        'user_avatar': data.data.joinavatar,
                                        'user_sign': data.data.joinsign,
                                        'group_id': data.data.groupid
                                    },
                                    function(res) {
                                        if (0 == res.code) {

                                            var join_data = '{"type":"JOIN_GROUP", "join_id":"' + data.data.joinid + '"' +
                                                ', "group_id": "' + data.data.groupid + '", "group_avatar": "' + data.data.groupavatar + '"' +
                                                ', "group_name": "' + data.data.groupname + '"}'
                                            WSHB.send(join_data)
                                        } else {
                                            layer.msg(res.msg, {time:2000})
                                        }
                                }, 'json')
                                layer.close(index)
                            }, function() {

                            })
                            break
                        // 删除面板的群组
                        case 'DEL_GROUP':
                            conf.layim.removeList({
                                type: 'group'
                                ,id: data.data.id //群组ID
                            })
                            break
                    }
                } catch(err) {
                    console.log('onmessage message: ', message, err)
                }
            }
        },
        contextmenu : function() {//定义右键操作
            var my_spread = $('.layim-list-friend >li')
            my_spread.mousedown(function(e) {
                var data = {
                    contextItem: "context-friend", // 添加class
                    target: function(ele) { // 当前元素
                        var status = ele.attr('class').search('layim-list-gray') == -1 ? 'online' : 'offline'
                        // console.log('status', status)
                        $(".context-friend")
                            .attr("data-id", ele.attr('class').replace(/[^0-9]/ig,""))
                            // .attr("data-id", ele.attr('data-id'))
                            .attr("data-name", ele.find("span").html())
                            .attr("data-status", status)
                        $(".context-friend").attr("data-img",ele.find("img").attr('src')).attr("data-type",'C2C')
                    },
                    menu:[]
                }
                data.menu.push(im.menuChat())
                data.menu.push(im.menuInfo())
                data.menu.push(im.menuChatLog())
                data.menu.push(im.menuNickName())
                var currentGroupid = $(this).find('h5').data('groupid')//当前分组id
                if (my_spread.length >= 2) { //当至少有两个分组时
                    var html = '<ul>';
                    for (var i = 0; i < my_spread.length; i++) {
                        var groupid = my_spread.eq(i).find('h5').data('groupid')
                        if (currentGroupid != groupid) {
                            var groupname = my_spread.eq(i).find('h5 span').html()
                            html += '<li class="ui-move-menu-item" data-groupid="'+groupid+'" data-groupname="'+groupname+'"><a href="javascript:void(0)"><span>'+groupname+'</span></a></li>'
                        }
                    }
                    html += '</ul>';
                    data.menu.push(im.menuMove(html))
                }
                data.menu.push(im.menuRemove())
                $(".layim-list-friend >li > ul > li").contextmenu(data)//好友右键事件
            })

            $(".layim-list-friend >li > h5").mousedown(function(e) {
                var data = {
                    contextItem: "context-mygroup", // 添加class
                    target: function(ele) { // 当前元素
                        console.log(ele.data('groupid'))
                        $(".context-mygroup")
                            .attr("data-id", ele.data('groupid'))
                            .attr("data-name", ele.find("span").html())
                    },
                    menu: []
                }
                data.menu.push(im.menuAddMyGroup())
                data.menu.push(im.menuRename())
                if ($(this).parent().find('ul li').data('index') !== 0) {
                    data.menu.push(im.menuDelMyGroup())
                }

                $(this).contextmenu(data)  //好友分组右键事件
            })

            $(".layim-list-group > li").mousedown(function(e) {
                    var data = {
                        contextItem: "context-group", // 添加class
                        target: function(ele) { // 当前元素
                            $(".context-group").attr("data-id",ele[0].id).attr("data-name",ele.find("span").html())
                            .attr("data-img",ele.find("img").attr('src')).attr("data-type",'group')
                        },
                        menu: []
                    }
                    data.menu.push(im.menuChat())
                    // data.menu.push(im.menuInfo())
                    data.menu.push(im.menuChatLog())
                    data.menu.push(im.menuLeaveGroupBySelf())

                $(this).contextmenu(data)  //面板群组右键事件
            })

            $('.groupMembers > li').mousedown(function(e) {//聊天页面群组右键事件
                var data = {
                    contextItem: "context-group-member", // 添加class
                    isfriend: $(".context-group-member").data("isfriend"), // 添加class
                    target: function(ele) { // 当前元素
                        $(".context-group-member").attr("data-id",ele[0].id)
                        $(".context-group-member").attr("data-img",ele.find("img").attr('src'))
                        $(".context-group-member").attr("data-name",ele.find("span").html())
                        $(".context-group-member").attr("data-isfriend",ele.attr('isfriend'))
                        $(".context-group-member").attr("data-manager",ele.attr('manager'))
                        $(".context-group-member").attr("data-groupid",ele.parent().data('groupid'))
                        $(".context-group-member").attr("data-type",'C2C')
                    },
                    menu:[]
                }
                var _this = $(this)
                var groupInfo = conf.layim.thisChat().data;
                var _time = (new Date()).valueOf()//当前时间
                var _gagTime = parseInt(_this.attr('gagTime'))//当前禁言时间
                if (cachedata.mine.id !== _this.attr('id')) {
                    data.menu.push(im.menuChat())
                    data.menu.push(im.menuInfo())
                    if (3 == e.which && $(this).attr('isfriend') == 0 ) { //点击右键并且不是好友
                        data.menu.push(im.menuAddFriend())
                    }
                } else {
                    data.menu.push(im.menuEditGroupNickName())
                }
                if (groupInfo.manager == 1 && cachedata.mine.id !== _this.attr('id')) {//是群主且操作的对象不是自己
                    if (_this.attr('manager') == 2) {
                        data.menu.push(im.menuRemoveAdmin())
                    } else if (_this.attr('manager') == 3) {
                        data.menu.push(im.menuSetAdmin())
                    }
                    data.menu.push(im.menuEditGroupNickName())
                    data.menu.push(im.menuLeaveGroup())
                    if (_gagTime < _time) {
                        data.menu.push(im.menuGroupMemberGag())
                    } else {
                        data.menu.push(im.menuLiftGroupMemberGag())
                    }
                }//群主管理

                layui.each(cachedata.group, function(index, item) {
                    if (item.id == _this.parent().data('groupid') && item.manager == 2 && _this.attr('manager') == 3 && cachedata.mine.id !== _this.attr('id')) {//管理员且操作的是群员
                        data.menu.push(im.menuEditGroupNickName())
                        data.menu.push(im.menuLeaveGroup())
                        if (_gagTime < _time) {
                            data.menu.push(im.menuGroupMemberGag())
                        } else {
                            data.menu.push(im.menuLiftGroupMemberGag())
                        }
                    }//管理员管理
                })
                $(".groupMembers > li").contextmenu(data)
            })
        },        //自定义消息，把消息格式定义为layim的消息类型
        defineMessage: function (message, msgType) {
            var msg;
            switch (msgType)
            {
                case 'Text': msg = message.data;break;
                case 'Picture': msg = 'img['+message.thumb+']';break;
                case 'Audio': msg = 'audio['+message.audio+']';break;
                case 'File': msg = 'file('+message.url+')['+message.filename+']';break;
                case 'Video': msg = 'video['+message.video+']';break;
            }
            //如果有命令参数
            if (message.ext.cmd) {
                switch (message.ext.cmd.cmdName)
                {
                    case 'gag': //禁言
                        im.setGag({
                            groupid: message.to,
                            type: 'set',
                            user: message.ext.cmd.id,
                            gagTime: message.data
                        })
                    break;
                    case 'liftGag': //取消禁言
                        im.setGag({
                            groupid: message.to,
                            type: 'lift',
                            user: message.ext.cmd.id,
                            gagTime: 0
                        })
                    break;
                    // case 'setGag': //禁言
                    // case 'joinGroup': //取消禁言
                    // case 'joinGroup': //加入群
                    // case 'leaveGroup': //退出群
                    // case 'setAdmin': //设置管理员
                    // case 'removeAdmin': //取消管理员
                    // break;
                    default:
                        conf.layim.getMessage({
                          system: true //系统消息
                          ,id: message.to //聊天窗口ID
                          ,type: "GROUP" //聊天窗口类型
                          ,content: msg
                        })
                }
            }
            if (message.type == 'chat') {
                var type = 'C2C';
                var id = message.from;
            } else if (message.type == 'groupchat') {
                var type = 'GROUP';
                var id = message.to;
            }
            if (message.delay) {//离线消息获取不到本地cachedata用户名称需要从服务器获取
                var timestamp = Date.parse(new Date(message.delay))
            } else {
                var timestamp = (new Date()).valueOf()
            }
            var data = {mine: false,cid: 0,account:message.ext.account,avatar:"./uploads/person/"+message.from+".jpg",content:msg,id:id,fromid: message.from,timestamp:timestamp,type:type}
            if (!message.ext.cmd) {conf.layim.getMessage(data) }

        },
        sendMsg: function (res) {  //根据layim提供的data数据，进行解析
            console.log('sendMessage res', res)

            console.log('WSHB.ws state: ', WSHB.ws.readyState, WebSocket.OPEN, WSHB.ws.readyState !== WebSocket.OPEN)
            if (WSHB.ws.readyState !== WebSocket.OPEN) {
                layer.open({
                    type: 1
                    , content: '请稍后重试'
                })
            }
            // 发送消息
            // var mine = JSON.stringify(res.mine)
            // var to = JSON.stringify(res.to)
            // var message = {
            //     token: token,
            //     'type': 'dialog',
            //     'mine': res.mine,
            //     'to': res.to,
            // }
            var message = {};
            if (!res.from) {
                var created_at = res.mine.timestamp ? res.mine.timestamp : (new Date()).valueOf();
                var payload = {
                    'msg_type': "text",
                    'text': res.mine.content,
                }
                res.to.type = res.to.type == 'friend' ? 'C2C' : res.to.type
                message = {
                    'id': uuidv4(),
                    'type': res.to.type,
                    'from': res.mine.id,
                    'to': res.to.id,
                    'payload': payload,
                    'created_at': created_at,
                }
            } else {
                message = res
            }

            var json = JSON.stringify(message)
            console.log('sendMessage json: ', json)
            WSHB.send(json)
        },
        getChatLog: function (data) {
            if (!cachedata.base.chatLog) {
                return layer.msg('未开启更多聊天记录')
            }
            var index = layer.open({
                type: 2
                ,maxmin: true
                ,title: '与 '+ data.name +' 的聊天记录'
                ,area: ['450px', '600px']
                ,shade: false
                ,skin: 'layui-box'
                ,anim: 2
                ,id: 'layui-layim-chatlog'
                ,content: cachedata.base.chatLog + '?id=' + data.id + '&type=' + data.type
            })
        },
        removeFriends: function (account) {
            conn.removeRoster({
                to: account,
                success: function () {  // 删除成功
                    $.get('class/doAction.php?action=removeFriends', {friend_id: account}, function (res) {
                        var data = eval('(' + res + ')')
                        if (data.code == 0) {
                            var index = layer.open()
                            layer.close(index)
                            conf.layim.removeList({//从我的列表删除
                                type: 'friend' //或者group
                                ,id: account //好友或者群组ID
                            })
                            im.removeHistory({//从我的历史列表删除
                                type: 'friend' //或者group
                                ,id: account //好友或者群组ID
                            })
                            parent.location.reload()
                        } else {
                            layer.msg(data.msg)
                        }
                    })
                },
                error: function () {
                    console.log('removeFriends faild')
                   // 删除失败
                }
            })
        },
        leaveGroupBySelf: function (to,account,roomId) {
            $.get('class/doAction.php?action=leaveGroup', {groupId:roomId,memberIdx:to}, function (res) {
                var data = eval('(' + res + ')')
                if (data.code == 0) {
                    var option = {
                        to: to,
                        roomId: roomId,
                        success: function (res) {
                            im.sendMsg({//系统消息
                                mine:{
                                    content:account+' 已退出该群',
                                    timestamp:new Date().getTime()
                                },
                                to:{
                                    id:roomId,
                                    type:'group',
                                    cmd:{
                                        cmdName:'leaveGroup',
                                        cmdValue:account
                                    }
                                }
                            })
                            conf.layim.removeList({
                                type: 'group' //或者group
                                ,id: roomId //好友或者群组ID
                            })
                            im.removeHistory({//从我的历史列表删除
                                type: 'group' //或者group
                                ,id: roomId //好友或者群组ID
                            })
                            var index = layer.open()
                            layer.close(index)
                            parent.location.reload()
                        },
                        error: function (res) {
                            console.log('Leave room faild')
                        }
                    }
                    conn.leaveGroupBySelf(option)
                } else {
                    layer.msg(data.msg)
                }
            })
        },
        removeHistory: function(data) {//删除好友或退出群后清除历史记录
            var history = cachedata.local.history;
            delete history[data.type+data.id];
            cachedata.local.history = history;
            layui.data('layim', {
              key: cachedata.mine.id
              ,value: cachedata.local
            })
            $('#layim-history'+data.id).remove()
            var hisElem = $('.layui-layim').find('.layim-list-history')
            var none = '<li class="layim-null">暂无历史会话</li>'
            if (hisElem.find('li').length === 0) {
              hisElem.html(none)
            }
        },
        IsExist: function (avatar) { //判断头像是否存在
            var ImgObj=new Image()
            ImgObj.src= avatar;
             if (ImgObj.fileSize > 0 || (ImgObj.width > 0 && ImgObj.height > 0))
             {
               return true;
             } else {
               return false;
            }
        },
        audio:function(msg) {//消息提示
            conf.layim.msgbox(msg)
            var audio = document.createElement("audio")
            audio.src = layui.cache.dir+'css/modules/layim/voice/'+ cachedata.base.voice;
            try {
              audio.play()
            } catch(err) {
              console.log(err)
            }
        },
        // 申请好友
        applyFriend: function(othis) {
            console.log('conf.layim', conf.layim)
            //实际使用时数据由动态获得
            conf.layim.add({
                type: 'friend'
                ,account: $(othis).attr('account')
                ,avatar: $(othis).attr('avatar')
                ,submit: function(group_id, remark, index) {
                    var to_user_id = $(othis).attr('user_id')
                    if (to_user_id==curr_id) {
                        layer.msg('没有必要添加自己为好友吧', {
                            icon: 2
                            ,shade: 0.5
                        }, function() {
                            layer.close(index)
                        })
                        return false
                    }
                    //通知对方
                    $.post(
                        apiurl.friend_apply,
                        {
                            to_user_id: to_user_id,
                            group_id: group_id,
                            remark: remark,
                        },
                        function(res) {
                            if (res.code != 0) {
                                return layer.msg(res.msg)
                            } else {
                                layer.msg('好友申请已发送，请等待对方确认', {
                                    icon: 1
                                    ,shade: 0.5
                                }, function() {
                                    layer.close(index)
                                })
                            }
                        }
                    )
                }
            })
        },
        receiveAapplyFriend:function(othis, action) {//确认添加好友或群
            var li = othis.parents('li')

            var friend_id = $(othis).parent().attr('friend_id')
            var _doAddFriend = function(friend_id, action, group_id, callback) {
                $.ajax({
                    type: "POST",
                    url: apiurl.friend_add,
                    data: {
                        friend_id: friend_id,
                        action: action,
                        group_id: group_id,
                    },
                    success: function(res) {
                        if(res.code != 0){
                            return layer.msg(res.msg)
                        }
                        // console.log('_doAddFriend callback', res)
                        callback && callback(res)
                    },
                    error: function(xhr){
                        if (xhr.responseJSON && xhr.responseJSON.msg) {
                            layer.msg(xhr.responseJSON.msg)
                        } else {
                            layer.msg("{{ _('未知错误') }}")
                        }
                    }
                })
            }
            if (action=='agree') {
                var li = othis.parents('li')
                var uid = li.data('uid')
                var account = li.data('account')
                var sign = li.data('sign')
                var avatar = li.data('avatar')
                //选择分组
                conf.layim.setFriendGroup({
                    type: 'friend'
                    ,account: account
                    ,avatar: avatar
                    ,group: conf.layim.cache().friend //获取好友分组数据
                    ,submit: function(group, index){
                        _doAddFriend(friend_id, action, group, function(res) {
                            // console.log('submit')
                            //将好友追加到主面板
                            conf.layim.addList({
                                type: 'friend'
                                ,avatar: avatar //好友头像
                                ,account: account //好友昵称
                                ,groupid: group //所在的分组id
                                ,id: uid //好友ID
                                ,sign: sign //好友签名
                            })
                            parent.layer.close(index)
                            that.parent().html('已同意')
                        })
                    }
                })
            } else {
                layer.confirm('确定拒绝吗？', function (index) {
                    _doAddFriend(friend_id, action, '0', function(res) {
                        if(res.code != 0){
                            return layer.msg(res.msg)
                        }
                        layer.close(index)
                        that.parent().html('<em>已拒绝</em>')
                    })
                })
            }
        },
        //创建群
        createGroup: function(othis) {
            var index = layer.open({
                type: 2
                ,title: '创建群'
                ,shade: false
                ,maxmin: false
                ,area: ['550px', '400px']
                ,skin: 'layui-box layui-layer-border'
                ,resize: false
                ,content: cachedata.base.createGroup
            })
        },
        commitGroupInfo: function(othis,data) {
            if (!data.groupname) {
                return false;
            }
            $.get('class/doAction.php?action=userMaxGroupNumber', {}, function(res) {
                var resData = eval('(' + res + ')')
                if (resData.code == 0) {
                    var options = {
                        data: {
                            groupname: data.groupname,
                            desc: data.des,
                            maxusers:data.number,
                            public: true,
                            approval: data.approval == '1'?true:false,
                            allowinvites: true
                        },
                        success: function (respData) {
                            if (respData.data.groupid) {
                                $.get('class/doAction.php?action=commitGroupInfo', {groupId:respData.data.groupid,groupname: data.groupname,des:data.des,number:data.number,approval:data.approval}, function(respdata) {
                                    var res = eval('(' + respdata + ')')
                                    if (res.code == 0) {
                                        //将群 追加到主面板
                                        var avatar = './uploads/person/'+respData.data.groupid+'.jpg';
                                        layer.msg(res.msg)
                                        conf.layim.addList({
                                            type: 'group'
                                            , avatar: im['IsExist'].call(this, avatar)?avatar:default_avatar //好友头像
                                            , groupname: data.groupname //群名称
                                            , id: respData.data.groupid //群id
                                        })
                                    } else {
                                        return layer.msg(res.msg)
                                    }
                                    layer.close(layer.index)
                                })
                            }

                        },
                        error: function () {}
                    }
                    conn.createGroupNew(options)
                } else {
                    return layer.msg(resData.msg)
                }
                layer.close(layer.index)
            })
        },
        getMyInformation: function() {
            var index = layer.open({
                type: 2
                ,title: '我的资料'
                ,shade: false
                ,maxmin: false
                ,area: ['400px', '670px']
                ,skin: 'layui-box layui-layer-border'
                ,resize: true
                ,content: cachedata.base.Information+'?id='+cachedata.mine.id+'&type=friend'
            })
        },
        getInformation: function(data) {
           var id = data.id || {},type = data.type || {}
            var index = layer.open({
                type: 2
                ,title: type  == 'friend'?(cachedata.mine.id == id?'我的资料':'好友资料') :'群资料'
                ,shade: false
                ,maxmin: false
                // ,closeBtn: 0
                ,area: ['400px', '670px']
                ,skin: 'layui-box layui-layer-border'
                ,resize: true
                ,content: cachedata.base.Information+'?id='+id+'&type='+type
            })
        },
        /*
        userStatus: function(data) {
            if (data.id) {
                $.get('class/doAction.php?action=userStatus', {id:data.id}, function (res) {
                    var data = eval('(' + res + ')')
                    if (data.code == 0) {
                        if (data.data == 'online') {
                            conf.layim.setChatStatus('<span style="color:#FF5722;">在线</span>')
                        } else {
                            conf.layim.setChatStatus('<span style="color:#444;">离线</span>')
                        }
                    } else {
                        //没有该用户
                    }
                })
            }
        },
        */
        groupMembers: function(othis, e) {
            var othis = $(this)
            var icon = othis.find('.layui-icon'), hide = function() {
                icon.html('&#xe602;')
                $("#layui-layim-chat > ul:eq(1)").remove()
                $(".layui-layim-group-search").remove()
                othis.data('show', null)
            }
            if (othis.data('show')) {
                hide()
            } else {
                icon.html('&#xe603;')
                othis.data('show', true)
                var members = cachedata.base.members || {},ul = $("#layui-layim-chat"), li = '', membersCache = {}
                var info = JSON.parse(decodeURIComponent(othis.parent().data('json')))
                members.data = $.extend(members.data, {
                  id: info.id
                })
                $.get(members, function(res) {
                    var resp = eval('(' + res + ')')
                    var html = '<ul class="layui-unselect layim-group-list groupMembers" data-groupid="'+info.id+'" style="height: 510px; display: block;right:-200px;padding-top: 10px;">';
                    layui.each(resp.data.list, function(index, item) {
                        html += '<li  id="'+item.id+'" isfriend="'+item.friendship+'" manager="'+item.type+'" gagTime="'+item.gagTime+'"><img src="'+ item.avatar +'">';
                        item.type == 1?
                            (html += '<span style="color:#e24242">'+ item.account +'</span><i class="layui-icon" style="color:#e24242">&#xe612;</i>'):
                            (item.type == 2?
                                (html += '<span style="color:#de6039">'+ item.account +'</span><i class="layui-icon" style="color:#eaa48e">&#xe612;</i>'):
                                (html += '<span>'+ item.account +'</span>'))
                        html += '</li>';
                        membersCache[item.id] = item;
                    })
                    html += '</ul>';
                    html += '<div class="layui-layim-group-search" socket-event="groupSearch"><input placeholder="搜索"></div>';
                    ul.append(html)
                    im.contextmenu()
                })
            }
        },
        closeAllGroupList: function() {
            var othis = $(".groupMembers")
            othis.remove()//关闭全部的群员列表
            $(".layui-layim-group-search").remove()
            var icon = $('.layim-tool-groupMembers').find('.layui-icon')
            $('.layim-tool-groupMembers').data('show', null)
            icon.html('&#xe602;')
        },
        groupSearch: function(othis) {
          var search = $("#layui-layim-chat").find('.layui-layim-group-search')
          var main = $("#layui-layim-chat").find('.groupMembers')
          var input = search.find('input'), find = function(e) {
            var val = input.val().replace(/\s/)
            var data = [];
            var group = $(".groupMembers li") || [], html = '';
            if (val === '') {
              for(var j = 0; j < group.length; j++) {
                  group.eq(j).css("display","block")
              }
            } else {
                for(var j = 0; j < group.length; j++) {
                    name = group.eq(j).find('span').html()
                    if (name.indexOf(val) === -1) {
                        group.eq(j).css("display","none")
                    } else {
                        group.eq(j).css("display","block")
                    }
                }
            }
          }
          if (!cachedata.base.isfriend && cachedata.base.isgroup) {
            events.tab.index = 1;
          } else if (!cachedata.base.isfriend && !cachedata.base.isgroup) {
            events.tab.index = 2;
          }
          search.show()
          input.focus()
          input.off('keyup', find).on('keyup', find)
        },
        addMyGroup: function() {
            // 新增分组
            api_ajax(apiurl.add_category, 'post', {}, function(res) {
                var data = res.payload
                // 给新建分组添加缓存
                cachedata.friend.push({id:data.id, groupname:data.name, list:[]})

                $('.layim-list-friend').append('<li><h5 layim-event="spread" lay-type="false" data-groupid="' + data.id + '"><i class="layui-icon">&#xe602;</i><span>' + data.name + '</span><em>(<cite class="layim-count"> 0</cite>)</em></h5><ul class="layui-layim-list"><li class="layim-null">该分组下暂无好友</li></ul></li>')
                im.contextmenu()
            }, default_error_callback)
        },
        delMyGroup: function(groupid) {
            // 删除分组
            // console.log('vcbxcfbxcvb',groupid)
            var params = {id:groupid}
            api_ajax(apiurl.delete_category, 'delete', params, function(res) {
                var group = $('.layim-list-friend li') || [];
                // 遍历每一个分组
                for(var j = 0; j < group.length; j++) {
                    var groupList = group.eq(j).find('h5').data('groupid')
                    // console.log('groupList', groupList)
                    if (groupList !== groupid) {
                        continue
                    }
                    //要删除的分组
                    if (group.eq(j).find('ul li').hasClass('layim-null')) {
                        // 删除的分组下没有好友
                        group.eq(j).remove()
                    } else {
                        // var html = group.eq(j).find('ul').html()//被删除分组的好友
                        var friend = group.eq(j).find('ul li')
                        var number = friend.length;//被删除分组的好友个数
                        for (var i = 0; i < number; i++) {
                            var friend_id = friend.eq(i).attr('class').replace(/^layim-friend/g, '')//好友id
                            var friend_name = friend.eq(i).find('span').html()//好友id
                            var signature = friend.eq(i).find('p').html()//好友id
                            var avatar = friend.eq(i).find('img').attr('src');
                            // console.log('friend ', friend_id, friend_name, avatar, signature, 'friend.eq(i) ', friend.eq(i))
                            conf.layim.removeList({//将好友从之前分组除去
                                type: 'friend'
                                ,id: friend_id //好友ID
                            })
                            conf.layim.addList({//将好友移动到新分组
                                type: 'friend'
                                , avatar: im['IsExist'].call(this, avatar) ? avatar : default_avatar //好友头像
                                , account: friend_name //好友昵称
                                , groupid: 0 //将好友添加到默认分组
                                , id: friend_id //好友ID
                                , sign: signature //好友签名
                            })
                        }
                        group.eq(j).remove()
                    }
                }
                im.contextmenu()
                layer.close(layer.index)
            }, default_error_callback)
        },
        setAdmin: function(othis) {
            var account = othis.data('id'),friend_avatar = othis.data('img'),
                isfriend = othis.data('isfriend'),name = othis.data('name'),
                gagTime = othis.data('gagtime'),groupid = othis.data('groupid')
            var options = {
                    groupId: groupid,
                    account: account,
                    success: function(resp) {
                        $.get('class/doAction.php?action=setAdmin', {groupid:groupid,memberIdx:account,type:2}, function (res) {
                            var admin = eval('(' + res + ')')
                            if (admin.code == 0) {
                                $("ul[data-groupid="+groupid+"] #"+account).remove()
                                var html = '<li id="'+account+'" isfriend="'+isfriend+'" manager="2" gagTime="'+gagTime+'"><img src="'+friend_avatar+'"><span style="color:#de6039">'+name+'</span><i class="layui-icon" style="color:#eaa48e"></i></li>'
                                $("ul[data-groupid="+groupid+"]").find('li').eq(0).after(html)
                                im.contextmenu()
                            }
                            layer.msg(admin.msg)
                        })
                    },
                    error: function(e) {
                    }
                }
            conn.setAdmin(options)
        },
        removeAdmin: function(othis) {
            var account = othis.data('id'),friend_avatar = othis.data('img'),
                isfriend = othis.data('isfriend'),name = othis.data('name').split('<'),
                gagTime = othis.data('gagtime'),groupid = othis.data('groupid')
            var options = {
                    groupId: groupid,
                    account: account,
                    success: function(resp) {
                        $.get('class/doAction.php?action=setAdmin', {groupid:groupid,memberIdx:account,type:3}, function (res) {
                            var admin = eval('(' + res + ')')
                            if (admin.code == 0) {
                                $("ul[data-groupid="+groupid+"] #"+account).remove()
                                var html = '<li id="'+account+'" isfriend="'+isfriend+'" manager="3" gagTime="'+gagTime+'"><img src="'+friend_avatar+'"><span>'+name[0]+'</span></li>'
                                $("ul[data-groupid="+groupid+"]").append(html)
                                im.contextmenu()
                            }
                            layer.msg(admin.msg)
                        })
                    },
                    error: function(e) {
                    }
                }
            conn.removeAdmin(options)
        },
        editGroupNickName: function(othis) {
            var memberIdx = othis.data('id'),name = othis.data('name').split('('),groupid = othis.data('groupid')
            layer.prompt({title: '请输入群名片，并确认', formType: 0,value: name[0]}, function(nickName, index) {
                $.get('class/doAction.php?action=editGroupNickName',{nickName:nickName,memberIdx:memberIdx,groupid:groupid},function(res) {
                    var data = eval('(' + res + ')')
                    if (data.code == 0) {
                        $("ul[data-groupid="+groupid+"] #"+memberIdx).find('span').html(nickName+'('+memberIdx+')')
                        layer.close(index)
                    }
                    layer.msg(data.msg)
                })
            })
        },
        leaveGroup: function(groupid,list,account) {//list为数组
            $.get('class/doAction.php?action=leaveGroup',{list:list,groupid:groupid},function(res) {
                var data = eval('(' + res + ')')
                if (data.code == 0) {
                    var options = {
                        roomId: groupid,
                        list: list,
                        success: function(resp) {
                            console.log(resp)
                        },
                        error: function(e) {
                            console.log(e)
                        }
                    }
                    conn.leaveGroup(options)
                    $("ul[data-groupid="+groupid+"] #"+data.data).remove()
                    im.sendMsg({//系统消息
                        mine:{
                            content:account+' 已被移出该群',
                            timestamp:new Date().getTime()
                        },
                        to:{
                            id:groupid,
                            type:'group',
                            cmd:{
                                cmdName:'leaveGroup',
                                cmdValue:account
                            }
                        }
                    })
                    var index = layer.open()
                    layer.close(index)
                }
                layer.msg(data.msg)
            })
        },
        setGag: function(options) {//设置禁言 取消禁言
            var _this_group = $('.layim-chat-list .layim-chatlist-group'+options.groupid)//选择操作的群
            if (_this_group.find('span').html()) {
                var index = _this_group.index()//对应面板的index
                var cont =  _this_group.parent().parent().find('.layim-chat-box div').eq(index)
                var info = JSON.parse(decodeURIComponent(cont.find('.layim-chat-tool').data('json')))
                // info.manager = message.ext.cmd.cmdValue;第一种
                //禁言 两种方案 第一种是改变用户的状态 优点：只需要判断该参数就能禁言
                // 第二种是设置一个禁言时间点，当当前时间小于该设置的时间则为禁言，优点：自动改变用户禁言状态
                if (options.type == 'set' && (options.user == cachedata.mine.id || options.user == 'ALL')) {//设置禁言单人或全体
                    if (options.gagTime) {
                        info.gagTime = parseInt(options.gagTime)
                        cont.find('.layim-chat-tool').data('json',encodeURIComponent(JSON.stringify(info)))
                        layui.each(cachedata.group, function(index, item) {
                            if (item.id === options.groupid) {
                                cachedata.group[index].gagTime = info.gagTime;
                            }
                        })
                    }
                    cont.find('.layim-chat-gag').css('display','block')
                } else if (options.type == 'lift' && (options.user == cachedata.mine.id || options.user == 'ALL')) {//取消禁言单人或全体
                    cont.find('.layim-chat-tool').data('json',encodeURIComponent(JSON.stringify(info)))
                    layui.each(cachedata.group, function(index, item) {
                        if (item.id === options.groupid) {
                            cachedata.group[index].gagTime = '0';
                        }
                    })
                    cont.find('.layim-chat-gag').css('display','none')
                }
            } else {
                if (options.type == 'set' && (options.user == cachedata.mine.id || options.user == 'ALL')) {//设置禁言单人或全体
                    if (options.gagTime) {
                        layui.each(cachedata.group, function(index, item) {
                            if (item.id === options.groupid) {
                                cachedata.group[index].gagTime = parseInt(options.gagTime)
                            }
                        })
                    }
                } else if (options.type == 'lift' && (options.user == cachedata.mine.id || options.user == 'ALL')) {//取消禁言单人或全体
                    layui.each(cachedata.group, function(index, item) {
                        if (item.id === options.groupid) {
                            cachedata.group[index].gagTime = '0';
                        }
                    })
                }
            }
        },
        popMsg: function(data,msg) {//删除本地最新一条发送失败的消息
            var logid = cachedata.local.chatlog[data.to.type+data.to.id];
                logid.pop()
            var timestamp = '.timestamp'+data.mine.timestamp;
            $(timestamp).html('<i class="layui-icon" style="color: #F44336;font-size: 20px;float: left;margin-top: 1px;">&#x1007;</i>'+msg)
        },
        readMsg: function(ids) {
            // console.log('readmsg othis: ', othis)
            console.log('readmsg ids: ', ids)
            // $.post('/conversation/msg/', {action:'set_allread'}, function (res) {
            // })
        },
        menuChat: function() {
            return data = {
                text: "发送消息",
                icon: "&#xe63a;",
                callback: function(ele) {
                    var othis = ele.parent(),type = othis.data('type'),
                        name = othis.data('name'),avatar = othis.data('img'),
                        id = othis.data('id')
                        // id = (new RegExp(substr).test('layim')?substr.replace(/[^0-9]/ig,""):substr)
                    conf.layim.chat({
                        name: name
                        ,type: type
                        ,avatar: avatar
                        ,id: id
                    })
                }
            }
        },
        menuInfo: function() {
            return data =  {
                text: "查看资料",
                icon: "&#xe62a;",
                callback: function(ele) {
                    var othis = ele.parent(),type = othis.data('type'),id = othis.data('id')
                        // id = (new RegExp(substr).test('layim')?substr.replace(/[^0-9]/ig,""):substr)
                    im.getInformation({
                        id: id,
                        type:type
                    })
                }
            }
        },
        menuChatLog: function() {
            return data =  {
                text: "聊天记录",
                icon: "&#xe60e;",
                callback: function(ele) {
                    var othis = ele.parent(),type = othis.data('type'),name = othis.data('name'),
                    id = othis.data('id')
                    im.getChatLog({
                        name: name,
                        id: id,
                        type:type
                    })
                }
            }
        },
        menuNickName: function() {
            return data =  {
                text: "修改好友备注",
                icon: "&#xe6b2;",
                callback: function(ele) {
                    var othis = ele.parent(),uid = othis.data('id'),friend_name = othis.data('name')
                    layer.prompt({title: '修改好友备注', formType: 0,value: friend_name}, function(remark, index) {
                        api_ajax(apiurl.change_remark, 'put',{remark:remark, uid:uid}, function(res) {
                            if (res.code == 0) {
                                $(".layim-friend"+uid).find('span').html(res.payload)
                                layer.close(index)
                            } else {
                                layer.msg(res.msg)
                            }
                        })
                    })
                }
            }
        },
        menuMove: function(html) {
            return data = {
                text: "移动联系人",
                icon: "&#xe630;",
                nav: "move",//子导航的样式
                navIcon: "&#xe602;",//子导航的图标
                navBody: html,//子导航html
                callback: function(ele) {
                    // 要移动的好友id
                    var uid = ele.parent().data('id')
                    var status = ele.parent().data('status')
                    var friend_name = ele.parent().data('name')
                    var avatar = ele.parent().data('img')
                    console.log('friend ', uid, friend_name, avatar, 'ele ', ele)
                    //获取签名
                    var signature = $('.layim-list-friend').find('#layim-friend'+uid).find('p').html()
                    var item = ele.find("ul li")
                    item.hover(function() {
                        var that_index = item.index(this)
                        // 将好友移动到分组的id
                        var groupid = item.eq(that_index).data('groupid')
                        var params = {category_id:groupid, uid:uid}
                        api_ajax(apiurl.friend_move, 'put', params, function(res) {
                            conf.layim.removeList({//将好友从之前分组除去
                                type: 'friend'
                                ,id: uid //好友ID
                            })
                            conf.layim.addList({//将好友移动到新分组
                                type: 'friend'
                                , avatar: im['IsExist'].call(this, avatar)?avatar:default_avatar //好友头像
                                , account: friend_name //好友昵称
                                , groupid: groupid //所在的分组id
                                , id: uid //好友ID
                                , sign: signature //好友签名
                            })
                            conf.layim.setFriendStatus(uid, status)
                        }, default_error_callback)
                    })
                }
            }
        },
        menuRemove: function() {
            return data = {
                text: "删除好友",
                icon: "&#xe640;",
                events: "removeFriends",
                callback: function(ele) {
                    var othis = ele.parent(),uid = othis.data('id'),account,sign;
                    layui.each(cachedata.friend, function(index1, item1) {
                        layui.each(item1.list, function(index, item) {
                            if (item.id === uid) {
                                account = item.account;
                                sign = item.sign;
                            }
                        })
                    })
                    layer.confirm('删除后对方将从你的好友列表消失，且以后不会再接收此人的会话消息。<div class="layui-layim-list"><li layim-event="chat" data-type="friend" data-index="0"><img src="./uploads/person/'+uid+'.jpg"><span>'+account+'</span><p>'+sign+'</p></li></div>', {
                        btn: ['确定','取消'], //按钮
                        title:['删除好友','background:#b4bdb8'],
                        shade: 0
                    }, function() {
                        im.removeFriends(uid)
                    }, function() {
                        var index = layer.open()
                        layer.close(index)
                    })
                }
            }
        },
        menuAddMyGroup: function() {
            return  data =  {
                text: "添加分组",
                icon: "&#xe654;",
                callback: function(ele) {
                    im.addMyGroup()
                }
            }
        },
        menuRename: function() {
            return  data =  {
                text: "重命名",
                icon: "&#xe642;",
                callback: function(ele) {
                    var othis = ele.parent()
                    var groupid = othis.data('id')
                    var groupname = othis.data('name')

                    console.log('menuRename ', groupid, groupname, ele, othis)
                    if (!(groupid>0)) {
                        return false
                    }
                    if (groupname=='未分组') {
                        layui.layer.msg('无法重命名【未分组】')
                        return false
                    }
                    layer.prompt({title: '请输入分组名，并确认', formType: 0, value: groupname}, function(groupname, index) {
                        if (groupname) {
                            var params = {id:groupid, name:groupname}
                            api_ajax(apiurl.rename_category, 'put', params, function(res) {
                                var friend_group = $(".layim-list-friend li")
                                for(var j = 0; j < friend_group.length; j++) {
                                    var groupid2 = friend_group.eq(j).find('h5').data('groupid')
                                    if (groupid2 == groupid) {//当前选择的分组
                                        friend_group.eq(j).find('h5').find('span').html(groupname)
                                    }
                                }
                                im.contextmenu()
                                layer.close(index)
                            }, default_error_callback)
                        }
                    })
                }
            }
        },
        menuDelMyGroup: function() {
            return  data =  {
                text: "删除该组",
                icon: "&#x1006;",
                callback: function(ele) {
                    var othis = ele.parent()
                    var groupid = othis.data('id')
                    console.log('groupid', groupid, 'ele', ele, 'othis', othis)
                    layer.confirm('<div style="float: left;width: 17%;margin-top: 14px;"><i class="layui-icon" style="font-size: 48px;color:#cc4a4a">&#xe607;</i></div><div style="width: 83%;float: left;"> 选定的分组将被删除，组内联系人将会移至默认分组。</div>', {
                        btn: ['确定','取消'], //按钮
                        title:['删除分组','background:#b4bdb8'],
                        shade: 0
                    }, function() {
                        im.delMyGroup(groupid)
                    }, function() {
                        var index = layer.open()
                        layer.close(index)
                    })
                }
            }
        },
        menuLeaveGroupBySelf: function() {
            return  data =  {
                text: "退出该群",
                icon: "&#xe613;",
                callback: function(ele) {
                    var othis = ele.parent(),
                        group_id = othis.data('id'),
                        groupname = othis.data('name')
                        avatar = othis.data('img')
                    layer.confirm('您真的要退出该群吗？退出后你将不会再接收此群的会话消息。<div class="layui-layim-list"><li layim-event="chat" data-type="friend" data-index="0"><img src="'+avatar+'"><span>'+groupname+'</span></li></div>', {
                        btn: ['确定','取消'], //按钮
                        title:['提示','background:#b4bdb8'],
                        shade: 0
                    }, function() {
                        var user = cachedata.mine.id;
                        var account = cachedata.mine.account;
                        im.leaveGroupBySelf(user,account,group_id)
                    }, function() {
                        var index = layer.open()
                        layer.close(index)
                    })
                }
            }
        },
        menuAddFriend: function(ele) {
            console.log('menuAddFriend', ele)
            return  data =  {
                text: "添加好友",
                icon: "&#xe654;",
                callback: function(ele) {
                    var othis = ele;
                    im.addFriendGroup(othis,'friend')
                }
            }
        },
        menuEditGroupNickName: function() {
            return  data =  {
                text: "修改群名片",
                icon: "&#xe60a;",
                callback: function(ele) {
                    var othis = ele.parent()
                    im.editGroupNickName(othis)
                }
            }
        },
        menuRemoveAdmin: function() {
            return  data =  {
                text: "取消管理员",
                icon: "&#xe612;",
                callback: function(ele) {
                    var othis = ele.parent()
                    im.removeAdmin(othis)
                }
            }
        },
        menuSetAdmin: function() {
            return  data =  {
                text: "设置为管理员",
                icon: "&#xe612;",
                callback: function(ele) {
                    var othis = ele.parent(),user = othis.data('id')
                    im.setAdmin(othis)
                }
            }
        },
        menuLeaveGroup: function() {
            return  data =  {
                text: "踢出本群",
                icon: "&#x1006;",
                callback: function(ele) {
                    var othis = ele.parent()
                    var friend_id = ele.parent().data('id')//要禁言的id
                    var account = ele.parent().data('name')
                    var groupId = ele.parent().data('groupid')
                    var list = new Array()
                    list[0] = friend_id;
                    im.leaveGroup(groupId,list,account)
                }
            }
        },
        menuGroupMemberGag: function() {
            return  data =  {
                text: "禁言",
                icon: "&#xe60f;",
                nav: "gag",//子导航的样式
                navIcon: "&#xe602;",//子导航的图标
                navBody: '<ul><li class="ui-gag-menu-item" data-gag="10m"><a href="javascript:void(0)"><span>禁言10分钟</span></a></li><li class="ui-gag-menu-item" data-gag="1h"><a href="javascript:void(0)"><span>禁言1小时</span></a></li><li class="ui-gag-menu-item" data-gag="6h"><a href="javascript:void(0)"><span>禁言6小时</span></a></li><li class="ui-gag-menu-item" data-gag="12h"><a href="javascript:void(0)"><span>禁言12小时</span></a></li><li class="ui-gag-menu-item" data-gag="1d"><a href="javascript:void(0)"><span>禁言1天</span></a></li></ul>',//子导航html
                callback: function(ele) {
                    var friend_id = ele.parent().data('id')//要禁言的id
                    friend_name = ele.parent().data('name')
                    groupid = ele.parent().data('groupid')
                    var item = ele.find("ul li")
                    item.hover(function() {
                        var _index = item.index(this),gagTime = item.eq(_index).data('gag')//禁言时间
                        $.get('class/doAction.php?action=groupMemberGag',{gagTime:gagTime,groupid:groupid,friend_id:friend_id},function(resp) {
                            var data = eval('(' + resp + ')')
                            if (data.code == 0) {
                                var gagTime = data.data.gagTime;
                                var res = {mine: {
                                                content: gagTime+'',
                                                timestamp: data.data.time
                                            },
                                            to: {
                                                type: 'group',
                                                id: groupid+"",
                                                cmd: {
                                                    id: friend_id,
                                                    cmdName:'gag',
                                                    cmdValue:data.data.value
                                                }
                                            }}
                                im.sendMsg(res)
                                $("ul[data-groupid="+groupid+"] #"+friend_id).attr('gagtime',gagTime)
                            }
                            layer.msg(data.msg)
                        })
                    })
                }
            }
        },
        menuLiftGroupMemberGag: function() {
            return  data =  {
                text: "取消禁言",
                icon: "&#xe60f;",
                callback: function(ele) {
                    var friend_id = ele.parent().data('id')//要禁言的id
                    friend_name = ele.parent().data('name')
                    groupid = ele.parent().data('groupid')
                    $.get('class/doAction.php?action=liftGroupMemberGag',{groupid:groupid,friend_id:friend_id},function(resp) {
                        var data = eval('(' + resp + ')')
                        if (data.code == 0) {
                            var res = {mine: {
                                content: '0',
                                timestamp: data.data.time
                            },
                            to: {
                                type: 'group',
                                id: groupid+"",
                                cmd: {
                                    id: friend_id,
                                    cmdName:'liftGag',
                                    cmdValue:data.data.value
                                }
                            }}
                            im.sendMsg(res)
                            $("ul[data-groupid="+groupid+"] #"+friend_id).attr('gagtime',0)
                        }
                        layer.msg(data.msg)
                    })
                }
            }
        },
    }
    exports('chat', chat)
    exports('im', im)
})
