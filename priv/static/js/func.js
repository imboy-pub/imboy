
function refreshtoken() {
    layui.jquery.ajax({
        type: "POST",
        url: '/refreshtoken',
        data: {},
        headers: {
            'imboy-refreshtoken': get_cookie('imboy-refreshtoken'),
        },
        success: function(res) {
            if (res && res.code==0) {
                // 设置有效时间为 10天
                set_cookie('imboy-refreshtoken', res.data.refreshtoken, {expires: 10, path: '/'})
                set_cookie('imboy-token', res.data.token, {expires: 1, path: '/'})
            } else if(res && res.code == 706) {
                location.href = res.next ? res.next : '/passport/login.html'
            } else if(res && res.msg) {
                layer.msg(res.msg)
            } else {
                console.log(res)
            }
        },
        error: function(xhr){
            console.log(xhr.responseJSON)
            if (xhr.responseJSON && xhr.responseJSON.msg) {
                layer.msg(xhr.responseJSON.msg)
            } else {
                layer.msg('未知错误')
            }
        }
    })
}

function set_cookie(name, value, options)
{
    var expires = options && options.expires ? options.expires : 1
    var path = options && options.path ? options.path : null
    var exp = new Date()
    if (value === null) {
        expires = -1
    }
    exp.setTime(exp.getTime() + expires * 86400000)
    document.cookie = name + "="+ value + "; expires=" + exp.toGMTString()+ "; path=" + path
}

function get_cookie(name) {
    var strcookie = document.cookie; //获取cookie字符串
    var arrcookie = strcookie.split("; ");//分割
    //遍历匹配
    for ( var i = 0; i < arrcookie.length; i++) {
        var cookie = arrcookie[i]
        var index = cookie.indexOf('=')
        if (cookie.slice(0, index) == name){
            return cookie.slice(index+1)
        }
    }
    return "";
}

function default_error_callback(xhr, res) {
    console.log('xhr ', xhr, 'res ', res)
    if (res && res.msg) {
        layui.layer.msg(res.msg, {icon:2})
    } else if (xhr && xhr.responseJSON && xhr.responseJSON.msg) {
        layui.layer.msg(xhr.responseJSON.msg, {icon:2})
    } else {
        layui.layer.msg('未知错误.', {icon:2})
    }
}
/**
 * [api_ajax description]
 * @param  {[type]}   url            [description]
 * @param  {[type]}   method         get post put delete
 * @param  {[type]}   params         [description]
 * @param  {Function} callback       [description]
 * @param  {[type]}   error_callback [description]
 * @param  {Boolean}  async          async. 默认是true，即为异步方式
 * @return {[type]}                  [description]
 */
function api_ajax(url, method, params, callback, error_callback, async) {
    if (!url) {
        return false
    }
    if (!method) {
        method = 'get'
    }
    if (!params) {
        params = {}
    }

    // params._xsrf = get_xsrf()
    headers = {
        'imboy-token': get_cookie('imboy-token'),
    }

    async = async===false ? false : true
    layui.jquery.ajax({
        type: method,
        url: url,
        async: async,
        data: params,
        headers: headers,
        dataType: 'json',
        success: function(res) {
            // console.log(res)
            if (res.code==0) {
                if (callback) {
                    callback(res)
                }
            } else if(res.code=='706') {
                current_token('clear')
                location.href = '/passport/login.html'
            } else {
                if (error_callback) {
                    error_callback(false, res)
                }
            }
        },
        error: function(xhr){
            if (error_callback) {
                error_callback(xhr, false)
            }
        }
    })
}
