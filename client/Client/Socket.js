exports.getSocketIOClientImpl = function (url) {
  return function () {
    return require('socket.io-client')(url)
  }
}

exports.socketOn = function (socket) {
  return function (str) {
    return function (callback) {
      return function () {
        socket.on(str, function (data) {
          return callback(data)()
        })
      }
    }
  }
}