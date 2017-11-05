exports.getSocketIOImpl = function (app) {
  return function () {
    return require('socket.io')(app)
  }
}

exports.ioOn = function (io) {
  return function (str) {
    return function (callback) {
      return function () {
        io.on(str, function (data) {
          return callback(data)()
        })
      }
    }
  }
}

exports.socketEmit = function (socket) {
  return function (str) {
    return function (a) {
      return function () {
        socket.emit(str, a)
      }
    }
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