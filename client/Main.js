exports.getElemValue = function (elem) {
  return function () {
    return elem.value
  }
}

exports.jQueryToElement = function (elem) {
  return elem[0]
}

exports.after = function (ob) {
  return function (ob1) {
    return function () {
      ob.after(ob1)
    }
  }
}

exports.trace = function (a) {
  console.log(a)
  return a
}