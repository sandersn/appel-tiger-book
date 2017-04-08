var ts = require('typescript')
exports.encodeURIComponent = encodeURIComponent
exports.parse = function (s) {
    return ts.createSourceFile('fake.ts', s).text
}
