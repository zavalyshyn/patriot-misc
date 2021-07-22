const fs = require('fs');

var imageAsBase64 = fs.readFileSync('./old/apps/honestassistant.svg', 'base64');

console.log('data:image/svg+xml;base64,'+imageAsBase64);