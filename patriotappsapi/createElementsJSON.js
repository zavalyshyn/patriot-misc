const fs = require('fs');
const elementsdb = require('./elements/elements');

let elementsObject = {};

for (let element in elementsdb) {
    let name = element;
    let deviceactions = elementsdb[element].deviceactions;
    let attributes = elementsdb[element].attributes;
    let attrtype = elementsdb[element].attrtype;
    let portdatatype = elementsdb[element].portdatatype;
    let stopport = elementsdb[element].stopport;
    let output = elementsdb[element].output;
    let helpers = elementsdb[element].helpers;

    elementsObject[name] = {
        name : name,
        deviceactions: deviceactions,
        attributes: attributes,
        attrtype : attrtype,
        portdatatype : portdatatype,
        stopport : stopport,
        output : output,
        helpers : helpers
    }
}

var json = JSON.stringify(elementsObject, null, 2);

fs.writeFile('./database/elements.json', json, 'utf8', (err) => {
    if (err) throw err;
    console.log('The file has been saved!');
});
