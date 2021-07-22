const devices = [
    {
        name: 'Living room camera',
        type: 'ipcamera',
        attrTypeIn: ['audio'],
    },
    {
        name: 'Baby camera',
        type: 'ipcamera',
        attrTypeIn: ['audio'],
    },
    {
        name: 'Door cam',
        type: 'ipcamera',
        attrTypeIn: ['audio'],
    },
    {
        name: 'Living room light',
        type: 'smartlight',
        attrTypeIn: ['boolean'],
    },
    {
        name: 'Bedroom light',
        type: 'smartlight',
        attrTypeIn: ['boolean'],
    },
    {
        name: 'Bedroom motion sensor',
        type: 'motionsensor',
        attrTypeIn: [],
    },
    {
        name: 'Door contact sensor',
        type: 'contactsensor',
        attrTypeIn: [],
    },
    {
        name: 'Internet',
        type: 'httprequest',
        attrTypeIn: ['any'],
    },
    {
        name: "Jack's phone",
        type: 'userphone',
        attrTypeIn: ['image','audio','video','text','boolean'],
    },
    {
        name: "Samantha's phone",
        type: 'userphone',
        attrTypeIn: ['image','audio','video','text','boolean'],
    },
    {
        name: "Guest's phone",
        type: 'userphone',
        attrTypeIn: ['image','audio','video','text','boolean'],
    },
    {
        name: "Home Alarm",
        type: 'alarm',
        attrTypeIn: ['boolean'],
    }
];

let attributes = [
    { name: 'Camera frame', type: 'image', devicetype: 'ipcamera'},
    { name: 'Video feed', type: 'video', devicetype: 'ipcamera'},
    { name: 'Audio feed', type: 'audio', devicetype: 'ipcamera'},
    { name: 'Light state', type: 'boolean', devicetype: 'smartlight'},
    { name: 'Motion detected', type: 'boolean', devicetype: 'motionsensor'},
    { name: 'Contact open', type: 'boolean', devicetype: 'contactsensor'},
];

let devicesAttributesObject = {
    devices: devices,
    attributes: attributes
};

exports.index = function(req, res) {
    res.json(devicesAttributesObject);
};