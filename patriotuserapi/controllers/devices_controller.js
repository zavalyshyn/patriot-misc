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
        name: 'Smart light',
        type: 'smartlight',
        attrTypeIn: ['boolean'],
    },
    {
        name: 'Motion sensor',
        type: 'motionsensor',
        attrTypeIn: [],
    },
    {
        name: 'Contact sensor',
        type: 'contactsensor',
        attrTypeIn: [],
    },
    {
        name: 'Internet',
        type: 'httprequest',
        attrTypeIn: ['any'],
    },
    {
        name: "My phone",
        type: 'userphone',
        attrTypeIn: ['image','audio','video','text','boolean'],
    },
    {
        name: "Nanny's phone",
        type: 'userphone',
        attrTypeIn: ['image','audio','video','text','boolean'],
    },
    // {
    //     name: "Guest's phone",
    //     type: 'userphone',
    //     attrTypeIn: ['image','audio','video','text','boolean'],
    // },
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