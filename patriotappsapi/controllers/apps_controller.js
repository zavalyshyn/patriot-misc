const apps = require('../database/apps');

exports.index = function(req, res) {
    res.json(apps);
};

exports.shortlist_get = function(req, res) {
    let shortlist = [];
    apps.forEach((value,index) => {
        let alldevices = ['IPCamera','SmartLight','SmartPlug','Alarm','MotionSensor','ContactSensor'];
        let appDevices = [];
        let appNotifications = [];
        let appInternetAccess = null;
        let appConfigs = [];
        // value.connections.forEach((con) => {
        //     if (alldevices.includes(con.from) && !appdevices.includes(con.from)) appdevices.push(con.from);
        //     if (alldevices.includes(con.to) && !appdevices.includes(con.to)) appdevices.push(con.to);
        //     if (((con.to || con.from)==='PushNotifier') && (!appnotifications.includes('PushNotifier')))
        //         appnotifications.push('PushNotifier');
        // });
        value.elements.forEach((el) => {
            if (alldevices.includes(el.type) && !appDevices.includes(el.type)) appDevices.push(el.type);
            if ((el.type==='PushNotifier') && (!appNotifications.includes('PushNotifier')))
                appNotifications.push('PushNotifier');
            if (el.type==='HttpRequest') appInternetAccess = {access:true, url: el.config.hostname+el.config.path};
            if (el.config) {
                if (el.type==='Timer') {
                    let config = {
                        elementType: el.type,
                        defaultValue: el.config.time
                    };
                    appConfigs.push(config);
                }
                if (el.type==='HttpRequest') {
                    let config = {
                        elementType: el.type,
                        defaultValue: {
                            hostname: el.config.hostname,
                            path: el.config.path
                        }
                    };
                    appConfigs.push(config);
                }
                if (el.type==='TimeController') {
                    let config = {
                        elementType: el.type,
                        defaultValue: {
                            starttime: el.config.starttime,
                            endtime: el.config.endtime
                        }
                    };
                    appConfigs.push(config)
                }

            }
        });
        let app = {
            id: value.id,
            name: value.name,
            description: value.description,
            icon: value.icon,
            // devices: value.devices,
            devices: appDevices,
            // notifications: value.notifications,
            notifications: appNotifications,
            // internetAccess: value.internetAccess
            internetAccess: appInternetAccess ? appInternetAccess : {access: false, url: ""},
            configs: appConfigs
        };
        shortlist.push(app);
    });
    res.json(shortlist);
};

exports.app_get = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index]);
        }
    })
};

exports.app_get_name = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].name);
        }
    })
};

exports.app_get_description = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].description);
        }
    })
};

exports.app_get_elements = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].elements);
        }
    })
};

exports.app_get_connections = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].connections);
        }
    })
};

exports.app_get_devices = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            let alldevices = ['IPCamera','SmartLight','SmartPlug','Alarm','MotionSensor','ContactSensor'];
            let appdevices = [];
                apps[index].connections.forEach((value) => {
                    if (alldevices.includes(value.from) && !appdevices.includes(value.from)) appdevices.push(value.from);
                    if (alldevices.includes(value.to) && !appdevices.includes(value.to)) appdevices.push(value.to);
            });
            res.json(appdevices);
        }
    })
};

exports.app_get_model = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].model);
        }
    })
};

exports.app_get_icon = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].icon);
        }
    })
};

exports.app_get_flows = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].flows);
        }
    })
};

exports.app_get_internetAccess = function(req, res) {
    apps.forEach((value,index) => {
        if (value.id===Number(req.params.id)) {
            res.json(apps[index].internetAccess);
        }
    })
};


