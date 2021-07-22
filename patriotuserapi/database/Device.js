const crypto = require("crypto");

class Device {
    constructor(device) {
        this.id = crypto.randomBytes(16).toString("hex");
        this.name = device.name;
        this.type = device.type;
        this.location = device.location;
        this.attrTypeIn = device.attrTypeIn;
    };

    getId() {
        return this.id;
    }

    getName() {
        return this.name;
    }

    getType() {
        return this.type
    }

    getLocation() {
        return this.location;
    }

    getAttrTypeIn() {
        return this.attrTypeIn;
    }

    updateDevice(newDevice) {
        this.name = newDevice.name;
        this.type = newDevice.type;
        this.location = newDevice.location;
        this.attrTypeIn = newDevice.attrTypeIn;
    }

    getDeviceObject() {
        return {
            id: this.id,
            name: this.name,
            type: this.type,
            location: this.location,
            attrTypeIn: this.attrTypeIn
        }
    }
}

module.exports = Device;