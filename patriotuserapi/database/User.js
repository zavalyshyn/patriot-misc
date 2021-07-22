const crypto = require("crypto");

class User {
    constructor(username,password) {
        this.id = crypto.randomBytes(16).toString("hex");
        this.username = username;
        this.password = password;
    };

    getId() {
        return this.id;
    }

    getUsername() {
        return this.username
    }

    getPassword() {
        return this.password
    }

    getUserObject() {
        return {
            id: this.id,
            username: this.username,
            password: this.password,
        }
    }
}

module.exports = User;