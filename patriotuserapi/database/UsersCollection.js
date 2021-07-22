const bcrypt = require('bcrypt');
const saltRounds = 10;
const User = require('./User');

class UsersCollection {
    constructor(userId) {
        this.users = {};
    }

    addUser(user,callback) {
        let username = user.username;
        let password = user.password;
        let that = this;
        // if the user with such username exists return error
        if (this.users[username]) {
            // console.log('User exists');
            callback(new Error('User exists'));
        }
        else {
            // create a password hash
            bcrypt.hash(password, saltRounds,  function(err, hash) {
                if (err) throw "Failed to hash a password";
                // Store a hashed password in a users collection.
                let newUser = new User(username,hash);
                // console.log("New user created: ", newUser.username, newUser.password);
                let id = newUser.getId();
                that.users[username] = newUser;
                callback(null,id);
            });
        }
    }

    checkUser(userData,callback) {
        let username = userData.username;
        let password = userData.password;
        // console.log(`Received new check user request for ${username} and ${password}`);
        let that = this;
        if (!this.users[username]) callback(new Error('No such user'));
        else {
            bcrypt.compare(password, this.users[username].getPassword(), function (err,res) {
                if (err) throw "Failed to check a hashed password";
                res ? callback(null,that.users[username].getId()) : callback(new Error('Username or password are incorrect'));
            });
        }
    }
}

module.exports = UsersCollection;