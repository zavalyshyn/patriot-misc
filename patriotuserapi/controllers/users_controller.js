const UsersCollection = require('../database/UsersCollection');

let usersCollection = new UsersCollection();

exports.user_check = function(req, res) {
    usersCollection.checkUser(req.body, (err, result) => {
        if (err) res.json(-1);
        else res.json({id: result});
    });
};

exports.user_post = function(req, res) {
    // console.log("Received new user registration request\n", req.body);
    usersCollection.addUser(req.body, (err, result) => {
        if (err) res.json(-1);
        else {
            res.json({id: result});
        }
    });

};