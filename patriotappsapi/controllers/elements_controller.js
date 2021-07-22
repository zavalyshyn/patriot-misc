const elements = require('../database/elements');

const modules = {
    contactsensor: {
        element: "contactsensor",
        inports: null,
        outports: "pout",
        datatypes: [
            "contact:true",
            "contact:false"
        ],
        prologrules: "output(X,pout,D):-ei(X,fromcontact),D=[t].\n" +
            "output(X,pout,D):-ei(X,fromcontact),D=[f].\n",
        code: "some JS code here. URL to Zip?"
    }
}


exports.index = function(req, res) {
    res.json(modules);
};
