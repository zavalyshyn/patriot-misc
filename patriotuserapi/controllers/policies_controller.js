const fs = require('fs');
const PolicyCollection = require('../database/PolicyCollection');

let userPolicyCollections = {};

let policiesLogStream = fs.createWriteStream("policies.json", {flags:'a'});

exports.index = function(req, res) {
    // console.log("Received new request for policies for the userID:", req.params.userId);
    let userId = req.params.userId;
    if (userPolicyCollections[userId]) res.json(userPolicyCollections[userId].getPoliciesObject());
    else res.json([])
};

exports.policy_post = function(req, res) {
    // console.log("Received new policy\n", req.body);
    let userId = req.params.userId;
    let policy = req.body;
    if (!userPolicyCollections[userId]) userPolicyCollections[userId] = new PolicyCollection(userId);
    let policyId = userPolicyCollections[userId].addPolicy(req.body);
    res.json({id: policyId});
    // try writing to a file for logging
    policy.id = policyId;
    policiesLogStream.write(JSON.stringify(policy) + ",\n");
};

exports.policy_update = function(req, res) {
    // console.log("Received request for policy update");
    let userId = req.params.userId;
    let policyId = req.params.id;
    if (!userPolicyCollections[userId]) res.json('Wrong userId');
    else {
        let result = userPolicyCollections[userId].updatePolicy(policyId,req.body);
        if (result!==-1) res.json('Policy updated');
        else res.json('Wrong policy ID');
    }
};

exports.policy_get = function(req, res) {
    // console.log("Received request for policy object");
    let userId = req.params.userId;
    let policyId = req.params.id;
    if (!userPolicyCollections[userId]) res.json('Wrong userId');
    else {
        let result = userPolicyCollections[userId].getPolicyObject(policyId);
        if (result!==-1) res.json(result);
        else res.json('Wrong policy ID');
    }
};

exports.policy_delete = function(req,res) {
    let userId = req.params.userId;
    let policyId = req.params.id;
    if (!userPolicyCollections[userId]) res.json('Wrong userId');
    else {
        let result = userPolicyCollections[userId].deletePolicy(policyId);
        if (result!==-1) res.json('Policy deleted');
        else res.json('Wrong policy ID');
    }
};

