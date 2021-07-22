const Policy = require('./Policy');

class PolicyCollection {
    constructor(userId) {
        this.userId = userId;
        this.policies = [];
    }

    getUserId() {
        return this.userId;
    }

    addPolicy(policy) {
        let newPolicy = new Policy(policy);
        let id = newPolicy.getId();
        this.policies.push(newPolicy);
        return id
    }

    deletePolicy(id) {
        let indexToRemove = null;
        this.policies.forEach((pol,index) => {
            if (pol.getId()===id) indexToRemove = index;
        });
        if (indexToRemove!==null) {
            this.policies.splice(indexToRemove,1);
            return
        }
        return -1
    }

    updatePolicy(id,newPolicy) {
        let result = -1;
        this.policies.forEach((pol,index) => {
            if (pol.getId()===id) {
                pol.updatePolicy(newPolicy);
                result = 0;
            }
        });
        return result;
    }

    getPolicy(id) {
        this.policies.forEach((policy) => {
            if (policy.getId()===id) return policy;
        });
    }

    getPolicyObject(id) {
        let policyToReturn = null;
        this.policies.forEach((policy) => {
            if (policy.getId()===id) policyToReturn = policy.getPolicyObject();
        });
        return policyToReturn ? policyToReturn : -1;
    }

    getPoliciesObject() {
        let objectsArray = [];
        this.policies.forEach((policy) => {
            objectsArray.push(policy.getPolicyObject());
        });
        return objectsArray;
    }
}

module.exports = PolicyCollection;