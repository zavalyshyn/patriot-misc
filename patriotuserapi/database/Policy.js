const crypto = require("crypto");

class Policy {
    constructor(policy) {
        this.id = crypto.randomBytes(16).toString("hex");
        this.source = policy.source;
        this.sourceType = policy.sourceType;
        this.attribute = policy.attribute;
        this.destination = policy.destination;
        this.destinationType = policy.destinationType;
        this.timeFrom = policy.timeFrom;
        this.timeTill = policy.timeTill;
        this.exception = policy.exception;
    };

    getId() {
        return this.id;
    }

    getSource() {
        return this.source
    }

    getSourceType() {
        return this.sourceType
    }

    getAttribute() {
        return this.attribute;
    }

    getDestination() {
        return this.destination;
    }

    getDestinationType() {
        return this.destinationType;
    }

    getTimeFrom() {
        return this.timeFrom;
    }

    getTimeTill() {
        return this.timeTill;
    }

    getException() {
        return this.exception;
    }

    updatePolicy(newPolicy) {
        this.source = newPolicy.source;
        this.sourceType = newPolicy.sourceType;
        this.attribute = newPolicy.attribute;
        this.destination = newPolicy.destination;
        this.destinationType = newPolicy.destinationType;
        this.timeFrom = newPolicy.timeFrom;
        this.timeTill = newPolicy.timeTill;
        this.exception = newPolicy.exception;
    }

    getPolicyObject() {
        return {
            id: this.id,
            source: this.source,
            sourceType: this.sourceType,
            attribute: this.attribute,
            destination: this.destination,
            destinationType: this.destinationType,
            timeFrom: this.timeFrom,
            timeTill: this.timeTill,
            exception: this.exception
        }
    }
}

module.exports = Policy;