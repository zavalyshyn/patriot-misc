const express = require('express');
const router = express.Router();

const devices_controller = require('../controllers/devices_controller');

router.get('/', devices_controller.index);

// router.post('/', devices_controller.device_post);
//
// router.get('/:id', devices_controller.device_get);
//
// router.put('/:id', devices_controller.device_update);
//
// router.delete('/:id', devices_controller.device_delete);

// router.get('/:id/source', policies_controller.policy_get_source);
//
// router.get('/:id/attribute', policies_controller.policy_get_attribute);
//
// router.get('/:id/destination', policies_controller.policy_get_destination);
//
// router.get('/:id/time', policies_controller.policy_get_time);

module.exports = router;