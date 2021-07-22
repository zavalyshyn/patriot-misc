const express = require('express');
const router = express.Router();

const policies_controller = require('../controllers/policies_controller');

router.get('/:userId', policies_controller.index);

router.post('/:userId', policies_controller.policy_post);

router.get('/:userId/:id', policies_controller.policy_get);

router.put('/:userId/:id', policies_controller.policy_update);

router.delete('/:userId/:id', policies_controller.policy_delete);

// router.get('/:id/source', policies_controller.policy_get_source);
//
// router.get('/:id/attribute', policies_controller.policy_get_attribute);
//
// router.get('/:id/destination', policies_controller.policy_get_destination);
//
// router.get('/:id/time', policies_controller.policy_get_time);

module.exports = router;