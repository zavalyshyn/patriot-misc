const express = require('express');
const router = express.Router();

const users_controller = require('../controllers/users_controller');

router.post('/user', users_controller.user_check);

router.post('/', users_controller.user_post);

module.exports = router;