const express = require('express');
const router = express.Router();

const elements_controller = require('../controllers/elements_controller');

router.get('/', elements_controller.index);

router.get('/:element', validateElement, elements_controller.element_get);

router.get('/:element/attributes', validateElement, elements_controller.element_get_attributes);

module.exports = router;