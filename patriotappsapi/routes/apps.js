const express = require('express');
const router = express.Router();
const apps = require('../database/apps');

const apps_controller = require('../controllers/apps_controller');

let appMaxIndex = apps.length;


router.get('/', apps_controller.index);

router.get('/shortlist', apps_controller.shortlist_get);

router.get('/:id', validateID, apps_controller.app_get);

router.get('/:id/name', validateID, apps_controller.app_get_name);

router.get('/:id/description', validateID, apps_controller.app_get_description);

router.get('/:id/elements', validateID, apps_controller.app_get_elements);

router.get('/:id/connections', validateID, apps_controller.app_get_connections);

router.get('/:id/devices', validateID, apps_controller.app_get_devices);

router.get('/:id/model', validateID, apps_controller.app_get_model);

router.get('/:id/icon', validateID, apps_controller.app_get_icon);

router.get('/:id/flows', validateID, apps_controller.app_get_flows);

router.get('/:id/internetAccess', validateID, apps_controller.app_get_internetAccess)


function validateID(req,res,next) {
    let id = req.params.id;
    if ((id > 0) && (id <= appMaxIndex)) return next();
    res.status(403);
    res.send("Wrong App ID.")
}

module.exports = router;