const prolog = require('tau-prolog');
require( "tau-prolog/modules/lists" )( prolog );
const allelements = require('./elements-old');
const apps = require('../database/apps');


// let app = apps[0];  // HonestAssistant
// let app = apps[1];  // LightMyPath
// let app = apps[2];  // PhotoBurstWhenMotionContact app
let app = apps[3];  // SmartCameraWEncryption app
// let app = apps[4];  // SmartSecurity app

let appName = app.name;

console.log(`Modelling ${appName} app\n`);

let appelements = app.elements;
let appconnections = app.connections;

// Model content blocks
let elements = "";
let untrustedel = "";
let elinstances = "";
let elinfacts = "";
let eloutfacts = "";
let eloutputrules = "";


let appElsMap = new Map(); // map to keep track of elements' names, types and instances
let instnum = 1;

/*
Define app elements and their facts & rules
 */
appelements.forEach((value) => {
    if (allelements[value.type.toLowerCase()]) {
        if (value.type==="untrusted") untrustedel += `untrusted(e${instnum}).\n`;
        let el = allelements[value.type.toLowerCase()];
        // define app elements
        elements += `element(${value.type.toLowerCase()}).\n`;
        // define element instance
        elinstances += `ei(e${instnum},${value.type.toLowerCase()}).\n`;
        // add new element instance to the map
        appElsMap.set(value.name.toLowerCase(),{ei: `e${instnum}`, type: `${value.type.toLowerCase()}`});
        // define input ports' facts
        if (el.inportsfacts) elinfacts += el.inportsfacts + "\n";
        // define output ports' facts
        if (el.outportsfacts) eloutfacts += el.outportsfacts + "\n";
        // define output rules
        if (el.outputrules) eloutputrules += el.outputrules + "\n\n";
    }
    else {
        console.error(`Unknown element ${value.type}`);
    }
    instnum += 1;
});

/*
Define connections between the elements
NOTE: duplex connection is defined as a simplex connection in opposite direction.
 */
let elconnections = "";
appconnections.forEach((value) => {
    let source = value.from.toLowerCase();
    let sourceInst = appElsMap.get(source).ei;
    let target = value.to.toLowerCase();
    let targetInst = appElsMap.get(target).ei;
    let outport = value.outport.toLowerCase();
    let inport = value.inport.toLowerCase();
    if (value.mode==="simplex") {
        elconnections += `con(${sourceInst},${outport},${targetInst},${inport}).` + "\n";
    }
    else {  // for duplex connections
        // http duplex ports are tricky, we model those as simplex ports.
        if (value.outport.toLowerCase()===("httppostport" || "httpputport" || "httpdeleteport")) {
            elconnections += `con(${sourceInst},${outport},${targetInst},${inport}).` + "\n";
        } else {
            elconnections += `con(${targetInst},${inport},${sourceInst},${outport}).` + "\n";
        }
    }
});

// define general input rule
let generalInputRule =
    "input(ElInstance,InPort,Data,Trans,Time) :-\n" +
    "\tcon(PrevElInstance,OutPort,ElInstance,InPort),\n" +
    "\tei(PrevElInstance,PrevElement),\n" +
    "\tei(ElInstance,Element),\n" +
    "\telement(PrevElement),\n" +
    "\telement(Element),\n" +
    "\toutput(PrevElInstance,OutPort,Data,Trans,Time).\n";

// define rules for untrusted elements
let allInputsTransTimesRule =
    "allinputs(ElInstance, W) :-\n" +
    "\tfindall(Data, input(ElInstance, _, Data, _, _), L),\n" +
    "\tappend(L, W).\n" +
    "\n" +
    "alltransforms(ElInstance, W) :-\n" +
    "\tfindall(Trans, input(ElInstance, _, _, Trans, _), L),\n" +
    "\tappend(L, W).\n" +
    "\n" +
    "alltimes(ElInstance, Times) :-\n" +
    "\tfindall(Time, input(ElInstance,_,_,_,Time), Times).\n";

// define endpoints for the app (e.g. trusted URL endpoints, users, phone numbers, etc.)
// TODO: static for now, change it so it is dynamically created based on user input
let endpoints =
    "endpoint(epcam1, ipcamera, bedroomcam).\t\t\t% my bedroom camera\n" +
    "endpoint(epcam2, ipcamera, kitchencam).\t\t\t% my kitchen camera\n" +
    "endpoint(epnet1, httprequest, httptarget1).\t\t% one more example of an endpoint for URL\n" +
    "endpoint(eppush1, pushnotifier, myphone).\t\t% my phone\n" +
    "endpoint(eppush2, pushnotifier, mysisterphone).\t% my sister's phone\n";

// define restrictors for the given endpoints
let restrictors =
    "restrictor(e5, [kitchencam]).\n" +
    // "%restrictor(e3, [httptarget1]).\n" +
    "restrictor(e4,[myphone]).\n";

// define sources and sinks TODO: static for now, make dynamicly created on user input
let sources =
    "source(ipcamera, getframeport, cameraframe).\n";

let sinks =
    "sink(pushnotifier, sendpushwithimageport).\n";

// define model checker rules
let checkerRules =
    "%% ---------------------------------------------------------\n" +
    "%% Model checker rules\n" +
    "%% ---------------------------------------------------------\n\n" +
    "selector(E, P) :- restrictor(E, PSET), member(P, PSET).\n" +
    "\n" +
    "flows(D, E1, P1, S1, E2, P2, S2, T) :-\n" +
    "    input(E2, P2, DSET2, T, _), selector(E2, S2),\n" +
    "    output(E1, P1, DSET1, _, _), selector(E1, S1),\n" +
    "    member(D, DSET1), member(D, DSET2).\n" +
    "\n" +
    "queryflows(EP1, EP2, D) :-\n" +
    "    endpoint(EP1, X1, S1), ei(XI1, X1), element(X1), source(X1, P1, D),\n" +
    "    endpoint(EP2, X2, S2), ei(XI2, X2), element(X2), sink(X2, P2),\n" +
    "    flows(D, XI1, P1, S1, XI2, P2, S2, _).\n" +
    "\n" +
    "querytransformed(EP1, EP2, D, T) :-\n" +
    "    endpoint(EP1, X1, S1), ei(XI1, X1), element(X1), source(X1, P1, D),\n" +
    "    endpoint(EP2, X2, S2), ei(XI2, X2), element(X2), sink(X2, P2),\n" +
    "    flows(D, XI1, P1, S1, XI2, P2, S2, TSET),\n" +
    "    member(t(TDSET, T, _), TSET), member(D, TDSET).\n";


let appmodel =
    ":- use_module(library(lists)).\n" +    // needed to have 'member/2' support
    elements + "\n" + elinstances + "\n" + untrustedel + "\n" +
    elconnections + "\n" + elinfacts + "\n" + eloutfacts + "\n" +
    eloutputrules + generalInputRule + "\n" + allInputsTransTimesRule +
    "\n" + endpoints + "\n" + restrictors + "\n" + sources + "\n" +
    sinks + "\n" + checkerRules;


console.log(appmodel);

// Limit the number of resolution steps that the interpreter can make
const session = prolog.create( 1000 );

session.consult(appmodel);

// Query the goal
// session.query("outport(X,Y).");
session.query("queryflows(epcam2,eppush1,cameraframe).");

// Show answers
session.answers( x => console.log( prolog.format_answer(x) ) );


// TODO: we need to define facts describing supported datatypes for all ports
// this way we could dramatically reduce the number of branches to check for
// illegitimate flows. We would only check those ports that can potentially
// carry the data type of interest.
// If we then just follow the propagation of a certain data type from a
// given source to a given target, we can find out which elements a given
// flow depends on. From those we can deduce the restrictions (if any) that
// affect the propagation of the flow (e.g. tap cond, timer, time, etc.).

// %%-----------------------------------------------------------------
// % Some policy examples
// %%-----------------------------------------------------------------
//
// % Does an app has Internet access
// internetaccess :-
//     element(httprequest).
//
// % Does an app has access to my kitchen camera?
// deviceaccess(DeviceID) :-
//     endpoint(_,DeviceType,DeviceID),
//     ei(ElInst,DeviceType),
//     restrictor(ElInst,DSET),
//     member(DeviceID,DSET).
