const prolog = require('tau-prolog');
require( "tau-prolog/modules/lists" )( prolog );
const allelements = require('./elements-old');
const apps = require('../database/apps');

// This model models the propagation of the attributes depending
// on thei type (e.g. boolean) and tracks their paths.
// doesn't handle the conditions for the propagation.

// let app = apps[0];  // HonestAssistant
// let app = apps[1];  // LightMyPath
// let app = apps[2];  // PhotoBurstWhenMotionContact app
// let app = apps[3];  // SmartCameraWEncryption app
let app = apps[4];  // SmartSecurity app

let appName = app.name;

console.log(`Modelling ${appName} app\n`);

let appelements = app.elements;
let appconnections = app.connections;

// Model content blocks
let elements = "";
let untrustedel = "";
let elinstances = "";
// let elinfacts = "";
// let eloutfacts = "";
// let eloutputrules = "";


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
    let mode = value.mode;
    elconnections += `connection(${sourceInst},${outport},${targetInst},${inport},${mode}).` + "\n";
});

let helperrules =
    "% supported data types for each elements' port\n" +
    "% portdatatype([INDATATYPES],[OUTDATATYPES],PORT,ELEMENT)\n" +
    "portdatatype([boolean],[],notinport,not).\n" +
    "portdatatype([],[boolean],notoutport,not).\n" +
    "portdatatype([],[boolean],controlsignalport,timecontroller).\n" +
    "portdatatype(any,any,httppostport,httprequest).\n" +
    "portdatatype(any,any,httpgetport,httprequest).\n" +
    "portdatatype(any,any,httpputport,httprequest).\n" +
    "portdatatype(any,any,httpdeleteport,httprequest).\n" +
    "portdatatype(any,[],encryptdatainport,aesencryption).\n" +
    "portdatatype([],[encrypteddata],encryptdataoutport,aesencryption).\n" +
    "portdatatype([boolean],[],tapconditionport,tap).\n" +
    "portdatatype(any,any,tapinport,tap). % can be duplex\n" +
    "portdatatype(any,any,tapoutport,tap).% can be duplex\n" +
    "portdatatype([],[boolean],motiondetectedport,motionsensor).\n" +
    "portdatatype([],[boolean],contactclosedport,contactsensor).\n" +
    "portdatatype([],[boolean],contactopenport,contactsensor).\n" +
    "portdatatype([boolean],[],orin1port,or).\n" +
    "portdatatype([boolean],[],orin2port,or).\n" +
    "portdatatype([],[boolean],oroutport,or).\n" +
    "portdatatype([text],[],sendpushmessageport,pushnotifier).\n" +
    "portdatatype([text,image],[],sendpushwithimageport,pushnotifier).\n" +
    "portdatatype([],[boolean],dismissreceivedport,pushnotifier).\n" +
    "portdatatype([],[image],getframeport,ipcamera).\n" +
    "portdatatype([],[image],cameraframeport,ipcamera).\n" +
    "portdatatype([],[],triggeralarmport,alarm).\n" +
    "portdatatype([image],[text],recognizefaceport,facerecognition).\n" +
    "portdatatype([boolean],[],starttimerport,timer).\n" +
    "portdatatype([boolean],[],starttimeronceport,timer).\n" +
    "portdatatype([boolean],[],stoptimerport,timer).\n" +
    "portdatatype([],[boolean],timerexpiredport,timer).\n" +
    "portdatatype([],[],turnonlightport,smartlight).\n" +
    "portdatatype([],[],turnofflightport,smartlight).\n" +
    "portdatatype([],[boolean],getlightstateport,smartlight).\n" +
    "\n" +
    "% type of each data attribute\n" +
    "% we need to specify this only for those elements and ports\n" +
    "% that generate an attribute\n" +
    "attrtype(motion_detected,boolean).\n" +
    "attrtype(motion_stopped,boolean).\n" +
    "attrtype(contact_open,boolean).\n" +
    "attrtype(contact_closed,boolean).\n" +
    "attrtype(cameraframe,image).\n" +
    "attrtype(timer_expired,boolean).\n" +
    "attrtype(light_state,boolean).\n" +
    "%attrtype(encrypted_data,encrypteddata).\n" +
    "attrtype(H,encrypteddata):-\n" +
    "\tportattr([H|_],encryptdataoutport,aesencryption).\n" +
    "attrtype(tap_state,boolean).\n" +
    "attrtype(control_signal,boolean).\n" +
    "attrtype(dismiss_received,boolean).\n" +
    "\n" +
    "% attributes for a given element's port\n" +
    "portattr([motion_detected],motiondetectedport,motionsensor).\n" +
    "portattr([motion_stopped],motionstoppedport,motionsensor).\n" +
    "portattr([contact_open],contactopenport,contactsensor).\n" +
    "portattr([contact_closed],contactclosedport,contactsensor).\n" +
    "portattr([cameraframe],getframeport,ipcamera).\n" +
    "portattr([cameraframe],cameraframeport,ipcamera).\n" +
    "portattr([timer_expired],timerexpiredport,timer).\n" +
    "portattr([light_state],getlightstateport,smartlight).\n" +
    "%portattr([encrypted_data],encryptdataoutport,aesencryption).\n" +
    "% aesencryption is modifying original data attribute\n" +
    "% to be 100% correct we would need to backtrack for\n" +
    "% the latest attribute type, but for now let's take\n" +
    "% it from the encryptdatainport. If there wasn't any\n" +
    "% attr type there, we would need to look further back.\n" +
    "% Consider the case camera -> tap -> aesencryption\n" +
    "portattr([OutAttr],encryptdataoutport,aesencryption) :-\n" +
    "\tei(Inst,aesencryption),\n" +
    "\t%attrpath(_,_,Inst,[H|_]), % this causes endless loop\n" +
    "\tconnection(Source,PortOut,Inst,encryptdatainport,simplex),\n" +
    "\tei(Source,El),\n" +
    "\tportattr([H|_],PortOut,El),\n" +
    "\tatom_concat('encrypted_',H,OutAttr).\n" +
    "portattr([tap_state],gettapstateport,tap).\n" +
    "portattr([control_signal],controlsignalport,timecontroller).\n" +
    "portattr([dismiss_received],dismissreceivedport,pushnotifier).\n" +
    "\n" +
    "% elements' ports that stop the attribute propagation\n" +
    "% at a given path\n" +
    "stopport(stoptimerport,timer).\n" +
    "stopport(tapconditionport,tap).\n" +
    "stopport(sendpushwithimageport,pushnotifier).\n" +
    "stopport(controlsignalport,timecontroller).\n" +
    "stopport(sendpushmessageport,pushnotifier).\n" +
    "stopport(triggeralarmport,alarm).\n" +
    "stopport(turnonlightport,smartlight).\n" +
    "stopport(turnofflightport,smartlight).\n" +
    "\n" +
    "%%-----------------------------------------------------------\n" +
    "% Function to check if a given connection allows for a certain\n" +
    "% DataType to go through from a given Source to Destination\n" +
    "%%-----------------------------------------------------------\n" +
    "\n" +
    "% case for trusted -> portout -> portin -> trusted\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tei(Element,Inst1),\n" +
    "\tei(Next,Inst2),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst1),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst2),\n" +
    "\tmember(DataType,DataOut),\n" +
    "\tmember(DataType,DataIn).\n" +
    "\n" +
    "% case for untrusted -> port -> trusted (simplex)\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tuntrusted(Element),\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tei(Next,Inst),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst),\n" +
    "\tmember(DataType,DataIn).\n" +
    "\n" +
    "% case for trusted -> port -> untrusted (simplex)\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tuntrusted(Next),\n" +
    "\tei(Element,Inst),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst),\n" +
    "\tmember(DataType,DataOut).\n" +
    "\n" +
    "% case for untrusted -> port -> trusted (duplex)\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tuntrusted(Element),\n" +
    "\tconnection(Element,PortOut,Next,PortIn,duplex),\n" +
    "\tei(Next,Inst),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst),\n" +
    "\tmember(DataType,DataIn).\n" +
    "\n" +
    "% case for trusted -> port -> untrusted (duplex)\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Next,PortOut,Element,PortIn,duplex),\n" +
    "\tuntrusted(Next),\n" +
    "\tei(Element,Inst),\n" +
    "\tportdatatype(_,DataOut,PortIn,Inst),\n" +
    "\tmember(DataType,DataOut).\n" +
    "\n" +
    "% case for trusted -> port -> trusted (duplex)\n" +
    "% this is only possible with tap element's tapout port\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,duplex),\n" +
    "\tei(Element,Inst),\n" +
    "\tei(Next,Inst2),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst2),\n" +
    "\tmember(DataType,DataOut),\n" +
    "\tmember(DataType,DataIn).\n" +
    "\n" +
    "%\n" +
    "% cases for ports with wildcard attributes (any)\n" +
    "%\n" +
    "\n" +
    "% case for trusted -> portout* -> portin -> trusted\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tei(Element,Inst1),\n" +
    "\tei(Next,Inst2),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst1),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst2),\n" +
    "\tDataOut == any,\n" +
    "\tmember(DataType,DataIn).\n" +
    "\n" +
    "% case for trusted -> portout* -> *portin -> trusted\n" +
    "compatiblecon(_,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tei(Element,Inst1),\n" +
    "\tei(Next,Inst2),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst1),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst2),\n" +
    "\tDataOut == any,\n" +
    "\tDataIn == any.\n" +
    "\n" +
    "% case for trusted -> portout -> *portin -> trusted\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tei(Element,Inst1),\n" +
    "\tei(Next,Inst2),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst1),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst2),\n" +
    "\tmember(DataType,DataOut),\n" +
    "\tDataIn == any.\n" +
    "\n" +
    "% case for untrusted -> *port -> trusted (simplex)\n" +
    "compatiblecon(_,Element,PortOut,Next,PortIn) :-\n" +
    "\tuntrusted(Element),\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tei(Next,Inst),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst),\n" +
    "\tDataIn == any.\n" +
    "\n" +
    "% case for trusted -> port* -> untrusted (simplex)\n" +
    "compatiblecon(_,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,simplex),\n" +
    "\tuntrusted(Next),\n" +
    "\tei(Element,Inst),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst),\n" +
    "\tDataOut == any.\n" +
    "\n" +
    "% case for untrusted -> *port -> trusted (duplex)\n" +
    "compatiblecon(_,Element,PortOut,Next,PortIn) :-\n" +
    "\tuntrusted(Element),\n" +
    "\tconnection(Element,PortOut,Next,PortIn,duplex),\n" +
    "\tei(Next,Inst),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst),\n" +
    "\tDataIn == any.\n" +
    "\n" +
    "% case for trusted -> port* -> untrusted (duplex)\n" +
    "compatiblecon(_,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Next,PortOut,Element,PortIn,duplex),\n" +
    "\tei(Element,Inst),\n" +
    "\tportdatatype(_,DataOut,PortIn,Inst),\n" +
    "\tDataOut == any.\n" +
    "\n" +
    "% case for trusted -> *port* -> trusted (duplex)\n" +
    "% this is only possible with tap element's tapout port\n" +
    "compatiblecon(DataType,Element,PortOut,Next,PortIn) :-\n" +
    "\tconnection(Element,PortOut,Next,PortIn,duplex),\n" +
    "\tei(Element,Inst),\n" +
    "\tei(Next,Inst2),\n" +
    "\tportdatatype(_,DataOut,PortOut,Inst),\n" +
    "\tportdatatype(DataIn,_,PortIn,Inst2),\n" +
    "\t((DataOut == any,DataIn == any) ; (member(DataType,DataOut), DataIn == any) ; (member(DataType,DataIn), DataOut == any)).\n" +
    "\n" +
    "%%-----------------------------------------------------------\n" +
    "% This function checks whether there exists a path for a\n" +
    "% certain Attribute and a corresponding AttrType from a\n" +
    "% given Source to a given Destination\n" +
    "%%-----------------------------------------------------------\n" +
    "\n" +
    "% case for direct connection between source and destination\n" +
    "datapath(Source,PortIn,AttrType,OrigAttr,Destination,Visited,PathAttr,NewPathAttr) :-\n" +
    "\tcompatiblecon(AttrType,Source,PortOut,Destination,_),\n" +
    "\tnot(member(Destination,Visited)),\n" +
    "\tmodifyattr(Source,PortIn,PortOut,PathAttr,NewPathAttr).\n" +
    "\n" +
    "\n" +
    "% case for indirect connection between source and destination\n" +
    "datapath(Source,PortIn,AttrType,OrigAttr,Destination,Visited, PathAttr,InterPathAttr) :-\n" +
    "\tcompatiblecon(AttrType,Source,PortOut,Intermediate,_),\n" +
    "\tmodifyattr(Source,PortIn,PortOut,PathAttr,NewPathAttr),\n" +
    "\tnot(member(Intermediate,Visited)),\n" +
    "\tadd(Intermediate,Visited,NewVisited),\n" +
    "\tdatapath(Intermediate,PortIn,AttrType,OrigAttr,Destination,NewVisited,NewPathAttr,InterPathAttr).\n" +
    "\n" +
    "add(X,[H|T],[X,H|T]).\n" +
    "\n" +
    "% updates an Attr list on a path accordingly\n" +
    "modifyattr(ElInst,PortIn,PortOut,PathAttr,NewPathAttr) :-\n" +
    "\t% if PortIn is a stop port, remove all current PathAttr\n" +
    "\t(ei(ElInst,Element),\n" +
    "\tstopport(PortIn,Element)) -> NewPathAttr = [] ;\n" +
    "\t% otherwise process attributes accordingly ...\n" +
    "\t% if outport generates a new attribute\n" +
    "\t((ei(ElInst,Element),\n" +
    "\tportattr(NewAttribute,PortOut,Element)) ->\n" +
    "\t% add new attr to the path\n" +
    "\tappend(NewAttribute,PathAttr,NewPathAttr) ;\n" +
    "\t% otherwise existing attributes remain the same\n" +
    "\tNewPathAttr = PathAttr).\n" +
    "\n" +
    "% query examples:\n" +
    "% once(datapath(e5,image,cameraframe,e4)). \t% true\n" +
    "% once(datapath(e1,boolean,motion_detected,e6)).% true\n" +
    "% once(datapath(e1,image,camera_frame,e6)).\t% false\n" +
    "\n" +
    "\n" +
    "%%-----------------------------------------------------------\n" +
    "% Same as datapath but for a concrete attribute path\n" +
    "%\n" +
    "% Note: the source must be a trusted element\n" +
    "%%-----------------------------------------------------------\n" +
    "\n" +
    "% case for direct connection between source and destination\n" +
    "%attrpath(Source,Attribute,Destination) :-\n" +
    "attrpath(Source,Attribute,Destination,[Attribute]) :-\n" +
    "\tattrtype(Attribute,AttrType),\n" +
    "\tei(Source,Element),\n" +
    "\tcompatiblecon(AttrType,Source,PortOut,Destination,_),\n" +
    "\tportattr(AttrList,PortOut,Element),\n" +
    "\tmember(Attribute,AttrList).\n" +
    "\n" +
    "% case for indirect connection between source and destination\n" +
    "%attrpath(Source,Attribute,Destination) :-\n" +
    "attrpath(Source,Attribute,Destination,FinalPathAttr) :-\n" +
    "\tattrtype(Attribute,AttrType),\n" +
    "\tei(Source,Element),\n" +
    "\tcompatiblecon(AttrType,Source,PortOut,Intermediate,PortIn),\n" +
    "\tportattr(AttrList,PortOut,Element),\n" +
    "\tmember(Attribute,AttrList),\n" +
    "\tdatapath(Intermediate,PortIn,AttrType,Attribute,Destination,[Source,Intermediate],[Attribute],FinalPathAttr),\n" +
    "\tmember(Attribute,FinalPathAttr).\n" +
    "\n" +
    "% query examples:\n" +
    "% attrpath(e4,cameraframe,e6).\n" +
    "% attrpath(e1,motion_detected,e6).\n" +
    "\n" +
    "%%--------------------------------------------------------\n" +
    "% Checks if a certain attribute arrives to a given element\n" +
    "%%--------------------------------------------------------\n" +
    "\n" +
    "flows(Attribute,Element) :-\n" +
    "\tei(Inst,Element),\n" +
    "\tattrpath(_,Attribute,Inst,_).\n" +
    "\n" +
    "%%--------------------------------------------------------\n" +
    "% Prints all the attributes that  arrive to a given element\n" +
    "%%--------------------------------------------------------\n" +
    "\n" +
    "allthatflowsto(Element,AllAttributes) :-\n" +
    "\tei(Inst,Element),\n" +
    "\tfindall(Attribute,attrpath(_,Attribute,Inst,_),AllAttributes).\n"


let appmodel =
    elements + "\n" + elinstances + "\n" + untrustedel + "\n" +
    elconnections + "\n" + helperrules;




console.log(appmodel);