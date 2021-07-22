const fs = require('fs');
const prolog = require('tau-prolog');
require( "tau-prolog/modules/lists" )( prolog );
// const allelements = require('./elements/elements');
const allelements = require('./database/elements');
const apps = require('./database/apps');


// let app = apps[0];  // HonestAssistant
// let app = apps[1];  // LightMyPath
// let app = apps[2];  // PhotoBurstWhenMotionContact app
// let app = apps[3];  // SmartCameraWEncryption app
// let app = apps[4];  // SmartSecurity app

let app = apps[5];  // SmartCameraWithoutEncryption app

let appName = app.name;

// console.log(`Modelling ${appName} app\n`);

let appelements = app.elements;
let appconnections = app.connections;

// Model content blocks
let elements = "";
let elementconfigs = "";
let untrustedel = "";
let elinstances = "";
let elattributes = "";
let elattributetypes = "";
let elportattrtypes = "";
let elstopports = "";
let eloutputrules = "";
let elhelpers = "";


let appElsMap = new Map(); // map to keep track of elements' names, types and instances
let instnum = 1;

/*
Define app elements and their facts & rules
 */
appelements.forEach((appelement) => {
    if (allelements[appelement.type.toLowerCase()]) {
        if (appelement.type==="untrusted") untrustedel += `untrusted(e${instnum}).\n`;
        let el = allelements[appelement.type.toLowerCase()];
        // define app elements
        elements += `element(${appelement.type.toLowerCase()}).\n`;
        // define element instance
        elinstances += `ei(e${instnum},${appelement.type.toLowerCase()}).\n`;
        // add new element instance to the map
        appElsMap.set(appelement.name.toLowerCase(),{ei: `e${instnum}`, type: `${appelement.type.toLowerCase()}`});
        // define element attributes (if any)
        if (el.attributes) {
            el.attributes.forEach((value) => {
                elattributes += `attribute(${value},${appelement.type.toLowerCase()}).\n`;
            })
        }
        // define element attribute types (if any)
        if (el.attrtype) elattributetypes += el.attrtype + "\n";
        // define element port attribute types
        if (el.portdatatype) elportattrtypes += el.portdatatype + "\n";
        // define element's stop ports (if any)
        if (el.stopport) elstopports += el.stopport + "\n";
        // define output rules
        if (el.output) eloutputrules += el.output + "\n\n";
        // define output helper rules
        if (el.helpers) elhelpers += el.helpers + "\n";
        // process element configs (if any)
        if (appelement.config) {
            if (appelement.type.toLowerCase() === 'timer') {
                elementconfigs += `config(${appelement.config.time},time,timer).\n`;
            }
            if (appelement.type.toLowerCase() === 'httprequest') {
                elementconfigs += `config('${appelement.config.hostname}${appelement.config.path}',hostname,httprequest).\n`
            }
            if (appelement.type.toLowerCase() === 'timecontroller') {
                let starthour = appelement.config.starttime.split(':')[0];
                let startmin = appelement.config.starttime.split(':')[1];
                let endhour = appelement.config.endtime.split(':')[0];
                let endmin = appelement.config.endtime.split(':')[1];
                elementconfigs += `config(${starthour},starthour,timecontroller).\n`;
                elementconfigs += `config(${startmin},startminute,timecontroller).\n`;
                elementconfigs += `config(${endhour},endhour,timecontroller).\n`;
                elementconfigs += `config(${endmin},endminute,timecontroller).\n`;
            }
        }
    }
    else {
        console.error(`Unknown element ${appelement.type}`);
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

/*
THIS IS VERY IMPORTANT. DO NOT DELETE

This is how you query the model to find out if certain flows exist at a given time of the day:

Query:
?- retractall(gettime(_)),createstamp(HOUR,MINUTE,Stamp),asserta(gettime(Stamp)),allattrthatflowto(ELEMENT_NAME,X),retract(gettime(Stamp)),asserta((gettime(T):-get_time(T))).

Example:

?- retractall(gettime(_)),createstamp(12,20,Stamp),asserta(gettime(Stamp)),allattrthatflowto(httprequest,X),retract(gettime(Stamp)),asserta((gettime(T):-get_time(T))).
Stamp = 1575458400.0,
X = [data_from_internet, encrypted_data, tap_state].

Note: any standard query can be performed in this way. Just wrap it in this gettime/1 assertion.
This is only needed for those apps that have TimeController element in their manifest file.
 */

let generalinputrules =
    "% case for simplex connections\n" +
    "input(ElInstance,InPort,NoDupAttr,Value,Trans,Time) :-\n" +
    "\tconnection(PrevElInstance,OutPort,ElInstance,InPort,_),\n" +
    "\tei(PrevElInstance,PrevElement),\n" +
    "\tei(ElInstance,Element),\n" +
    "\telement(PrevElement),\n" +
    "\telement(Element),\n" +
    "\toutput(PrevElInstance,OutPort,Attr,Value,Trans,Time),\n" +
    "\tfiltersimplex(Attr,ElInstance,InPort,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr). % remove duplicates\n" +
    "\n" +
    "% case for duplex connections\n" +
    "% (untrusted <-duplex-> trusted)\n" +
    "% element is untrusted\n" +
    "input(ElInstance,OutPort,NoDupAttr,Value,Trans,Time) :-\n" +
    "\tconnection(ElInstance,OutPort,NextElInstance,InPort,Mode),\n" +
    "\tMode = duplex,\n" +
    "\tei(NextElInstance,NextElement),\n" +
    "\tei(ElInstance,Element),\n" +
    "\telement(NextElement),\n" +
    "\telement(Element),\n" +
    "\toutput(NextElInstance,InPort,Attr,Value,Trans,Time),\n" +
    "\tfilterduplex(Attr,ElInstance,InPort,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr). % remove duplicates\n" +
    "\n" +
    "% case for duplex connections (trusted elements)\n" +
    "% trusted <-duplex-> trusted\n" +
    "% e.g. tap <-httppostport-> httprequest\n" +
    "\n" +
    "input(ElInst,OutPort,NoDupAttr,Value,Trans,Time) :-\n" +
    "\tconnection(ElInst,OutPort,NextElInst,InPort,Mode),\n" +
    "\tMode = duplex,\n" +
    "\tei(NextElInst,NextElement),\n" +
    "\tei(ElInst,Element),\n" +
    "\telement(NextElement),\n" +
    "\telement(Element),\n" +
    "\toutput(NextElInst,InPort,Attr,Value,Trans,Time),\n" +
    "\tfilterduplex(Attr,ElInst,OutPort,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr). % remove duplicates\n" +
    "\n" +
    "%----------------------------------------------------------\n" +
    "%% Filter input attributes based on the input port's\n" +
    "%% supported attributes list\n" +
    "%----------------------------------------------------------\n" +
    "\n" +
    "filterattributessimplex([],_,_,[]).\n" +
    "\n" +
    "filterattributessimplex([H|T],ElInst,Port,[H|Filtered]) :-\n" +
    "\tattrtype(H,Type),\n" +
    "\tei(ElInst,Element),\n" +
    "\tportdatatype(SupportedInTypesList,_,Port,Element),\n" +
    "\tmember(Type,SupportedInTypesList),\n" +
    "\tfilterattributessimplex(T,ElInst,Port,Filtered).\n" +
    "\n" +
    "filterattributessimplex([H|T],ElInst,Port,[H|Filtered]) :-\n" +
    "\tattrtype(H,_),\n" +
    "\tei(ElInst,Element),\n" +
    "\tportdatatype(SupportedInTypesList,_,Port,Element),\n" +
    "\tSupportedInTypesList == any,\n" +
    "\tfilterattributessimplex(T,ElInst,Port,Filtered).\n" +
    "\n" +
    "filterattributessimplex([_|T],ElInst,Port,Filtered) :-\n" +
    "\tfilterattributessimplex(T,ElInst,Port,Filtered).\n" +
    "\n" +
    "filtersimplex(AttrList,ElInst,Port,Final):-\n" +
    "\tfindall(Filtered,filterattributessimplex(AttrList,ElInst,Port,Filtered), Result),\n" +
    "\tappend(Result,Final).\n" +
    "\n" +
    "filterattributesduplex([],_,_,[]).\n" +
    "\n" +
    "filterattributesduplex([H|T],ElInst,Port,[H|Filtered]) :-\n" +
    "\tattrtype(H,Type),\n" +
    "\tei(ElInst,Element),\n" +
    "\tportdatatype(_,SupportedOutTypesList,Port,Element),\n" +
    "\tmember(Type,SupportedOutTypesList),\n" +
    "\tfilterattributesduplex(T,ElInst,Port,Filtered).\n" +
    "\n" +
    "filterattributesduplex([H|T],ElInst,Port,[H|Filtered]) :-\n" +
    "\tattrtype(H,_),\n" +
    "\tei(ElInst,Element),\n" +
    "\tportdatatype(_,SupportedOutTypesList,Port,Element),\n" +
    "\tSupportedOutTypesList == any,\n" +
    "\tfilterattributesduplex(T,ElInst,Port,Filtered).\n" +
    "\n" +
    "filterattributesduplex([_|T],ElInst,Port,Filtered) :-\n" +
    "\tfilterattributesduplex(T,ElInst,Port,Filtered).\n" +
    "\n" +
    "filterduplex(AttrList,ElInst,Port,Final):-\n" +
    "\tfindall(Filtered,filterattributesduplex(AttrList,ElInst,Port,Filtered), Result),\n" +
    "\tappend(Result,Final).\n";


let checkerrules =
    "endpoint(epcam1, ipcamera, bedroomcam).\t\t% my bedroom camera\n" +
    "endpoint(epcam2, ipcamera, kitchencam).\t\t% my kitchen camera\n" +
    "endpoint(epnet1, httprequest, httptarget1).\t% one more example of an endpoint for URL\n" +
    "endpoint(eppush1, pushnotifier, myphone).\t% my phone\n" +
    "endpoint(eppush2, pushnotifier, mysisterphone).\t% my sister's phone\n" +
    "\n" +
    "restrictor(e5, [kitchencam]).\n" +
    "restrictor(e4,[myphone]).\n" +
    "\n" +
    "source(ipcamera, getframeport, cameraframe).\n" +
    "\n" +
    "sink(pushnotifier, sendpushwithimageport).\n" +
    "\n" +
    "%% ---------------------------------------------------------\n" +
    "%% Model checker rules\n" +
    "%% ---------------------------------------------------------\n" +
    "\n" +
    "selector(E, P) :- restrictor(E, PSET), member(P, PSET).\n" +
    "\n" +
    "flows(V, E1, P1, S1, E2, P2, S2, T) :-\n" +
    "  input(E2, P2, _, VSET2, T, _), selector(E2, S2),\n" +
    "  output(E1, P1, _, VSET1, _, _), selector(E1, S1),\n" +
    "  member(V, VSET1), member(V, VSET2).\n" +
    "\n" +
    "queryflows(EP1, EP2, D) :-\n" +
    "  endpoint(EP1, X1, S1), ei(XI1, X1), element(X1), source(X1, P1, D),\n" +
    "  endpoint(EP2, X2, S2), ei(XI2, X2), element(X2), sink(X2, P2),\n" +
    "  flows(D, XI1, P1, S1, XI2, P2, S2, _).\n" +
    "\n" +
    "querytransformed(EP1, EP2, D, T) :-\n" +
    "  endpoint(EP1, X1, S1), ei(XI1, X1), element(X1), source(X1, P1, D),\n" +
    "  endpoint(EP2, X2, S2), ei(XI2, X2), element(X2), sink(X2, P2),\n" +
    "  flows(D, XI1, P1, S1, XI2, P2, S2, TSET),\n" +
    "  member(t(TDSET, T, _), TSET), member(D, TDSET).\n" +
    "\n" +
    "%-----------------------------------------------------------\n" +
    "%% Queries for the attributes that arrive to a given element\n" +
    "%-----------------------------------------------------------\n" +
    "\n" +
    "% Outputs all Attributes that may flow to a certain Element\n" +
    "allattrthatflowto(Element,Result) :-\n" +
    "\tei(Inst,Element),\n" +
    "\tfindall(Attr,input(Inst,_,Attr,_,_,_),AttrList),\n" +
    "\tappend(AttrList,Interm),\n" +
    "\tsort(Interm,Result). % remove duplicates\n" +
    "\n" +
    "% Outputs all Values that may flow to a certain Element\n" +
    "allvaluesthatflowto(Element,SortedValues) :-\n" +
    "\tei(Inst,Element),\n" +
    "\tfindall(Value,input(Inst,_,_,Value,_,_),Values),\n" +
    "\tappend(Values,UnsortedValues),\n" +
    "\tsort(UnsortedValues,SortedValues).\n" +
    "\n" +
    "% Outputs all Transforms that may flow to a certain Element\n" +
    "alltransthatflowto(Element,SortedTranses) :-\n" +
    "\tei(Inst,Element),\n" +
    "\tfindall(Trans,input(Inst,_,_,_,Trans,_),Transes),\n" +
    "\tappend(Transes,UnsortedTranses),\n" +
    "\tsort(UnsortedTranses,SortedTranses).\n" +
    "\n" +
    "%------------------------------------------------------------\n" +
    "% Checks if a certain Attribure can flow to a given Element\n" +
    "%------------------------------------------------------------\n" +
    "\n" +
    "canattrflowto(Attribute,Element) :-\n" +
    "  allattrthatflowto(Element,AttrList),\n" +
    "  member(Attribute,AttrList).\n" +
    "\n" +
    "%------------------------------------------------------------\n" +
    "%% Checks if a certain Attribute can flow to a given element\n" +
    "%% from an untrusted element directly or indirectly avoiding\n" +
    "%% tap element restrictions.\n" +
    "%------------------------------------------------------------\n" +
    "\n" +
    "existspathfromuntrusted(_,Element):-\n" +
    "\tei(ElInst,Element),\n" +
    "\tuntrusted(ElInst).\n" +
    "\n" +
    "existspathfromuntrusted(Attribute,Element):-\n" +
    "\tei(ElInst,Element),\n" +
    "\tcanattrflowto(Attribute,Element),\n" +
    "\tinput(ElInst,PortIn,AttrList,_,_,_),\n" +
    "\tmember(Attribute,AttrList),\n" +
    "\t(connection(PrevElInstance,_,ElInst,PortIn,_) ;\n" +
    "\tconnection(ElInst,PortIn,PrevElInstance,_,_)),\n" +
    "\tei(PrevElInstance,PrevElement),\n" +
    "\tnot(ei(PrevElInstance,tap)), % important\n" +
    "\texistspathfromuntrusted(Attribute,PrevElement).\n" +
    "\n" +
    "%% Backtracks to Tap element and checks the condition attribute\n" +
    "%% Outputs the Attribute that opens a Tap\n" +
    "\n" +
    "backtracktotap(_,tap,TapInst,CondAttr):-\n" +
    "\tei(TapInst,tap),\n" +
    "\tfindall(Attr,(input(TapInst,tapconditionport,Attr,AttrValue,_,_),\n" +
    "\tmember(true,AttrValue)), % we're interested in those that open a tap\n" +
    "\tAttrList),\n" +
    "\tappend(AttrList,ProperList),\n" +
    "\tsort(ProperList,CondAttr).\n" +
    "\n" +
    "\n" +
    "backtracktotap(Attribute,Element,TapInst,CondAttr) :-\n" +
    "\tei(ElInst,Element),\n" +
    "\tinput(ElInst,PortIn,Attr,_,_,_),\n" +
    "\tmember(Attribute,Attr),\n" +
    "\t(connection(PrevElInstance,_,ElInst,PortIn,_) ;\n" +
    "\tconnection(ElInst,PortIn,PrevElInstance,_,_)),\n" +
    "\tei(PrevElInstance,PrevElement),\n" +
    "\tbacktracktotap(Attribute,PrevElement,TapInst,CondAttr).\n" +
    "\n" +
    "\n" +
    "%% Finds the original source element instance (SourceElInst) of\n" +
    "%% the Attribute that arrived to a given element instance (ElInst).\n" +
    "% Note: if returns false, either smth is wrong or attribute arrived\n" +
    "% from an untrusted element.\n" +
    "\n" +
    "getattrsource(Attribute,ElInst,ElInst) :-\n" +
    "\tfindall(Attr,input(ElInst,_,Attr,_,_,_),Attrs),\n" +
    "\tappend(Attrs,InputAttrs),\n" +
    "\tnot(member(Attribute,InputAttrs)). % no other sources of attribute\n" +
    "\n" +
    "\n" +
    "getattrsource(Attribute,ElInst,SourceElInst):-\n" +
    "\tfindall(Attr,input(ElInst,_,Attr,_,_,_),Attrs),\n" +
    "\tappend(Attrs,InputAttrs),\n" +
    "\tmember(Attribute,InputAttrs),\n" +
    "\tconnection(PrevElInst,PortOut,ElInst,_,_),\n" +
    "\toutput(PrevElInst,PortOut,AttrList,_,_,_),\n" +
    "\tmember(Attribute,AttrList),\n" +
    "\tnot(untrusted(PrevElInst)), % ignore attr that came from untrusted element\n" +
    "\tgetattrsource(Attribute,PrevElInst,SourceElInst).\n" +
    "\n" +
    "%% When the CondAttr is found we need to find if it's anyhow affected by the\n" +
    "% untrusted element, i.e. if any element higher in the path has inputs from\n" +
    "% untrusted element.\n" +
    "\n" +
    "hasuntrustedinputs(ElInst) :-\n" +
    "\tconnection(PrevElInst,_,ElInst,Port,_),\n" +
    "\tei(ElInst,El),\n" +
    "\tnot(stopport(Port,El)),\n" +
    "\tuntrusted(PrevElInst).\n" +
    "\n" +
    "hasuntrustedinputs(ElInst) :-\n" +
    "\tconnection(PrevElInst,_,ElInst,Port,_),\n" +
    "\tei(ElInst,El),\n" +
    "\tnot(stopport(Port,El)),\n" +
    "\tnot(untrusted(PrevElInst)),\n" +
    "\thasuntrustedinputs(PrevElInst).\n" +
    "\n" +
    "%% Outputs the list of attributes that trigger a certain Attribute\n" +
    "elementdependson(ElInst,Attr,AttrValue) :-\n" +
    "\tinput(ElInst,Port,Attr,AttrValue,_,_),\n" +
    "\tei(ElInst,Element),\n" +
    "\tnot(stopport(Port,Element)).\n" +
    "\n" +
    "%%------------------------------------------------------------\n" +
    "\n" +
    "hasinternetaccess :-\n" +
    "\telement(httprequest).\n" +
    "\n" +
    "attrsenttointernet(AttrList) :-\n" +
    "\tallattrthatflowto(httprequest,AttrList).\n" +
    "\n" +
    "transsenttointernet(TransList) :-\n" +
    "\talltransthatflowto(httprequest,TransList).\n" +
    "\n" +
    "encryptedattrsenttointernet(Final) :-\n" +
    "\tattrsenttointernet(AttrList),\n" +
    "\tmember(encrypted_data,AttrList),\n" +
    "\tei(Inst,httprequest),\n" +
    "\tfindall(Trans,input(Inst,_,_,_,Trans,_),Transes),\n" +
    "\tappend(Transes,Final).\n" +
    "\n" +
    "hasnotificationsaccess :-\n" +
    "\telement(pushnotifier).\n" +
    "\n" +
    "existsflow(Source,Attribute,Sink) :-\n" +
    "\tattribute(Attribute,Source),\n" +
    "\tallattrthatflowto(Sink,AttrList),\n" +
    "\tmember(Attribute,AttrList).\n" +
    "\n\n" +
    "%------------------------------------------------------------\n" +
    "% Checks if a given Attribure can flow from a given Source to\n" +
    "% a given Sink skipping the time-controlled Tap element.\n" +
    "% If true - flow can exist any time.\n" +
    "% if false - flow can only exist when Tap is opened (i.e. Time\n" +
    "% Controller config values)\n" +
    "%------------------------------------------------------------\n" +
    "\n" +
    "% final node in path (i.e. sink)\n" +
    "canskiptimetap(Sink,_,Sink,_).\n" +
    "\n" +
    "\t\n" +
    "% direct source-sink connection (simplex connections)\n" +
    "canskiptimetap(Source,Attr,Sink,[H|T]) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\t\\+(connection(_,controlsignalport,SourceInst,tapconditionport,_)),\n" +
    "\tei(SinkInst,Sink),\n" +
    "\tconnection(SourceInst,PortOut,SinkInst,PortIn,_),\n" +
    "\toutput(SourceInst,PortOut,AttrList,_,_,_),\n" +
    "\tfiltersimplex(AttrList,SinkInst,PortIn,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr), % remove duplicates\n" +
    "\tmember(Attr,NoDupAttr),\n" +
    "\t\\+(member(SinkInst,H)),\n" +
    "\t\\+(member(SinkInst,T)),\n" +
    "\tcanskiptimetap(Sink,Attr,Sink,[SinkInst,H|T]).\n" +
    "\t\n" +
    "% direct source-sink connection (duplex connections) untrusted\n" +
    "canskiptimetap(Source,Attr,Sink,[H|T]) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\t\\+(connection(_,controlsignalport,SourceInst,tapconditionport,_)),\n" +
    "\tei(SinkInst,Sink),\n" +
    "\tconnection(SinkInst,PortOut,SourceInst,PortIn,_),\n" +
    "\toutput(SourceInst,PortIn,AttrList,_,_,_),\n" +
    "\tfilterduplex(AttrList,SinkInst,PortOut,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr), % remove duplicates\n" +
    "\tmember(Attr,NoDupAttr),\n" +
    "\t\\+(member(SinkInst,H)),\n" +
    "\t\\+(member(SinkInst,T)),\n" +
    "\tcanskiptimetap(Sink,Attr,Sink,[SinkInst,H|T]).\n" +
    "\n" +
    "\n" +
    "% direct source-sink connection (duplex connections) trusted\n" +
    "canskiptimetap(Source,Attr,Sink,[H|T]) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\t%not(ei(SourceInst,tap)),\n" +
    "\t\\+(connection(_,controlsignalport,SourceInst,tapconditionport,_)),\n" +
    "\tei(SinkInst,Sink),\n" +
    "\tconnection(SinkInst,PortOut,SourceInst,PortIn,_),\n" +
    "\toutput(SourceInst,PortIn,AttrList,_,_,_),\n" +
    "\tfilterduplex(AttrList,SinkInst,PortOut,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr), % remove duplicates\n" +
    "\tmember(Attr,NoDupAttr),\n" +
    "\t\\+(member(SinkInst,H)),\n" +
    "\t\\+(member(SinkInst,T)),\n" +
    "\tcanskiptimetap(Sink,Attr,Sink,[SinkInst,H|T]).\n" +
    "\n" +
    "% indirect source-interm-sink connections\n" +
    "\n" +
    "% indirect connection (simplex connections)\n" +
    "canskiptimetap(Source,Attr,Sink,[H|T]) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\tei(NextInst,NextElement),\n" +
    "\t%\\+(ei(NextInst,tap)),\n" +
    "\t\\+(connection(_,controlsignalport,NextInst,tapconditionport,_)),\n" +
    "\tconnection(SourceInst,PortOut,NextInst,PortIn,_),\n" +
    "\toutput(SourceInst,PortOut,AttrList,_,_,_),\n" +
    "\tfiltersimplex(AttrList,NextInst,PortIn,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr), % remove duplicates\n" +
    "\tmember(Attr,NoDupAttr),\n" +
    "\t\\+(member(NextInst,H)),\n" +
    "\t\\+(member(NextInst,T)),\n" +
    "\tcanskiptimetap(NextElement,Attr,Sink,[NextInst,H|T]).\n" +
    "\n" +
    "% indirect source-sink connection (duplex connections) untrusted\n" +
    "canskiptimetap(Source,Attr,Sink,[H|T]) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\tei(NextInst,NextElement),\n" +
    "\t%\\+(ei(NextInst,tap)),\n" +
    "\t\\+(connection(_,controlsignalport,NextInst,tapconditionport,_)),\n" +
    "\tconnection(NextInst,PortOut,SourceInst,PortIn,_),\n" +
    "\toutput(SourceInst,PortIn,AttrList,_,_,_),\n" +
    "\tfilterduplex(AttrList,NextInst,PortOut,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr), % remove duplicates\n" +
    "\tmember(Attr,NoDupAttr),\n" +
    "\t\\+(member(NextInst,H)),\n" +
    "\t\\+(member(NextInst,T)),\n" +
    "\tcanskiptimetap(NextElement,Attr,Sink,[NextInst,H|T]).\n" +
    "\n" +
    "% indirect source-sink connection (duplex connections) trusted\n" +
    "canskiptimetap(Source,Attr,Sink,[H|T]) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\tei(NextInst,NextElement),\n" +
    "\t%\\+(ei(NextInst,tap)),\n" +
    "\t\\+(connection(_,controlsignalport,NextInst,tapconditionport,_)),\n" +
    "\tconnection(NextInst,PortOut,SourceInst,PortIn,_),\n" +
    "\toutput(SourceInst,PortIn,AttrList,_,_,_),\n" +
    "\tfilterduplex(AttrList,NextInst,PortOut,FilteredAttr),\n" +
    "\tsort(FilteredAttr,NoDupAttr), % remove duplicates\n" +
    "\tmember(Attr,NoDupAttr),\n" +
    "\t\\+(member(NextInst,H)),\n" +
    "\t\\+(member(NextInst,T)),\n" +
    "\tcanskiptimetap(NextElement,Attr,Sink,[NextInst,H|T]).\n" +
    "\n" +
    "% helper method for canskiptimetap predicate\n" +
    "\n" +
    "checkifflowswithouttimetap(Source,Attr,Sink) :-\n" +
    "\tei(SourceInst,Source),\n" +
    "\tcanskiptimetap(Source,Attr,Sink,[SourceInst]).";


let appmodel =
    ":- use_module(library(lists)).\n\n" +    // loads lists procedures to tau prolog
    "%%---------------------------------------------------------\n" +
    `% ${appName} App Model\n` +
    "%%---------------------------------------------------------\n\n"
    +
    "%%---------------------------------------------------------\n" +
    "% App elements\n" +
    "%%---------------------------------------------------------\n\n" +
    elements + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Element instances\n" +
    "%%---------------------------------------------------------\n\n" +
    elinstances + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Untrusted element\n" +
    "%%---------------------------------------------------------\n\n" +
    untrustedel + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% App connections\n" +
    "%%---------------------------------------------------------\n\n" +
    elconnections + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% App configs\n" +
    "%%---------------------------------------------------------\n\n" +
    elementconfigs + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Attributes\n" +
    "%%---------------------------------------------------------\n\n" +
    elattributes + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Attribute types\n" +
    "%%---------------------------------------------------------\n\n" +
    elattributetypes + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Supported attribute types for a given element's port\n" +
    "% portdatatype([INTYPES],[OUTTYPES],PORT,ELEMENT)\n" +
    "%%---------------------------------------------------------\n\n" +
    elportattrtypes + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Elements' ports that stop attribute propagation\n" +
    "%%---------------------------------------------------------\n\n" +
    elstopports + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Elements' output rules\n" +
    "%%---------------------------------------------------------\n\n" +
    eloutputrules + "\n"
    +
    "%----------------------------------------------------------\n" +
    "%% Helper rules for the elements' output\n" +
    "%----------------------------------------------------------\n\n" +
    elhelpers + "\n"
    +
    "%%----------------------------------------------------------\n" +
    "% General input rules\n" +
    "%%----------------------------------------------------------\n\n" +
    generalinputrules + "\n"
    +
    "%%----------------------------------------------------------\n" +
    "% Checker rules\n" +
    "%%----------------------------------------------------------\n\n" +
    checkerrules + "\n\n";


let justappmodel =
    "%%---------------------------------------------------------\n" +
    `% ${appName} App Model\n` +
    "%%---------------------------------------------------------\n\n"
    +
    "%%---------------------------------------------------------\n" +
    "% App elements\n" +
    "%%---------------------------------------------------------\n\n" +
    elements + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Element instances\n" +
    "%%---------------------------------------------------------\n\n" +
    elinstances + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Untrusted element\n" +
    "%%---------------------------------------------------------\n\n" +
    untrustedel + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% App connections\n" +
    "%%---------------------------------------------------------\n\n" +
    elconnections + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% App configs\n" +
    "%%---------------------------------------------------------\n\n" +
    elementconfigs + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Attributes\n" +
    "%%---------------------------------------------------------\n\n" +
    elattributes + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Attribute types\n" +
    "%%---------------------------------------------------------\n\n" +
    elattributetypes + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Supported attribute types for a given element's port\n" +
    "% portdatatype([INTYPES],[OUTTYPES],PORT,ELEMENT)\n" +
    "%%---------------------------------------------------------\n\n" +
    elportattrtypes + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Elements' ports that stop attribute propagation\n" +
    "%%---------------------------------------------------------\n\n" +
    elstopports + "\n"
    +
    "%%---------------------------------------------------------\n" +
    "% Elements' output rules\n" +
    "%%---------------------------------------------------------\n\n" +
    eloutputrules + "\n"
    +
    "%----------------------------------------------------------\n" +
    "%% Helper rules for the elements' output\n" +
    "%----------------------------------------------------------\n\n" +
    elhelpers;



// Define possible app flows

let flowDevices = [];
let timeControllerExists = false;
let hourFrom = null;
let minFrom = null;
let hourTill = null;
let minTill = null;
let sourceAttrPairs = [];
let flowExistsQueries = [];
let flowCandidates = [];

appelements.forEach((appelement) => {
    if (appelement.type.toLowerCase()==='timecontroller') {
        timeControllerExists = true;
        hourFrom = appelement.config.starttime.split(':')[0];
        minFrom = appelement.config.starttime.split(':')[1];
        hourTill = appelement.config.endtime.split(':')[0];
        minTill = appelement.config.endtime.split(':')[1];
    }
    if ((allelements[appelement.type.toLowerCase()]) && (allelements[appelement.type.toLowerCase()].device)) {
        let el = allelements[appelement.type.toLowerCase()];
        flowDevices.push(el.name);
        if (el.attributes.length>0) {
            el.attributes.forEach((attr) => {
                let pair = {
                    source: el.name,
                    attribute: attr
                };
                sourceAttrPairs.push(pair);
            })
        }
    }
});

if (sourceAttrPairs.length>0) {
    sourceAttrPairs.forEach((pair) => {
        let source = pair.source;
        let attribute = pair.attribute;
        flowDevices.forEach((sink) => {
            if (sink!==source) {
                let flowQuery = `existsflow(${source},${attribute},${sink}).`;
                flowExistsQueries.push(flowQuery);
                let flowCandidate = {
                    source: source,
                    attribute: attribute,
                    sink: sink,
                    flowQuery: flowQuery
                };
                flowCandidates.push(flowCandidate);
            }
        })
    })
}

console.log(flowExistsQueries);

// verify flow queries


// flowExistsQueries.forEach((query) => {
//     const session = prolog.create( 50000 );
//     session.consult(appmodel);
//     // Query the goal
//     session.query(query);
//     // Show answers
//     session.answers( x => {
//         // console.log(query,prolog.format_answer(x))
//         if (prolog.format_answer(x)==='true ;') {
//             console.log(query,prolog.format_answer(x));
//         }
//     });
// });

flowCandidates.forEach((candidate) => {
    const session = prolog.create( 50000 );
    session.consult(appmodel);
    // Query the goal
    session.query(candidate.flowQuery);
    // Show answers
    session.answers( x => {
        // console.log(candidate.flowQuery,prolog.format_answer(x))
        if (prolog.format_answer(x)==='true ;') {
            console.log(candidate.flowQuery,prolog.format_answer(x));
            candidate.flowExists = true;
            let timecheckquery = `once(checkifflowswithouttimetap(${candidate.source},${candidate.attribute},${candidate.sink})).`;
            // console.log(timecheckquery);
            checkIfFlowsWithoutTimeTap(timecheckquery,(result) => {
                console.log("RESULT: ",result);
                if (result === 'true ;') {
                    // candidate.timeFrom = '_';
                    // candidate.timeTill = '_';
                    candidate.hourFrom = '_';
                    candidate.minFrom = '_';
                    candidate.hourTill = '_';
                    candidate.minTill = '_';
                } else if (result === 'false.') {
                    // candidate.timeFrom = `${hourFrom}:${minFrom}`;
                    // candidate.timeTill = `${hourTill}:${minTill}`;
                    candidate.hourFrom = hourFrom;
                    candidate.minFrom = minFrom;
                    candidate.hourTill = hourTill;
                    candidate.minTill = minTill;
                } else {
                    console.error("Wrong query processing result");
                }
            })

        } else {
            candidate.flowExists = false;
        }
    });
});


function checkIfFlowsWithoutTimeTap(query,callback) {
    const session = prolog.create( 50000 );
    session.consult(appmodel);
    // Query the goal
    session.query(query);
    // Show answers
    let answers = '';
    session.answers( x => {
        // console.log(query,prolog.format_answer(x));
        // if (prolog.format_answer(x) === 'true ;') callback(true);
        answers += prolog.format_answer(x);
    });
    callback(answers);
}


// // Limit the number of resolution steps that the interpreter can make
// const session = prolog.create( 10000 );
//
// session.consult(appmodel);
//
// // Query the goal
// session.query("allattrthatflowto(httprequest,X).");
//
// // Show answers
// session.answers( x => console.log( prolog.format_answer(x) ) );

// Create flows facts for the app model

let flows = [];
flowCandidates.forEach((flow) => {
    if (flow.flowExists===true) {
        // let flowModelfact = `flow(${flow.source},${flow.attribute},${flow.sink},${flow.timeFrom},${flow.timeTill}).\n`;
        // let flowAPIfact = `flow(${flow.source},${flow.attribute},${flow.sink},${flow.timeFrom},${flow.timeTill}).`;
        let flowModelfact = `flow(${flow.source},${flow.attribute},${flow.sink},${flow.hourFrom},${flow.minFrom},${flow.hourTill},${flow.minTill}).\n`;
        // let flowAPIfact = `flow(${flow.source},${flow.attribute},${flow.sink},${flow.hourFrom},${flow.minFrom},${flow.hourTill},${flow.minTill}).`;
        let flowAPIfact = {
            source: flow.source,
            attribute: flow.attribute,
            sink: flow.sink,
            hourFrom: flow.hourFrom,
            minFrom: flow.minFrom,
            hourTill: flow.hourTill,
            minTill: flow.minTill
        };
        flows.push(flowAPIfact);
        appmodel += flowModelfact;
    }
});

console.log(JSON.stringify(flows));

// Print the model
// console.log(appmodel);


// save app model to JSON

// var json = JSON.stringify({model: justappmodel}, null, 2);
// var json = JSON.stringify({model: appmodel}, null, 2);
//
// fs.writeFile(`./app-models/${appName.toLowerCase()}.json`, json, 'utf8', (err) => {
//     if (err) throw err;
//     console.log('The json file has been saved!');
// });

// fs.writeFile(`./app-models/prolog/${appName.toLowerCase()}.pl`, appmodel, 'utf8', (err) => {
//     if (err) throw err;
//     console.log('The prolog file has been saved!');
// });
