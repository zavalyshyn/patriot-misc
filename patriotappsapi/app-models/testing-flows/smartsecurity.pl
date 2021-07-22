:- use_module(library(lists)).

%%---------------------------------------------------------
% SmartSecurity App Model
%%---------------------------------------------------------

%%---------------------------------------------------------
% App elements
%%---------------------------------------------------------

element(motionsensor).
element(contactsensor).
element(or).
element(pushnotifier).
element(ipcamera).
element(untrusted).
element(alarm).
element(timer).
element(tap).
element(not).
element(httprequest).

%%---------------------------------------------------------
% Element instances
%%---------------------------------------------------------

ei(e1,motionsensor).
ei(e2,contactsensor).
ei(e3,or).
ei(e4,pushnotifier).
ei(e5,ipcamera).
ei(e6,untrusted).
ei(e7,alarm).
ei(e8,timer).
ei(e9,tap).
ei(e10,not).
ei(e11,httprequest).

%%---------------------------------------------------------
% Untrusted element
%%---------------------------------------------------------

untrusted(e6).

%%---------------------------------------------------------
% App connections
%%---------------------------------------------------------

connection(e1,motiondetectedport,e3,orin1port,simplex).
connection(e2,contactopenport,e3,orin2port,simplex).
connection(e3,oroutport,e6,oroutport,simplex).
connection(e3,oroutport,e8,starttimeronceport,simplex).
connection(e5,cameraframeport,e6,cameraframeport,simplex).
connection(e6,getframeport,e5,getframeport,duplex).
connection(e6,triggeralarmport,e7,triggeralarmport,simplex).
connection(e6,sendpushwithimageport,e4,sendpushwithimageport,simplex).
connection(e6,tapinport,e9,tapinport,duplex).
connection(e6,gettapstateport,e9,gettapstateport,duplex).
connection(e4,dismissreceivedport,e10,notinport,simplex).
connection(e4,dismissreceivedport,e8,stoptimerport,simplex).
connection(e8,timerexpiredport,e9,tapconditionport,simplex).
connection(e9,tapoutport,e11,httppostport,duplex).
connection(e10,notoutport,e9,tapconditionport,simplex).

%%---------------------------------------------------------
% App configs
%%---------------------------------------------------------

config(10000,time,timer).
config('146.193.41.162/intrusion',hostname,httprequest).

%%---------------------------------------------------------
% Attributes
%%---------------------------------------------------------

attribute(motion_detected,motionsensor).
attribute(contact_open,contactsensor).
attribute(contact_closed,contactsensor).
attribute(dismiss_received,pushnotifier).
attribute(camera_frame,ipcamera).
attribute(timer_expired,timer).
attribute(tap_state,tap).
attribute(data_from_internet,httprequest).

%%---------------------------------------------------------
% Attribute types
%%---------------------------------------------------------

attrtype(motion_detected,boolean).
attrtype(contact_open,boolean).
attrtype(contact_closed,boolean).
attrtype(dismiss_received,boolean).
attrtype(camera_frame,image).
attrtype(timer_expired,boolean).
attrtype(tap_state,boolean).
attrtype(data_from_internet,externaldata).

%%---------------------------------------------------------
% Supported attribute types for a given element's port
% portdatatype([INTYPES],[OUTTYPES],PORT,ELEMENT)
%%---------------------------------------------------------

portdatatype([],[boolean],motiondetectedport,motionsensor).
portdatatype([],[boolean],contactclosedport,contactsensor).
portdatatype([],[boolean],contactopenport,contactsensor).
portdatatype([boolean],[],orin1port,or).
portdatatype([boolean],[],orin2port,or).
portdatatype([],[boolean],oroutport,or).
portdatatype([text,image],[],sendpushwithimageport,pushnotifier).
portdatatype([text],[],sendpushmessageport,pushnotifier).
portdatatype([],[boolean],dismissreceivedport,pushnotifier).
portdatatype([],[image],getframeport,ipcamera).
portdatatype([],[image],cameraframeport,ipcamera).
portdatatype(any,any,_,untrusted).
portdatatype([],[],triggeralarmport,alarm).
portdatatype([boolean],[],starttimerport,timer).
portdatatype([boolean],[],starttimeronceport,timer).
portdatatype([boolean],[],stoptimerport,timer).
portdatatype([],[boolean],timerexpiredport,timer).
portdatatype([boolean],[],tapconditionport,tap).
portdatatype(any,any,tapinport,tap). % can be duplex
portdatatype(any,any,tapoutport,tap).% can be duplex
portdatatype([boolean],[],notinport,not).
portdatatype([],[boolean],notoutport,not).
portdatatype(any,any,httppostport,httprequest).
portdatatype(any,any,httpgetport,httprequest).
portdatatype(any,any,httpputport,httprequest).
portdatatype(any,any,httpdeleteport,httprequest).

%%---------------------------------------------------------
% Elements' ports that stop attribute propagation
%%---------------------------------------------------------

stopport(sendpushwithimageport,pushnotifier).
stopport(sendpushmessageport,pushnotifier).
stopport(triggeralarmport,alarm).
stopport(stoptimerport,timer).
stopport(tapconditionport,tap).

%%---------------------------------------------------------
% Elements' output rules
%%---------------------------------------------------------

output(X,motiondetectedport,Attr,Value,Trans,Time) :-
	ei(X,motionsensor),
	Attr = [motion_detected],
	Value=[true],
	Trans = [],
	Time is 0.

output(X,motiondetectedport,Attr,Value,Trans,Time) :-
	ei(X,motionsensor),
	Attr = [motion_detected],
	Value=[false],
	Trans = [],
	Time is 0.

output(X,contactopenport,Attr,Value,Trans,Time) :-
	ei(X,contactsensor),
	Attr = [contact_open],
	Value=[true],
	Trans = [],
	Time is 0.

output(X,contactopenport,Attr,Value,Trans,Time) :-
	ei(X,contactsensor),
	Attr = [contact_open],
	Value=[false],
	Trans = [],
	Time is 0.

output(X,contactclosedport,Attr,Value,Trans,Time) :-
	ei(X,contactsensor),
	Attr = [contact_closed],
	Value=[true],
	Trans = [],
	Time is 0.

output(X,contactclosedport,Attr,Value,Trans,Time) :-
	ei(X,contactsensor),
	Attr = [contact_closed],
	Value=[false],
	Trans = [],
	Time is 0.

output(X,oroutport,Attr,Value,Trans,Time) :-
	ei(X,or),
	input(X,orin1port,Attr,ValueIn,Trans,TimeIn),
	member(true,ValueIn),
	Value = [true],
	Time is TimeIn.

output(X,oroutport,Attr,Value,Trans,Time) :-
	ei(X,or),
	input(X,orin2port,Attr,ValueIn,Trans,TimeIn),
	member(true,ValueIn),
	Value = [true],
	Time is TimeIn.

%output(X,oroutport,Attr,Value,Trans,Time) :-
%	ei(X,or),
%	input(X,orin1port,Attr1,ValueIn1,Trans1,TimeIn1),
%	member(false,ValueIn1),
%	input(X,orin2port,Attr2,ValueIn2,Trans2,TimeIn2),
%	member(false,ValueIn2),
%	Value = [false],
%	Attr = [Attr1,Attr2],
%	Trans = [Trans1,Trans2],
%	Time is max(TimeIn1,TimeIn2).

output(X,dismissreceivedport,Attr,Value,Trans,Time) :-
	ei(X,pushnotifier),
	Attr = [dismiss_received],
	Value = [true],
	Trans = [],
	Time is 0.

output(X,cameraframeport,Attr,Value,Trans,Time) :-
	ei(X,ipcamera),
	Attr = [camera_frame],
	Value = [cameraframe],
	Trans = [],
	Time is 0.

output(X,getframeport,Attr,Value,Trans,Time) :-
	ei(X,ipcamera),
	Attr = [camera_frame],
	Value = [cameraframe],
	Trans = [],
	Time is 0.

output(X,_,Attr,Value,Trans,Time):-
	ei(X,untrusted),
	untrusted(X),
	allattr(X,Attr),
	allvalues(X,Value),
	alltransforms(X,Trans),
	alltimes(X,Time).

output(X,timerexpiredport,Attr,Value,Trans,Time) :-
	ei(X,timer),
	config(Timeout,time,timer),
	input(X,settimerport,_,_,Trans,TimeIn),
	Attr = [timer_expired],
	Value = [true],
	Time is TimeIn + Timeout.

output(X,timerexpiredport,Attr,Value,Trans,Time) :-
	ei(X,timer),
	config(Timeout,time,timer),
	input(X,starttimerport,_,_,Trans,TimeIn),
	Attr = [timer_expired],
	Value = [true],
	Time is TimeIn + Timeout.

output(X,timerexpiredport,Attr,Value,Trans,Time) :-
	ei(X,timer),
	config(Timeout,time,timer),
	input(X,starttimeronceport,_,_,Trans,TimeIn),
	Attr = [timer_expired],
	Value = [true],
	Time is TimeIn + Timeout.

output(X,tapoutport,Attr,Value,Trans,Time) :-
	ei(X,tap),
	input(X,tapconditionport,_,ValueCond,_,TimeInCond),
	member(true,ValueCond),
	input(X,tapinport,Attr,Value,Trans,TimeIn),
	Time is max(TimeIn,TimeInCond).

output(X,tapinport,Attr,Value,Trans,Time) :-
	ei(X,tap),
	input(X,tapconditionport,_,ValueCond,_,_),
	member(true,ValueCond),
	input(X,tapoutport,Attr,Value,Trans,Time).

output(X,gettapstateport,Attr,Value,Trans,Time) :-
	ei(X,tap),
	Attr = [tap_state],
	Value = [true],
	Trans = [],
	Time is 0.

output(X,gettapstateport,Attr,Value,Trans,Time) :-
	ei(X,tap),
	Attr = [tap_state],
	Value = [false],
	Trans = [],
	Time is 0.

output(X,notoutport,Attr,Value,Trans,Time) :-
	ei(X,not),
	input(X,notinport,Attr,ValueIn,Trans,TimeIn),
	Time is TimeIn,
	% A -> X ; Y means if A is true then X otherwise Y
	(member(true,ValueIn) -> Value = [false] ; Value = [true]).

output(X,httpgetport,Attr,Value,Trans,Time) :-
	ei(X,httprequest),
	Attr = [data_from_internet],
	Value = [],
	Trans = [],
	Time is 0.

output(X,httppostport,Attr,Value,Trans,Time) :-
	ei(X,httprequest),
	Attr = [data_from_internet],
	Value = [],
	Trans = [],
	Time is 0.

output(X,httpputport,Attr,Value,Trans,Time) :-
	ei(X,httprequest),
	Attr = [data_from_internet],
	Value = [],
	Trans = [],
	Time is 0.

output(X,httpdeleteport,Attr,Value,Trans,Time) :-
	ei(X,httprequest),
	Attr = [data_from_internet],
	Value = [],
	Trans = [],
	Time is 0.


%----------------------------------------------------------
%% Helper rules for the elements' output
%----------------------------------------------------------

% Helper rules for the untrusted element
allattr(ElInstance, Result) :-
	findall(Attr, input(ElInstance,_,Attr,_,_,_),L),
	append(L, W),
	sort(W,Result).

allvalues(ElInstance, Result) :-
	findall(Value, input(ElInstance,_,_,Value,_,_),L),
	append(L, W),
	sort(W,Result).

alltransforms(ElInstance, Result) :-
	findall(Trans, input(ElInstance,_,_,_,Trans,_),L),
	append(L, W),
	sort(W,Result).

alltimes(ElInstance, Final) :-
	findall(Time, input(ElInstance,_,_,_,_,Time),Times),
	findmax(Times,Final).


accMax([H|T],Acc,Max) :-
	H > Acc,
	accMax(T,H,Max).

accMax([H|T],Acc,Max) :-
	H =< Acc,
	accMax(T,Acc,Max).

accMax([],Acc,Acc).

findmax(List,Max) :-
	List = [H|_],
	accMax(List,H,Max).

%%----------------------------------------------------------
% General input rules
%%----------------------------------------------------------

% case for simplex connections
input(ElInstance,InPort,NoDupAttr,Value,Trans,Time) :-
	connection(PrevElInstance,OutPort,ElInstance,InPort,_),
	ei(PrevElInstance,PrevElement),
	ei(ElInstance,Element),
	element(PrevElement),
	element(Element),
	output(PrevElInstance,OutPort,Attr,Value,Trans,Time),
	filtersimplex(Attr,ElInstance,InPort,FilteredAttr),
	sort(FilteredAttr,NoDupAttr). % remove duplicates

% case for duplex connections
% (untrusted <-duplex-> trusted)
% element is untrusted
input(ElInstance,OutPort,NoDupAttr,Value,Trans,Time) :-
	connection(ElInstance,OutPort,NextElInstance,InPort,Mode),
	Mode = duplex,
	ei(NextElInstance,NextElement),
	ei(ElInstance,Element),
	element(NextElement),
	element(Element),
	output(NextElInstance,InPort,Attr,Value,Trans,Time),
	filterduplex(Attr,ElInstance,InPort,FilteredAttr),
	sort(FilteredAttr,NoDupAttr). % remove duplicates

% case for duplex connections (trusted elements)
% trusted <-duplex-> trusted
% e.g. tap <-httppostport-> httprequest

input(ElInst,OutPort,NoDupAttr,Value,Trans,Time) :-
	connection(ElInst,OutPort,NextElInst,InPort,Mode),
	Mode = duplex,
	ei(NextElInst,NextElement),
	ei(ElInst,Element),
	element(NextElement),
	element(Element),
	output(NextElInst,InPort,Attr,Value,Trans,Time),
	filterduplex(Attr,ElInst,OutPort,FilteredAttr),
	sort(FilteredAttr,NoDupAttr). % remove duplicates

%----------------------------------------------------------
%% Filter input attributes based on the input port's
%% supported attributes list
%----------------------------------------------------------

filterattributessimplex([],_,_,[]).

filterattributessimplex([H|T],ElInst,Port,[H|Filtered]) :-
	attrtype(H,Type),
	ei(ElInst,Element),
	portdatatype(SupportedInTypesList,_,Port,Element),
	member(Type,SupportedInTypesList),
	filterattributessimplex(T,ElInst,Port,Filtered).

filterattributessimplex([H|T],ElInst,Port,[H|Filtered]) :-
	attrtype(H,_),
	ei(ElInst,Element),
	portdatatype(SupportedInTypesList,_,Port,Element),
	SupportedInTypesList == any,
	filterattributessimplex(T,ElInst,Port,Filtered).

filterattributessimplex([_|T],ElInst,Port,Filtered) :-
	filterattributessimplex(T,ElInst,Port,Filtered).

filtersimplex(AttrList,ElInst,Port,Final):-
	findall(Filtered,filterattributessimplex(AttrList,ElInst,Port,Filtered), Result),
	append(Result,Final).

filterattributesduplex([],_,_,[]).

filterattributesduplex([H|T],ElInst,Port,[H|Filtered]) :-
	attrtype(H,Type),
	ei(ElInst,Element),
	portdatatype(_,SupportedOutTypesList,Port,Element),
	member(Type,SupportedOutTypesList),
	filterattributesduplex(T,ElInst,Port,Filtered).

filterattributesduplex([H|T],ElInst,Port,[H|Filtered]) :-
	attrtype(H,_),
	ei(ElInst,Element),
	portdatatype(_,SupportedOutTypesList,Port,Element),
	SupportedOutTypesList == any,
	filterattributesduplex(T,ElInst,Port,Filtered).

filterattributesduplex([_|T],ElInst,Port,Filtered) :-
	filterattributesduplex(T,ElInst,Port,Filtered).

filterduplex(AttrList,ElInst,Port,Final):-
	findall(Filtered,filterattributesduplex(AttrList,ElInst,Port,Filtered), Result),
	append(Result,Final).

%%----------------------------------------------------------
% Checker rules
%%----------------------------------------------------------

endpoint(epcam1, ipcamera, bedroomcam).		% my bedroom camera
endpoint(epcam2, ipcamera, kitchencam).		% my kitchen camera
endpoint(epnet1, httprequest, httptarget1).	% one more example of an endpoint for URL
endpoint(eppush1, pushnotifier, myphone).	% my phone
endpoint(eppush2, pushnotifier, mysisterphone).	% my sister's phone

restrictor(e5, [kitchencam]).
restrictor(e4,[myphone]).

source(ipcamera, getframeport, cameraframe).

sink(pushnotifier, sendpushwithimageport).

%% ---------------------------------------------------------
%% Model checker rules
%% ---------------------------------------------------------

selector(E, P) :- restrictor(E, PSET), member(P, PSET).

flows(V, E1, P1, S1, E2, P2, S2, T) :-
  input(E2, P2, _, VSET2, T, _), selector(E2, S2),
  output(E1, P1, _, VSET1, _, _), selector(E1, S1),
  member(V, VSET1), member(V, VSET2).

queryflows(EP1, EP2, D) :-
  endpoint(EP1, X1, S1), ei(XI1, X1), element(X1), source(X1, P1, D),
  endpoint(EP2, X2, S2), ei(XI2, X2), element(X2), sink(X2, P2),
  flows(D, XI1, P1, S1, XI2, P2, S2, _).

querytransformed(EP1, EP2, D, T) :-
  endpoint(EP1, X1, S1), ei(XI1, X1), element(X1), source(X1, P1, D),
  endpoint(EP2, X2, S2), ei(XI2, X2), element(X2), sink(X2, P2),
  flows(D, XI1, P1, S1, XI2, P2, S2, TSET),
  member(t(TDSET, T, _), TSET), member(D, TDSET).

%-----------------------------------------------------------
%% Queries for the attributes that arrive to a given element
%-----------------------------------------------------------

% Outputs all Attributes that may flow to a certain Element
allattrthatflowto(Element,Result) :-
	ei(Inst,Element),
	findall(Attr,input(Inst,_,Attr,_,_,_),AttrList),
	append(AttrList,Interm),
	sort(Interm,Result). % remove duplicates

% Outputs all Values that may flow to a certain Element
allvaluesthatflowto(Element,SortedValues) :-
	ei(Inst,Element),
	findall(Value,input(Inst,_,_,Value,_,_),Values),
	append(Values,UnsortedValues),
	sort(UnsortedValues,SortedValues).

% Outputs all Transforms that may flow to a certain Element
alltransthatflowto(Element,SortedTranses) :-
	ei(Inst,Element),
	findall(Trans,input(Inst,_,_,_,Trans,_),Transes),
	append(Transes,UnsortedTranses),
	sort(UnsortedTranses,SortedTranses).

%------------------------------------------------------------
% Checks if a certain Attribure can flow to a given Element
%------------------------------------------------------------

canattrflowto(Attribute,Element) :-
  allattrthatflowto(Element,AttrList),
  member(Attribute,AttrList).

%------------------------------------------------------------
%% Checks if a certain Attribute can flow to a given element
%% from an untrusted element directly or indirectly avoiding
%% tap element restrictions.
%------------------------------------------------------------

existspathfromuntrusted(_,Element):-
	ei(ElInst,Element),
	untrusted(ElInst).

existspathfromuntrusted(Attribute,Element):-
	ei(ElInst,Element),
	canattrflowto(Attribute,Element),
	input(ElInst,PortIn,AttrList,_,_,_),
	member(Attribute,AttrList),
	(connection(PrevElInstance,_,ElInst,PortIn,_) ;
	connection(ElInst,PortIn,PrevElInstance,_,_)),
	ei(PrevElInstance,PrevElement),
	not(ei(PrevElInstance,tap)), % important
	existspathfromuntrusted(Attribute,PrevElement).

%% Backtracks to Tap element and checks the condition attribute
%% Outputs the Attribute that opens a Tap

backtracktotap(_,tap,TapInst,CondAttr):-
	ei(TapInst,tap),
	findall(Attr,(input(TapInst,tapconditionport,Attr,AttrValue,_,_),
	member(true,AttrValue)), % we're interested in those that open a tap
	AttrList),
	append(AttrList,ProperList),
	sort(ProperList,CondAttr).


backtracktotap(Attribute,Element,TapInst,CondAttr) :-
	ei(ElInst,Element),
	input(ElInst,PortIn,Attr,_,_,_),
	member(Attribute,Attr),
	(connection(PrevElInstance,_,ElInst,PortIn,_) ;
	connection(ElInst,PortIn,PrevElInstance,_,_)),
	ei(PrevElInstance,PrevElement),
	backtracktotap(Attribute,PrevElement,TapInst,CondAttr).


%% Finds the original source element instance (SourceElInst) of
%% the Attribute that arrived to a given element instance (ElInst).
% Note: if returns false, either smth is wrong or attribute arrived
% from an untrusted element.

getattrsource(Attribute,ElInst,ElInst) :-
	findall(Attr,input(ElInst,_,Attr,_,_,_),Attrs),
	append(Attrs,InputAttrs),
	not(member(Attribute,InputAttrs)). % no other sources of attribute


getattrsource(Attribute,ElInst,SourceElInst):-
	findall(Attr,input(ElInst,_,Attr,_,_,_),Attrs),
	append(Attrs,InputAttrs),
	member(Attribute,InputAttrs),
	connection(PrevElInst,PortOut,ElInst,_,_),
	output(PrevElInst,PortOut,AttrList,_,_,_),
	member(Attribute,AttrList),
	not(untrusted(PrevElInst)), % ignore attr that came from untrusted element
	getattrsource(Attribute,PrevElInst,SourceElInst).

%% When the CondAttr is found we need to find if it's anyhow affected by the
% untrusted element, i.e. if any element higher in the path has inputs from
% untrusted element.

hasuntrustedinputs(ElInst) :-
	connection(PrevElInst,_,ElInst,Port,_),
	ei(ElInst,El),
	not(stopport(Port,El)),
	untrusted(PrevElInst).

hasuntrustedinputs(ElInst) :-
	connection(PrevElInst,_,ElInst,Port,_),
	ei(ElInst,El),
	not(stopport(Port,El)),
	not(untrusted(PrevElInst)),
	hasuntrustedinputs(PrevElInst).

%% Outputs the list of attributes that trigger a certain Attribute
elementdependson(ElInst,Attr,AttrValue) :-
	input(ElInst,Port,Attr,AttrValue,_,_),
	ei(ElInst,Element),
	not(stopport(Port,Element)).

%%------------------------------------------------------------

hasinternetaccess :-
	element(httprequest).

attrsenttointernet(AttrList) :-
	allattrthatflowto(httprequest,AttrList).

transsenttointernet(TransList) :-
	alltransthatflowto(httprequest,TransList).

encryptedattrsenttointernet(Final) :-
	attrsenttointernet(AttrList),
	member(encrypted_data,AttrList),
	ei(Inst,httprequest),
	findall(Trans,input(Inst,_,_,_,Trans,_),Transes),
	append(Transes,Final).

hasnotificationsaccess :-
	element(pushnotifier).

existsflow(Source,Attribute,Sink) :-
	attribute(Attribute,Source),
	allattrthatflowto(Sink,AttrList),
	member(Attribute,AttrList).


%------------------------------------------------------------
% Checks if a given Attribure can flow from a given Source to
% a given Sink skipping the time-controlled Tap element.
% If true - flow can exist any time.
% if false - flow can only exist when Tap is opened (i.e. Time
% Controller config values)
%------------------------------------------------------------

% final node in path (i.e. sink)
canskiptimetap(Sink,_,Sink,_).

	
% direct source-sink connection (simplex connections)
canskiptimetap(Source,Attr,Sink,[H|T]) :-
	ei(SourceInst,Source),
	not(connection(_,controlsignalport,SourceInst,tapconditionport,_)),
	ei(SinkInst,Sink),
	connection(SourceInst,PortOut,SinkInst,PortIn,_),
	output(SourceInst,PortOut,AttrList,_,_,_),
	filtersimplex(AttrList,SinkInst,PortIn,FilteredAttr),
	sort(FilteredAttr,NoDupAttr), % remove duplicates
	member(Attr,NoDupAttr),
	not(member(SinkInst,H)),
	not(member(SinkInst,T)),
	canskiptimetap(Sink,Attr,Sink,[SinkInst,H|T]).
	
% direct source-sink connection (duplex connections) untrusted
canskiptimetap(Source,Attr,Sink,[H|T]) :-
	ei(SourceInst,Source),
	%not(ei(SourceInst,tap)),
	not(connection(_,controlsignalport,SourceInst,tapconditionport,_)),
	ei(SinkInst,Sink),
	connection(SinkInst,PortOut,SourceInst,PortIn,_),
	output(SourceInst,PortIn,AttrList,_,_,_),
	filterduplex(AttrList,SinkInst,PortOut,FilteredAttr),
	sort(FilteredAttr,NoDupAttr), % remove duplicates
	member(Attr,NoDupAttr),
	not(member(SinkInst,H)),
	not(member(SinkInst,T)),
	canskiptimetap(Sink,Attr,Sink,[SinkInst,H|T]).


% direct source-sink connection (duplex connections) trusted
canskiptimetap(Source,Attr,Sink,[H|T]) :-
	ei(SourceInst,Source),
	%not(ei(SourceInst,tap)),
	not(connection(_,controlsignalport,SourceInst,tapconditionport,_)),
	ei(SinkInst,Sink),
	connection(SinkInst,PortOut,SourceInst,PortIn,_),
	output(SourceInst,PortIn,AttrList,_,_,_),
	filterduplex(AttrList,SinkInst,PortOut,FilteredAttr),
	sort(FilteredAttr,NoDupAttr), % remove duplicates
	member(Attr,NoDupAttr),
	not(member(SinkInst,H)),
	not(member(SinkInst,T)),
	canskiptimetap(Sink,Attr,Sink,[SinkInst,H|T]).

% indirect source-interm-sink connections

% indirect connection (simplex connections)
canskiptimetap(Source,Attr,Sink,[H|T]) :-
	ei(SourceInst,Source),
	ei(NextInst,NextElement),
	%not(ei(NextInst,tap)),
	not(connection(_,controlsignalport,NextInst,tapconditionport,_)),
	connection(SourceInst,PortOut,NextInst,PortIn,_),
	output(SourceInst,PortOut,AttrList,_,_,_),
	filtersimplex(AttrList,NextInst,PortIn,FilteredAttr),
	sort(FilteredAttr,NoDupAttr), % remove duplicates
	member(Attr,NoDupAttr),
	not(member(NextInst,H)),
	not(member(NextInst,T)),
	canskiptimetap(NextElement,Attr,Sink,[NextInst,H|T]).

% indirect source-sink connection (duplex connections) untrusted
canskiptimetap(Source,Attr,Sink,[H|T]) :-
	ei(SourceInst,Source),
	ei(NextInst,NextElement),
	%not(ei(NextInst,tap)),
	not(connection(_,controlsignalport,NextInst,tapconditionport,_)),
	connection(NextInst,PortOut,SourceInst,PortIn,_),
	output(SourceInst,PortIn,AttrList,_,_,_),
	filterduplex(AttrList,NextInst,PortOut,FilteredAttr),
	sort(FilteredAttr,NoDupAttr), % remove duplicates
	member(Attr,NoDupAttr),
	not(member(NextInst,H)),
	not(member(NextInst,T)),
	canskiptimetap(NextElement,Attr,Sink,[NextInst,H|T]).

% indirect source-sink connection (duplex connections) trusted
canskiptimetap(Source,Attr,Sink,[H|T]) :-
	ei(SourceInst,Source),
	ei(NextInst,NextElement),
	%not(ei(NextInst,tap)),
	not(connection(_,controlsignalport,NextInst,tapconditionport,_)),
	connection(NextInst,PortOut,SourceInst,PortIn,_),
	output(SourceInst,PortIn,AttrList,_,_,_),
	filterduplex(AttrList,NextInst,PortOut,FilteredAttr),
	sort(FilteredAttr,NoDupAttr), % remove duplicates
	member(Attr,NoDupAttr),
	not(member(NextInst,H)),
	not(member(NextInst,T)),
	canskiptimetap(NextElement,Attr,Sink,[NextInst,H|T]).

% helper method for canskiptimetap predicate

checkifflowswithouttimetap(Source,Attr,Sink) :-
	ei(SourceInst,Source),
	canskiptimetap(Source,Attr,Sink,[SourceInst]).