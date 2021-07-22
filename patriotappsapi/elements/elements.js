exports.motionsensor = {
    "deviceactions": [],
    "attributes": ["motion_detected"],
    "attrtype" :
        "attrtype(motion_detected,boolean).",
    "portdatatype" :
        "portdatatype([],[boolean],motiondetectedport,motionsensor).",
    "stopport" : "",
    "output" :
        "output(X,motiondetectedport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,motionsensor),\n" +
        "\tAttr = [motion_detected],\n" +
        "\tValue=[true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,motiondetectedport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,motionsensor),\n" +
        "\tAttr = [motion_detected],\n" +
        "\tValue=[false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" : ""
};

exports.contactsensor = {
    "deviceactions": [],
    "attributes": ["contact_open","contact_closed"],
    "attrtype" :
        "attrtype(contact_open,boolean).\n" +
        "attrtype(contact_closed,boolean).",
    "portdatatype" :
        "portdatatype([],[boolean],contactclosedport,contactsensor).\n" +
        "portdatatype([],[boolean],contactopenport,contactsensor).",
    "stopport" : "",
    "output" :
        "output(X,contactopenport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tAttr = [contact_open],\n" +
        "\tValue=[true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,contactopenport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tAttr = [contact_open],\n" +
        "\tValue=[false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,contactclosedport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tAttr = [contact_closed],\n" +
        "\tValue=[true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,contactclosedport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tAttr = [contact_closed],\n" +
        "\tValue=[false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" : ""
};

exports.wakeworddetection = {
    "deviceactions": [],
    "attributes": ["voice_command"],
    "attrtype" :
        "attrtype(voice_command,audio).",
    "portdatatype" :
        "portdatatype([],[audio],audioport,wakeworddetection).",
    "stopport" : "",
    "output" :
        "output(X,audioport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,wakeworddetection),\n" +
        "\tAttr = [voice_command],\n" +
        "\tValue = [audiorecording],\n" +
        "\tTrans = [],\n" +
        "\tTime = 0.",
    "helpers" : ""
};

exports.smartlight = {
    "deviceactions": ["turnofflightport","turnonlightport"],
    "attributes": ["light_state"],
    "attrtype" :
        "attrtype(light_state,boolean).",
    "portdatatype" :
        "portdatatype([],[],turnonlightport,smartlight).\n" +
        "portdatatype([],[],turnofflightport,smartlight).\n" +
        "portdatatype([],[boolean],getlightstateport,smartlight).",
    "stopport" :
        "stopport(turnonlightport,smartlight).\n" +
        "stopport(turnofflightport,smartlight).",
    "output" :
        "output(X,getlightstateport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,smartlight),\n" +
        "\tAttr = [light_state],\n" +
        "\tValue = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime = 0.\n" +
        "\n" +
        "output(X,getlightstateport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,smartlight),\n" +
        "\tAttr = [light_state],\n" +
        "\tValue = [false],\n" +
        "\tTrans = [],\n" +
        "\tTime = 0.",
    "helpers" : ""
};

exports.or = {
    "deviceactions": [],
    "attributes": [],
    "attrtype" : "",
    "portdatatype" :
        "portdatatype([boolean],[],orin1port,or).\n" +
        "portdatatype([boolean],[],orin2port,or).\n" +
        "portdatatype([],[boolean],oroutport,or).",
    "stopport" : "",
    "output" :
        "output(X,oroutport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,or),\n" +
        "\tinput(X,orin1port,Attr,ValueIn,Trans,TimeIn),\n" +
        "\tmember(true,ValueIn),\n" +
        "\tValue = [true],\n" +
        "\tTime is TimeIn.\n" +
        "\n" +
        "output(X,oroutport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,or),\n" +
        "\tinput(X,orin2port,Attr,ValueIn,Trans,TimeIn),\n" +
        "\tmember(true,ValueIn),\n" +
        "\tValue = [true],\n" +
        "\tTime is TimeIn.\n" +
        "\n" +
        "%output(X,oroutport,Attr,Value,Trans,Time) :-\n" +
        "%\tei(X,or),\n" +
        "%\tinput(X,orin1port,Attr1,ValueIn1,Trans1,TimeIn1),\n" +
        "%\tmember(false,ValueIn1),\n" +
        "%\tinput(X,orin2port,Attr2,ValueIn2,Trans2,TimeIn2),\n" +
        "%\tmember(false,ValueIn2),\n" +
        "%\tValue = [false],\n" +
        "%\tAttr = [Attr1,Attr2],\n" +
        "%\tTrans = [Trans1,Trans2],\n" +
        "%\tTime is max(TimeIn1,TimeIn2).",
    "helpers" : ""
};

exports.pushnotifier = {
    "deviceactions": [],
    "attributes": ["dismiss_received"],
    "attrtype" :
        "attrtype(dismiss_received,boolean).",
    "portdatatype" :
        "portdatatype([text,image],[],sendpushwithimageport,pushnotifier).\n" +
        "portdatatype([text],[],sendpushmessageport,pushnotifier).\n" +
        "portdatatype([],[boolean],dismissreceivedport,pushnotifier).",
    "stopport" :
        "stopport(sendpushwithimageport,pushnotifier).\n" +
        "stopport(sendpushmessageport,pushnotifier).",
    "output" :
        "output(X,dismissreceivedport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,pushnotifier),\n" +
        "\tAttr = [dismiss_received],\n" +
        "\tValue = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" : ""
};

exports.ipcamera = {
    "deviceactions": [],
    "attributes": ["camera_frame"],
    "attrtype" :
        "attrtype(camera_frame,image).",
    "portdatatype" :
        "portdatatype([],[image],getframeport,ipcamera).\n" +
        "portdatatype([],[image],cameraframeport,ipcamera).",
    "stopport" : "",
    "output" :
        "output(X,cameraframeport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,ipcamera),\n" +
        "\tAttr = [camera_frame],\n" +
        "\tValue = [cameraframe],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,getframeport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,ipcamera),\n" +
        "\tAttr = [camera_frame],\n" +
        "\tValue = [cameraframe],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" : ""
};

exports.timer = {
    "deviceactions": [],
    "attributes": ["timer_expired"],
    "attrtype" :
        "attrtype(timer_expired,boolean).",
    "portdatatype" :
        "portdatatype([boolean],[],starttimerport,timer).\n" +
        "portdatatype([boolean],[],starttimeronceport,timer).\n" +
        "portdatatype([boolean],[],stoptimerport,timer).\n" +
        "portdatatype([],[boolean],timerexpiredport,timer).",
    "stopport" :
        "stopport(stoptimerport,timer).",
    "output" :
        "output(X,timerexpiredport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tconfig(Timeout,time,timer),\n" +
        "\tinput(X,settimerport,_,_,Trans,TimeIn),\n" +
        "\tAttr = [timer_expired],\n" +
        "\tValue = [true],\n" +
        "\tTime is TimeIn + Timeout.\n" +
        "\n" +
        "output(X,timerexpiredport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tconfig(Timeout,time,timer),\n" +
        "\tinput(X,starttimerport,_,_,Trans,TimeIn),\n" +
        "\tAttr = [timer_expired],\n" +
        "\tValue = [true],\n" +
        "\tTime is TimeIn + Timeout.\n" +
        "\n" +
        "output(X,timerexpiredport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tconfig(Timeout,time,timer),\n" +
        "\tinput(X,starttimeronceport,_,_,Trans,TimeIn),\n" +
        "\tAttr = [timer_expired],\n" +
        "\tValue = [true],\n" +
        "\tTime is TimeIn + Timeout.",
    "helpers" : ""
};

exports.tap = {
    "deviceactions": [],
    "attributes": ["tap_state"],
    "attrtype" :
        "attrtype(tap_state,boolean).",
    "portdatatype" :
        "portdatatype([boolean],[],tapconditionport,tap).\n" +
        "portdatatype(any,any,tapinport,tap). % can be duplex\n" +
        "portdatatype(any,any,tapoutport,tap).% can be duplex",
    "stopport" :
        "stopport(tapconditionport,tap).",
    "output" :
        "output(X,tapoutport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tinput(X,tapconditionport,_,ValueCond,_,TimeInCond),\n" +
        "\tmember(true,ValueCond),\n" +
        "\tinput(X,tapinport,Attr,Value,Trans,TimeIn),\n" +
        "\tTime is max(TimeIn,TimeInCond).\n" +
        "\n" +
        "output(X,tapinport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tinput(X,tapconditionport,_,ValueCond,_,_),\n" +
        "\tmember(true,ValueCond),\n" +
        "\tinput(X,tapoutport,Attr,Value,Trans,Time).\n" +
        "\n" +
        "output(X,gettapstateport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tAttr = [tap_state],\n" +
        "\tValue = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,gettapstateport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tAttr = [tap_state],\n" +
        "\tValue = [false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" : ""
};

exports.not = {
    "deviceactions": [],
    "attributes": [],
    "attrtype" : "",
    "portdatatype" :
        "portdatatype([boolean],[],notinport,not).\n" +
        "portdatatype([],[boolean],notoutport,not).",
    "stopport" : "",
    "output" :
        "output(X,notoutport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,not),\n" +
        "\tinput(X,notinport,Attr,ValueIn,Trans,TimeIn),\n" +
        "\tTime is TimeIn,\n" +
        "\t% A -> X ; Y means if A is true then X otherwise Y\n" +
        "\t(member(true,ValueIn) -> Value = [false] ; Value = [true]).",
    "helpers" : ""
};

exports.httprequest = {
    "deviceactions": [],
    "attributes": ["data_from_internet"],
    "attrtype" :
        "attrtype(data_from_internet,externaldata).",
    "portdatatype" :
        "portdatatype(any,any,httppostport,httprequest).\n" +
        "portdatatype(any,any,httpgetport,httprequest).\n" +
        "portdatatype(any,any,httpputport,httprequest).\n" +
        "portdatatype(any,any,httpdeleteport,httprequest).",
    "stopport" : "",
    "output" :
        "output(X,httpgetport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,httprequest),\n" +
        "\tAttr = [data_from_internet],\n" +
        "\tValue = [],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,httppostport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,httprequest),\n" +
        "\tAttr = [data_from_internet],\n" +
        "\tValue = [],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,httpputport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,httprequest),\n" +
        "\tAttr = [data_from_internet],\n" +
        "\tValue = [],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,httpdeleteport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,httprequest),\n" +
        "\tAttr = [data_from_internet],\n" +
        "\tValue = [],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" : ""
};

exports.aesencryption = {
    "deviceactions": [],
    "attributes": ["encrypted_data"],
    "attrtype" :
        "attrtype(encrypted_data,encrypteddata).",
    "portdatatype" :
        "portdatatype(any,[],encryptdatainport,aesencryption).\n" +
        "portdatatype([],[encrypteddata],encryptdataoutport,aesencryption).",
    "stopport" : "",
    "output" :
        "output(X,encryptdataoutport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,aesencryption),\n" +
        "\tinput(X,encryptdatainport,AttrIn,_,TransIn,Time),\n" +
        "\taddencryptedprefix(AttrIn,TransOut),\n" +
        "\tAttr = [encrypted_data],\n" +
        "\tValue = [],\n" +
        "\t%append(TransIn, [t(H, encrypted, X)], Trans),  % nuno's ver.\n" +
        "\tappend(TransIn, TransOut, Trans).",
    "helpers" :
        "% Helper rules for the aesencryption element\n" +
        "\n" +
        "addencryptedprefix([],[]).\n" +
        "\n" +
        "addencryptedprefix([H|T],[NewAttr|Rest]) :-\n" +
        "\tatom_concat('encrypted_',H,NewAttr),\n" +
        "\taddencryptedprefix(T,Rest)."
};

exports.timecontroller = {
    "deviceactions": [],
    "attributes": ["time_control_signal"],
    "attrtype" :
        "attrtype(time_control_signal,boolean).",
    "portdatatype" :
        "portdatatype([],[boolean],controlsignalport,timecontroller).",
    "stopport" :
        "stopport(controlsignalport,timecontroller).",
    "output" :
        "% true signal is only sent when the time is right\n" +
        "output(X,controlsignalport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,timecontroller),\n" +
        "\tconfig(StartH,starthour,timecontroller),\n" +
        "\tconfig(StartM,startminute,timecontroller),\n" +
        "\tconfig(EndH,endhour,timecontroller),\n" +
        "\tconfig(EndM,endminute,timecontroller),\n" +
        "\ttimeok(StartH,StartM,EndH,EndM),\n" +
        "\tAttr = [time_control_signal],\n" +
        "\tValue = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "% false signal is sent otherwise\n" +
        "output(X,controlsignalport,Attr,Value,Trans,Time) :-\n" +
        "\tei(X,timecontroller),\n" +
        "\tconfig(StartH,starthour,timecontroller),\n" +
        "\tconfig(StartM,startminute,timecontroller),\n" +
        "\tconfig(EndH,endhour,timecontroller),\n" +
        "\tconfig(EndM,endminute,timecontroller),\n" +
        "\tnot(timeok(StartH,StartM,EndH,EndM)),\n" +
        "\tAttr = [time_control_signal],\n" +
        "\tValue = [false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.",
    "helpers" :
        "% Helper rules for the timecontroller element\n" +
        "currenttime(Year,Month,Day,Hour,Minute,Second,Off,TZ,DST) :-\n" +
        "\tget_time(Stamp),\n" +
        "\tstamp_date_time(Stamp, DateTime, local),\n" +
        "\tdate_time_value(year, DateTime, Year),\n" +
        "\tdate_time_value(month, DateTime, Month),\n" +
        "\tdate_time_value(day, DateTime, Day),\n" +
        "\tdate_time_value(hour, DateTime, Hour),\n" +
        "\tdate_time_value(minute, DateTime, Minute),\n" +
        "\tdate_time_value(second, DateTime, Second),\n" +
        "\tdate_time_value(utc_offset, DateTime, Off),\n" +
        "\tdate_time_value(time_zone, DateTime, TZ),\n" +
        "\tdate_time_value(daylight_saving, DateTime, DST).\n" +
        "\n" +
        "createstamp(Hour,Minute,Stamp) :-\n" +
        "\tcurrenttime(Year,Month,Day,_,_,_,Off,TZ,DST),\n" +
        "\tdate_time_stamp(date(Year,Month,Day,Hour,Minute,0,Off,TZ,DST), Stamp).\n" +
        "\n" +
        "isWithinRange(Start,End) :-\n" +
        "\t%get_time(Stamp),\t% system function\n" +
        "\tgettime(Stamp),\t\t% custom function\n" +
        "\tStamp < End,\n" +
        "\tStamp > Start.\n" +
        "\n" +
        ":- dynamic gettime/1.\n" +
        "\n" +
        "gettime(X) :-\n" +
        "\tget_time(X).\n" +
        "\n" +
        "% start time comes before end time\n" +
        "timeok(StartH,StartM,EndH,EndM) :-\n" +
        "\tcreatestamp(StartH,StartM,StampFrom),\n" +
        "\tcreatestamp(EndH,EndM,StampTill),\n" +
        "\tStampFrom < StampTill,\n" +
        "\tisWithinRange(StampFrom,StampTill).\n" +
        "\n" +
        "% start time comes after end time\n" +
        "timeok(StartH,StartM,EndH,EndM) :-\n" +
        "\tcreatestamp(StartH,StartM,StampFrom),\n" +
        "\tcreatestamp(EndH,EndM,StampTill),\n" +
        "\tStampFrom > StampTill,\n" +
        "\tisWithinRange(StampTill,StampFrom)."
};

exports.alarm = {
    "deviceactions": ["triggeralarmport"],
    "attributes": [],
    "attrtype" : "",
    "portdatatype" :
        "portdatatype([],[],triggeralarmport,alarm).",
    "stopport" :
        "stopport(triggeralarmport,alarm).",
    "output" : "",
    "helpers" : ""
};

exports.untrusted = {
    "deviceactions": [],
    "attributes": [],
    "attrtype" : "",
    "portdatatype" :
        "portdatatype(any,any,_,untrusted).",
    "stopport" : "",
    "output" :
        "output(X,_,Attr,Value,Trans,Time):-\n" +
        "\tei(X,untrusted),\n" +
        "\tuntrusted(X),\n" +
        "\tallattr(X,Attr),\n" +
        "\tallvalues(X,Value),\n" +
        "\talltransforms(X,Trans),\n" +
        "\talltimes(X,Time).",
    "helpers" :
        "% Helper rules for the untrusted element\n" +
        "allattr(ElInstance, Result) :-\n" +
        "\tfindall(Attr, input(ElInstance,_,Attr,_,_,_),L),\n" +
        "\tappend(L, W),\n" +
        "\tsort(W,Result).\n" +
        "\n" +
        "allvalues(ElInstance, Result) :-\n" +
        "\tfindall(Value, input(ElInstance,_,_,Value,_,_),L),\n" +
        "\tappend(L, W),\n" +
        "\tsort(W,Result).\n" +
        "\n" +
        "alltransforms(ElInstance, Result) :-\n" +
        "\tfindall(Trans, input(ElInstance,_,_,_,Trans,_),L),\n" +
        "\tappend(L, W),\n" +
        "\tsort(W,Result).\n" +
        "\n" +
        "alltimes(ElInstance, Final) :-\n" +
        "\tfindall(Time, input(ElInstance,_,_,_,_,Time),Times),\n" +
        "\tfindmax(Times,Final).\n" +
        "\n" +
        "\n" +
        "accMax([H|T],Acc,Max) :-\n" +
        "\tH > Acc,\n" +
        "\taccMax(T,H,Max).\n" +
        "\n" +
        "accMax([H|T],Acc,Max) :-\n" +
        "\tH =< Acc,\n" +
        "\taccMax(T,Acc,Max).\n" +
        "\n" +
        "accMax([],Acc,Acc).\n" +
        "\n" +
        "findmax(List,Max) :-\n" +
        "\tList = [H|_],\n" +
        "\taccMax(List,H,Max)."
};

