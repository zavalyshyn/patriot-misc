exports.or = {
    inports: [
        {
            name: "orin1port",
            type: "internal boolean"
        },
        {
            name: "orin2port",
            type: "internal boolean"
        }
    ],
    outports: [
        {
            name: "oroutport",
            type: "internal boolean"
        }
    ],
    inportsfacts:
        "inport(orin1port,or).\n" +
        "inport(orin2port,or)."
    ,
    outportsfacts:
      "outport(oroutport,or)."
    ,
    outputrules:
        "output(X,oroutport,Data,Trans,Time) :-\n" +
        "\tei(X,or),\n" +
        "\tinput(X,orin1port,DataIn1,Trans,TimeIn1),\n" +
        "\tmember(true,DataIn1),\n" +
        "\tData = [true],\n" +
        "\tTime is TimeIn1.\n" +
        "\n" +
        "output(X,oroutport,Data,Trans,Time) :-\n" +
        "\tei(X,or),\n" +
        "\tinput(X,orin2port,DataIn2,Trans,TimeIn2),\n" +
        "\tmember(true,DataIn2),\n" +
        "\tData = [true],\n" +
        "\tTime is TimeIn2.\n" +
        "\n" +
        "output(X,oroutport,Data,Trans,Time) :-\n" +
        "\tei(X,or),\n" +
        "\tinput(X,orin1port,DataIn1,Trans1,TimeIn1),\n" +
        "\tmember(false,DataIn1),\n" +
        "\tinput(X,orin2port,DataIn2,Trans2,TimeIn2),\n" +
        "\tmember(false,DataIn2),\n" +
        "\tData = [false],\n" +
        "\tappend(Trans1,Trans2,Trans),\n" +
        "\tTime is max(TimeIn1,TimeIn2)."
};

exports.contactsensor = {
    inports: [],
    outports: [
        {
            name: "contactopenport",
            type: "sensor state"
        },
        {
            name: "contactclosedport",
            type: "sensor state"
        }
    ],
    inportsfacts: "",
    outportsfacts:
        "outport(contactopenport,contactsensor).\n" +
        "outport(contactclosedport,contactsensor).",
    outputrules:
        "output(X,contactopenport,Data,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tData=[true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,contactopenport,Data,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tData=[false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,contactclosedport,Data,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tData=[true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,contactclosedport,Data,Trans,Time) :-\n" +
        "\tei(X,contactsensor),\n" +
        "\tData=[false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.motionsensor = {
    inports: [],
    outports: [
        {
            name: "motiondetectedport",
            type: "sensor state"
        }
    ],
    inportsfacts: "",
    outportsfacts:
        "outport(motiondetectedport,motionsensor).",
    outputrules:
        "output(X,motiondetectedport,Data,Trans,Time) :-\n" +
        "\tei(X,motionsensor),\n" +
        "\tData=[true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,motiondetectedport,Data,Trans,Time) :-\n" +
        "\tei(X,motionsensor),\n" +
        "\tData=[false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.not = {
    inports: [
        {
            name: "notinport",
            type: "internal boolean"
        }
    ],
    outports: [
        {
            name: "notoutport",
            type: "internal boolean"
        }
    ],
    inportsfacts:
        "inport(notinport,not).",
    outportsfacts:
        "outport(notoutport,not).",
    outputrules:
        "output(X,notoutport,Data,Trans,Time) :-\n" +
        "\tei(X,not),\n" +
        "\tinput(X,notinport,DataIn,Trans,TimeIn),\n" +
        "\tTime is TimeIn,\n" +
        "\t% A -> X ; Y means if A is true then X otherwise Y\n" +
        "\t(member(true,DataIn) -> Data = [false] ; Data = [true])."
};

exports.tap = {
    inports: [
        {
            name: "tapinport",
            type: "internal any"
        },
        {
            name: "tapconditionport",
            type: "internal boolean"
        }
    ],
    outports: [
        {
            name: "tapoutports",
            type: "internal any"
        },
        {
            name: "gettapstateport",
            type: "internal boolean"
        }
    ],
    inportsfacts:
        "inport(tapinport,tap).\n" +
        "inport(tapconditionport,tap).",
    outportsfacts:
        "outport(tapoutport,tap).\n" +
        "outport(gettapstateport,tap).",
    outputrules:
        "output(X, tapoutport, Data, Trans, Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tinput(X,tapconditionport,DataCond,_,TimeInCond),\n" +
        "\tmember(true,DataCond),\n" +
        "\tinput(X,tapinport,Data,Trans,TimeIn),\n" +
        "\tTime is max(TimeIn,TimeInCond)." +
        "\n" +
        "output(X,gettapstateport,Data,Trans,Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tData = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0." +
        "\n" +
        "output(X,gettapstateport,Data,Trans,Time) :-\n" +
        "\tei(X,tap),\n" +
        "\tData = [false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.ipcamera = {
    inports: [],
    outports: [
        {
            name: "getframeport",
            type: "device data: frame"
        },
        {
            name: "cameraframeport",
            type: "device data: frame"
        },
    ],
    inportsfacts: "",
    outportsfacts:
        "outport(cameraframeport,ipcamera).\n" +
        "outport(getframeport,ipcamera).",
    outputrules:
        "output(X,cameraframeport,Data,Trans,Time) :-\n" +
        "\tei(X,ipcamera),\n" +
        "\tData = [cameraframe],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,getframeport,Data,Trans, Time) :-\n" +
        "\tei(X,ipcamera),\n" +
        "\tData = [cameraframe],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.pushnotifier = {
    inports: [
        {
            name: "sendpushmessageport",
            type: "data: text"
        },
        {
            name: "sendpushwithimageport",
            type: "data: text, image"
        }
    ],
    outports: [
        {
            name: "dismissreceivedport",
            type: "internal boolean"
        }
    ],
    inportsfacts:
        "inport(sendpushmessageport,pushnotifier).\n" +
        "inport(sendpushwithimageport,pushnotifier).",
    outportsfacts:
        "outport(dismissreceivedport,pushnotifier).",
    outputrules:
        "output(X,dismissreceivedport,Data,Trans,Time) :-\n" +
        "\tei(X,pushnotifier),\n" +
        "\tData = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,dismissreceivedport,Data,Trans,Time) :-\n" +
        "\tei(X,pushnotifier),\n" +
        "\tData = [false],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.wakeworddetection = {
    inports: [],
    outports: [
        {
            name: "audioport",
            type: "data: voice command"
        }
    ],
    inportsfacts: "",
    outportsfacts:
        "outport(audioport,wakeworddetection).",
    outputrules:
        "output(X,audioport,Data,Trans,Time) :-\n" +
        "\tei(X,wakeworddetection),\n" +
        "\tData = [voice command],\n" +
        "\tTrans = []," +
        "\tTime is 0."
};

exports.httprequest = {
    inports: [
        {
            name: "httpdeleteport",
            type: "data: any"
        },
        {
            name: "httppostport",
            type: "data: any"
        },
        {
            name: "httpputport",
            type: "data: any"
        }
    ],
    outports: [
        {
            name: "httpgetport",
            type: "data: any external data"
        }
    ],
    inportsfacts:
        "inport(httpdeleteport,httprequest).\n" +
        "inport(httppostport,httprequest).\n" +
        "inport(httpputport,httprequest).",
    outportsfacts:
        "outport(httpgetport,httprequest).",
    outputrules:
        "output(X,httpgetport,Data,Trans,Time) :-\n" +
        "\tei(X,httprequest),\n" +
        "\tData = [externaldata],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.untrusted = {
    inports: [],
    outports: [],
    inportsfacts: "",
    outportsfacts: "",
    outputrules:
        "output(X,_,Data,Trans,Time):-\n" +
        "\tei(X,_),\n" +
        "\tuntrusted(X),\n" +
        "\tallinputs(X,Data),\n" +
        "\talltransforms(X,Trans),\n" +
        "\talltimes(X,Time)."
};

exports.smartlight = {
    inports: [
        {
            name: "turnonlightport",
            type: "device action: turn on"
        },
        {
            name: "turnofflightport",
            type: "device action: turn off"
        }
    ],
    outports: [
        {
            name: "getlightstateport",
            type: "device state: on/off"
        }
    ],
    inportsfacts:
        "inport(turnonlightport,smartlight).\n" +
        "inport(turnofflightport,smartlight).",
    outportsfacts:
        "outport(getlightstateport,smartlight).",
    outputrules:
        "output(X,getlightstateport,Data,Trans,Time) :-\n" +
        "\tei(X,smartlight),\n" +
        "\tData = [light.on],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0.\n" +
        "\n" +
        "output(X,getlightstateport,Data,Trans,Time) :-\n" +
        "\tei(X,smartlight),\n" +
        "\tData = [light.off],\n" +
        "\tTrans = []," +
        "\tTime is 0."
};

exports.timer = {
    inports: [
        {
            name: "settimerport",
            type: "internal: boolean"
        },
        {
            name: "starttimerport",
            type: "internal: boolean"
        },
        {
            name: "starttimeronceport",
            type: "internal: boolean"
        },
        {
            name: "stoptimerport",
            type: "internal: boolean"
        }
    ],
    outports: [
        {
            name: "timerexpiredport",
            type: "internal: boolean"
        }
    ],
    inportsfacts:
        "inport(settimerport,timer).\n" +
        "inport(starttimeronceport,timer).\n" +
        "inport(starttimerport,timer).\n" +
        "inport(stoptimerport,timer).",
    outportsfacts:
        "outport(timerexpiredport,timer).",
    outputrules:
        "output(X,timerexpiredport,Data,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tconfig(Timeout,timerexpiredport,X),\n" +
        "\tinput(X,settimerport,_,Trans,TimeIn),\n" +
        "\tData = [true],\n" +
        "\tTime is TimeIn + Timeout.\n" +
        "\n" +
        "output(X,timerexpiredport,Data,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tconfig(Timeout,timerexpiredport,X),\n" +
        "\tinput(X,starttimerport,_,Trans,TimeIn),\n" +
        "\tData = [true],\n" +
        "\tTime is TimeIn + Timeout.\n" +
        "\n" +
        "output(X,timerexpiredport,Data,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tconfig(Timeout,timerexpiredport,X),\n" +
        "\tinput(X,starttimeronceport,_,Trans,TimeIn),\n" +
        "\tData = [true],\n" +
        "\tTime is TimeIn + Timeout.\n" +
        "\n" +
        "output(X,timerexpiredport,Data,Trans,Time) :-\n" +
        "\tei(X,timer),\n" +
        "\tinput(X,stoptimerport,_,Trans,TimeIn),\n" +
        "\tData = [false],\n" +
        "\tTime is TimeIn."
};

exports.aesencryption = {
    inports: [
        {
            name: "encryptdatainport",
            type: "data: any"
        }
    ],
    outports: [
        {
            name: "encryptdataoutport",
            type: "data: any [encrypted]"
        }
    ],
    inportsfacts:
        "inport(encryptdatainport,aesencryption).",
    outportsfacts:
        "outport(encryptdataoutport,aesencryption).",
    outputrules:
        "output(X,encryptdataoutport,Data,Trans,Time) :-\n" +
        "\tei(X,aesencryption),\n" +
        "\tinput(X,encryptdatainport,Data,TransIn,Time),\n" +
        "\tappend(TransIn, [t(Data, encrypted, X)], Trans)."
};

exports.timecontroller = {
    inports: [],
    outports: [
        {
            name: "controlsignalport",
            type: "internal: boolean"
        }
    ],
    inportsfacts: "",
    outportsfacts:
        "outport(controlsignalport,timecontroller).",
    outputrules:
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
        "\tget_time(Stamp),\n" +
        "\tStamp < End,\n" +
        "\tStamp > Start.\n" +
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
        "\tisWithinRange(StampTill,StampFrom).\n" +
        "\n" +
        "% true signal is only send when the time is right\n" +
        "output(X,controlsignalport,Data,Trans,Time) :-\n" +
        "\tei(X,timecontroller),\n" +
        "\tconfig(StartH,starthour,X),\n" +
        "\tconfig(StartM,startminute,X),\n" +
        "\tconfig(EndH,endhour,X),\n" +
        "\tconfig(EndM,endminute,X),\n" +
        "\ttimeok(StartH,StartM,EndH,EndM),\n" +
        "\tData = [true],\n" +
        "\tTrans = [],\n" +
        "\tTime is 0."
};

exports.alarm = {
    inports: [
        {
            name: "triggeralarmport",
            type: "device action: trigger alarm"
        }
    ],
    outports: [],
    inportsfacts:
        "inport(triggeralarmport,alarm).",
    outportsfacts: "",
    outputrules: ""
};
