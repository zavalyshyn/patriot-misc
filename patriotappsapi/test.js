// const allelements = require('./elements/elements');


let time = '12:00 AM';

// let starthour = starttime.split(':')[0];
// let startmin = starttime.split(':')[1];
//
// console.log(starthour,"---",startmin);
//
//
// console.log(allelements.motionsensor.attrtype);

let hours = Number(time.match(/^(\d+)/)[1]);
let minutes = Number(time.match(/:(\d+)/)[1]);
let AMPM = time.match(/\s(.*)$/)[1];
if(AMPM === "PM" && hours<12) hours = hours+12;
if(AMPM === "AM" && hours===12) hours = hours-12;
let sHours = hours.toString();
let sMinutes = minutes.toString();
if(hours<10) sHours = "0" + sHours;
if(minutes<10) sMinutes = "0" + sMinutes;

console.log(sHours, sMinutes);