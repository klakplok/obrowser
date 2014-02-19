/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*  implementation of a UNIX-like terminal in JavaScript               */
/*                                                                     */
/*  Copyright 2010 Benjamin Canou.                                     */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, version 2 or newer.                        */
/***********************************************************************/

var all_consoles = []
var nb_consoles = 0
var opened_console = -1
var consoleListDiv = null

function Console (title) {
  all_consoles [nb_consoles] = this
  this.id = nb_consoles++
  this.title = title
  this.shown = false
  this.lines = []
  this.diaplay = []
  this.curLine = 0
  this.curCol = 0
  if (!consoleListDiv) {
    consoleListDiv = document.createElement ("div") ;
    consoleListDiv.setAttribute ("style",
      "position: fixed; bottom: 1ex ; left: 1ex; padding: 0px ; height: 4ex ; font-family: Monospace")
    document.body.appendChild (consoleListDiv)
  }
  consoleListDiv.innerHTML += "<button onclick='all_consoles[" + this.id + "].showHide ()'>" + this.title + "</button>&nbsp;"
  this.consoleDiv = document.createElement ("div")
  for (var i = 0;i < 25;i++) {
    var line = document.createElement ("div")
    this.display [i] = []
    for (var j = 0;j < 80;i++) {
      this.display [i][j] = document.createElement ("span")
      this.display [i][j].innerHTML = "X"
      line.appendChild (this.display [i][j])      
    }
    this.consoleDiv.appendChild (line)
  }
  document.body.appendChild (this.consoleDiv)
  this.updateStyle ()
}

Console.prototype.showHide = function () {
  if (this.shown)
    this.hide ()
  else
    this.show ()
}

Console.prototype.show = function () {
  if (opened_console >= 0)
    all_consoles [opened_console].hide ()
  this.shown = true
  opened_console = this.id
  this.updateStyle ()
}

Console.prototype.hide = function () {
  this.shown = false
  this.updateStyle ()
  opened_console = -1
}

Console.prototype.updateStyle = function () {
  if (this.shown) {
    this.consoleDiv.setAttribute ("style",
      "background-color: black ; position: fixed ; bottom: 5ex ; left: 1ex ; border-radius: 3px ; \
       border: 1px white solid ; color: white ; padding: 4px ; font-family: Monospace ;\
       cursor: pointer ; visibility: visible ; cursor: text ; width: 80ex ; height: 25ex ;\
       overflow: hidden")
  } else {
    this.consoleDiv.setAttribute ("style", "visibility: hidden")
  }
}

Console.prototype.updateDisplay = function () {
  var line = this.curLine < 24 ? this.curLine : 24
  var col = this.curCol
}

Console.prototype.output = function (a) {
  switch (typeof (a)) {
  case "number": 
    switch (a) {
    case 32:
      this.consoleDiv.innerHTML += "&nbsp;"
      break
    case 10:
    case 13:
      this.consoleDiv.innerHTML += "<br />"
      break
    default:
      this.consoleDiv.innerHTML += String.fromCharCode (a)
    }
    break;
  case "string":
    for (var i = 0;i < a.length;i++)
      this.output (a.charCodeAt (i))
    break
  }
  this.updateDisplay ()
}
