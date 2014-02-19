/***********************************************************************/
/*                              O'Browser                              */
/*                                                                     */
/*  Copyright 2008 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: children
// Type:      t -> t list
function caml_js_node_children (n) {
  var node = n;
  try {
    var res = UNIT;
    var cur = UNIT;
    var children = node.childNodes;
    for (c = 0;c < children.length;c++) {
      if (res == UNIT) {
	res = mk_pair (children[c],UNIT);
	cur = res;
      } else {
	cur.set(1, mk_pair (children[c],UNIT));
	cur = field (cur, 1);
      }
    }
	return res;
  } catch (e) {
    caml_failwith ("caml_js_node_children: " + e.message);
    }
}
// Caml name: n_children
// Type:      t -> int
function caml_js_node_n_children (n) {
  var node = n;
  try {
    return node.childNodes.length;
  } catch (e) {
    caml_failwith ("caml_js_node_n_children: " + e.message);
  }
}
 
// Caml name: child
// Type:      t -> int -> t
function caml_js_node_child (n, i) {
  var node = n;
  try {
    return node.childNodes[i];
  } catch (e) {
    caml_failwith ("caml_js_node_n_children: " + e.message);
  }
}


// Caml name: create
// Type:      unit -> t
function caml_js_fragment_create (v) {
  return document.createDocumentFragment ();
}

// Caml name: append
// Type:      t -> Node.t -> unit
function caml_js_fragment_append (f, n) {
  var fragment = f;
  var node = n;
  try {
    fragment.appendChild (node);
  } catch (e) {
    caml_failwith ("caml_js_fragment_append: " + e.message);
  }
  return UNIT;
}

// Caml name: flush
// Type:      Node.t -> t -> unit
function caml_js_fragment_flush (n,f) {
  var fragment = f;
  var node = n;
  try {
    node.appendChild (fragment);
  } catch (e) {
    caml_failwith ("caml_js_fragment_flush: " + e.message);
  }
  return UNIT;
}

// Caml name: alert
// Type:      string -> unit
function caml_js_alert (msg) {
  window.alert (string_val (msg));
  return UNIT;
}

// Caml name: params
// Type:      unit -> string array
function caml_js_params (v) {
  return og$argv;
}

// Caml name: http_get_with_status
// Type:      string -> (int *  string)
function caml_js_http_get_with_status (vurl) {
  var url = string_val (vurl);
  
  var xmlhttp = false;
  /* get request object */
  if (window.ActiveXObject) {
    try {
      xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
    } catch (e) {
      try {
	xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
      } catch (E) {
	o$fatal ("caml_js_http_get_with_status: unsupported Internet Explorer");
      }
    }
  } else {
    xmlhttp = new XMLHttpRequest();
  }
  /* do request */
  try {
    xmlhttp.onreadystatechange = function () {
      notify (xmlhttp);
    }
    xmlhttp.open("GET", url, true);
    xmlhttp.send(null);
    var cont = function  () {
      if (xmlhttp.readyState != 4)
	wait_for (xmlhttp, cont);		
      return mk_block ([xmlhttp.status, val_string (xmlhttp.responseText)], 0);
    }
    wait_for (xmlhttp, cont);
  } catch (e) {
    caml_catch(e);
    caml_failwith("unable to load url " + url + ": " + e);
  }
}

// Caml name: http_post
// Type:      string -> string -> string -> (int *  string)
function caml_js_http_post (vurl, type, data) {
    var url = string_val (vurl);

    var xmlhttp = false;
    /* get request object */
    if (window.ActiveXObject) {
      try {
	xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
      } catch (e) {
	try {
	  xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
	} catch (E) {
	  throw new Error ("Unsupported Internet Explorer");
	}
      }
    } else {
      xmlhttp = new XMLHttpRequest();
    }
  /* do request */
  try {
    xmlhttp.onreadystatechange = function () {
      notify (xmlhttp);
    }
    xmlhttp.open("POST", url, true);
    xmlhttp.setRequestHeader("Content-Type", string_from_value (type));
    xmlhttp.send(string_from_value (data));
    var cont = function  () {
      if (xmlhttp.readyState != 4)
	wait_for (xmlhttp, cont);		
      return mk_block ([xmlhttp.status, val_string (xmlhttp.responseText)], 0);
    }
    wait_for (xmlhttp, cont);
  } catch (e) {
    caml_catch (e);
    caml_failwith ("unable to load url " + url + ": " + e.message);
  }
}

// Caml name: dom_of_xml
// Type:      string -> JSOO.obj
function caml_js_dom_of_xml (str)  {
  var sstr = string_from_value (str);
  try {
    try { //IE
      xmlDoc = new ActiveXObject("Microsoft.XMLDOM");
      xmlDoc.async = "false";
      xmlDoc.loadXML(sstr);
      return xmlDoc; 
    } catch(e) {
      parser = new DOMParser();
      xmlDoc = parser.parseFromString(sstr,"text/xml");
      return xmlDoc;
    }
  } catch(e) { caml_failwith ("unable to parse : " + e.message) }
}

// Caml name: pretty_xml_of_dom
// Type:      JSOO.obj -> string
function caml_js_pretty_xml_of_dom (o) {
  try {
    var serializer = new XMLSerializer();
    var prettyString = XML(serializer.serializeToString(o)).toXMLString();
    return (val_string (prettyString)) ;
  } catch(e) { caml_failwith ("unable to pretty print : " + e.message) }
}

// Caml name: xml_of_dom
// Type:      JSOO.obj -> string
function caml_js_xml_of_dom (o) {
  try {
    var serializer = new XMLSerializer();
    var xml = serializer.serializeToString(o);
    return (val_string (xml)) ;
  } catch (e) { caml_failwith ("unable to print : " + e.message) }
}

// Caml name: basic_io_write
// Type:      string -> unit
function caml_basic_io_write (s) {
  o$message (string_val (s));
  return UNIT;
}
