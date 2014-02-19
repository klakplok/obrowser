function O_vio_console_in (name) {
    this.name = name;
}
O_vio_console_in.prototype.close = function () {
}

function O_vio_console_out (name) {
    this.name = name;
}
O_vio_console_out.prototype.close = function () {
}

var O_vio_protocols_in = {
    console: O_vio_console_in,
//    http: O_vio_http_in
}

var O_vio_protocols_out = {
    console: O_vio_console_out,
//    http: O_vio_http_in
}

function O_vio_open_in (uri) {
    var parts = /([^:]+):\/\/(.*)/.exec (uri);
    var protocol, name;
    if (parts) {
	protocol = O_vio_protocols_in[parts[1]];
	name = parts[2];
    } else {
	protocol = O_vio_protocols_in["http"];
	name = uri;
    }
    return new protocol (name);
}

function O_vio_open_out (uri) {
    var parts = /([^:]+):\/\/(.*)/.exec (uri);
    var protocol, name;
    if (parts) {
	protocol = O_vio_protocols_out[parts[1]];
	name = parts[2];
    } else {
	protocol = O_vio_protocols_out["post"];
	name = uri;
    }
    return new protocol (name);
}


/****************************************************************************/

var O_vio_ops = {
    id: "_vio",
    compare: function (x, y) { return (x == y); },
    hash: function () { O_fatal ("cannot hash a vio channel"); },
    serialize: function () { O_fatal ("cannot serialize a vio channel"); },
    deserialize: function () { O_fatal ("cannot deserialize a vio channel"); }
}

function caml_ml_open_descriptor_in (descr /* : int */)  /* : in_channel */ {
    if (int_val (descr) == 0)
	return mk_custom (O_vio_ops, new O_vio_console_in ("STDIN"));
    else
	O_fatal ("caml_ml_open_descriptor_in unsupported");
}

function caml_ml_open_descriptor_out (descr /* : int */)  /* : out_channel */ {
    if (int_val (descr) == 1)
	return mk_custom (O_vio_ops, new O_vio_console_out ("STDOUT"));
    else if (int_val (descr) == 2)
	return mk_custom (O_vio_ops, new O_vio_console_out ("STDERR"));
    else
	O_fatal ("caml_ml_open_descriptor_out unsupported");
}

