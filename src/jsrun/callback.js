#if false

var MAGIC_CAML_CALLBACK = 987654321
#define MAGIC_CAML_CALLBACK 987654321

function O_callback (n) {
    function _p1 (c) {
	c.a = c.s[c.sp + n + 3];
	O_apply (3, _p2);
	return true;
    }
    function _p2 (c) {
	c.sp++;
	c.pc = null;
	return false;
    }
    return _p1;
}

function caml_callback (clos, args) {
    var n = args.length;
    var c = {
	pc : O_callback (n),
	sp : STACK_SIZE - narg - 4,
	xsp : -1,
	a : UNIT,
	s : [],
 	env : mk_block (0, 0),
	xa : 0,
	status : RUN,
	pid : 0
    } ;

    c.sp -= 4;
    c.s[c.sp] = code.shift (4);
    c.s[c.sp + 1] = UNIT;
    c.s[c.sp + 2] = 0;
    c.s[c.sp + 3] = clos;

    c.sp -= narg;
    for (i = 0; i < narg; i++)
	c.stack[c.sp + i] = args[i];
    
    code.set (0, IACC);
    code.set (1, narg + 3);
    code.set (2, IAPPLY);
    code.set (3, narg);
    code.set (4, IPOP);
    code.set (5, 1);
    code.set (6, ISTOP);

    running_vm = this;
    try {
	while (c.cur_code.get (c.pc) != ISTOP) {
	    if (! i_tbl_cb [c.cur_code.get (c.pc++)] (this, c)) {
		this.c = oc;
		this.failwith ("blocking functions in callbacks not supported");
	    }
	}
    } catch (e) {
	this.c = oc;
	if (e[0] == MAGIC_CAML_CALLBACK) {
	    this.raise (e[1]);
	} else {
	    throw e;
	}
    }
    running_vm = null;
    var r = c.accu;
    this.c = oc;
    return r;
}




VM.prototype.callback_method = function (obj, name, oargs) {
    var lab = plabel_jsstr (name);
    /* resolve method */
    var meths = obj.get (0);
    var li = 3;
    var hi = meths.get (0) * 2 + 1;
    while (li < hi) {
	var mi = ((li + hi) >> 1) | 1;
	if (lab < meths.get (mi))
	    hi = mi - 2;
	else
	li = mi;
    } 
    var clos = meths.get (li - 1);
    /* add obj to args */
    var args = [obj];
    for (i = 0;i < oargs.length;i++) {
	args[i + 1] = oargs[i];
    }
    return this.callback(clos, args);
}

#endif