/***************** -*- js-indent-level: 2; indent-tabs-mode: nil ; -*- */
/*                                                                     */
/*                        O'Browser II le retour                       */
/*                                                                     */
/*  Copyright 2011 Benjamin Canou. This file is distributed under the  */
/*  terms of the GNU Library General Public License described in file  */
/*  ../LICENSE.                                                        */
/*                                                                     */
/***********************************************************************/

// Caml name: gettimeofday
// Type:      unit -> float
function unix_gettimeofday () {
  return val_float ((new Date ()).getTime () * 0.001);
}

caml_primitive_stub ("unix_inet_addr_of_string", val_string (""));

/* THAT'S ALL FOLKS ! **************************************************/
