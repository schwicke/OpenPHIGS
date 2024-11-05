/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium
Copyright (c) 2014 Surplus Users Ham Society

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef _phg_h
#define _phg_h

#include "phigs.h"
#include "phgtype.h"
#include "err.h"
#include "phgargs.h"
#include "phgretdata.h"
#include "util.h"
#include "errnum.h"
#include "css.h"
#include "attr.h"
#include "ws_inp.h"
#include "ws.h"
#include "ws_type.h"
#include "phigsfunc.h"
#include "psl.h"
#include "phg_dt.h"
#include "sin.h"
#include "private/evtP.h"
#include "ar.h"

#ifdef __cplusplus
extern "C" {
#endif

#define PHG_NUM_EVENTS         35

typedef struct {
   Psl_handle      psl;
   Err_handle      erh;
   Css_handle      css;
   Phg_sin_evt_tbl *evt_table;
   Input_q_handle  input_q;
   List            wst_list;
   Ws_handle       *ws_list;
   Ar_handle       ar_list;
   Phg_scratch     scratch;
} Phg_struct;

typedef Phg_struct *Phg_handle;

extern Phg_handle phg;

#define PHG_ERH (phg->erh)
#define PHG_PSL (phg->psl)
#define PHG_CSS (phg->css)
#define PHG_EVT_TABLE (phg->evt_table)
#define PHG_INPUT_Q (phg->input_q)
#define PHG_WST_LIST (phg->wst_list)
#define PHG_WS_LIST (phg->ws_list)
#define PHG_AR_LIST (phg->ar_list)
#define PHG_WSID(n) (phg->ws_list[(n)])
#define PHG_SCRATCH (phg->scratch)

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _phg_h */

