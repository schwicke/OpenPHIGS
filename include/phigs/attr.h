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

#ifndef _attr_h
#define _attr_h

#include "phigs.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
   Pline_bundle_plus   line_bundle;
   Pmarker_bundle_plus marker_bundle;
   Ptext_bundle_plus   text_bundle;
   Pedge_bundle_plus   edge_bundle;
   Pint_bundle_plus    int_bundle;
} Pattr_group;

struct _Ws;

/*******************************************************************************
 * phg_attr_group_create
 *
 * DESCR:       Create attribute group
 * RETURNS:     Pointer to attribute group or NULL
 */

Pattr_group* phg_attr_group_create(
   void
   );

/*******************************************************************************
 * phg_attr_group_init
 *
 * DESCR:       Initialize attribute group
 * RETURNS:     TRUE or FALSE
 */

int phg_attr_group_init(
   Pattr_group *attr_group
   );

/*******************************************************************************
 * phg_attr_group_destroy
 *
 * DESCR:       Destroy attribute group
 * RETURNS:     N/A
 */

void phg_attr_group_destroy(
   Pattr_group *attr_group
   );

/*******************************************************************************
 * phg_attr_group_set_line_bundle
 *
 * DESCR:       Set attribute group line bundle
 * RETURNS:     N/A
 */

void phg_attr_group_set_line_bundle(
   struct _Ws *ws,
   Pattr_group *attr_group,
   Pline_bundle_plus *line_bundle
   );

/*******************************************************************************
 * phg_attr_group_set_marker_bundle
 *
 * DESCR:       Set attribute group marker bundle
 * RETURNS:     N/A
 */

void phg_attr_group_set_marker_bundle(
   struct _Ws *ws,
   Pattr_group *attr_group,
   Pmarker_bundle_plus *marker_bundle
   );

/*******************************************************************************
 * phg_attr_group_set_text_bundle
 *
 * DESCR:       Set attribute group text bundle
 * RETURNS:     N/A
 */

void phg_attr_group_set_text_bundle(
   struct _Ws *ws,
   Pattr_group *attr_group,
   Ptext_bundle_plus *text_bundle
   );

/*******************************************************************************
 * phg_attr_group_set_edge_bundle
 *
 * DESCR:       Set attribute group edge bundle
 * RETURNS:     N/A
 */

void phg_attr_group_set_edge_bundle(
   struct _Ws *ws,
   Pattr_group *attr_group,
   Pedge_bundle_plus *edge_bundle
   );

/*******************************************************************************
 * phg_attr_group_set_int_bundle
 *
 * DESCR:       Set attribute group interior bundle
 * RETURNS:     N/A
 */

void phg_attr_group_set_int_bundle(
   struct _Ws *ws,
   Pattr_group *attr_group,
   Pint_bundle_plus *int_bundle
   );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _attr_h */

