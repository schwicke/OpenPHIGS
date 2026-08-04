/******************************************************************************

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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "phg.h"
#include "util.h"

/*******************************************************************************
 * pbuild_tran_matrix3
 *
 * DESCR:       Generate 3D transformation matrix
 * RETURNS:     N/A
 */
void pbuild_tran_matrix3(
                         Ppoint3 *pt,               /* fixed point */
                         Pvec3 *shift,              /* shift vector */
                         Pfloat x_angle,            /* rotation angle X */
                         Pfloat y_angle,            /* rotation angle Y */
                         Pfloat z_angle,            /* rotation angle Z */
                         Pvec3 *scale,              /* scale vector */
                         Pint *error_ind,           /* OUT error indicator */
                         Pmatrix3 matrix            /* OUT transformation matrix */
                         )
{
  ERR_SET_CUR_FUNC(PHG_ERH, Pfn_INQUIRY);

  if (PSL_SYS_STATE(PHG_PSL) != PSYS_ST_PHOP) {
    *error_ind = ERR2;
  } else {
    *error_ind = 0;
    build_transform3(pt, shift, x_angle, y_angle, z_angle, scale, matrix);
  }
}

/*******************************************************************************
 * void build_transform
 *
 * DESCR:       Generate transformation matrix helper function
 * RETURNS:     N/A
 */
static void build_transform(
                            Ppoint *pt,                /* fixed point */
                            Pvec *shift,               /* shift vector */
                            Pfloat ang,                /* rotation angle */
                            Pvec *scl,                 /* scale vector */
                            Pmatrix m                  /* OUT transformation matrix */
                            )
{
  /* Translate pt to the origin, scale, rotate, translate back to pt,
   * shift:
   *      T * Tf~ * R * S * Tf.
   *
   *    where:  T is the "shift" transform,
   *      Tf ia the translation of pt to the origin and
   *      Tf~ is it's inverse,
   *      R is the rotation transform,
   *      S is the scaling transform.
   */
  float *r;
  float c, s;

  c = cos(ang);
  s = sin(ang);

  r = m[0];
  r[0] = c * scl->delta_x;
  r[1] = -s * scl->delta_y;
  r[2] = shift->delta_x + pt->x -
    c * scl->delta_x * pt->x + s * scl->delta_y * pt->y;

  r = m[1];
  r[0] = s * scl->delta_x;
  r[1] = c * scl->delta_y;
  r[2] = shift->delta_y + pt->y -
    (s * scl->delta_x * pt->x + c * scl->delta_y * pt->y);

  r = m[2];
  r[0] = r[1] = 0.0;
  r[2] = 1.0;
}

/*******************************************************************************
 * void build_tran_matrix
 *
 * DESCR:       Generate transformation matrix
 * RETURNS:     N/A
 */
void pbuild_tran_matrix(
                        Ppoint *pt,                /* fixed point */
                        Pvec *shift,               /* shift vector */
                        Pfloat angle,              /* rotation angle */
                        Pvec *scale,               /* scale vector */
                        Pint *error_ind,           /* OUT error indicator */
                        Pmatrix matrix             /* OUT transformation matrix */
                        )
{
  ERR_SET_CUR_FUNC(PHG_ERH, Pfn_INQUIRY);

  if (PSL_SYS_STATE(PHG_PSL) != PSYS_ST_PHOP) {
    *error_ind = ERR2;
  } else {
    *error_ind = 0;
    build_transform(pt, shift, angle, scale, matrix);
  }
}

