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

/**
 * \file ptran_point.c
 *
 * \brief       Translate point using transformation matrix
 */
void ptran_point(
                 Ppoint *p,                 /* point */
                 Pmatrix m,                 /* transformation matrix */
                 Pint *error_ind,           /* OUT error indicator */
                 Ppoint *r                  /* OUT transformed point */
                 )
{
  /* TODO: need error code for w = 0. */
  float w;                   /* homogenous coordinate */
  Ppoint t;

  ERR_SET_CUR_FUNC(PHG_ERH, Pfn_INQUIRY);

  if (PSL_SYS_STATE(PHG_PSL) != PSYS_ST_PHOP) {
    *error_ind = ERR2;
  } else if (PHG_NEAR_ZERO( w = m[2][0] * p->x + m[2][1] * p->y + m[2][2])) {
    *error_ind = -999;
  } else {
    *error_ind = 0;
    w = 1.0 / w;
    if (p != r) {
      r->x = w * (m[0][0] * p->x +
                  m[0][1] * p->y + m[0][2]);
      r->y = w * (m[1][0] * p->x +
                  m[1][1] * p->y + m[1][2]);
    } else {
      t.x =  w * (m[0][0] * p->x +
                  m[0][1] * p->y + m[0][2]);
      t.y =  w * (m[1][0] * p->x +
                  m[1][1] * p->y + m[1][2]);

      r->x = t.x;
      r->y = t.y;
    }
  }
}

/**
 * void build_transform3
 *
 * \brief       Generate 3D transformation matrix helper function
 */
static void build_transform3(
                             Ppoint3 *pt,               /* fixed point */
                             Pvec3 *shift,              /* shift vector */
                             Pfloat ax,                 /* rotation angle X */
                             Pfloat ay,                 /* rotation angle Y */
                             Pfloat az,                 /* rotation angle Z */
                             Pvec3 *scl,                /* scale vector */
                             Pmatrix3 m                 /* OUT transformation matrix */
                             )
{
  /* Translate pt to the origin, scale, rotate, translate back to pt,
   * shift:
   *      T * Tf~ * Rz * Ry * Rx * S * Tf.
   *
   *    where:  T is the "shift" transform,
   *      Tf ia the translation of pt to the origin and
   *      Tf~ is it's inverse,
   *      Ri is the rotation transform about the i'th axis,
   *      S is the scaling transform.
   */
  float *r;
  float cz, sz, cx, sx, cy, sy;

  cx = cos(ax);
  sx = sin(ax);
  cy = cos(ay);
  sy = sin(ay);
  cz = cos(az);
  sz = sin(az);

  r = m[0];
  r[0] = cz * cy * scl->delta_x;
  r[1] = (cz * sx * sy - sz * cx) * scl->delta_y;
  r[2] = (cz * sy * cx + sz * sx) * scl->delta_z;
  r[3] = shift->delta_x + pt->x -
    (r[0] * pt->x + r[1] * pt->y + r[2] * pt->z);

  r = m[1];
  r[0] = sz * cy * scl->delta_x;
  r[1] = (sz * sx * sy + cz * cx) * scl->delta_y;
  r[2] = (sz * sy * cx - cz * sx) * scl->delta_z;
  r[3] = shift->delta_y + pt->y -
    (r[0] * pt->x + r[1] * pt->y + r[2] * pt->z);

  r = m[2];
  r[0] = -sy * scl->delta_x;
  r[1] = cy * sx * scl->delta_y;
  r[2] = cy * cx * scl->delta_z;
  r[3] = shift->delta_z + pt->z -
    (r[0] * pt->x + r[1] * pt->y + r[2] * pt->z);

  r = m[3];
  r[0] = r[1] = r[2] = 0.0;
  r[3] = 1.0;
}

