/******************************************************************************
*   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
*
*   This file is part of Open PHIGS
*   Copyright (C) 2014 Surplus Users Ham Society
*
*   Open PHIGS is free software: you can redistribute it and/or modify
*   it under the terms of the GNU Lesser General Public License as published by
*   the Free Software Foundation, either version 2.1 of the License, or
*   (at your option) any later version.
*
*   Open PHIGS is distributed in the hope that it will be useful,
*   but WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*   GNU Lesser General Public License for more details.
*
*   You should have received a copy of the GNU Lesser General Public License
*   along with Open PHIGS. If not, see <http://www.gnu.org/licenses/>.
******************************************************************************
* Changes:   Copyright (C) 2022-2023 CERN
******************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "phg.h"
#include "css.h"
#include "private/phgP.h"
#include "util/ftn.h"

/**
 * \file ppm.c
 *
 * \brief       polymarker
 */

FTN_SUBROUTINE(ppm)(
                    FTN_INTEGER(n),
                    FTN_REAL_ARRAY(pxa),
                    FTN_REAL_ARRAY(pya)
                    )
{
  int num_points = FTN_INTEGER_GET(n);
  Ppoint points[num_points];
  Ppoint_list ppoint_list;
  int i;
  ppoint_list.num_points = num_points;
  for (i=0; i<num_points; i++){
    points[i].x = FTN_REAL_ARRAY_GET(pxa, i);
    points[i].y = FTN_REAL_ARRAY_GET(pya, i);
#ifdef DEBUG
    printf("PPM %d pos %f %f\n", i, points[i].x, points[i].y);
#endif
  }
  ppoint_list.points = &points[0];
  ppolymarker(&ppoint_list);
}

