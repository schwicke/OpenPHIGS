/******************************************************************************
 *   Do NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <gl2ps.h>
#include "phigs.h"
#include "phg.h"
#include "private/phgP.h"
#include "private/wsglP.h"
#include "css.h"
#include "ws.h"
#include "util/ftn.h"
#include "phconf.h"

/**
 * \file pxscm.c
 *
 * \brief       Set color map
 * \param       wkid: Workstation ID
 *
 * This functions is an extension to PHIGS.
 * - In COLORMODE=1 it creates 125 colors starting with index 16. Any colors with lower indices will not be touched.
 * - COLORMODE=2 does same as 1. In addition, it creates 5 levels of transparent colors for all colors, with offset 200 with decreasing transparency. This is done as well for any existing colors with indices between 1 and 15.
 *
 * The color mode is set with pxscm.
 *
 * \sa pscm popwk
 */
FTN_SUBROUTINE(pxscm)(
                      FTN_INTEGER(wkid)
                      )
{
  Pint ws_id = FTN_INTEGER_GET(wkid);
  pxset_color_map(ws_id);
}

