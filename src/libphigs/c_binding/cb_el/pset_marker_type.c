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

/**
 * \file pset_marker_type.c
 *
 * \brief       Set marker type
 *
 * \param[in]  marker_type  marker type
 * \verbatim
  1  PMARKER_DOT       dot
  2  PMARKER_PLUS      plus sign
  3  PMARKER_ASTERISK  asterisk
  4  PMARKER_CIRCLE    circle
  5  PMARKER_CROSS     diagonal cross
  6  PMARKER_TRIANG    triangle
  7  PMARKER_SQUARE    square
  8  PMARKER_PENTAGON  pentagon
  9  PMARKER_HEXAGON   hexagon
\endverbatim
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa psmk pinq_marker_rep pset_indiv_asf pset_line_rep
 */
void pset_marker_type(
                      Pint marker_type
                      )
{
  Phg_args_add_el args;

  if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_marker_type)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      args.el_type = PELEM_MARKER_TYPE;
      args.el_size = sizeof(Pint);
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
        ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
        args.el_data = PHG_SCRATCH.buf;
        memcpy(args.el_data, &marker_type, args.el_size);
        phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

