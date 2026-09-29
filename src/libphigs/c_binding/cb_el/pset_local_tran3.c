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
 * \file pset_local_tran3.c
 *
 * \brief       Set local transformation 3
 *
 * \param[in]  local_tran    local modelling transformation matrix
 * \param[in]  compose_type  composition type
 * \verbatim
  0  PTYPE_PRECONCAT   preconcatenate
  1  PTYPE_POSTCONCAT  postconcatenate
  2  PTYPE_REPLACE     replace
\endverbatim
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa pslmt3
 */
void pset_local_tran3(
                      Pmatrix3 local_tran,
                      Pcompose_type compose_type
                      )
{
  Phg_args_add_el args;
  Pint *data;

  if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_local_tran3)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      args.el_type = PELEM_LOCAL_MODEL_TRAN3;
      args.el_size = sizeof(Pint) + 16 * sizeof(Pfloat);
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
        ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
        args.el_data = PHG_SCRATCH.buf;
        data = (Pint *) args.el_data;
        data[0] = (Pint) compose_type;
        phg_mat_unpack((Pfloat *) &data[1], local_tran);
        phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

