/******************************************************************************
*   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
*
*   This file is part of Open PHIGS
*   Copyright (C) 2022-2023 CERN
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
******************************************************************************/

#include <stdlib.h>
#include <stdio.h>

#include "phg.h"
#include "private/phgP.h"
#include "private/cbP.h"

void pgse(
	  Pgse_type gse_type,
	  Pgse_data *gse_data
	  )
{
  Phg_args_add_el args;
  Pgse_elem gse_elem;
  
  if (phg_entry_check(PHG_ERH, ERR5, Pfn_gse)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      args.el_type = PELEM_GSE;
      int elem_size = sizeof(gse_type) + sizeof(Pint);
      gse_elem.gse_type = gse_type;
      switch (gse_type) {
      case PGSE_ID_NO_OPERATION:
	elem_size += sizeof(int);
	gse_elem.gse_size = sizeof(int);
	memcpy(&gse_elem.gse_data, gse_data, sizeof(int));
	break;
      case PGSE_ID_HIGHLIGHT_COLOR:
	elem_size += sizeof(Pgcolr);
	gse_elem.gse_size = sizeof(Pgcolr);
	memcpy(&gse_elem.gse_data, gse_data, sizeof(Pgcolr));
	break;
      };
      args.el_size = elem_size;
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
	ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
	args.el_data = PHG_SCRATCH.buf;
	Pgse_elem *data = (Pgse_elem *) args.el_data;
	memcpy(data, &gse_elem, sizeof(Pgse_elem));
	phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

void pxset_highlight_colr  (
			    Pgcolr *colr
			    )
{
  Pgse_data  gse_data;
  
  memcpy(&gse_data.colr.highlight_colr, colr, sizeof(Pgcolr));  
  pgse(PGSE_ID_HIGHLIGHT_COLOR, &gse_data);
}
