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
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <phigs.h>
#include <phg.h>
#include <css.h>
#include <private/phgP.h>
#include <private/wsxP.h>
#include <util/ftn.h>

/**
 * \file pinpk.c
 *
 * \brief       initialize pick
 * \param       INTEGER WKID              workstation identifier
 * \param       INTEGER PKDNR             pick device number
 * \param       INTEGER ISTAT             initial status (POK,PNPICK)
 * \param       INTEGER IPPD              depth of initial pick path
 * \param       INTEGER PP(3, IPPD)       initial pick path
 * \param       INTEGER PET               prompt and echo type
 * \param       REAL XMIN, XMAX,YMIN,YMAX  echo area (DC)
 * \param       INTEGER LDR               dimension of data record array
 * \param       CHARACTER*80 DATREC(LDR)  data record
 * \param       INTEGER PPORDR            pick path order (PPOTOP, PPOBOT)
 */
FTN_SUBROUTINE(pinpk)(
                      FTN_INTEGER(wkid),
                      FTN_INTEGER(pkdnr),
                      FTN_INTEGER(istat),
                      FTN_INTEGER(ippd),
                      FTN_INTEGER_ARRAY(pp),
                      FTN_INTEGER(pet1),
                      FTN_REAL(xmin),
                      FTN_REAL(xmax),
                      FTN_REAL(ymin),
                      FTN_REAL(ymax),
                      FTN_INTEGER(ndr1),
                      char* datrec,
                      FTN_INTEGER(ppordr)
                      )
{
  Pint ws_id = FTN_INTEGER_GET(wkid);
  Pint pick_num = FTN_INTEGER_GET(pkdnr);
  Pin_status init_status = FTN_INTEGER_GET(istat);
  Pint pet = FTN_INTEGER_GET(pet1);
  Pint depth = FTN_INTEGER_GET(ippd);
  Ppick_path init_pick;
  Plimit3 echo_area;
  Ppath_order order = FTN_INTEGER_GET(ppordr);
  Pint ndr = FTN_INTEGER_GET(ndr1);
  Ppick_data data;
  Ppick_path_elem *elems = NULL;
  Pint i;

  if (depth > 0) {
    elems = (Ppick_path_elem *) malloc(depth * sizeof(Ppick_path_elem));
    if (elems == NULL) {
      /* report ERR900 via whatever mechanism this binding uses */
      return;
    }
    for (i = 0; i < depth; i++) {
      elems[i].struct_id = FTN_INTEGER_ARRAY_GET(pp, i * 3);
      elems[i].pick_id   = FTN_INTEGER_ARRAY_GET(pp, i * 3 + 1);
      elems[i].elem_pos  = FTN_INTEGER_ARRAY_GET(pp, i * 3 + 2);
    }
  }

  init_pick.depth = depth;
  init_pick.path_list = elems;

  echo_area.x_min = FTN_REAL_GET(xmin);
  echo_area.x_max = FTN_REAL_GET(xmax);
  echo_area.y_min = FTN_REAL_GET(ymin);
  echo_area.y_max = FTN_REAL_GET(ymax);
  echo_area.z_min = 0.0;
  echo_area.z_max = 1.0;

  /* only echo mode 1 */
  data.pets.pet_r1.unused = 0;
  /* FIXME check if we properly copy stuff from the data record */
  pinit_pick3(ws_id, pick_num, init_status, &init_pick, pet, &echo_area, &data, order);
  free(elems);
}
