/******************************************************************************
*   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
*
*   This file is part of Open PHIGS
*             (C) 2022-2023 CERN
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
*******************************************************************************/
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "phconf.h"

/**
 * \file pxset_conf_hcsf.c
 *
 * \brief       set the hardcopy scale factor (OpenPHIGS extension)
 *
 * \param   wkid work station ID
 * \param   hcsf scale factor, a positive real number
 *
 * \note This setting is only relevant for work station types 4 - 9 and is ignored for other work station types. See popen_wk(3) for available work station types. The function must be called BEFORE the workstation is opened.
 *
 * \pre The workstation WKID must not be open yet.
 * \sa popen_wk
 */
#include "phg.h"
#include "css.h"
#include "ws.h"
#include "private/phgP.h"
void pxset_conf_hcsf(
                     Pint ws_id,
                     Pfloat hcsf
                     ){
  Ws_handle wsh;
  if (phg_ws_open(ws_id, Pfn_close_ws) != NULL) {
    /* workstation is already open. Ignoring the call.*/
    ERR_REPORT(PHG_ERH, ERR53);
    return;
  }
  if (ws_id >=0 && ws_id <100){
    if (hcsf > 0. && hcsf <= 32.){
      config[ws_id].hcsf = hcsf;
    } else {
      printf("ERROR: configuration error. Ignoring unreasonable scale factor of: %f\n", hcsf);
      return;
    }
  } else {
    printf("FATAL: configuration error. Work station ID out of range: %d\n", ws_id);
    exit(1);
  }
  
}
