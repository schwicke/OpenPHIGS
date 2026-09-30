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

#include <stdio.h>
#include <stdlib.h>

#include "phg.h"
#include "private/phgP.h"
#include "private/sinqP.h"
#include "private/wsxP.h"
#include "private/cb_internal.h"

/**
 * \file pset_string_mode.c
 *
 * \brief       Set string mode
 *
 * \param[in]  ws_id        workstation identifier
 * \param[in]  string_dev   string device number
 * \param[in]  op_mode      operating mode
 * \verbatim
  0  POP_REQ     request
  1  POP_SAMPLE  sample
  2  POP_EVENT   event
\endverbatim
 * \param[in]  echo_switch  echo switch
 * \verbatim
  0  PSWITCH_NO_ECHO  no echo
  1  PSWITCH_ECHO     echo
\endverbatim
 *
 * \pre The workstation must be open
 *
 * \sa psstm pawait_event pinit_string pinit_string3 preq_string psample_string
 */
void pset_string_mode(
                      Pint ws_id,
                      Pint string_dev,
                      Pop_mode op_mode,
                      Pecho_switch echo_switch
                      )
{
  Wst_input_wsdt *idt;

  idt = input_ws_open(ws_id, Pfn_set_string_mode, NULL, NULL);
  if (idt != NULL) {
    if (string_dev > 0) {
      set_mode(ws_id, PHG_ARGS_INP_STR, string_dev, op_mode, echo_switch);
    }
      else {
        ERR_REPORT(PHG_ERH, ERR250);
      }
  }
}

