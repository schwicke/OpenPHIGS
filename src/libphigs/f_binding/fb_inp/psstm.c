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
 * \file psstm.c
 *
 * \brief       Set string mode
 *
 * \param[in]  wkid   workstation identifier
 * \param[in]  stdnr  string device number
 * \param[in]  mode   operating mode
 * \verbatim
  0  PREQU   request
  1  PSAMPL  sample
  2  PEVENT  event
\endverbatim
 * \param[in]  esw    echo switch
 * \verbatim
  0  PNECHO  no echo
  1  PECHO   echo
\endverbatim
 *
 * \pre The workstation must be open
 *
 * \sa pset_string_mode pwait pinst pinst3 prqst psmst
 */
FTN_SUBROUTINE(psstm)(
                      FTN_INTEGER(wkid),
                      FTN_INTEGER(stdnr),
                      FTN_INTEGER(mode),
                      FTN_INTEGER(esw)
                      ){
  Pint ws_id = FTN_INTEGER_GET(wkid);
  Pint string_dev = FTN_INTEGER_GET(stdnr);
  Pint opmode = FTN_INTEGER_GET(mode);
  Pint echo = FTN_INTEGER_GET(esw);
  pset_string_mode(ws_id, string_dev, opmode, echo);
}

