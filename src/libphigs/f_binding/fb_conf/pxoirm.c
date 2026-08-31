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
*******************************************************************************/
#include <string.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "phg.h"
#include "css.h"
#include "private/phgP.h"
#include "util/ftn.h"
#include "phconf.h"

/**
 * \file pxoirm.c
 *
 * \brief   set the hardcopy scale factor for workstation (OpenPHIGS extension)
 *
 * \param   wkid work station ID
 * \param   hcsf scale factor, a positive real number
 *
 * \note This setting is only relevant if shader version 420 is in use. Mode must be lower or equal to 16. Note that the larger the number, the more memory hungry the system will be.
 *
 * \pre This setting can be set via the configuration as well. As it is used to configure the workstation, the workstation must not be open yet in order to have an effect.
 *.
 * \sa popwk
 */
FTN_SUBROUTINE(pxoirm)(
                        FTN_INTEGER(wkid),
                        FTN_INTEGER(mode)
                       )
{
  Pint ws_id = FTN_INTEGER_GET(wkid);
  Pint oirmode = FTN_REAL_GET(mode);
  pxset_oir_mode(ws_id, oirmode);
}
