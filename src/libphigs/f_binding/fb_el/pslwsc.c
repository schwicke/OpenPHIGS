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
 * \file pslwsc.c
 *
 * \brief       Set linewidth scale factor
 *
 * \param[in]  lwidth  linewidth scale factor
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa pset_linewidth psiasf psplr
 */

FTN_SUBROUTINE(pslwsc)(
                       FTN_REAL(lwidth)
                       )
{
  Phg_args_add_el args;
  Pfloat linewidth = FTN_REAL_GET(lwidth);
#ifdef DEBUG
  printf("DEBUG: PSLWSC line width attribute\n");
#endif
  pset_linewidth(linewidth);
}

