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
 * \file psis.c
 *
 * \brief       Set interior style
 *
 * \param[in]  ints  interior style
 * \verbatim
  0  PISEMP  empty
  1  PHOLLO  hollow (outline only)
  2  PSOLID  solid
  3  PHATCH  hatch
  4  PPATTR  pattern
\endverbatim
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa pset_int_style psiasf psisi pbsi psbis
 */
FTN_SUBROUTINE(psis)(
                     FTN_INTEGER(ints)
                     )
{
  Pint_style interior_style = (Pint_style) FTN_INTEGER_GET(ints);
#ifdef DEBUG
  printf("DEBUG: PSIS interior style called to %d\n", (int)interior_style);
#endif
  pset_int_style(interior_style);
}

