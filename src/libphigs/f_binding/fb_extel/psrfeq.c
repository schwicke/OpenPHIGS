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

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <phigs.h>
#include <phg.h>
#include <css.h>
#include <private/phgP.h>
#include <util/ftn.h>

#ifndef  MAX_ARRAY_SIZE
#define  MAX_ARRAY_SIZE 400
#endif

/**
 * \file psrfeq.c
 *
 * \brief       Set interior reflectance equation
 *
 * \param[in]  refl_eq  reflectance equation
 * \verbatim
  1  PNORM   no reflectance calculation
  2  PARM    ambient
  3  PADRM   ambient and diffuse
  4  PADSRM  ambient, diffuse and specular
\endverbatim
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa pset_refl_eqn psbrfm
 */
FTN_SUBROUTINE(psrfeq)(
                       FTN_INTEGER(refl_eq)
                       ){
  Pint refl_equation = FTN_INTEGER_GET(refl_eq);
  pset_refl_eqn(refl_equation);
}

