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
#include "phg.h"
#include "css.h"
#include "private/phgP.h"
#include "util/ftn.h"

/**
 * \file psatal.c
 *
 * \brief       Set annotation text alignment
 *
 * \param[in]  atalh  horizontal annotation text alignment
 * \verbatim
  0  PAHNOR  normal
  1  PALEFT  left
  2  PACENT  centre
  3  PARITE  right
\endverbatim
 * \param[in]  atalv  vertical annotation text alignment
 * \verbatim
  0  PAVNOR  normal
  1  PATOP   top
  2  PACAP   cap
  3  PAHALF  half
  4  PABASE  base
  5  PABOTT  bottom
\endverbatim
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa pset_anno_align pstxpr
 */
FTN_SUBROUTINE(psatal)(
                       FTN_INTEGER(atalh),
                       FTN_INTEGER(atalv)
                       )
{
#ifdef DEBUG
  printf("DEBUG: PSATAL text align called\n");
#endif
  Ptext_align text_align;
  text_align.hor = (Phor_text_align) FTN_INTEGER_GET(atalh);
  text_align.vert = (Pvert_text_align) FTN_INTEGER_GET(atalv);
  pset_anno_align(&text_align);
}

