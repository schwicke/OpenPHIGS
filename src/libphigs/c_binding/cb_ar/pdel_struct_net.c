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
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "phg.h"
#include "css.h"
#include "private/phgP.h"
#include "private/cbP.h"

/**
 * \file pdel_struct_net.c
 *
 * \brief       Delete structure network
 *
 * \param[in]  struct_id  structure identifier
 * \param[in]  ref_flag   what to do with structures in the network that are referenced from outside of it
 * \verbatim
  0  PFLAG_DEL   delete referenced structures too
  1  PFLAG_KEEP  keep structures referenced from elsewhere
\endverbatim
 *
 * \sa pdel_struct
 */
void pdel_struct_net(
                     Pint struct_id,
                     Pref_flag ref_flag
                     )
{
  Phg_args_del_struct_net args;

  if (phg_entry_check(PHG_ERH, ERR2, Pfn_del_struct_net)) {
    args.id = struct_id;
    args.flag = ref_flag;
    phg_del_struct_net(PHG_CSS, &args);
  }
}

