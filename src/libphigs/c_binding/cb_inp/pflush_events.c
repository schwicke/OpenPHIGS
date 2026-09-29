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
 * \file pflush_events.c
 *
 * \brief       Flush device events
 *
 * \param[in]  ws_id      workstation identifier
 * \param[in]  inp_class  input class
 * \verbatim
  0  PIN_NONE    none
  1  PIN_LOC     locator
  2  PIN_STROKE  stroke
  3  PIN_VAL     valuator
  4  PIN_CHOICE  choice
  5  PIN_PICK    pick
  6  PIN_STRING  string
\endverbatim
 * \param[in]  dev        device number
 *
 * \pre The workstation must be open
 *
 * \sa pflush
 */
void pflush_events(
                   Pint        ws_id,     /* workstation identifier       */
                   Pin_class   inp_class,  /* device class */
                   Pint        dev    /* logical input device number  */
                   )
{
  Wst_input_wsdt *idt;
  if ( (idt = input_ws_open(ws_id, Pfn_flush_events, NULL, NULL))){
    if (PHG_INPUT_Q != NULL)
      phg_sin_q_flush_device(PHG_INPUT_Q, ws_id, inp_class, dev);
  }
}
