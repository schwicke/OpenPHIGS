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
 * \file pinch3.c
 *
 * \brief       initialize choice 3
 * \param       INTEGER WKID              workstation identifier
 * \param       INTEGER CHDNR             choice device number
 * \param       INTEGER ISTAT             initial status (POK,PNCHOI)
 * \param       INTEGER ICHNR             initial choice number
 * \param       INTEGER PET               prompt and echo type
 * \param       REAL    EVOL(6)           echo volume (DC), xmin, xmax, ymin, ymax, zmin, zmax
 * \param       INTEGER LDR               dimension of data record array
 * \param       CHARACTER*80 DATREC(LDR)  data record
 *
 * OpenPHIGS supports the following echo modes:
 * - echo mode 1:
 *    + opens a new window
 *    +  echo area given in NC coordinates for the root window
 * - echo mode -1:
 *    + displays window on top of the root window
 *    + echo area is given as a fraction of the root window
 * - echo mode 3:
 *    + opens a new window
 *    + echo area given in NC coordinates for the root window
 *    + expects titels as strings
 * - echo mode -3:
 *    + as 3 but echo area is given as a fraction of the root window
 * - echo mode 4:
 *    + as 3 but expects one more string
 *    + last string will be used as title
 * - echo mode -4:
 *    + as 4 but
 *    + echo area is given as a fraction of the root window
 *
 * \todo The function implements only a subset of PETs foreseen. The 2dim function implementation is missing.
 * \sa pprec
 */

FTN_SUBROUTINE(pinch3)(
                       FTN_INTEGER(wkid),
                       FTN_INTEGER(chdnr),
                       FTN_INTEGER(istat),
                       FTN_INTEGER(ichnr),
                       FTN_INTEGER(ipet),
                       FTN_REAL_ARRAY(evol),
                       FTN_INTEGER(ldr),
                       char * datrec)
{
# define MAX_PROMPTS 32
  Pint ws_id = FTN_INTEGER_GET(wkid);
  Pint choice_dev = FTN_INTEGER_GET(chdnr);
  Pin_status init_status = (Pin_status)FTN_INTEGER_GET(istat);
  Pint init_choice = FTN_INTEGER_GET(ichnr);
  Pint pet = FTN_INTEGER_GET(ipet);
  int num_prompts;
  Ppr_switch prompts[MAX_PROMPTS];
  int i;
  int nstrings, charlen[MAX_PROMPTS+1];
  char *strings, *buffer;
  char *str[MAX_PROMPTS+1];

  int *ip;

  Plimit3 echo_volume;
  Pchoice_data3 choice_data_rec;
  echo_volume.x_min = FTN_REAL_ARRAY_GET(evol, 0);
  echo_volume.x_max = FTN_REAL_ARRAY_GET(evol, 1);
  echo_volume.y_min = FTN_REAL_ARRAY_GET(evol, 2);
  echo_volume.y_max = FTN_REAL_ARRAY_GET(evol, 3);
  echo_volume.z_min = FTN_REAL_ARRAY_GET(evol, 4);
  echo_volume.z_max = FTN_REAL_ARRAY_GET(evol, 5);

  ip = (int*)&datrec[0];
  num_prompts = ip[0];
  ip = &ip[1];
  for (i=0; i<num_prompts;i++){
    prompts[i] = (Ppr_switch)ip[i];
  }
  /* skip number of floats at ip[num_prompts] */
  if (ip[num_prompts] != 0){
    printf("Error in npinch3: unexpected number of floats %d\n", ip[num_prompts]);
  };
  ip = (int*)&ip[num_prompts+1];
  nstrings = ip[0];
  ip = &ip[1];
  /* copy over the length of the strings */
  memcpy(&charlen[0], (int*)&ip[0], nstrings*sizeof(int));
  strings = (char *) &ip[nstrings];
  /* We need to release these strings later on */
  for (i=0; i<nstrings; i++){
    str[i] = strings;
#ifdef DEBUG
    printf("DEBUG pinch3: string nr %d %s length %d expected %d\n", i, str[i], (int) strlen(str[i]), charlen[i]);
#endif
    strings += 1 + strlen(strings);
  }
#ifdef DEBUG
  printf("DEBUG pinch3: got %d strings\n", nstrings);
  for (i=0; i<nstrings; i++){
    printf("DEBUG pinch3 Nr.: %d: Content: \"%s\" length %d\n", i, str[i], (int)strlen(str[i]));
  }
#endif
  switch (pet) {
  case 1:
  case -1:
    break;
  case 2:
  case-2:
    choice_data_rec.pets.pet_r2.num_prompts = num_prompts;
    choice_data_rec.pets.pet_r2.prompts = &prompts[0];
    break;
    /* first string is the title */
  case 3:
  case -3:
  case 4:
  case -4:
    choice_data_rec.pets.pet_r3.num_strings = num_prompts+1;
    choice_data_rec.pets.pet_r3.strings = str;
  }
  pinit_choice3(ws_id, choice_dev, init_status, init_choice, pet, &echo_volume, &choice_data_rec);
}

