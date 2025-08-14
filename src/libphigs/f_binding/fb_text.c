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

/*******************************************************************************
 * ptx
 *
 * DESCR:       Text
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(ptx)(
                    FTN_REAL(px),
                    FTN_REAL(py),
                    FTN_CHARACTER(chars)
                    )
{
  Phg_args_add_el args;
  Pint len;
  Ppoint text_pos;
  Ppoint *data;
  char *char_string;
#ifdef DEBUG
  printf("DEBUG: text\n");
#endif

  if (phg_entry_check(PHG_ERH, ERR5, Pfn_text)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      len = FTN_CHARACTER_LEN(chars);
      args.el_type = PELEM_TEXT;
      args.el_size = sizeof(Ppoint) + len + 1;
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
        ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
        args.el_data = PHG_SCRATCH.buf;
        data = (Ppoint *) args.el_data;
        text_pos.x = FTN_REAL_GET(px);
        text_pos.y = FTN_REAL_GET(py);
        memcpy(data, &text_pos, sizeof(Ppoint));
        char_string = (char *) &data[1];
        strncpy(char_string, FTN_CHARACTER_GET(chars), len);
        char_string[len] = '\0';
        phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

/*******************************************************************************
 * ptx3
 *
 * DESCR:       TEXT 3
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(ptx3)(
                     FTN_REAL(px),
                     FTN_REAL(py),
                     FTN_REAL(pz),
                     FTN_REAL_ARRAY(tdx),
                     FTN_REAL_ARRAY(tdy),
                     FTN_REAL_ARRAY(tdz),
                     FTN_CHARACTER(chars)
                     )
{

  Phg_args_add_el args;
  Pint len;
  Ppoint3 text_pos;
  Pvec3 plane[2];
  char text[1024];

  Ppoint3 *data;
#ifdef DEBUG
  printf("DEBUG: text3\n");
#endif

  len = FTN_CHARACTER_LEN(chars);
  if (len < 1024) {
    strncpy(&text[0], FTN_CHARACTER_GET(chars), len);
    text[len] = '\0';
    text_pos.x = FTN_REAL_GET(px);
    text_pos.y = FTN_REAL_GET(py);
    text_pos.z = FTN_REAL_GET(pz);
    plane[0].delta_x = FTN_REAL_ARRAY_GET(tdx, 0);
    plane[0].delta_y = FTN_REAL_ARRAY_GET(tdy, 0);
    plane[0].delta_z = FTN_REAL_ARRAY_GET(tdz, 0);
    plane[1].delta_x = FTN_REAL_ARRAY_GET(tdx, 1);
    plane[1].delta_y = FTN_REAL_ARRAY_GET(tdy, 1);
    plane[1].delta_z = FTN_REAL_ARRAY_GET(tdz, 1);

    ptext3(&text_pos, &plane[0], &text[0]);

  } else {
    printf("ERROR: Buffer overlow in PTX3: Ignoring function");
  }
}

/*******************************************************************************
 * pstxfn
 *
 * DESCR:       Set text font
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pstxfn)(
                       FTN_INTEGER(font)
                       )
{
#ifdef DEBUG
  printf("DEBUG: pset text font called\n");
#endif
  pset_text_font(FTN_INTEGER_GET(font));
}


/*******************************************************************************
 * pstxpr
 *
 * DESCR:       Set text precision
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pstxpr)(
                       FTN_INTEGER(prec)
                       )
{
#ifdef DEBUG
  printf("DEBUG: pset text prec called\n");
#endif
  Ptext_prec tprec = (Ptext_prec)FTN_INTEGER_GET(prec);
  pset_text_prec(tprec);
}

/*******************************************************************************
 * pstxp
 *
 * DESCR:       Set text path
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pstxp)(
                      FTN_INTEGER(txp)
                      )
{
  Ptext_path tpath = (Ptext_path)FTN_INTEGER_GET(txp);
#ifdef DEBUG
  printf("DEBUG: pset text path to %d\n", *txp);
#endif
  pset_text_path(tpath);
}

/*******************************************************************************
 * pstxal
 *
 * DESCR:       Set text alignment
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pstxal)(
                       FTN_INTEGER(txalh),
                       FTN_INTEGER(txalv)
                       )
{
#ifdef DEBUG
  printf("DEBUG: PSTXAL text align called\n");
#endif
  Ptext_align text_align;
  text_align.hor = (Phor_text_align) FTN_INTEGER_GET(txalh);
  text_align.vert = (Pvert_text_align) FTN_INTEGER_GET(txalv);
  pset_text_align(&text_align);
}

/*******************************************************************************
 * pstxci
 *
 * DESCR:       Set text colour index
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pstxci)(
                       FTN_INTEGER(coli)
                       )
{
#ifdef DEBUG
  printf("DEBUG PSTXCI text color index called %d\n", (int)*coli);
#endif
  pset_text_colr_ind((Pint)FTN_INTEGER_GET(coli));
}

/*******************************************************************************
 * pstxi
 *
 * DESCR:       Set text index
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pstxi)(
                      FTN_INTEGER(txi)
                      )
{
#ifdef DEBUG
  printf("DEBUG PSTXCI text color index called %d\n", (int)*txi);
#endif
  pset_text_ind((Pint)FTN_INTEGER_GET(txi));
}

/*******************************************************************************
 * patr
 *
 * DESCR:       Annotation text relative
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(patr)(
                     FTN_REAL(rpx),
                     FTN_REAL(rpy),
                     FTN_REAL(apx),
                     FTN_REAL(apy),
                     FTN_CHARACTER(chars)
                     ){
  Ppoint ref_point;
  Pvec offset;
  int len = FTN_CHARACTER_LEN(chars);
  char * text = (char*)malloc(len+1);
  memcpy(text, FTN_CHARACTER_GET(chars), len);
  text[len] = '\0';
  ref_point.x = FTN_REAL_GET(rpx);
  ref_point.y = FTN_REAL_GET(rpy);
  offset.delta_x = FTN_REAL_GET(apx);
  offset.delta_y = FTN_REAL_GET(apy);
  panno_text_rel(&ref_point, &offset, text);
}

/*******************************************************************************
 * patr3
 *
 * DESCR:       ANNOTATION TEXT RELATIVE 3
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(patr3)(
                      FTN_REAL(rpx),
                      FTN_REAL(rpy),
                      FTN_REAL(rpz),
                      FTN_REAL(apx),
                      FTN_REAL(apy),
                      FTN_REAL(apz),
                      FTN_CHARACTER(chars)
                      ){
  Ppoint3 ref_point;
  Pvec3 offset;
  int len = FTN_CHARACTER_LEN(chars);
  char * text = (char*)malloc(len+1);
  memcpy(text, FTN_CHARACTER_GET(chars), len);
  text[len] = '\0';

  ref_point.x = FTN_REAL_GET(rpx);
  ref_point.y = FTN_REAL_GET(rpy);
  ref_point.z = FTN_REAL_GET(rpz);
  offset.delta_x = FTN_REAL_GET(apx);
  offset.delta_y = FTN_REAL_GET(apy);
  offset.delta_z = FTN_REAL_GET(apz);
  panno_text_rel3(&ref_point, &offset, text);
}

/*******************************************************************************
 * psatch
 *
 * DESCR:       Set annotation text character height
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(psatch)(
                       FTN_REAL(atchh)
                       )
{
#ifdef DEBUG
  printf("DEBUG PSATCH annotation text height  %f\n", (float)*atchh);
#endif
  pset_anno_char_ht((Pfloat)FTN_INTEGER_GET(atchh));
}

/*******************************************************************************
 * psatal
 *
 * DESCR:
 * RETURNS:     N/A
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

/*******************************************************************************
 * psatp
 *
 * DESCR:       Set annotation text path
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(psatp)(
                      FTN_INTEGER(path)
                      )
{
  Ptext_path tpath = (Ptext_path)FTN_INTEGER_GET(path);
#ifdef DEBUG
  printf("DEBUG: pset anno text path to %d\n", *path);
#endif
  pset_anno_path(tpath);
}
