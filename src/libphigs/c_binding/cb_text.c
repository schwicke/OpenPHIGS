/******************************************************************************
*   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
*
*   This file is part of Open PHIGS
*   Copyright (C) 2014 Surplus Users Ham Society
*             (C) 2022-2023 CERN
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

/*******************************************************************************
 * ptext
 *
 * DESCR:	Creates a new element - Text
 * RETURNS:	N/A
 */

void ptext(
   Ppoint *text_pos,
   char *char_string
   )
{
   Phg_args_add_el args;
   Ppoint *data;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_text)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_TEXT;
         args.el_size = sizeof(Ppoint) + strlen(char_string) + 1;
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            data = (Ppoint *) args.el_data;
            memcpy(data, text_pos, sizeof(Ppoint));
            strcpy((char *) &data[1], char_string);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}
/*******************************************************************************
 * ptext3
 *
 * DESCR:	Creates a new element - Text
 * RETURNS:	N/A
 */

void ptext3(
   Ppoint3 *text_pos,
   Pvec3 plane[2],
   char *char_string
   )
{
   Phg_args_add_el args;
   Ppoint3 *data;
   Pvec3 * data1;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_text)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_TEXT3;
         args.el_size = sizeof(Ppoint3) + +2*sizeof(Pvec3) + strlen(char_string) + 1;
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            data = (Ppoint3 *) args.el_data;
            memcpy(data, text_pos, sizeof(Ppoint3));
	    data1 = (Pvec3*) &data[1];
            memcpy(data1, &plane[0], 2*sizeof(Pvec3));
            strcpy((char *) &data1[2], char_string);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}
/*******************************************************************************
 * pset_text_font
 *
 * DESCR:	Creates a new element - Text Font Attribute
 * RETURNS:	N/A
 */

void pset_text_font(
   Pint font
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_font)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_TEXT_FONT;
         args.el_size = sizeof(Pint);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            memcpy(args.el_data, &font, args.el_size);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * pset_text_prec
 *
 * DESCR:	Creates a new element - Text Precision Attribute
 * RETURNS:	N/A
 */

void pset_text_prec(
   Ptext_prec prec
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_prec)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_TEXT_PREC;
         args.el_size = sizeof(Pint);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            *((Pint *) args.el_data) = (Pint) prec;
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * pset_text_path
 *
 * DESCR:	Creates a new element - Text Path Attribute
 * RETURNS:	N/A
 */

void pset_text_path(
   Ptext_path text_path
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_path)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_TEXT_PATH;
         args.el_size = sizeof(Pint);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            *((Pint *) args.el_data) = (Pint) text_path;
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * pset_text_align
 *
 * DESCR:	Creates a new element - Text Alignment Attribute
 * RETURNS:	N/A
 */

void pset_text_align(
   Ptext_align *text_align
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_align)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_TEXT_ALIGN;
         args.el_size = sizeof(Ptext_align);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            memcpy(args.el_data, text_align, args.el_size);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * pset_text_colr_ind
 *
 * DESCR:	Creates a new element - Text Color Attribute
 * RETURNS:	N/A
 */

void pset_text_colr_ind(
   Pint colr_ind
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_colr_ind)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else if (colr_ind < 0) {
         ERR_REPORT(PHG_ERH, ERR113);
      }
      else {
         args.el_type = PELEM_TEXT_COLR_IND;
         args.el_size = sizeof(Pint);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            memcpy(args.el_data, &colr_ind, args.el_size);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * pset_text_ind
 *
 * DESCR:	Creates a new element - Text Attribute Index
 * RETURNS:	N/A
 */

void pset_text_ind(
   Pint text_ind
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_ind)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else if (text_ind < 1) {
         ERR_REPORT(PHG_ERH, ERR100);
      }
      else {
         args.el_type = PELEM_TEXT_IND;
         args.el_size = sizeof(Pint);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            memcpy(args.el_data, &text_ind, args.el_size);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * panno_text_rel
 *
 * DESCR:	Creates a new element - annotation text relative
 * RETURNS:	N/A
 */

void panno_text_rel(
		    Ppoint *ref_point,
		    Pvec *offset,
		    char *text){

  Phg_args_add_el args;
  char * data;

  if (phg_entry_check(PHG_ERH, ERR5, Pfn_anno_text_rel)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      args.el_type = PELEM_ANNO_TEXT_REL;
      args.el_size = sizeof(Ppoint)+sizeof(Pvec)+strlen(text) + 1;
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
	ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
	args.el_data = PHG_SCRATCH.buf;
	Ppoint * refp = (Ppoint *) args.el_data;
	memcpy(refp, ref_point, sizeof(Ppoint));
	Pvec * offs = (Pvec *) &refp[1];
	memcpy(offs, offset, sizeof(Pvec));
	strcpy((char*)&offs[1], text);
	phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

void panno_text_rel3(
		    Ppoint3 *ref_point,
		    Pvec3 *offset,
		    char *text){

  Phg_args_add_el args;
  int length = strlen(text);
  char * data;

  if (phg_entry_check(PHG_ERH, ERR5, Pfn_anno_text_rel3)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      args.el_type = PELEM_ANNO_TEXT_REL3;
      args.el_size = sizeof(Ppoint3)+sizeof(Pvec3)+strlen(text) + 1;
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
	ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
	args.el_data = PHG_SCRATCH.buf;
	Ppoint3 *refp = (Ppoint3 *) args.el_data;
	memcpy(refp, ref_point, sizeof(Ppoint3));
	Pvec3 *offs = (Pvec3 *) &refp[1];
	memcpy(offs, offset, sizeof(Pvec3));
	strcpy((char*)&offs[1], text);
	phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

/*******************************************************************************
 * pset_anno_char_ht
 *
 * DESCR:	Creates a new element - set annotation char height
 * RETURNS:	N/A
 */

void pset_anno_char_ht(
		       Pfloat height
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_ind)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else if (height < 0) {
         ERR_REPORT(PHG_ERH, ERR100);
      }
      else {
         args.el_type = PELEM_ANNO_CHAR_HT;
         args.el_size = sizeof(Pfloat);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            memcpy(args.el_data, &height, args.el_size);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}


/*******************************************************************************
 * pset_text_align
 *
 * DESCR:	Creates a new element - Text Alignment Attribute
 * RETURNS:	N/A
 */

void pset_anno_align(
   Ptext_align *text_align
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_align)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_ANNO_ALIGN;
         args.el_size = sizeof(Ptext_align);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            memcpy(args.el_data, text_align, args.el_size);
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}

/*******************************************************************************
 * pset_anno_path
 *
 * DESCR:	Creates a new element - Annotation Text Path Attribute
 * RETURNS:	N/A
 */

void pset_anno_path(
   Ptext_path text_path
   )
{
   Phg_args_add_el args;

   if (phg_entry_check(PHG_ERH, ERR5, Pfn_set_text_path)) {
      if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
         ERR_REPORT(PHG_ERH, ERR5);
      }
      else {
         args.el_type = PELEM_ANNO_PATH;
         args.el_size = sizeof(Pint);
         if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
            ERR_REPORT(PHG_ERH, ERR900);
         }
         else {
            args.el_data = PHG_SCRATCH.buf;
            *((Pint *) args.el_data) = (Pint) text_path;
            phg_add_el(PHG_CSS, &args);
         }
      }
   }
}
