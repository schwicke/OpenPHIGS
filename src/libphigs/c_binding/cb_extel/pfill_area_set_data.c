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

#include "phg.h"
#include "css.h"
#include "private/phgP.h"

/**
 * \file pfill_area_set_data.c
 *
 * \brief       Fill area set with data
 *
 * Not part of the standard. Takes the same arguments as pfill_area_set3_data.
 *
 * \param[in]  fflag      facet data flag
 * \verbatim
  0  PFACET_NONE           no facet data
  1  PFACET_COLOUR         facet colour
  2  PFACET_NORMAL         facet normal
  3  PFACET_COLOUR_NORMAL  facet colour and normal
\endverbatim
 * \param[in]  eflag      edge data flag
 * \verbatim
  0  PEDGE_NONE        no edge data
  1  PEDGE_VISIBILITY  edge visibility flags
\endverbatim
 * \param[in]  vflag      vertex data flag
 * \verbatim
  0  PVERT_COORD                coordinates only
  1  PVERT_COORD_COLOUR         coordinates and colour
  2  PVERT_COORD_NORMAL         coordinates and normal
  3  PVERT_COORD_COLOUR_NORMAL  coordinates, colour and normal
\endverbatim
 * \param[in]  colr_type  colour type of facet and vertex colours
 * \verbatim
  0  PINDIRECT    colour index
  1  PMODEL_RGB   RGB
  2  PMODEL_RGBA  RGB with alpha
\endverbatim
 * \param[in]  fdata      facet data (colour and/or normal, according to fflag)
 * \param[in]  nfa        number of fill areas in the set
 * \param[in]  edata      edge visibility flags per fill area (if eflag is PEDGE_VISIBILITY)
 * \param[in]  vdata      vertex data per fill area (according to vflag)
 *
 * \note Adds a new element to the current structure
 * \pre The current structure must be open for editing
 *
 * \sa pfill_area_set3_data pfasd
 */
void pfill_area_set_data(
                         Pint fflag,
                         Pint eflag,
                         Pint vflag,
                         Pint colr_type,
                         Pfacet_data3 *fdata,
                         Pint nfa,
                         Pedge_data_list *edata,
                         Pfacet_vdata_list3 *vdata
                         )
{
  Phg_args_add_el args;
  Pint i;
  unsigned facet_size, vertex_size;
  Pint *data;
  char *tp;
  Pint num_vertices;

  if (phg_entry_check(PHG_ERH, ERR5, Pfn_fill_area_set_data)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      switch (fflag) {
      case PFACET_COLOUR:
        facet_size = sizeof(Pcoval);
        break;

      case PFACET_NORMAL:
        facet_size = sizeof(Pvec3);
        break;

      case PFACET_COLOUR_NORMAL:
        facet_size = sizeof(Pconorm3);
        break;

      default:
        facet_size = 0;
               break;
      }

      switch (vflag) {
      case PVERT_COORD:
        vertex_size = sizeof(Ppoint);
        break;

      case PVERT_COORD_COLOUR:
        vertex_size = sizeof(Pptco3);
        break;

      case PVERT_COORD_NORMAL:
        vertex_size = sizeof(Pptnorm3);
        break;

      case PVERT_COORD_COLOUR_NORMAL:
        vertex_size = sizeof(Pptconorm3);
        break;

      default:
        vertex_size = 0;
        break;
      }

      args.el_type = PELEM_FILL_AREA_SET_DATA;
      args.el_size = 5 * sizeof(Pint) + facet_size;

      if (eflag == PEDGE_VISIBILITY) {
        for (i = 0; i < nfa; i++) {
          args.el_size += sizeof(Pint);   /* Pint num_edges */
          args.el_size += edata[i].num_edges * sizeof(Pedge_flag);
        }
      }

      for (i = 0; i < nfa; i++) {
        args.el_size += sizeof(Pint);      /* Pint num_vertices */
        args.el_size += vdata[i].num_vertices * vertex_size;
      }

      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
        ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
        args.el_data = PHG_SCRATCH.buf;
        data = (Pint *) args.el_data;
        data[0] = fflag;
        data = &data[1];
        data[0] = eflag;
        data = &data[1];
        data[0] = vflag;
        data = &data[1];
        data[0] = colr_type;
        tp = (char *) &data[1];

        switch(fflag) {
        case PFACET_COLOUR:
          memcpy(tp, &fdata->colr, sizeof(Pcoval));
          tp += sizeof(Pcoval);
          break;

        case PFACET_NORMAL:
          memcpy(tp, &fdata->norm, sizeof(Pvec3));
          tp += sizeof(Pvec3);
          break;

        case PFACET_COLOUR_NORMAL:
          memcpy(tp, &fdata->conorm, sizeof(Pconorm3));
          tp += sizeof(Pconorm3);
          break;

        default:
          break;
        }

        data = (Pint *) tp;
        data[0] = nfa;
        tp = (char *) &data[1];

        if (eflag == PEDGE_VISIBILITY) {
          for (i = 0; i < nfa; i++) {
            num_vertices = edata[i].num_edges;
            data = (Pint *) tp;
            data[0] = num_vertices;
            tp = (char *) &data[1];
            memcpy(tp, edata[i].edgedata.edges,
                   sizeof(Pedge_flag) * num_vertices);
            tp += sizeof(Pedge_flag) * num_vertices;
          }
        }

        for (i = 0; i < nfa; i++) {
          num_vertices = vdata[i].num_vertices;

          data = (Pint *) tp;
          data[0] = num_vertices;
          tp = (char *) &data[1];

          switch (vflag) {
          case PVERT_COORD:
            memcpy(tp, vdata[i].vertex_data.points,
                   num_vertices * sizeof(Ppoint));
            tp += num_vertices * sizeof(Ppoint);
            break;

          case PVERT_COORD_COLOUR:
            memcpy(tp, vdata[i].vertex_data.ptcolrs,
                   num_vertices * sizeof(Pptco3));
            tp += num_vertices * sizeof(Pptco3);
            break;

          case PVERT_COORD_NORMAL:
            memcpy(tp, vdata[i].vertex_data.ptnorms,
                   num_vertices * sizeof(Pptnorm3));
            tp += num_vertices * sizeof(Pptnorm3);
            break;

          case PVERT_COORD_COLOUR_NORMAL:
            memcpy(tp, vdata[i].vertex_data.ptconorms,
                   num_vertices * sizeof(Pptconorm3));
            tp += num_vertices * sizeof(Pptconorm3);
            break;

          default:
            break;
          }
        }
        phg_add_el(PHG_CSS, &args);
      }
    }
  }
}

