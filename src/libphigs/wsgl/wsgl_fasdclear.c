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
******************************************************************************
* Changes:   Copyright (C) 2022-2023 CERN
******************************************************************************/

/* Derived from the 3d version */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <GL/gl.h>

#include "phg.h"
#include "private/phgP.h"
#include "ws.h"
#include "private/wsglP.h"
#include "private/fasd3P.h"

/*******************************************************************************
 * priv_clear_area_points
 *
 * DESCR:	Clear fill area with point data 3D helper function
 * RETURNS:	N/A
 */

static void priv_clear_area_points(
                                   Pint num_vertices,
                                   Ppoint3 *points
                                   )
{
  Pint i;
  int vertex_indices[MAX_VERTICES];
  int n_vertices = 0;

  glBegin(GL_POLYGON);
  for (i = 0; i < num_vertices; i++) {
    glVertex3f(points[i].x,
               points[i].y,
               0.);
    if (record_geom){
      wsgl_add_vertex(points[i].x,
                      points[i].y,
                      0.);
      n_vertices ++;
    }
  }
  if (record_geom){
    wsgl_add_geometry(GEOM_FACE, vertex_indices, n_vertices);
  }
  glEnd();
}

/*******************************************************************************
 * priv_clear_area_ptcolrs
 *
 * DESCR:	Clear fill area with point and colour data 3D helper function
 * RETURNS:	N/A
 */

static void priv_clear_area_ptcolrs(
                                    Pint num_vertices,
                                    Pptco3 *ptcolrs
                                    )
{
  Pint i;
  int vertex_indices[MAX_VERTICES];
  int n_vertices = 0;

  glBegin(GL_POLYGON);
  for (i = 0; i < num_vertices; i++) {
    glVertex3f(ptcolrs[i].point.x,
               ptcolrs[i].point.y,
               0.0);
    if (record_geom){
      wsgl_add_vertex(ptcolrs[i].point.x,
                      ptcolrs[i].point.y,
                      0.0);
      n_vertices ++;
    }
  }
  if (record_geom){
    wsgl_add_geometry(GEOM_FACE, vertex_indices, n_vertices);
  }
  glEnd();
}

/*******************************************************************************
 * priv_clear_area_ptnorms
 *
 * DESCR:	Clear fill area with point and normal data 3D helper function
 * RETURNS:	N/A
 */

static void priv_clear_area_ptnorms(
                                    Pint num_vertices,
                                    Pptnorm3 *ptnorms
                                    )
{
  Pint i;
  int vertex_indices[MAX_VERTICES];
  int n_vertices = 0;

  glBegin(GL_POLYGON);
  for (i = 0; i < num_vertices; i++) {
    glVertex3f(ptnorms[i].point.x,
               ptnorms[i].point.y,
               0.0);
    if (record_geom){
      wsgl_add_vertex(ptnorms[i].point.x,
                      ptnorms[i].point.y,
                      0.0);
      n_vertices ++;
    }
  }
  if (record_geom){
    wsgl_add_geometry(GEOM_FACE, vertex_indices, n_vertices);
  }
  glEnd();
}

/*******************************************************************************
 * priv_clear_area_ptconorms
 *
 * DESCR:	Clear fill area with point, colour and normal data 3D
 *              helper function
 * RETURNS:	N/A
 */

static void priv_clear_area_ptconorms(
                                      Pint num_vertices,
                                      Pptconorm3 *ptconorms
                                      )
{
  Pint i;
  int vertex_indices[MAX_VERTICES];
  int n_vertices = 0;

  glBegin(GL_POLYGON);
  for (i = 0; i < num_vertices; i++) {
    glVertex3f(ptconorms[i].point.x,
               ptconorms[i].point.y,
               0.0);
    if (record_geom){
      wsgl_add_vertex(ptconorms[i].point.x,
                      ptconorms[i].point.y,
                      0.0);
      n_vertices ++;
    }
  }
  if (record_geom){
    wsgl_add_geometry(GEOM_FACE, vertex_indices, n_vertices);
  }
  glEnd();
}

/*******************************************************************************
 * wsgl_clear_area_set3_data
 *
 * DESCR:	Clear fill area set with data 3D
 * RETURNS:	N/A
 */

void wsgl_clear_area_set_data(
                              Ws *ws,
                              void *pdata,
                              Ws_attr_st *ast
                              )
{
  Pint i;
  Pfasd3 fasd3;
  Pedge_data_list edata;
  Pfacet_vdata_list3 vdata;

  fasd3.edata = &edata;
  fasd3.vdata = &vdata;
  fasd3_head(&fasd3, pdata);

  glPolygonOffset(WS_CLEAR_AREA_OFFSET, wsgl_get_edge_width(ast));
  glEnable(GL_POLYGON_OFFSET_FILL);
  wsgl_setup_background(ws);

  switch (fasd3.vflag) {
  case PVERT_COORD:
    for (i = 0; i < fasd3.nfa; i++) {
      priv_clear_area_points(fasd3.vdata->num_vertices,
                             fasd3.vdata->vertex_data.points);

      /* Advance to next set of data */
      fasd3_next_vdata3(&fasd3);
      if (fasd3.eflag == PEDGE_VISIBILITY) {
        fasd3_next_edata(&fasd3);
      }
    }
    break;

  case PVERT_COORD_COLOUR:
    for (i = 0; i < fasd3.nfa; i++) {
      priv_clear_area_ptcolrs(fasd3.vdata->num_vertices,
                              fasd3.vdata->vertex_data.ptcolrs);

      /* Advance to next set of data */
      fasd3_next_vdata3(&fasd3);
      if (fasd3.eflag == PEDGE_VISIBILITY) {
        fasd3_next_edata(&fasd3);
      }
    }
    break;

  case PVERT_COORD_NORMAL:
    for (i = 0; i < fasd3.nfa; i++) {
      priv_clear_area_ptnorms(fasd3.vdata->num_vertices,
                              fasd3.vdata->vertex_data.ptnorms);

      /* Advance to next set of data */
      fasd3_next_vdata3(&fasd3);
      if (fasd3.eflag == PEDGE_VISIBILITY) {
        fasd3_next_edata(&fasd3);
      }
    }
    break;

  case PVERT_COORD_COLOUR_NORMAL:
    for (i = 0; i < fasd3.nfa; i++) {
      priv_clear_area_ptconorms(fasd3.vdata->num_vertices,
                                fasd3.vdata->vertex_data.ptconorms);

      /* Advance to next set of data */
      fasd3_next_vdata3(&fasd3);
      if (fasd3.eflag == PEDGE_VISIBILITY) {
        fasd3_next_edata(&fasd3);
      }
    }
    break;

  default:
    break;
  }
  glDisable(GL_POLYGON_OFFSET_FILL);
}
