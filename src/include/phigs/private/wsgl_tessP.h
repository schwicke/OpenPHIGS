/******************************************************************************
*   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
*
*   This file is part of Open PHIGS
*   Copyright (C) 2026 Paramveer Singh
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

#ifndef _wsgl_tessP_h
#define _wsgl_tessP_h

#include <GL/gl.h>
#include <GL/glu.h>
#include "phg.h"

typedef struct {
    GLdouble pos[3];

    /* Callback for applying per-vertex attributes (like color/normal) */
    void (*apply_cb)(void *data);
    void *cb_data;
    
    /* Variables to avoid dynamic allocation for cb_data in simple cases */
    Ws *ws;
    Pint colr_type;
    Pcoval colr;
    Ws_attr_st *ast;
    
    int has_norm;
    Pfloat norm[3];
    
} Wsgl_tess_vertex;

typedef struct {
   Wsgl_tess_vertex **verts;
   int count;
   int capacity;
} Wsgl_tess_pool;

void wsgl_draw_tess_polygon(Wsgl_tess_vertex *vertices, int num_vertices, int record_geom_flag);

#endif
