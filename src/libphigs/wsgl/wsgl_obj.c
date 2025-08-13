/******************************************************************************
*   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
*
*   This file is part of Open PHIGS
*   Copyright (C) 2025 CERN
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
* Code inspired by OpenAI ChatGPT generated sample code
******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#ifdef GLEW
#include <GL/glew.h>
#include <GL/glx.h>
#else
#include <epoxy/gl.h>
#include <epoxy/glx.h>
#endif
#include "phg.h"
#include "private/phgP.h"
#include "private/wsglP.h"

/*******************************************************************************
 * wsgl_add_vertex(float x, float y, float z)
 *
 * DESCR:       add 3d vertex
 * RETURNS:     Non zero or zero on error
 */
int wsgl_add_vertex(float x, float y, float z) {
    vertices = realloc(vertices, sizeof(float) * 3 * (vertex_count + 1));
    if (!vertices) {
        fprintf(stderr, "Out of memory adding vertex\n");
        exit(1);
    }
    vertices[3 * vertex_count + 0] = x;
    vertices[3 * vertex_count + 1] = y;
    vertices[3 * vertex_count + 2] = z;
    return ++vertex_count; // OBJ indices are 1-based
}

/*******************************************************************************
 * wsgl_add_geometry(float x, float y, float z)
 *
 * DESCR:       add 3d geometry
 * RETURNS:     Non zero or zero on error
 */
void wsgl_add_geometry(GeomType type, const int* verts, int count) {
    geometries = realloc(geometries, sizeof(Geometry) * (geom_count + 1));
    if (!geometries) {
        fprintf(stderr, "Out of memory adding geometry\n");
        exit(1);
    }

    geometries[geom_count].type = type;
    geometries[geom_count].count = count;
    geometries[geom_count].indices = malloc(sizeof(int) * count);
    memcpy(geometries[geom_count].indices, verts, sizeof(int) * count);
    geom_count++;
}

/*******************************************************************************
 * wsgl_export_obj(const char* filename)
 * DESCR:       export as OBJ file
 * RETURNS:     Non zero or zero on error
 */
void wsgl_export_obj(const char* filename) {
  FILE* f = fopen(filename, "w");
  if (!f) {
    perror("fopen");
    return;
  }

  for (int i = 0; i < vertex_count; ++i) {
    fprintf(f, "v %f %f %f\n",
            vertices[i * 3 + 0],
            vertices[i * 3 + 1],
            vertices[i * 3 + 2]);
  }

  for (int i = 0; i < geom_count; ++i) {
    fprintf(f, (geometries[i].type == GEOM_FACE) ? "f" : "l");
    for (int j = 0; j < geometries[i].count; ++j)
      fprintf(f, " %d", geometries[i].indices[j]);
    fprintf(f, "\n");
  }

  fclose(f);
}

/*******************************************************************************
 * wsgl_clear_geometry()
 *
 * DESCR:       cleanup geometry records
 * RETURNS:     Non zero or zero on error
 */
void wsgl_clear_geometry() {
  if (vertices != NULL){
    free(vertices);
    vertices = NULL;
    vertex_count = 0;
  }
  if (geom_count > 0){
    for (int i = 0; i < geom_count; ++i)
      free(geometries[i].indices);
    free(geometries);
    geometries = NULL;
    geom_count = 0;
  }
}
