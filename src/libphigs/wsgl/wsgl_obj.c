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

Ppoint3 * vertices = NULL;
Ppoint3 * normals = NULL;
int vertex_count = 0;
int normal_count = 0;

Geometry* geometries = NULL;
int geom_count = 0;
Ppoint3 current_normal;

int record_geom = FALSE;
int record_geom_fill = TRUE;
int normal_valid = FALSE;

/*******************************************************************************
 * wsgl_set_current_normal(float x, float y, float z)
 *
 * DESCR:       add 3d vertex
 * RETURNS:     Non zero or zero on error
 */
void wsgl_set_current_normal(float x, float y, float z) {
  current_normal.x = x;
  current_normal.y = y;
  current_normal.z = z;
  normal_valid = TRUE;
};

/*******************************************************************************
 * wsgl_add_vertex(float x, float y, float z)
 *
 * DESCR:       add 3d vertex
 * RETURNS:     Non zero or zero on error
 */
int wsgl_add_vertex(float x, float y, float z) {
  vertices = realloc(vertices, sizeof(Ppoint3) * (vertex_count + 1));
  if (!vertices) {
    fprintf(stderr, "Out of memory adding vertex\n");
    exit(1);
  }
  vertices[vertex_count].x = x;
  vertices[vertex_count].y = y;
  vertices[vertex_count].z = z;
#ifdef DEBUG_OBJ
  printf("wsgl_obj: vertex count is %d \n", vertex_count +1);
#endif
  return ++vertex_count;
}

/*******************************************************************************
 * wsgl_add_normal(float x, float y, float z)
 *
 * DESCR:       add 3d vertex
 * RETURNS:     Non zero or zero on error
 */
int wsgl_add_normal(float x, float y, float z) {
  normals = realloc(normals, sizeof(Ppoint3) * (normal_count + 1));
  if (!normals) {
    fprintf(stderr, "Out of memory adding normal\n");
    exit(1);
  }
  
  normals[normal_count].x = current_normal.x;
  normals[normal_count].y = current_normal.y;
  normals[normal_count].z = current_normal.z;
#ifdef DEBUG_OBJ
  printf("wsgl_obj: normal count is %d \n", vertex_count +1);
#endif
  return ++normal_count;
}

/*******************************************************************************
 * wsgl_add_geometry(float x, float y, float z)
 *
 * DESCR:       add 3d geometry
 * RETURNS:     Non zero or zero on error
 */
void wsgl_add_geometry(GeomType type, const int* verts, const int* norms, int count) {
  geometries = realloc(geometries, sizeof(Geometry) * (geom_count + 1));
  if (!geometries) {
    fprintf(stderr, "Out of memory adding geometry\n");
    exit(1);
  }
#ifdef DEBUG_OBJ
  printf("wsgl_obj: adding geometry with %d vertices\n", count);
#endif
  geometries[geom_count].type = type;
  geometries[geom_count].count = count;
  geometries[geom_count].indices = malloc(sizeof(int) * count);
  memcpy(geometries[geom_count].indices, verts, sizeof(int) * count);
  if (norms != NULL){
    geometries[geom_count].norms = malloc(sizeof(int) * count);
    memcpy(geometries[geom_count].norms, norms, sizeof(int) * count);
  }
  else
    geometries[geom_count].norms = NULL;
  geom_count++;
}

/*******************************************************************************
 * wsgl_export_obj(const char* filename)
 * DESCR:       export as OBJ file
 * RETURNS:     Non zero or zero on error
 */
void wsgl_export_obj(const char* filename, const char* title) {
  FILE* f = fopen(filename, "w");
  if (!f) {
    perror("fopen");
    return;
  }
#ifdef DEBUG_OBJ
  printf("wsgl_obj: exporting %d vertices and %d normals\n", vertex_count, normal_count);
#endif
  fprintf(f, "#name:%s\n", title);
  for (int i = 0; i < normal_count; ++i) {
    fprintf(f, "vn %f %f %f\n",
            normals[i].x,
            normals[i].y,
            normals[i].z);
  }
  for (int i = 0; i < vertex_count; ++i) {
    fprintf(f, "v %f %f %f\n",
            vertices[i].x,
            vertices[i].y,
            vertices[i].z);
  }
  
  for (int i = 0; i < geom_count; ++i) {
    switch (geometries[i].type) {
    case GEOM_FACE: 
      fprintf(f, "f");
      for (int j = 0; j < geometries[i].count; ++j)
        fprintf(f, " %d//%d", geometries[i].indices[j], geometries[i].norms[j]);
        break;
    case GEOM_LINE:
      fprintf(f, "l");
      for (int j = 0; j < geometries[i].count; ++j)
        fprintf(f, " %d", geometries[i].indices[j]);
      break;
    default:
      printf("ERROR: Unknown geometry type detected. Ignoring");
      continue;
    }
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
#ifdef DEBUG_OBJ
  printf("wsgl_obj: cleaning geometry\n");
  if (record_geom)
    printf("wsgl_obj: Recording is ON\n");
  else 
    printf("wsgl_obj: Recording is OFF\n");
#endif    
  if (vertices != NULL){
    free(vertices);
    vertices = NULL;
    vertex_count = 0;
#ifdef DEBUG_OBJ
  } else {
    printf("wsgl_obj: no vertices found\n");
#endif
  }
  if (normals != NULL){
    free(normals);
    normals = NULL;
    normal_count = 0;
#ifdef DEBUG_OBJ
  } else {
    printf("wsgl_obj: no normals found\n");
#endif
  }
  if (geom_count > 0){
    for (int i = 0; i < geom_count; ++i)
      free(geometries[i].indices);
    free(geometries);
    geometries = NULL;
    geom_count = 0;
#ifdef DEBUG_OBJ
  } else {
    printf("wsgl_obj: no geometries found\n");
#endif
  }
  normal_valid = FALSE;
  current_normal.x = 0.0;
  current_normal.y = 0.0;
  current_normal.z = 1.0;
}
