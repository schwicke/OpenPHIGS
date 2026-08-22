/******************************************************************************
 *   DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
 *
 *   This file is part of Open PHIGS
 *   Copyright (C) 2026 CERN
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
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xlib.h>
#ifdef GLEW
#include <GL/glew.h>
#else
#include <epoxy/gl.h>
#include <epoxy/glx.h>
#endif

#include "phg.h"
#include "private/phgP.h"
#include "ws.h"
#include "private/wsglP.h"

static char* data;
static GLuint head_p_texture;
static GLuint head_p_initializer;
static GLuint acounter_buffer;
static GLuint frag_storage_buffer;
/*******************************************************************************
 * wsgl_oir_ini
 *
 * DESCR:       Initialise Order Independent Rendering
 *              Called when opening the workstation. 
 * RETURNS:     N/A
 * BUGS:        Possible conflicts in case of serveral workstations ?
 */
void wsgl_oir_ini(Pint width, Pint height){
  size_t n_pixels = width * height;
  glGenTextures(1, &head_p_texture);
  glBindTexture(GL_TEXTURE_2D, head_p_texture);
  glTexImage2D(GL_TEXTURE_2D, 0,
               GL_R32UI,
               width, height,
               0,
               GL_RED_INTEGER,
               GL_UNSIGNED_INT,
               NULL
               );
  glGenBuffers(1, &head_p_initializer);
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, head_p_initializer);
  glBufferData(GL_PIXEL_UNPACK_BUFFER, n_pixels* sizeof(GLuint), NULL, GL_STATIC_DRAW);
  data = (char*)glMapBuffer(GL_PIXEL_UNPACK_BUFFER, GL_WRITE_ONLY);
  memset(data, 0xFF, n_pixels*sizeof(GLuint));
  glUnmapBuffer(GL_PIXEL_UNPACK_BUFFER);

  glGenBuffers(1, &acounter_buffer);
  glBindBuffer(GL_ATOMIC_COUNTER_BUFFER, acounter_buffer);
  glBufferData(GL_ATOMIC_COUNTER_BUFFER, sizeof(GLuint), NULL, GL_DYNAMIC_COPY);

  glGenBuffers(1, &frag_storage_buffer);
  glBindBuffer(GL_TEXTURE_BUFFER, frag_storage_buffer);
  glBufferData(GL_TEXTURE_BUFFER, 2*n_pixels*4*sizeof(GLfloat), NULL, GL_DYNAMIC_COPY);
}

/*******************************************************************************
 * wsgl_oir_reset
 *
 * DESCR:       Reset Order Independent Rendering
 *              Called for each new frame 
 * RETURNS:     N/A
 * BUGS:        Possible conflicts in case of serveral workstations ?
 */
void wsgl_oir_reset(Pint width, Pint height){
  glBindBuffer(GL_PIXEL_UNPACK_BUFFER, head_p_initializer);
  glBindTexture(GL_TEXTURE_2D, head_p_initializer);
  glTexImage2D(GL_TEXTURE_2D, 0,
               GL_R32UI,
               width, height,
               0,
               GL_RED_INTEGER,
               GL_UNSIGNED_INT,
               NULL );
  glBindImageTexture(0,
                     head_p_texture,
                     0,
                     GL_FALSE,
                     0,
                     GL_READ_WRITE,
                     GL_R32UI);
  glBindBufferBase(GL_ATOMIC_COUNTER_BUFFER, 0, acounter_buffer);
  const GLuint zero = 0;
  glBufferSubData(GL_ATOMIC_COUNTER_BUFFER, 0, sizeof(zero), &zero);
}
