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
#include <phigs.h>
#include <phg.h>
#include <css.h>
#include <private/phgP.h>
#include <util/ftn.h>

/*******************************************************************************
 * pbltm3
 *
 * DESCR:       Build transformation matrix 3
 * RETURNS:     error index, transformation matrix
 */
FTN_SUBROUTINE(pbltm3)(
                       FTN_REAL(xo), FTN_REAL(yo), FTN_REAL(zo),
                       FTN_REAL(dx), FTN_REAL(dy), FTN_REAL(dz),
                       FTN_REAL(phix), FTN_REAL(phiy), FTN_REAL(phiz),
                       FTN_REAL(fx), FTN_REAL(fy), FTN_REAL(fz),
                       int* err_ind, Pfloat *xfrmt
                       )
{
  Ppoint3 opoint;
  Pvec3 dpoint;
  Pvec3 fpoint;
  int i, j;
  Pmatrix3 x;

#ifdef DEBUG
  printf("DEBUG: pbltm3 called\n");
#endif
  opoint.x = FTN_REAL_GET(xo);
  opoint.y = FTN_REAL_GET(yo);
  opoint.z = FTN_REAL_GET(zo);

  dpoint.delta_x = FTN_REAL_GET(dx);
  dpoint.delta_y = FTN_REAL_GET(dy);
  dpoint.delta_z = FTN_REAL_GET(dz);

  fpoint.delta_x = FTN_REAL_GET(fx);
  fpoint.delta_y = FTN_REAL_GET(fy);
  fpoint.delta_z = FTN_REAL_GET(fz);

  pbuild_tran_matrix3(&opoint, &dpoint,
                      FTN_REAL_GET(phix),
                      FTN_REAL_GET(phiy),
                      FTN_REAL_GET(phiz),
                      &fpoint,
                      err_ind, x);
  for (i=0; i<4; i++){
    for (j=0; j<4; j++){
      xfrmt[4*j+i] = (float)x[i][j];
    }
  }
#ifdef DEBUG
  printf("DEBUG: pbltm3 returned %d\n", *err_ind);
  if (*err_ind == 0){
    printf("Resulting matrix:");
    printf("    %f %f %f %f\n", xfrmt[0],xfrmt[1],xfrmt[2],xfrmt[3]    );
    printf("    %f %f %f %f\n", xfrmt[4],xfrmt[5],xfrmt[6],xfrmt[7]    );
    printf("    %f %f %f %f\n", xfrmt[8],xfrmt[9],xfrmt[10],xfrmt[11]  );
    printf("    %f %f %f %f\n", xfrmt[12],xfrmt[13],xfrmt[14],xfrmt[15]);
  }
#endif
}

/*******************************************************************************
 * pevmm3
 *
 * DESCR:       EVALUATE VIEW MAPPING MATRIX 3
 * RETURNS:     error index, view mapping matrix
 */
FTN_SUBROUTINE(pevmm3)(
                       FTN_REAL_ARRAY(vwwnlm),
                       FTN_REAL_ARRAY(pjvplm),
                       FTN_INTEGER(pjtype),
                       FTN_REAL(pjrx),
                       FTN_REAL(pjry),
                       FTN_REAL(pjrz),
                       FTN_REAL(vp),
                       FTN_REAL(bp),
                       FTN_REAL(fp),
                       int * err_ind,
                       Pfloat * vwmpmt
                       )
{
  Pproj_type proj_type;
  Pview_map3 map;
  int i, j;
  Pmatrix3 x;
#ifdef DEBUG
  printf("DEBUG: pevmm3 called\n");
#endif

  map.win.x_min = FTN_REAL_ARRAY_GET(vwwnlm, 0);
  map.win.x_max = FTN_REAL_ARRAY_GET(vwwnlm, 1);
  map.win.y_min = FTN_REAL_ARRAY_GET(vwwnlm, 2);
  map.win.y_max = FTN_REAL_ARRAY_GET(vwwnlm, 3);
  map.win.z_min = 0;
  map.win.z_max = 0;

  map.proj_vp.x_min = FTN_REAL_ARRAY_GET(pjvplm, 0);
  map.proj_vp.x_max = FTN_REAL_ARRAY_GET(pjvplm, 1);
  map.proj_vp.y_min = FTN_REAL_ARRAY_GET(pjvplm, 2);
  map.proj_vp.y_max = FTN_REAL_ARRAY_GET(pjvplm, 3);
  map.proj_vp.z_min = FTN_REAL_ARRAY_GET(pjvplm, 4);
  map.proj_vp.z_max = FTN_REAL_ARRAY_GET(pjvplm, 5);

  map.proj_type = FTN_INTEGER_GET(pjtype);

  map.proj_ref_point.x = FTN_REAL_GET(pjrx);
  map.proj_ref_point.y = FTN_REAL_GET(pjry);
  map.proj_ref_point.z = FTN_REAL_GET(pjrz);

  map.view_plane = FTN_REAL_GET(vp);
  map.back_plane = FTN_REAL_GET(bp);
  map.front_plane = FTN_REAL_GET(fp);

  peval_view_map_matrix3(&map, err_ind, x);
  for (i=0; i<4; i++){
    for (j=0; j<4; j++){
      vwmpmt[4*j+i] = (float)x[i][j];
    }
  }

#ifdef DEBUG
  printf("DEBUG: pevmm3 returned %d\n", *err_ind);
  if (*err_ind == 0){
    printf("Resulting matrix:");
    printf("    %f %f %f %f\n", vwmpmt[0],vwmpmt[1],vwmpmt[2],vwmpmt[3]    );
    printf("    %f %f %f %f\n", vwmpmt[4],vwmpmt[5],vwmpmt[6],vwmpmt[7]    );
    printf("    %f %f %f %f\n", vwmpmt[8],vwmpmt[9],vwmpmt[10],vwmpmt[11]  );
    printf("    %f %f %f %f\n", vwmpmt[12],vwmpmt[13],vwmpmt[14],vwmpmt[15]);
  }
#endif
}

/*******************************************************************************
 * pevom3
 *
 * DESCR:       Evaluate view orientation matrix 3
 * RETURNS:     Error index, orientation matrix
 */
FTN_SUBROUTINE(pevom3)(
                       FTN_REAL(vwrx), FTN_REAL(vwry), FTN_REAL(vwrz),
                       FTN_REAL(vpnx), FTN_REAL(vpny), FTN_REAL(vpnz),
                       FTN_REAL(vupx), FTN_REAL(vupy), FTN_REAL(vupz),
                       int* err_ind, Pfloat * vwormt
                       )
{
  Ppoint3 vrp;
  Pvec3 vpn;
  Pvec3 vup;
  int i, j;
  Pmatrix3 x;
#ifdef DEBUG
  printf("DEBUG: pevom3 called\n");
#endif
  vrp.x = FTN_REAL_GET(vwrx);
  vrp.y = FTN_REAL_GET(vwry);
  vrp.z = FTN_REAL_GET(vwrz);

  vpn.delta_x = FTN_REAL_GET(vpnx);
  vpn.delta_y = FTN_REAL_GET(vpny);
  vpn.delta_z = FTN_REAL_GET(vpnz);

  vup.delta_x = FTN_REAL_GET(vupx);
  vup.delta_y = FTN_REAL_GET(vupy);
  vup.delta_z = FTN_REAL_GET(vupz);

  peval_view_ori_matrix3(&vrp, &vpn, &vup, err_ind, x);
  for (i=0; i<4; i++){
    for (j=0; j<4; j++){
      vwormt[4*j+i] = (float)x[i][j];
    }
  }
#ifdef DEBUG
  printf("DEBUG: pevom3 returned %d\n", *err_ind);
  if (*err_ind == 0){
    printf("Resulting matrix:");
    printf("    %f %f %f %f\n", vwormt[0], vwormt[1], vwormt[2], vwormt[3]    );
    printf("    %f %f %f %f\n", vwormt[4], vwormt[5], vwormt[6], vwormt[7]    );
    printf("    %f %f %f %f\n", vwormt[8], vwormt[9], vwormt[10],vwormt[11]  );
    printf("    %f %f %f %f\n", vwormt[12],vwormt[13],vwormt[14],vwormt[15]);
  }
#endif
}

/*******************************************************************************
 * pslmt3
 *
 * DESCR:       SET LOCAL TRANSFORMATION 3
 * RETURNS:     N/A
 */
FTN_SUBROUTINE(pslmt3)(
                       float * xfrmt,
                       FTN_INTEGER(ctype)
                       )
{
  Phg_args_add_el args;
  Pint *data;
  int i, j;
  Pmatrix3 x;
#ifdef DEBUG
  printf("DEBUG: pslmt3 called\n");
#endif
  for (i=0; i<4; i++){
    for (j=0; j<4; j++){
      x[j][i] = xfrmt[4*i+j];
    }
  }
  if (phg_entry_check(PHG_ERH, 0, Pfn_set_local_tran3)) {
    if (PSL_STRUCT_STATE(PHG_PSL) != PSTRUCT_ST_STOP) {
      ERR_REPORT(PHG_ERH, ERR5);
    }
    else {
      args.el_type = PELEM_LOCAL_MODEL_TRAN3;
      args.el_size = sizeof(Pint) + 16 * sizeof(Pfloat);
      if (!PHG_SCRATCH_SPACE(&PHG_SCRATCH, args.el_size)) {
        ERR_REPORT(PHG_ERH, ERR900);
      }
      else {
        args.el_data = PHG_SCRATCH.buf;
        data = (Pint *) args.el_data;
        data[0] = (Pint) FTN_INTEGER_GET(ctype);
        phg_mat_unpack((Pfloat *) &data[1], x);
        phg_add_el(PHG_CSS, &args);
      }
    }
  }
}
