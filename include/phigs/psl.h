/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium
Copyright (c) 2014 Surplus Users Ham Society

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of
the software without specific, written prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

#ifndef _psl_h
#define _psl_h

#include "phigs.h"
#include "ws_type.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_NO_OPEN_WS                         10
#define MAX_NO_OPEN_ARFILES                    10

typedef struct {
   Pint       used;                            /* Mark if workstation is used */
   Pint       wsid;                            /* Workstation id */
   char       *connid;                         /* Connection identity */
   Wst        *wstype;                         /* Workstation type */
} Psl_ws_info;

typedef struct {
   Pint used;
   char *fname;
   Pint arid;
} Psl_ar_info;

typedef struct {
   Pevent             id;
   Phg_inp_event_data data;
} Psl_inp_event;

typedef struct {
   Psys_st       phg_sys_state;                 /* System state */
   Pws_st        phg_ws_state;                  /* Workstation state */
   Pstruct_st    phg_struct_state;              /* Structure state */
   Par_st        phg_ar_state;                  /* Archive state */
   Psl_ws_info   open_ws[MAX_NO_OPEN_WS];       /* Info list for workstations */
   Psl_ar_info   ar_files[MAX_NO_OPEN_ARFILES]; /* Info list for archives */
   Pconf_res     ar_res;                        /* Archive confl. resolution */
   Pconf_res     retr_res;                      /* Retreive confl. resolution */
   Psl_inp_event cur_event;                     /* Event got by pwait_event */
   Pedit_mode    edit_mode;                     /* Structure edit mode */
   Pint          open_struct;                   /* Open structure */
} Phg_state_list;

typedef Phg_state_list *Psl_handle;

#define PSL_SYS_STATE(_psl) \
   ((_psl)->phg_sys_state)

#define PSL_WS_STATE(_psl) \
   ((_psl)->phg_ws_state)

#define PSL_STRUCT_STATE(_psl) \
   ((_psl)->phg_struct_state)

#define PSL_AR_STATE(_psl) \
   ((_psl)->phg_ar_state)

#define PSL_ARCHIVE_CONFLICT(_psl) \
    ((_psl)->ar_res)

#define PSL_RETRIEVE_CONFLICT(_psl) \
    ((_psl)->retr_res)

#define PSL_SET_CUR_EVENT_ID(_psl, _id) \
   ((_psl)->cur_event.id = (_id))

#define PSL_SET_CUR_EVENT_DATA(_psl, _data) \
   ((_psl)->cur_event.data = (_data))

#define PSL_CUR_EVENT_CLASS(_psl) \
   ((_psl)->cur_event.id.in_class)

#define PSL_CUR_EVENT_DATA(_psl, _class) \
   ((_psl)->cur_event.data._class)

#define PSL_CLEAR_CUR_EVENT( _psl) \
    {   switch ((_psl)->cur_event.id.in_class) { \
          case PIN_STROKE: \
            if ( (_psl)->cur_event.data.stk.num_points > 0) \
                free(((_psl)->cur_event.data.stk.points)); \
            break; \
          case PIN_PICK: \
            if ( (_psl)->cur_event.data.pik.status == PIN_STATUS_OK \
                && (_psl)->cur_event.data.pik.pick_path.depth > 0) \
                free(((_psl)->cur_event.data.pik.pick_path.path_list)); \
            break; \
          case PIN_STRING: \
            if ( (_psl)->cur_event.data.str.length > 0) \
                free(((_psl)->cur_event.data.str.string)); \
            break; \
          default: \
            break; \
        } \
        (_psl)->cur_event.id.in_class = PIN_NONE; \
    }

#define PSL_OPEN_STRUCT(_psl) \
   ((_psl)->open_struct)

#define PSL_EDIT_MODE(_psl) \
   ((_psl)->edit_mode)

/*******************************************************************************
 * phg_psl_create
 *
 * DESCR:       Create PHIGS state list
 * RETURNS:     Pointer to state list or NULL
 */

Psl_handle phg_psl_create(
   void
   );

/*******************************************************************************
 * phg_psl_init
 *
 * DESCR:       Initialize PHIGS state list
 * RETURNS:     TRUE or FALSE
 */

int phg_psl_init(
   Psl_handle psl
   );

/*******************************************************************************
 * phg_psl_destroy
 *
 * DESCR:       Destroy PHIGS state list
 * RETURNS:     N/A
 */

void phg_psl_destroy(
   Psl_handle psl
   );

/*******************************************************************************
 * phg_psl_inq_ws_open
 *
 * DESCR:       Inquire open workstation
 * RETURNS:     TRUE or FALSE
 */

int phg_psl_inq_ws_open(
   Psl_handle psl,
   Pint wsid
   );

/*******************************************************************************
 * phg_psl_inq_wsids
 *
 * DESCR:       Inquire workstation ids
 * RETURNS:     Number of open workstations
 */

int phg_psl_inq_wsids(
   Psl_handle psl,
   Pint *wsids
   );

/*******************************************************************************
 * phg_psl_get_ws_info
 * 
 * DESCR:       Get open workstation information
 * RETURNS:     Pointer to information structure
 */

Psl_ws_info* phg_psl_get_ws_info(
   Psl_handle psl,
   Pint wsid
   );

/*******************************************************************************
 * phg_psl_ws_free_slot 
 *
 * DESCR:       Find if there is a free slot for a workstation
 * RETURNS:     TRUE or there is a slot free otherwise FALSE
 */

int phg_psl_ws_free_slot(
   Psl_handle psl
   );

/*******************************************************************************
 * phg_psl_add_ws
 *
 * DESCR:       Add workstation to list
 * RETURNS:     TRUE if wsid could be added, else FALSE
 */

int phg_psl_add_ws(
   Psl_handle psl,
   Pint wsid,
   char *connid,
   Wst *type
   );

/*******************************************************************************
 * phg_psl_rem_ws
 *
 * DESCR:       Remove workstation from list
 * RETURNS:     N/A
 */

void phg_psl_rem_ws(
   Psl_handle psl,
   Pint wsid
   );

/*******************************************************************************
 * phg_psl_inq_ar_open
 *
 * DESCR:       Check if archive is already open
 * RETURNS:     Non-zero on open, otherwise zero
 */

int phg_psl_inq_ar_open(
   Psl_handle psl,
   Pint       arid
   );

/*******************************************************************************
 * phg_psl_get_ar_info
 *
 * DESCR:       Get information about archive
 * RETURNS:     Pointer to archive info structure or NULL
 */

Psl_ar_info* phg_psl_get_ar_info(
   Psl_handle psl,
   Pint arid
   );

/*******************************************************************************
 * phg_psl_ar_free_slot
 *
 * DESCR:       Check if there is a free slot for archive
 * RETURNS:     Non-zero success, otherwise zero
 */

int phg_psl_ar_free_slot(
   Psl_handle psl
   );

/*******************************************************************************
 * phg_psl_add_ar
 *
 * DESCR:       Add achive to list
 * RETURNS:     Non-zero success, otherwise zero
 */

int phg_psl_add_ar(
   Psl_handle psl,
   Pint arid,
   char *fname
   );

/*******************************************************************************
 * phg_psl_rem_ar
 *
 * DESCR:       Remove archive from list
 * RETURNS:     N/A
 */

void phg_psl_rem_ar(
   Psl_handle psl,
   Pint arid
   );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _psl_h */

