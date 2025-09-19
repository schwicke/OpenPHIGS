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

#ifndef _err_h
#define _err_h

#include "phigs.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    Pint errnum;
    Pint funcnum;
} Err_msg;

typedef struct {
    char *fname;
} Err_local_data;

typedef struct {
    int  sfd;
    char *fname;
} Err_client_data;

typedef struct {
    int sfd;
} Err_remote_data;

typedef struct _Err_struct {
    Perr_mode mode;
    Perr_mode err_state;
    int       cur_func_num;
    void      (*buf_func)(struct _Err_struct *erh, Pint errnum);
    void      (*flush_func)(struct _Err_struct *erh);
    void      (*report_func)(struct _Err_struct *erh, Pint errnum);
    void      (*destroy_func)(struct _Err_struct *erh);
    union {
	Err_local_data  local;
	Err_client_data	client;
	Err_remote_data remote;
    } data;
} Err_struct;

typedef Err_struct *Err_handle;

  extern void (*phg_errhandle)(int, int, char*);

#define ERR_SET_CUR_FUNC(erh, funcnum) \
    ((erh)->cur_func_num = (funcnum))

#define ERR_BUF(erh, errnum) \
    if ((erh)->mode == PERR_ON) (*(erh)->buf_func)((erh), (errnum))

#define ERR_FLUSH(erh) \
    if ((erh)->flush_func) (*(erh)->flush_func)(erh)

#define ERR_HANDLE(errnum, funcnum, erf) \
    (*phg_errhandle)((errnum), (funcnum), (erf))

#define ERR_REPORT(erh, errnum) \
    if ((erh)->mode == PERR_ON && (erh)->report_func) \
       (*(erh)->report_func)((erh), (errnum))

#define ERR_DESTROY(erh) \
    if ((erh)->destroy_func) (*(erh)->destroy_func)(erh)

/*******************************************************************************
 * phg_erh_create
 *
 * DESCR:       Create error handler
 * RETURNS:     Pointer to handler or NULL
 */

Err_handle phg_erh_create(
   char *err_file
   );

/*******************************************************************************
 * phg_erh_init
 *
 * DESCR:       Initialize error handler
 * RETURNS:     TRUE or FALSE
 */

int phg_erh_init(
   Err_handle erh,
   char *err_file
   );

/*******************************************************************************
 * phg_erh_destroy
 * 
 * DESCR:       Initialize error handler
 * RETURNS:     TRUE or FALSE
 */
 
void phg_erh_destroy(
   Err_handle erh
   );

/*******************************************************************************
 * phg_err_store_name
 *
 * DESCR:       Store error filename, err_file == NULL means stderr
 * RETURNS:     TRUE or FALSE
 */

int phg_err_store_name(
   Err_handle erh,
   char *err_file,
   char **ptr
   );

/*******************************************************************************
 * phg_format_err_msg
 *
 * DESCR:       Create error message and place it in buffer
 * RETURNS:     N/A
 */

void phg_format_err_msg(
   Pint binding,
   Pint errnum,
   Pint funcnum,
   char *buf
   );

/*******************************************************************************
 * perr_log
 *
 * DESCR:       Log error
 * RETURNS:     N/A
 */

void perr_log(
   Pint errnum,
   Pint funcnum,
   char *fname
   );

/*******************************************************************************
 * pset_err_hand
 *
 * DESCR:       Set error handler
 * RETURNS:     N/A
 */

void pset_err_hand(
   void (*new_err_hand)(int,  int,  char *),
   void (**old_err_hand)(int,  int,  char *)
   );

/*******************************************************************************
 * perr_hand
 *
 * DESCR:       Handle error
 * RETURNS:     N/A
 */

void perr_hand(
   Pint errnum,
   Pint funcnum,
   char *fname
   );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _err_h */
