/* $XConsortium: ws_pm.c,v 5.4 94/04/17 20:42:29 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium
Copyright (c) 2022-2023 CERN

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

/* PEX/PHIGS workstation utility functions for the A model (server side
 * workstations and structure storage).
 */

#include <stdio.h>
#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "private/wsxP.h"
#include <X11/IntrinsicI.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#ifdef MOTIF
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/PushB.h>
#else
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#endif

int done_pressed = 0;

void done_button( Widget w,
		  XtPointer client_data,
		  XtPointer call_data )
{
  Ws *ws = (Ws *)client_data;
#ifdef DEBUGINP
  printf("Done button pressed\n");
#endif
  done_pressed = 1;
  XtPopdown( ws->msg_shell );
}

void create_message_win( Ws *ws )
{
  Widget frame, pane, box, text, button;

#ifdef DEBUGINP
  printf("Creating message window\n");
#endif
  /* Create the containing shell. */
  ws->msg_shell = XtVaCreatePopupShell( "message",
					applicationShellWidgetClass, ws->top_level, NULL );
#ifdef DEBUGINP
  printf("Called XtVaCreatePopupShell. msg_shell is %ld\n", (long)ws->msg_shell);
#endif
#ifdef MOTIF
  frame =
    XtVaCreateManagedWidget( "frame", xmFrameWidgetClass, ws->msg_shell,
			     NULL );
  pane =
    XtVaCreateManagedWidget( "pane", xmPanedWindowWidgetClass, frame,
			     XtNwidth, 400,
			     XtNheight, 50,
			     NULL );
  ws->msg_label = XtVaCreateManagedWidget("message",
					  xmLabelWidgetClass, pane,
					  XmNheight, 60,
					  NULL);
  button = XtVaCreateManagedWidget("Close", xmPushButtonWidgetClass, pane,
				   NULL);
  XtAddCallback( button, XmNactivateCallback, done_button, (XtPointer)ws );

#else
  /* Create the containing box. */
  box = XtVaCreateManagedWidget( "box", boxWidgetClass, ws->msg_shell,
				 XtNwidth, 400, XtNheight, 50,
				 NULL );
  /* Create the done button. */
  button = XtVaCreateManagedWidget( "button", commandWidgetClass, box,
				    XtNlabel, "OK", NULL );
  XtAddCallback( button, XtNcallback, done_button, (XtPointer)ws );

  /* Create the label. */
  ws->msg_label = XtVaCreateManagedWidget( "label", labelWidgetClass, box,
					   NULL );
#endif
}

void phg_wsb_message(
		     Ws  *ws,
		     Phg_args_message *args )
{
  int status;
  XtInputMask m = XtIMAll;
  XtInputMask t;
  if ( !ws->msg_shell ){
    create_message_win( ws );
#ifdef DEBUGINP
    printf("Created message window.\n");
#endif
  }

  if ( args->msg_length > 0 ) {
    /* The unmanage and manage is to get the parent box to allow the
     * label resize.
     */
#ifdef DEBUGINP
    printf("Setting message window parameters. Message is %s\n", args->msg);
#endif
    XtUnmanageChild( ws->msg_label );
#ifdef MOTIF
    XmString btext = XmStringCreateSimple(args->msg);
    XtVaSetValues( ws->msg_label,
		   XmNlabelString, btext,
		   XmNlabelType, XmSTRING,
		   NULL );
    XmStringFree(btext);
#else
    XtVaSetValues( ws->msg_label, XtNlabel, (XtArgVal)args->msg, NULL );
#endif
    XtManageChild( ws->msg_label );
    XtPopup( ws->msg_shell, XtGrabNone );
#ifdef DEBUGINP
    printf("Popup done.\n");
#endif
  }
  phg_wsx_update_ws_rect( ws );
#ifdef DEBUGINP
  printf("DEBUG: ws_pm: entering main loop\n");
#endif
  done_pressed = 0;
  XtAppContext app = _XtDefaultAppContext();
  do {
    if (m == 0) {
      m = XtIMAll;
      /* wait for any event, blocking */
      XtAppProcessEvent(app, m);
    }
    else if (((t = XtAppPending(app)) & m)) {
      /* wait for certain events, stepping through choices */
      XtAppProcessEvent(app, t & m);
    }
    m >>= 1;
  } while (done_pressed == 0);
  done_pressed = 0;
}

#ifdef TODO
void phg_wsx_pm_create_message_win( ws )
    Ws		*ws;
{
    /* Don't really create it yet, wait until it's used. */
    ws->message = message;
}

#endif
