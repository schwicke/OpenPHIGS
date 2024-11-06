C *****************************************************************************
C * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
C *
C * This file is part of Open PHIGS
C * Copyright (C) 2022-2023 CERN
C *
C * Open PHIGS is free software: you can redistribute it and/or modify
C * it under the terms of the GNU Lesser General Public License as published by
C * the Free Software Foundation, either version 2.1 of the License, or
C * (at your option) any later version.
C *
C * Open PHIGS is distributed in the hope that it will be useful,
C * but WITHOUT ANY WARRANTY; without even the implied warranty of
C * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C * GNU Lesser General Public License for more details.
C *
C * You should have received a copy of the GNU Lesser General Public License
C * along with Open PHIGS. If not, see <http://www.gnu.org/licenses/>.
C *****************************************************************************

      SUBROUTINE VFILLR(X, NUM, WHAT)
      INTEGER NUM
      REAL X(*)
      REAL WHAT
      DO I=1, NUM
        X(I) = WHAT
      END DO
      END

      SUBROUTINE VFILLI(IX, NUM, IWHAT)
      INTEGER NUM
      INTEGER IX(*)
      INTEGER IWHAT
      DO I=1, NUM
        IX(I) = IWHAT
      END DO
      END

      SUBROUTINE INITCOLS(IWK1)
      COMMON /KXCCOL/ NCOL, IBLACK, IWHIT, IRED, IGREEN, IBLUE, IYEL,
     ,                IMAG, ICYAN, IORAN, ICOL(7),
     ,                COLR(16), COLG(16), COLB(16)
      INTEGER         NCOL, IBLACK, IWHIT, IRED, IGREEN, IBLUE, IYEL,
     ,                IMAG, ICYAN, IORAN, ICOL
      REAL            COLR    ,COLG     ,COLB
      REAL            FCOLR(3)
      INTEGER IWK1
      NCOL = 6
      IBLACK = 1
      COLR(1) = 0.0
      COLG(1) = 0.0
      COLB(1) = 0.0

      IWHITE = 2
      COLR(2) = 1.0
      COLG(2) = 1.0
      COLB(2) = 1.0

      IRED = 3
      COLR(3) = 1.0
      COLG(3) = 0.0
      COLB(3) = 0.0

      IGREEN = 4
      COLR(4) = 0.0
      COLG(4) = 1.0
      COLB(4) = 1.0

      IBLUE = 5
      COLR(5) = 0.0
      COLG(5) = 0.0
      COLB(5) = 1.0

      IYEL = 6
      COLR(6) = 0.0
      COLG(6) = 1.0
      COLB(6) = 0.0

      DO I=1, NCOL
         FCOLR(1) = COLR(I)
         FCOLR(2) = COLG(I)
         FCOLR(3) = COLB(I)
         CALL PSCR (IWK1, I, 3, FCOLR)
      END DO
C define more colors
C      CALL PXSCM(IWK1)

      END

      PROGRAM DRAWLINE

C      Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'

      INTEGER    TFNTPS    , EFNTPS, NMARKS, NMSIZS
      PARAMETER (TFNTPS = 3, EFNTPS = 7, NMARKS=8, NMSIZS=8)

      INTEGER MARKER, ISIZE, MARKRS(NMARKS)
C      DATA MARKRS/-2,-3,-4,-5,-44,-45,-46,-47,-51,-52,-53,-57/
      DATA MARKRS/2, 3, 4, 5, 6, 7, 8, 9 /
      INTEGER    SLABEL     , HLABEL     , PICKAB     , HIGHLI
      PARAMETER (SLABEL=9996, HLABEL=9997, PICKAB=9998, HIGHLI=9999)

      INTEGER NSGTSP, NSGTIT, NSGSAB, NSGPLE, NSGPLM, NSGPLD, NSGCTB
      INTEGER NSGSEP, NSGDET, NSGHST, NSGAXE, NSGTXT, NSGARM, NSGBEA
      INTEGER NSGARJ, NSGTMP, NSGWIN, NSGARI, NSGBSP, NSGPVX, NSGDIP
      INTEGER NSGSCL, NSGCSY, NSGBTG, IMKST0
      PARAMETER (NSGDET=10000,NSGTMP=19998)
      PARAMETER (NSGTIT=20000,NSGTSP=20005,NSGSAB=20010,NSGPLE=20011)
      PARAMETER (NSGPLM=20012,NSGPLD=20013,NSGHST=20015,NSGAXE=20016)
      PARAMETER (NSGCTB=20020,NSGTXT=20050,NSGBEA=20055,NSGSEP=20100)
      PARAMETER (NSGARI=20200,NSGARJ=20300,NSGARM=20400,NSGBSP=20500)
      PARAMETER (NSGPVX=20600,NSGBTG=20680,NSGSCL=20690,NSGCSY=20695)
      PARAMETER (NSGDIP=20700,NSGWIN=21000,IMKST0=30000)
      COMMON /KXCCOL/ NCOL, IBLACK, IWHIT, IRED, IGREEN, IBLUE, IYEL,
     ,                IMAG, ICYAN, IORAN, ICOL(7),
     ,                COLR(16), COLG(16), COLB(16)
      INTEGER         NCOL, IBLACK, IWHIT, IRED, IGREEN, IBLUE, IYEL,
     ,                IMAG, ICYAN, IORAN, ICOL
      INTEGER STRID, TUS, IWK, ICL, IDNR, ITNR
      REAL RXDV, RYDV, XPK, YPK, STATUS
      REAL RW(4),CW(4),RV(4),CV(4),CSPEC(3)
      CHARACTER(LEN=80) DATREC
      INTEGER IPPD,STAT,PPD,PP(3,5)

C     Open PHIGS and a workstation
      IWK1 = 1
      CALL POPPH(0, 0)
      CALL POPWK(IWK1, 0, 3)
C query work station
      CALL PQWKT(IWK1,IERR,TUS,RW,CW,RV,CV)
      IF (IERR.NE.0) THEN
         WRITE (6,'(A,I4,A)') ' Error', IERR, ' PQWKT'
      ENDIF
      RXDV = RV (2)
      RYDV = RV (4)
* Initialize a locator
      CALL PINLC (IWK1,1,0,0.,0.,1,0.,RXDV,0.,RYDV,0,DATREC)
      print*, rxdv, rydv
* Initialize a pick
      CALL PINPK (IWK1,1,PNPICK,0,0,1,0.,RXDV,0.,RYDV,0,DATREC,PPOTOP)
* Initialize pick filter
      CALL PSPKFT (IWK1,1,0,0,0,0)
* Set Pick mode
      CALL PSPKM (IWK1,1,PEVENT,PNECHO)

*     init colors
      CALL INITCOLS(IWK1)

* Create structures for Polymarker Selection
      DY = 1./(FLOAT(NMARKS)+1.)
      DX = 1./(FLOAT(NMSIZS)+1.)
      DO I = 1, NMARKS
         MARKER = MARKRS(I)
         print*, marker
         Y = 1. - FLOAT(I) * DY
         DO ISIZE = 1, NMSIZS
            X = 0.05 + FLOAT(ISIZE-1) * DX
            STRID=IMKST0+(I-1)*NMSIZS+ISIZE
            print*, "open structure", STRID
            CALL POPST(STRID)
            CALL PSIASF(3, 1)
            CALL PSIASF(4, 1)
            CALL PSIASF(5, 1)
            CALL PSPMI(1)
            CALL PSPMCI(IRED)
            CALL PADS (1,PICKAB)
            CALL PSMK (MARKER)
            print*, "marker: ", marker
            CALL PSMKSC (FLOAT(ISIZE)/400.)
            print*, "size", isize
            CALL PSPKID(400*MARKER+ISIZE)
            CALL PPM (1, X, Y)
            CALL PCLST
C      Post structure to workstation
            CALL PPOST(IWK1, STRID, 1.0)
         ENDDO
      ENDDO

C      Busy loop
      DO WHILE (1 .GT. 0)
         CALL PWAIT (0.1, IWK, ICL, IDNR)
         IF ((ICL.EQ.PPICK).AND.(IWK.EQ.IWK1)) THEN
            print*, "Got Pick event for logical input device", IDNR
C           CALL PSMLC (IWK1,IDNR,ITNR,XPK,YPK)
C           print*, "at ", xpk, ypk
            CALL PGTPK (5,STAT,PPD,PP)
            print*, status, ppd, pp
         ENDIF
      END DO

      STOP
      END
