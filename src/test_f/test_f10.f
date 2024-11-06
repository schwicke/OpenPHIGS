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

      PROGRAM VALUATOR
      IMPLICIT NONE

C     Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'

C     Delcare variables
      CHARACTER(LEN=80) DATREC(10)
      INTEGER I, WKID, IERR, ISTAT, VLDNR
      INTEGER LDR, PET
      INTEGER MLDR
      REAL EVOL(6), RA(2), DEFAULT, VALUE
      INTEGER LSTR(4)
      PARAMETER(MLDR=20)
      CHARACTER*20 config(4)
      DATA CONFIG /"Input:", "%8.3g", "min", "max"/
C     Delcare arrays
      REAL COLR(1:3)
      REAL TIMOUT
      INTEGER WKIDI, ICL, IDEV
      INTEGER NSLID

C     Set some parameters
C     Workstation ID
      WKID=1
C     Initial value
      DEFAULT = 30.
C     First Device ID
      VLDNR = 1
C     Number of sliders to create
      NSLID = 5

C     Open PHIGS and a workstation
      CALL POPPH(0, 1)
      CALL POPWK(WKID, 0, 3)
C     Define colour in workstation table
      COLR(1) = 1.0
      COLR(2) = 0.0
      COLR(3) = 0.0
      CALL PSCR(WKID, 1, 3, COLR)

C     Open structure
      CALL POPST(0)

C     Set background color to white
      COLR(1) = 1.0
      COLR(2) = 1.0
      COLR(3) = 1.0
      CALL PSCR(WKID, 0, 3, COLR)

C     Set text attributes
      CALL PSCHH(0.04)

C     Draw text
      CALL PSTXCI(1)
      CALL PTX(0.22, 0.48, 'Testing valuator')

C     Close structure
      CALL PCLST

C      Post structure to workstation
      CALL PPOST(WKID, 0, 0.0)

C initialise data: real string lengths
      LSTR(1)=6
      LSTR(2)=5
      LSTR(3)=3
      LSTR(4)=3
C scale from .. to
      RA(1) = 0.
      RA(2) = 100.
C encode it
      CALL PPREC(1, NSLID, 2, RA, 4, LSTR, CONFIG,
     +     MLDR, IERR, LDR, DATREC)
C     for echo type 1 and -1:
C      *  display in dedicated window
C      *  echo volume absolute in device coordinates (DC)
      PET = -1
      EVOL(1) = 1200.0
      EVOL(2) = 1500.0
      EVOL(3) = 1000.0
      EVOL(4) = 1030.0
      EVOL(5) = 0.
      EVOL(6) = 1.
C for echo type -1: as for 1 but we can specify the strings
      PET = -3

      EVOL(1) = 0.50
      EVOL(2) = 0.80
      EVOL(3) = 0.0
      EVOL(4) = 0.05
      EVOL(5) = 0.
      EVOL(6) = 1.
C Busy loop
      TIMOUT=60.0
C create 5 sliders
      DO I=1, NSLID 
         CALL PSVLM(WKID, VLDNR+I-1, PREQU, PECHO)
         CALL PINVL3(WKID, VLDNR+I-1, DEFAULT, PET, EVOL, LDR, DATREC)
         CALL PSVLM(WKID, VLDNR+I-1, PEVENT, PECHO)
      ENDDO
      I=0
      DO I=1, NSLID
         CALL PSVLM(WKID, VLDNR+I-1, PREQU, PECHO)
         CALL PINVL3(WKID, VLDNR+I-1, DEFAULT, PET, EVOL, LDR, DATREC)
         CALL PSVLM(WKID, VLDNR+I-1, PEVENT, PECHO)
      ENDDO

      DO WHILE (1 .GT. 0)
         CALL PWAIT(TIMOUT, WKIDI, ICL, IDEV )
         IF (ICL.EQ.PVALUA) THEN
            CALL PFLUSH(WKID, ICL, IDEV)
            IDEV=MOD(IDEV, 10)
            CALL PGTVL (VALUE)
            print*, "Device ",IDEV,"Value:", VALUE
            DEFAULT=VALUE
         ENDIF
      ENDDO

      CALL PCLWK(WKID)

C Close PHIGS
      CALL PCLPH

      STOP
      END
