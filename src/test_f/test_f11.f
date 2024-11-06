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
      SUBROUTINE INITCH(WKID)
      IMPLICIT NONE
C     Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'
      INTEGER N, MAXSTR
      PARAMETER(N=5, MAXSTR=30)
      CHARACTER(LEN=MAXSTR) STRING(N+1)
      CHARACTER(LEN=80) DATREC(MAXSTR)
      INTEGER PROMPT(N),LSTR(N+1)
      INTEGER WKID, I, ISTAT
      INTEGER CHDNR, ICHNR, MLDR, IERR, LDR, PET
      REAL EVOL(6)
      REAL EMPTY(1)
      DATA EMPTY/0.0/
      DATA STRING/'EXIT','VAL ON/OFF',
     +     'CHOICE3','CHOICE4','CHOICE5', 'Please choose an option'/
      PET = -4
      EVOL(1) = 0.50
      EVOL(2) = 0.80
      EVOL(3) = 0.0
      EVOL(4) = 0.05
      EVOL(5) = 0.
      EVOL(6) = 1.

      CHDNR = 1
      ICHNR = 0
      MLDR = MAXSTR
C Init prompts and actual string length
      DO 3 I = 1,N
         PROMPT(I) = PON
         LSTR(I) = LEN(STRING(I))
 3    CONTINUE
      LSTR(N+1) = LEN(STRING(N+1))

      CALL PPREC(4, PROMPT, 0, EMPTY, N+1, LSTR, STRING, MLDR,
     +     IERR, LDR, DATREC)
      CALL PSCHM(WKID, CHDNR, PREQU, PECHO)
      CALL PINCH3(WKID, CHDNR, ISTAT, ICHNR, PET, EVOL, LDR, DATREC)
      END

      SUBROUTINE INITVAL(WKID)
      IMPLICIT NONE
C     Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'
      INTEGER WKID, I, NSLID(1), IERR, LDR, PET
      INTEGER LSTR(4)
      CHARACTER*20 config(4)
      DATA CONFIG /"Input:", "%8.3g", "min", "max"/
      REAL EVOL(6), RA(2), DEFAULT, VALUE
      INTEGER MLDR, VLDNR
      CHARACTER(LEN=80) DATREC(10)
      PARAMETER(MLDR=20)

C     Init the string lengths
      DO I=1, 4
         LSTR(I) = LEN(CONFIG(I))
      ENDDO
C     scale from .. to
      RA(1) = 0.
      RA(2) = 100.
C     create 5 sliders
      NSLID(1) = 5
      CALL PPREC(1, NSLID(1), 2, RA, 4, LSTR, CONFIG,
     +     MLDR, IERR, LDR, DATREC)

C     for echo type -1: as for 1 but we can specify the strings
      PET = -3

      EVOL(1) = 0.50
      EVOL(2) = 0.80
      EVOL(3) = 0.0
      EVOL(4) = 0.05
      EVOL(5) = 0.
      EVOL(6) = 1.


C     create 5 sliders
      VLDNR = 1
      DO I=1, NSLID(1)
         CALL PSVLM(WKID, VLDNR+I-1, PREQU, PECHO)
         CALL PINVL3(WKID, VLDNR+I-1, DEFAULT, PET, EVOL, LDR, DATREC)
         CALL PSVLM(WKID, VLDNR+I-1, PEVENT, PECHO)
      ENDDO

      END

      PROGRAM COMBINED
      IMPLICIT NONE

C     Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'

C     Delcare variables
      INTEGER I, WKID, IERR, ISTAT, CHDNR
      INTEGER WKIDI, ICL, ICH, IDEV
      INTEGER LSTR(4)
      LOGICAL SLIDER
      DATA SLIDER/.FALSE./
      REAL DEFAULT, VALUE
      REAL TIMOUT
C     Delcare arrays
      REAL COLR(1:3)

C     Set some parameters
C     Workstation ID
      WKID=1
C     Initial value
      DEFAULT = 30.
C     First Device ID
      CHDNR = 1
C     Time out in seconds
      TIMOUT = 60.
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
      CALL PTX(0.22, 0.48, 'Combined test')

C     Close structure
      CALL PCLST

C      Post structure to workstation
      CALL PPOST(WKID, 0, 0.0)
C     init choices
      CALL INITCH(WKID)
C     Wait for events
      DO WHILE (1 .GT. 0)
         CALL PSCHM(WKID, CHDNR, PEVENT, PECHO)
         CALL PWAIT(TIMOUT, WKIDI, ICL, IDEV )
C     Check for Choice events
         IF (ICL.EQ.PCHOIC) THEN
            CALL PFLUSH(WKID, ICL, IDEV)
            CALL PGTCH(ISTAT,ICH)
            IF (ISTAT.EQ.POK) THEN
               IF (ICH.EQ.2) THEN
                  IF (.NOT.SLIDER) THEN
                     SLIDER=.TRUE.
C     init valuators
                     CALL INITVAL(WKID)
                  ELSE
                     SLIDER=.FALSE.
                     DO 70 I=1,5
                        CALL PSVLM(WKID,I,PREQU,PECHO)
 70                  CONTINUE
                  END IF
               ELSE
                  IF (ICH.EQ.1) THEN
                     GOTO 222
                  ENDIF
                  print*, "Other choice event ", ICH
               ENDIF
            ELSE
               PRINT*, "Choice event status NOT OK"
            ENDIF

         ENDIF
C     check for valuator events
         IF (ICL.EQ.PVALUA) THEN
            CALL PFLUSH(WKID, ICL, IDEV)
            IDEV=MOD(IDEV, 10)
            CALL PGTVL (VALUE)
            print*, "Device ",IDEV,"Value:", VALUE
            DEFAULT=VALUE
         ENDIF
      ENDDO
 222  CONTINUE
      CALL PCLWK(WKID)

C Close PHIGS
      CALL PCLPH

      STOP
      END
