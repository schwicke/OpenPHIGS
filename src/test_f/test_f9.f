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

      PROGRAM CHOICE
      IMPLICIT NONE

C      Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'

C     Delcare variables
      INTEGER N, MAXSTR
      PARAMETER(N=5, MAXSTR=30)
      CHARACTER(LEN=80) DATREC(MAXSTR)
      CHARACTER(LEN=MAXSTR) STRING(N+1)
      INTEGER I, IERR, WKID, ISTAT, ICHNR, MLDR, CHDNR
      INTEGER LDR, PET, STAT, CHNR
      INTEGER PROMPT(N),LSTR(N+1)
      DATA STRING/'EXIT','RE-INIT',
     +     'CHOICE3','CHOICE4','CHOICE5', 'Please choose an option'/
      REAL EVOL(6)
      REAL TIMOUT

      INTEGER IWK
      PARAMETER (IWK=1)

      INTEGER IWKI, ICL, IDEV
C     Open PHIGS and a workstation

      CALL POPPH(0, 1)
      CALL POPWK(IWK, 0, 3)

      ISTAT = PNCHOI
      ICHNR = 0
      MLDR = MAXSTR

      PET = -4
      EVOL(1) = 0.50
      EVOL(2) = 0.80
      EVOL(3) = 0.0
      EVOL(4) = 0.05
      EVOL(5) = 0.
      EVOL(6) = 1.
C
      CHDNR = 1
      DO 3 I = 1,N
         PROMPT(I) = PON
 3    CONTINUE

C Adjust string length
      LSTR(1) = 4
      LSTR(2) = 7
      LSTR(3) = 7
      LSTR(4) = 7
      LSTR(5) = 7
      LSTR(6) = 24
      TIMOUT=60.0
 10   CALL PPREC(4, PROMPT, 0, 0., N+1, LSTR, STRING, MLDR,
     +     IERR, LDR, DATREC)
      CALL PSCHM(IWK, CHDNR, PREQU, PECHO)
      CALL PINCH3(IWK, CHDNR, ISTAT, ICHNR, PET, EVOL, LDR, DATREC)
      DO WHILE (1 .GT. 0)
C Put the device into event mode
         CALL PSCHM(IWK, CHDNR, PEVENT, PECHO)
         CALL PWAIT(TIMOUT, IWKI, ICL, IDEV )
C         CALL PFLUSH(IWKI, ICL, IDEV)
         CALL PGTCH(STAT, CHNR)
         IF (STAT.eq.POK) THEN
            PRINT*, "Chosen option is ", CHNR
            ICHNR = CHNR
            IF (CHNR.EQ.2) GOTO 10
            IF (CHNR.EQ.1) GOTO 20
         ELSE
            PRINT*, "Nothing chosen"
         ENDIF
C         CALL PSCHM(IWK, CHDNR, 0, 0)
      ENDDO
 20   CONTINUE
      CALL PCLWK(IWK)

C     CLOSE PHIGS
      CALL PCLPH

      STOP
      END
