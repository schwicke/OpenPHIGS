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

      PROGRAM TEXTINPUT

C      Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'

C     Delcare variables
      INTEGER        KR4
      REAL           VALR4(80)
      CHARACTER(LEN=80)  NAMR4
*
      CHARACTER(LEN=80) TCHAR
      CHARACTER(LEN=80) DATREC

      INTEGER LENOCC,LNAMR4,LSTEDI,I
      INTEGER STDNR,STAT,LCHAR,PET,IA(2),IERR,MLDR,LDR
      REAL XMIN,XMAX,YMIN,YMAX

      INTEGER IWK
      PARAMETER (IWK=1)

C      Open PHIGS and a workstation
      CALL POPPH(0, 1)
      CALL POPWK(IWK, 0, 3)

      STDNR =  1
      PET   =  -1
      MLDR  =  1
      IA(1) =  80
      IA(2) =  10
      LCHAR =  7
      TCHAR =  'Input: '
*
      XMIN  =  0.35
      XMAX  =  0.65
      YMIN  =  0.57
      YMAX  =  0.6
*
      CALL PPREC(2,IA,0,0,0,0,0,MLDR,IERR,LDR,DATREC)
      CALL PINST(IWK,STDNR,LCHAR,TCHAR,PET,XMIN,XMAX,
     +     YMIN,YMAX,LDR,DATREC)
      CALL PRQST(IWK,STDNR,STAT,LCHAR,TCHAR)
      IF (LCHAR.GT.0) THEN
         PRINT *, TCHAR
      ENDIF

      STOP
      END
