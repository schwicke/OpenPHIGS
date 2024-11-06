C *****************************************************************************
C * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER
C *
C * This file is part of Open PHIGS
C * Copyright (C) 2014 Surplus Users Ham Society
C *           (C) 2022-2023 CERN
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

      PROGRAM DRAWLINE

C      Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'

C      Delcare arrays
      REAL COLR(1:3)
      REAL PL_x(2)
      REAL PL_y(2)
      REAL PL_z(2)

C      Open PHIGS and a workstation
      CALL POPPH(0, 0)
      CALL POPWK(0, 0, 0)

C      Define line colour in workstation table
      COLR(1) = 1.0
      COLR(2) = 1.0
      COLR(3) = 0.0
      CALL PSCR(0, 1, 3, COLR)

C      Open structure
      CALL POPST(0)

C      Set text attributes
      CALL PSCHH(0.02)

C      Draw text
      CALL PSTXAL (PAHNOR, PAVNOR)
      CALL PTX(0.5, 0.1, 'PTX text norm  !')
      CALL PSTXAL (PALEFT, PAVNOR)
      CALL PTX(0.5, 0.15, 'PTX text left !')
      CALL PSTXAL (PACENT, PAVNOR)
      CALL PTX(0.5, 0.2, 'PTX text cent  !')
      CALL PSTXAL (PARITE, PAVNOR)
      CALL PTX(0.5, 0.25, 'PTX text right !')
C
      CALL PSTXAL (PAHNOR, PAVNOR)
      CALL PTX(0.0, 0.4, 'normal')
      CALL PSTXAL (PAHNOR, PATOP)
      CALL PTX(0.2, 0.4, 'on top')
      CALL PSTXAL (PAHNOR, PAHALF)
      CALL PTX(0.4, 0.4, 'on half')
      CALL PSTXAL (PAHNOR, PABOTT)
      CALL PTX(0.6, 0.4, 'on bottom')

C     3d
      RZ = 0.0;
      PL_X(1) = 1.;
      PL_Y(1) = 0.;
      PL_Z(1) = RZ;

      PL_X(2) = 0.;
      PL_Y(2) = 1.;
      PL_Z(2) = RZ;
C     set up vector
      CALL PSCHUP(0.5, 0.5)
C      CALL PSCHUP(0.0, 1.0)
      CALL PSTXAL (PAHNOR, PAVNOR)
      CALL PTX3(0.0, 0.6, RZ, PL_X, PL_Y, PL_Z, 'ptx3 norm')
      CALL PSTXAL (PAHNOR, PATOP)
      CALL PTX3(0.2, 0.6, RZ, PL_X, PL_Y, PL_Z, 'ptx3 top')
      CALL PSTXAL (PAHNOR, PAHALF)
      CALL PTX3(0.4, 0.6, RZ, PL_X, PL_Y, PL_Z, 'ptx3 half')
      CALL PSTXAL (PAHNOR, PABOTT)
      CALL PTX3(0.6, 0.6, RZ, PL_X, PL_Y, PL_Z, 'ptx3 bot')


C      Draw text
      CALL PSATCH(0.02)
      CALL PATR3(0.0, 0.9, 0., 0., 0., 0.,
     + 'Hello annotation text patr3!')
      CALL PATR(0.1, 0.8, 0., 0.,
     +     'Hello annotation text patr!')
      CALL PATR3(0.2, 0.0, 0.0, 0., 0., 0.,'0.2')
      CALL PATR3(0.5, 0.0, 1.0, 0., 0., 0.,'0.5')
      CALL PATR3(0.8, 0.0, 0.0, 0., 0., 0.,'0.8')

      CALL PSATAL (PAHNOR, PATOP)
      CALL PATR3(0.4, 0.5, 0.0, 0.0, 0.0, 0.0, 'patr3 top')
      CALL PSATAL (PAHNOR, PAHALF)
      CALL PATR3(0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 'patr3 half')
      CALL PSATAL (PAHNOR, PABOTT)
      CALL PATR3(0.6, 0.5, 0.0, 0.0, 0.0, 0.0, 'patr3 bottom')

C     Close structure
      CALL PCLST

C      Post structure to workstation
      CALL PPOST(0, 0, 0.0)

C      Buisy loop
      DO WHILE (1 .GT. 0)
      END DO

      STOP
      END
