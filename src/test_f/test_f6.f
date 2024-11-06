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

      subroutine cmp_mat(x, y)
      real x(4,4), y(4,4)
      integer i,j
      do i=1,4
         do j=1,4
            if (x(i,j).ne.y(i,j)) then
               print*, "ERROR: Arrays are different"
               print*, i, j, " expected ", x(i,j), " got: ", y(i,j)
            endif
         enddo
      enddo
      end

      subroutine cmp_vec(x, y)
      real x(6), y(6)
      integer i
      do i=1,6
         if (x(i).ne.y(i)) then
            print*, "ERROR: Arrays are different"
            print*, i, " expected ", x(i), " got: ", y(i)
         endif
      enddo
      end

      subroutine print_mat(x)
      real x(4,4)
      integer i
      do i=1,4
         print*,  x(i,1), x(i, 2), x(i,3), x(i,4)
      enddo
      end

      PROGRAM TEST_INQUIRE

C     Include PHIGS enumeration file
      INCLUDE 'phigsf77.h'
      REAL VWORMT(4,4), VWMPMT(4,4), VWCPLM(6)
      REAL VWORMTO(4,4), VWMPMTO(4,4), VWCPLMO(6)
      DATA VWORMT /
     +     1.0, 0.0, 0.0, 0.0,
     +     0.0, 1.0, 0.0, 0.0,
     +     0.0, 0.0, 1.0, 0.0,
     +     0.0, 0.0, 0.0, 1.0 /
      DATA VWMPMT /
     +    1.0, 0.0, 0.0, 0.0,
     +    0.0, 1.0, 0.0, 0.0,
     +    0.0, 0.0, 1.0, 0.0,
     +    0.0, 0.0, 0.0, 1.0 /
      DATA VWCPLM / 0.1, 0.2, 0.3, 0.4, 0.5, 0.6/
      REAL WLIM(6)
      REAL WLIMO(6)
      DATA WLIM / 0.1,0.9, 0.2, 0.8, 0.3, 0.7 /
      INTEGER XYCPLI, BCLIPI, FCLIPI
      INTEGER XYCPLIO, BCLIPIO, FCLIPIO
      INTEGER VWUPD
      REAL RWIN(4), CWIN(4), RVIE(4), CVIE(4)
C     Open PHIGS and a workstation
      IWK1 = 1
      CALL POPPH(0, 0)
      CALL POPWK(IWK1, 0, 3)
C query work station
      CALL PQWKT(IWK1, IERR, ITUS, RWIN, CWIN, RVIE, CVIE)
      IF (IERR.NE.0) THEN
         WRITE (6,'(A,I4,A)') ' Error', IERR, ' PQWKT'
      ENDIF
C test PSVWR3 and PQVWR
      IVW = 1
      XYCPLI = 0
      BCLIPI = 0
      FCLIPI = 1
      PRINT*, "Testing PSVWR3 and PQVWR"
      call print_mat(VWORMT)
      call print_mat(VWMPMT)
      CALL PSVWR3 (IWK1,IVW, VWORMT, VWMPMT, VWCPLM, XYCPLI,
     +     BCLIPI, FCLIPI)
      CALL PQVWR (IWK1,IVW,IPCURVL,IERR,VWUPD,VWORMTO,VWMPMTO,
     +     VWCPLMO,XYCPLIO,BCLIPIO,FCLIPIO)
      if (IERR == 0) THEN
         PRINT*, "PQVWR CHECKING RESULTS"
         IF (XYCPLI.EQ.XYCPLIO.AND.
     +        BCLIPI.EQ.BCLIPIO.AND.
     +        FCLIPI.EQ.FCLIPIO) THEN
            PRINT*, "CLIPPING CORRECT"
         ELSE
            PRINT *, "CLIPPING PARAMETER ARE WRONG"
            print *, XYCPLI, XYCPLIO
            print *, BCLIPI, BCPLIPIO
            print *, FCLIPI, FCLIPIO
            GOTO 99
         ENDIF
         print*, "Checking VMORMT"
         call cmp_mat(VWORMT, VWORMTO)
         print*, "Checking VWMPMT"
         call cmp_mat(VWMPMT, VWMPMTO)
         print*, "Checking VWCPLMO"
         call cmp_vec(VWCPLM, VWCPLMO)
      ELSE
         PRINT*, "PQVWR FAILED"
         GOTO 99
      ENDIF
C     TEST PSWKW3 AND PQWKT
      CALL PSWKW3(IWK1, WLIM)
      IERR = 0
      CALL PQWKT(IWK1, IERR, ITUS, RWIN, CWIN, RVIE, CVIE)
      IF (IERR.EQ.0) THEN
         PRINT*, RWIN(1),RWIN(2),RWIN(3),RWIN(4)
         PRINT*, CWIN(1),CWIN(2),CWIN(3),CWIN(4)
         PRINT*, RVIE(1),RVIE(2),RVIE(3),RVIE(4)
         PRINT*, CVIE(1),CVIE(2),CVIE(3),CVIE(4)
         IF (RWIN(1).EQ.WLIM(1).AND.
     +        RWIN(2).EQ.WLIM(2).AND.
     +        RWIN(3).EQ.WLIM(3).AND.
     +        RWIN(4).EQ.WLIM(4)) THEN
            PRINT*, "TEST OF PSWKW3 AND PQWKW PASSED"
         ELSE
            PRINT*, "TEST OF PSWKW3 AND PQWKW FAILED"
         ENDIF
      ELSE
         PRINT*, "TEST OF PSWKW3 AND PQWKW RETURNED ERROR"
      ENDIF
C     TEST PQEDM EDIT MODE
      CALL PSEDM(0)
      CALL PQEDM(IERR, IMODE)
      IF (IERR.EQ.0) THEN
         IF (IMODE.EQ.0) THEN
            PRINT*, "PQEDM PASSED TEST 1"
         ELSE
            PRINT*, "PQEDM RETURNED ", IMODE, " EXPECTED 0"
            GOTO 99
         ENDIF
      ELSE
         PRINT*, "CALL TO PQEDM FAILED"
      ENDIF
      CALL PSEDM(1)
      CALL PQEDM(IERR, IMODE)
      IF (IERR.EQ.0) THEN
         IF (IMODE.EQ.1) THEN
            PRINT*, "PQEDM PASSED TEST 2"
         ELSE
            PRINT*, "PQEDM RETURNED ", IMODE, " EXPECTED 1"
            GOTO 99
         ENDIF
      ELSE
         PRINT*, "CALL TO PQEDM FAILED"
      ENDIF
C TEST PSHRM AND PQHRM
      CALL PSHRM(IWK1, 0)
      CALL PQHRM(IWK1, IERR, IHUP, ICMODE, IRMODE)
      IF (IERR.EQ.0) THEN
         IF (IRMODE.EQ.0) THEN
            PRINT*, "PQHRM PASSED TEST 1"
         ELSE
            PRINT*, "PQHRM RETURNED ", IMODE, " EXPECTED 0"
            GOTO 99
         ENDIF
      ELSE
         PRINT*, "CALL TO PQHRM FAILED"
      ENDIF
      CALL PSHRM(IWK1, 1)
      CALL PQHRM(IWK1, IERR, IHUP, ICMODE, IRMODE)
      IF (IERR.EQ.0) THEN
         IF (IRMODE.EQ.1) THEN
            PRINT*, "PQHRM PASSED TEST 2"
         ELSE
            PRINT*, "PQHRM RETURNED ", IMODE, " EXPECTED 1"
            GOTO 99
         ENDIF
      ELSE
         PRINT*, "CALL TO PQHRM FAILED"
      ENDIF
 99   END
