000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.   T00MYMEM.
000030 ENVIRONMENT    DIVISION.
000040 DATA DIVISION.
000050 WORKING-STORAGE SECTION.
000060*--------------------------------------------------------------
000070*-
000080*--------------------------------------------------------------
000090 01       WK                SYNC.
000100   03     WKI               PIC 9(5).
000100   03     WKJ               PIC 9(5).
000100   03     WKK               PIC 9(5).
000220*--------------------------------------------------------------
000000*-
000320*--------------------------------------------------------------
000330 01       PMEM-PRM          SYNC..
000390   03     PMEM-COD          PIC S9(1).
000400   03     PMEM-MSG          PIC X(80).
000340   03     PMEM-FNC          PIC X(4).
000350   03     PMEM-MEMARA-SZE   PIC 9(5).
000360   03     PMEM-VLL          PIC 9(5).
000370   03     PMEM-PVL          PIC 9(5).
000220*--------------------------------------------------------------
000000*-
000320*--------------------------------------------------------------
000481 01       PMEM-VAL          SYNC.
000481   03     FILLER            PIC X(100).
000650*--------------------------------------------------------------
000000*-
000700*--------------------------------------------------------------
000710 01       PMEM-MEMARA       SYNC..
000610   03     FILLER            PIC X(100).
000301 PROCEDURE DIVISION.
000302 A00-MAIN SECTION.
000303*--------------------------------------------------------------
000310*-
000320*--------------------------------------------------------------
000330 A00-010.
000411*--ABEND CASE.
000340*  MOVE SPACE               TO PMEM-PRM.
000350*  MOVE 'INIT'              TO PMEM-FNC.
000360*  MOVE 10                  TO PMEM-MEMARA-SZE.
000370*  CALL 'SUBMEM'       USING PMEM-PRM
000000*                              PMEM-VAL
000380*                              PMEM-MEMARA.
000505*  DISPLAY 'PRM:(' PMEM-PRM ')'.
000000*  DISPLAY 'VAL:(' PMEM-VAL ')'.
000510*  DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000410*  IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
000411*--
000340   MOVE SPACE               TO PMEM-PRM.
000350   MOVE 'INIT'              TO PMEM-FNC.
000360   MOVE 100                 TO PMEM-MEMARA-SZE.
000370   CALL 'SUBMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000380                               PMEM-MEMARA.
000505   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000510   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000410   IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
000411*--
000420   MOVE SPACE               TO PMEM-PRM.
000430   MOVE 'SETL'              TO PMEM-FNC.
000420   MOVE SPACE               TO PMEM-VAL(1:4).
000440   MOVE 'ABCD'              TO PMEM-VAL.
000450   MOVE 4                   TO PMEM-VLL.
000370   CALL 'SUBMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000380                               PMEM-MEMARA.
000680   MOVE PMEM-PVL            TO WKI.
000505   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000510   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000531   IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
000540*--
000420   MOVE SPACE               TO PMEM-PRM.
000430   MOVE 'SETL'              TO PMEM-FNC.
000420   MOVE SPACE               TO PMEM-VAL(1:4).
000440   MOVE 'EFGH'              TO PMEM-VAL.
000450   MOVE 4                   TO PMEM-VLL.
000370   CALL 'SUBMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000380                               PMEM-MEMARA.
000680   MOVE PMEM-PVL            TO WKJ.
000505   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000510   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000531   IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
000671*--
000690   MOVE SPACE               TO PMEM-PRM.
000700   MOVE 'GET_'              TO PMEM-FNC.
000710   MOVE WKI                 TO PMEM-PVL.
000370   CALL 'SUBMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000380                               PMEM-MEMARA.
000505   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000510   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000510   DISPLAY 'EXPECTED:(ABCD)'.
000510   DISPLAY 'ACTUAL  :(' PMEM-VAL(1:PMEM-VLL) ')'.
000531   IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
000671*--
000690   MOVE SPACE               TO PMEM-PRM.
000700   MOVE 'GET_'              TO PMEM-FNC.
000710   MOVE WKJ                 TO PMEM-PVL.
000370   CALL 'SUBMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000380                               PMEM-MEMARA.
000505   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000510   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000510   DISPLAY 'EXPECTED:(EFGH)'.
000510   DISPLAY 'ACTUAL  :(' PMEM-VAL(1:PMEM-VLL) ')'.
000531   IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
000540*--
000420   MOVE SPACE               TO PMEM-PRM.
000430   MOVE 'SETD'              TO PMEM-FNC.
000000   COMPUTE WKK = 100.
000420   MOVE ALL HIGH-VALUE      TO PMEM-VAL(1:WKK).
000440   MOVE 'IJKL'              TO PMEM-VAL(1:4).
000370   CALL 'SUBMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000380                               PMEM-MEMARA.
000680   MOVE PMEM-PVL            TO WKJ.
000505   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000510   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000531   IF PMEM-COD < ZERO THEN GO TO A00-EXIT.
001061 A00-EXIT.
001070   MOVE ZERO TO RETURN-CODE.
001080   GOBACK.
