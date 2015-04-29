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
000220*--------------------------------------------------------------
000230*- メモリパラメータ引数。
000320*--------------------------------------------------------------
000140 01       PMYMEM-PRM        SYNC.
000340   COPY   RMYMEM-PRM.
000410*--------------------------------------------------------------
000420*- 値受渡引数。
000430*- I:VALの値をVLLの長さ分メモリにセット。
000440*- O:メモリの値をVLLの長さ分VALにセット。
000470*--------------------------------------------------------------
000220 01       PMYMEM-VAL        SYNC.
000230   03     FILLER            PIC X(100).
000260*--------------------------------------------------------------
000261*- メモリエリア。
000262*- MEM:メモリ領域。
000265*--------------------------------------------------------------
000266 01       PMYMEM-MEM        SYNC.
000270   03     FILLER            PIC X(100).
000280*--------------------------------------------------------------
000281*- IN1
000282*--------------------------------------------------------------
000283 01       PIN100.
000284   03     FILLER            PIC X(10) VALUE '1234567890'.
000285   03     FILLER            PIC X(90).
000286*--------------------------------------------------------------
000287*- IN2
000288*--------------------------------------------------------------
000289 01       PIN200.
000290   03     FILLER            PIC X(100).
000291*--------------------------------------------------------------
000292*- IN3
000293*--------------------------------------------------------------
000294 01       PIN300.
000295   03     FILLER            PIC X(100).
000296*--------------------------------------------------------------
000297*- OUT
000298*--------------------------------------------------------------
000299 01       POUT00.
000300   03     FILLER            PIC X(100).
000301 PROCEDURE DIVISION.   
000302 A00-MAIN SECTION.
000303*--------------------------------------------------------------
000310*-
000320*--------------------------------------------------------------
000330 A00-010.
000340   MOVE SPACE               TO PMYMEM-PRM.
000350   MOVE 'INIT'              TO PMYMEM-FNC.
000360   MOVE 100                 TO PMYMEM-MEM-SZE.
000370   CALL 'SUBMYMEM'       USING PMYMEM-PRM
000380                               PMYMEM-VAL
000400                               PMYMEM-MEM
000401                               PIN100
000402                               PIN200
000403                               PIN300
000404                               POUT00.
000410   IF PMYMEM-COD < ZERO THEN GO TO A00-EXIT.
000411*-- 
000420   MOVE SPACE               TO PMYMEM-PRM.
000430   MOVE 'SET_'              TO PMYMEM-FNC.
000440   MOVE 'LABC'              TO PMYMEM-VAL.
000450   MOVE 4                   TO PMYMEM-VLL.
000460   CALL 'SUBMYMEM'       USING PMYMEM-PRM
000470                               PMYMEM-VAL
000500                               PMYMEM-MEM
000501                               PIN100
000502                               PIN200
000503                               PIN300
000504                               POUT00.
000505   DISPLAY 'PRM:(' PMYMEM-PRM ')'.
000510   DISPLAY 'VAL:(' PMYMEM-VAL(1:PMYMEM-VLL) ')'.
000530   DISPLAY 'MEM:(' PMYMEM-MEM ')'.
000531   IF PMYMEM-COD < ZERO THEN GO TO A00-EXIT.
000540*--
000550   MOVE SPACE               TO PMYMEM-PRM.
000560   MOVE 'SET_'              TO PMYMEM-FNC.
000570   MOVE 'LDEF'              TO PMYMEM-VAL.
000580   MOVE 4                   TO PMYMEM-VLL.
000590   CALL 'SUBMYMEM'       USING PMYMEM-PRM
000600                               PMYMEM-VAL
000630                               PMYMEM-MEM
000631                               PIN100
000632                               PIN200
000633                               PIN300
000634                               POUT00.
000635   DISPLAY 'PRM:(' PMYMEM-PRM ')'.
000640   DISPLAY 'VAL:(' PMYMEM-VAL(1:PMYMEM-VLL) ')'.
000660   DISPLAY 'MEM:(' PMYMEM-MEM ')'.
000670   IF PMYMEM-COD < ZERO THEN GO TO A00-EXIT.
000671*--
000680   MOVE PMYMEM-VLI          TO WKI.
000690   MOVE SPACE               TO PMYMEM-PRM.
000700   MOVE 'GET_'              TO PMYMEM-FNC.
000710   MOVE WKI                 TO PMYMEM-VLI.
000720   CALL 'SUBMYMEM'       USING PMYMEM-PRM
000730                               PMYMEM-VAL
000760                               PMYMEM-MEM
000761                               PIN100
000762                               PIN200
000763                               PIN300
000764                               POUT00.
000765   DISPLAY 'PRM:(' PMYMEM-PRM ')'.
000770   DISPLAY 'VAL:(' PMYMEM-VAL(1:PMYMEM-VLL) ')'.
000790   DISPLAY 'MEM:(' PMYMEM-MEM ')'.
000800   IF PMYMEM-COD < ZERO THEN GO TO A00-EXIT.
000801*--
000802*-- PIN100の5桁目から2桁というアドレス情報をセット
000810   MOVE SPACE               TO PMYMEM-PRM.
000820   MOVE 'SET_'              TO PMYMEM-FNC.
000830   MOVE '10000500002'          TO PMYMEM-VAL.
000840   MOVE 11                   TO PMYMEM-VLL.
000850   CALL 'SUBMYMEM'       USING PMYMEM-PRM
000860                               PMYMEM-VAL
000890                               PMYMEM-MEM
000891                               PIN100
000892                               PIN200
000893                               PIN300
000894                               POUT00.
000895   DISPLAY 'PRM:(' PMYMEM-PRM ')'.
000900   DISPLAY 'VAL:(' PMYMEM-VAL(1:PMYMEM-VLL) ')'.
000920   DISPLAY 'MEM:(' PMYMEM-MEM ')'.
000930   IF PMYMEM-COD < ZERO THEN GO TO A00-EXIT.
000931*--
000932*-- PIN100の5桁目から2桁をGETする。
000940   MOVE PMYMEM-VLI          TO WKI.
000950   MOVE SPACE               TO PMYMEM-PRM.
000960   MOVE 'GET_'              TO PMYMEM-FNC.
000970   MOVE WKI                 TO PMYMEM-VLI.
000980   CALL 'SUBMYMEM'       USING PMYMEM-PRM
000990                               PMYMEM-VAL
001020                               PMYMEM-MEM
001021                               PIN100
001022                               PIN200
001023                               PIN300
001024                               POUT00.
001025   DISPLAY 'PRM:(' PMYMEM-PRM ')'.
001030   DISPLAY 'VAL:(' PMYMEM-VAL(1:PMYMEM-VLL) ')'.
001050   DISPLAY 'MEM:(' PMYMEM-MEM ')'.
001060   IF PMYMEM-COD < ZERO THEN GO TO A00-EXIT.
001061 A00-EXIT.
001070   MOVE ZERO TO RETURN-CODE.
001080   GOBACK.
001090
