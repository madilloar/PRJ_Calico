000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.   SUBSTACK.
000030 ENVIRONMENT    DIVISION.
000040 CONFIGURATION  SECTION.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070*--------------------------------------------------------------
000080*- ABEND時の手がかり用。
000090*--------------------------------------------------------------
000100 01       WK000             SYNC.
000110   03     WK000MYNAME.
000120     05   FILLER            PIC X(11) VALUE 'PROGRAM-ID:'.
000130     05   FILLER            PIC X(8)  VALUE 'SUBSTACK'.
000140     05   WK000SECTION      PIC X(8).
000150*--------------------------------------------------------------
000160*- WORK
000170*--------------------------------------------------------------
000180 01       WK                SYNC.
000190   03     WKNXP             PIC 9(5).
000200   03     WKCNT             PIC 9(5).
000210 LINKAGE SECTION.
000220*--------------------------------------------------------------
000230*- スタックパラメータ引数。
000320*--------------------------------------------------------------
000330 01       PSTACK-PRM.
000340   COPY   RSTACK-PRM.
000410*--------------------------------------------------------------
000420*- 値受渡引数。
000430*- I:VALの値をVLLの長さ分メモリにセット。
000440*- O:メモリの値をVLLの長さ分VALにセット。
000450*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000460*- 領域を取っている前提。
000470*--------------------------------------------------------------
000480 01       PSTACK-VAL        PIC X(1).
000490*--------------------------------------------------------------
000500*- スタックメモリコントロールエリア。
000590*--------------------------------------------------------------
000600 01       PSTACK-CTR.
000340   COPY   RSTACK-CTR.
000652*--------------------------------------------------------------
000660*- メモリエリア。
000670*- MEM:メモリ領域。
000680*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000690*- 領域を取っている前提。
000700*--------------------------------------------------------------
000710 01       PSTACK-MEM        PIC X(1).
000720*--------------------------------------------------------------
000730*-
000740*--------------------------------------------------------------
000750 PROCEDURE DIVISION USING PSTACK-PRM
000760                          PSTACK-VAL
000770                          PSTACK-CTR
000780                          PSTACK-MEM.
000790 A00-MAIN SECTION.
000800*--------------------------------------------------------------
000810*-
000820*--------------------------------------------------------------
000830 A00-010.
000840   MOVE 'A00-010.'          TO WK000SECTION.
000850   PERFORM S00-INIT.
000860   EVALUATE PSTACK-FNC
000870   WHEN 'INIT' PERFORM B00-INIT
000880   WHEN 'PUSH' PERFORM C00-PUSH
000890   WHEN 'PEEK' PERFORM D00-PEEK
000910   WHEN 'POP_' PERFORM E00-POP
000911   WHEN 'GET_' PERFORM F00-GET
000912   WHEN OTHER
000920        MOVE 'A00-010.'     TO WK000SECTION
000930        MOVE -1             TO PSTACK-COD
000940        STRING WK000MYNAME SPACE ',ABNORMAL END.'
000950                            INTO PSTACK-MSG
000960   END-EVALUATE.
000970 A00-EXIT.
000980   MOVE ZERO TO RETURN-CODE.
000990   GOBACK.
001000 B00-INIT SECTION.
001010*--------------------------------------------------------------
001020*- CLEAR MEMORY.
001030*--------------------------------------------------------------
001040 B00-010.
001050   MOVE 'B00-010.'          TO WK000SECTION.
001060   MOVE 1                   TO PSTACK-CNT.
001070   MOVE ZERO                TO PSTACK-ADRIDX.
001080   MOVE PSTACK-MEM-SZE      TO PSTACK-SZE.
001090   MOVE ALL HIGH-VALUE      TO PSTACK-MEM(1:PSTACK-SZE).
001100 B00-EXIT.
001110   MOVE ZERO                TO PSTACK-COD.
001120   STRING WK000MYNAME SPACE ',NORMAL END.  '
001130                            INTO PSTACK-MSG.
001140   EXIT.
001150 C00-PUSH SECTION.
001160*--------------------------------------------------------------
001170*- PUSH TO STACK.
001180*--------------------------------------------------------------
001190 C00-010.
001200   MOVE 'C00-010.'          TO WK000SECTION.
001210*- 次のポインターを計算する
001220   COMPUTE WKNXP = PSTACK-CNT + PSTACK-VLL + 1.
001230   IF WKNXP > PSTACK-SZE
001240     THEN
001250*- メモリーサイズを超えたら異常。呼び出し元でＡＢＥＮＤのこと。
001960      MOVE -1               TO PSTACK-COD
001970      STRING WK000MYNAME SPACE ',ABNORMAL END.'
001980                          INTO PSTACK-MSG
001980      GO TO C00-EXIT
001270   END-IF.
001280*- 引数の値をカレントポインターからセットする。
001290   MOVE PSTACK-VAL(1:PSTACK-VLL)
001300                       TO PSTACK-MEM(PSTACK-CNT:PSTACK-VLL).
001310*- カレントポインターを覚える。
001320   ADD 1                    TO PSTACK-ADRIDX.
001330   MOVE PSTACK-CNT          TO PSTACK-ADRTBL(PSTACK-ADRIDX).
001360*- 次のポインターをカレントポインターにする。
001370   MOVE WKNXP               TO PSTACK-CNT.
001390   MOVE ZERO                TO PSTACK-COD.
001400   STRING WK000MYNAME SPACE ',NORMAL END.  '
001410                            INTO PSTACK-MSG.
001380 C00-EXIT.
001420   EXIT.
001430 D00-PEEK SECTION.
001440*--------------------------------------------------------------
001450*- PEEK STACK TOP.
001460*--------------------------------------------------------------
001470 D00-010.
001580   MOVE 'D00-010.'          TO WK000SECTION.
001480   MOVE PSTACK-ADRTBL(PSTACK-ADRIDX)
001490                            TO WKNXP.
001500   MOVE ZERO                TO WKCNT.
001510   INSPECT PSTACK-MEM(WKNXP:PSTACK-SZE)
001520     TALLYING WKCNT FOR CHARACTERS BEFORE HIGH-VALUE.
001530   MOVE SPACE               TO PSTACK-VAL.
001540   MOVE PSTACK-MEM(WKNXP:WKCNT)
001550                            TO PSTACK-VAL(1:WKCNT).
001560   MOVE WKCNT               TO PSTACK-VLL.
001570 D00-EXIT.
001590   MOVE ZERO                TO PSTACK-COD.
001600   STRING WK000MYNAME SPACE ',NORMAL END.  '
001610                            INTO PSTACK-MSG.
001620   EXIT.
001630 E00-POP SECTION.
001640*--------------------------------------------------------------
001650*- POP STACK TOP.
001660*--------------------------------------------------------------
001670 E00-010.
001860   MOVE 'E00-010.'          TO WK000SECTION.
001680   IF PSTACK-ADRIDX <= ZERO
001690     THEN
001700       MOVE SPACE           TO PSTACK-VAL
001710       MOVE ZERO            TO PSTACK-VLL
001720       GO TO E00-EXIT
001730   END-IF.
001740   MOVE PSTACK-ADRTBL(PSTACK-ADRIDX)
001750                            TO WKNXP.
001760   MOVE ZERO                TO WKCNT.
001770   INSPECT PSTACK-MEM(WKNXP:PSTACK-SZE)
001780     TALLYING WKCNT FOR CHARACTERS BEFORE HIGH-VALUE.
001790   MOVE SPACE               TO PSTACK-VAL.
001800   MOVE PSTACK-MEM(WKNXP:WKCNT)
001810                            TO PSTACK-VAL(1:WKCNT).
001820   MOVE WKCNT               TO PSTACK-VLL.
001830   MOVE ALL HIGH-VALUE      TO PSTACK-MEM(WKNXP:WKCNT).
001840   ADD -1                   TO PSTACK-ADRIDX.
001841   COMPUTE WKNXP = PSTACK-CNT - PSTACK-VLL - 1.
001843   MOVE WKNXP               TO PSTACK-CNT.
001850 E00-EXIT.
001870   MOVE ZERO                TO PSTACK-COD.
001880   STRING WK000MYNAME SPACE ',NORMAL END.  '
001890                            INTO PSTACK-MSG.
001900   EXIT.
001910 F00-GET SECTION.
001911*--------------------------------------------------------------
001912*- GET STACK BY INDEX.
001913*--------------------------------------------------------------
001914 F00-010.
001946   MOVE 'F00-010.'          TO WK000SECTION.
001921   IF PSTACK-VLI <= ZERO
001922     THEN
001923       MOVE SPACE           TO PSTACK-VAL
001924       MOVE ZERO            TO PSTACK-VLL
001925       GO TO F00-EXIT
001926   END-IF.
001927   IF PSTACK-VLI > PSTACK-ADRIDX
001928     THEN
001929       MOVE SPACE           TO PSTACK-VAL
001930       MOVE ZERO            TO PSTACK-VLL
001931       GO TO F00-EXIT
001932   END-IF.
001933   MOVE PSTACK-ADRTBL(PSTACK-VLI)
001934                            TO WKNXP.
001935   MOVE ZERO                TO WKCNT.
001936   INSPECT PSTACK-MEM(WKNXP:PSTACK-SZE)
001937     TALLYING WKCNT FOR CHARACTERS BEFORE HIGH-VALUE.
001938   MOVE SPACE               TO PSTACK-VAL.
001939   MOVE PSTACK-MEM(WKNXP:WKCNT)
001940                            TO PSTACK-VAL(1:WKCNT).
001941   MOVE WKCNT               TO PSTACK-VLL.
001945 F00-EXIT.
001947   MOVE ZERO                TO PSTACK-COD.
001948   STRING WK000MYNAME SPACE ',NORMAL END.  '
001949                            INTO PSTACK-MSG.
001950   EXIT.
001951 S00-INIT SECTION.
001952*--------------------------------------------------------------
001953*- 初期化。
001954*--------------------------------------------------------------
001955 S00-010.
001956   MOVE 'S00-010.'          TO WK000SECTION.
001960   MOVE -1                  TO PSTACK-COD.
001970   STRING WK000MYNAME SPACE ',ABNORMAL END.'
001980                            INTO PSTACK-MSG.
001990 S00-EXIT.
