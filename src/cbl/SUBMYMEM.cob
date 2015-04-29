000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID.   SUBMYMEM.
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
000130     05   FILLER            PIC X(8)  VALUE 'SUBMYMEM'.
000140     05   WK000SECTION      PIC X(8).
000150*--------------------------------------------------------------
000160*- WORK
000170*--------------------------------------------------------------
000180 01       WK                SYNC.
000190   03     WKNXP             PIC 9(5).
000200   03     WKCNT             PIC 9(5).
000201   03     WK-OPD.
000203     05   WK-TY             PIC X(1).
000204     05   WK-POS            PIC 9(5).
000205     05   WK-LEN            PIC 9(5). 
000490*--------------------------------------------------------------
000500*- メモリコントロールエリア。
000510*- SZE:メモリのバイト数。
000520*- CNT:空きメモリの開始アドレス。
000590*--------------------------------------------------------------
000600 01       PMYMEM-CTR        SYNC.
000610   03     PMYMEM-SZE        PIC 9(5).
000620   03     PMYMEM-CNT        PIC 9(5).
000210 LINKAGE SECTION.
000220*--------------------------------------------------------------
000230*- メモリパラメータ引数。
000320*--------------------------------------------------------------
000330 01       PMYMEM-PRM.
000340   COPY   RMYMEM-PRM.
000410*--------------------------------------------------------------
000420*- 値受渡引数。
000430*- I:VALの値をVLLの長さ分メモリにセット。
000440*- O:メモリの値をVLLの長さ分VALにセット。
000450*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000460*- 領域を取っている前提。
000470*--------------------------------------------------------------
000480 01       PMYMEM-VAL.
000481   03     FILLER            PIC X(1).
000650*--------------------------------------------------------------
000660*- メモリエリア。
000670*- MEM:メモリ領域。
000680*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000690*- 領域を取っている前提。
000700*--------------------------------------------------------------
000710 01       PMYMEM-MEM        PIC X(1).
000711*--------------------------------------------------------------
000712*- IN1
000713*--------------------------------------------------------------
000714 01       PIN100.
000715   03     FILLER            PIC X(1).
000716*--------------------------------------------------------------
000717*- IN2
000718*--------------------------------------------------------------
000719 01       PIN200.
000720   03     FILLER            PIC X(1).
000721*--------------------------------------------------------------
000722*- IN3
000723*--------------------------------------------------------------
000724 01       PIN300.
000725   03     FILLER            PIC X(1).
000726*--------------------------------------------------------------
000727*- OUT
000728*--------------------------------------------------------------
000729 01       POUT00.
000730   03     FILLER            PIC X(1).
000732*--------------------------------------------------------------
000733*-
000740*--------------------------------------------------------------
000750 PROCEDURE DIVISION USING PMYMEM-PRM
000760                          PMYMEM-VAL
000780                          PMYMEM-MEM
000781                          PIN100
000782                          PIN200
000783                          PIN300
000784                          POUT00.
000790 A00-MAIN SECTION.
000800*--------------------------------------------------------------
000810*-
000820*--------------------------------------------------------------
000830 A00-010.
000840   MOVE 'A00-010.'          TO WK000SECTION.
000850   PERFORM S00-INIT.
000860   EVALUATE PMYMEM-FNC
000870   WHEN 'INIT' PERFORM B00-INIT
000880   WHEN 'SET_' PERFORM C00-SET
000890   WHEN 'GET_' PERFORM D00-GET
000900   WHEN OTHER
000910        MOVE 'A00-010.'     TO WK000SECTION
000920        MOVE -1             TO PMYMEM-COD
000930        STRING WK000MYNAME SPACE ',ABNORMAL END.'
000940                            INTO PMYMEM-MSG
000950   END-EVALUATE.
000951*DEBUG
000952*   IF PMYMEM-FNC NOT = 'INIT'
000953*    THEN
000961*     DISPLAY 'MYMEM:' PMYMEM-FNC ':' PMYMEM-VAL(1:PMYMEM-VLL)
000962*     DISPLAY 'MYMEM:' PMYMEM-FNC ':' PMYMEM-MEM(1:PMYMEM-SZE).
000963 A00-EXIT.
000970   MOVE ZERO TO RETURN-CODE.
000980   GOBACK.
000990 B00-INIT SECTION.
001000*--------------------------------------------------------------
001010*- CLEAR MEMORY.
001020*--------------------------------------------------------------
001030 B00-010.
001040   MOVE 'B00-010.'          TO WK000SECTION.
001050   MOVE 1                   TO PMYMEM-CNT.
001070   MOVE PMYMEM-MEM-SZE      TO PMYMEM-SZE.
001080   MOVE ALL HIGH-VALUE       TO PMYMEM-MEM(1:PMYMEM-SZE).
001090 B00-EXIT.
001100   MOVE ZERO                TO PMYMEM-COD.
001110   STRING WK000MYNAME SPACE ',NORMAL END.  '
001120                            INTO PMYMEM-MSG.
001130   EXIT.
001140 C00-SET SECTION.
001150*--------------------------------------------------------------
001160*- SET VALUE TO MY-MEMORY.
001170*--------------------------------------------------------------
001180 C00-010.
001190   MOVE 'C00-010.'          TO WK000SECTION.
001200*- 次のポインターを計算する
001210   COMPUTE WKNXP = PMYMEM-CNT + PMYMEM-VLL + 1.
001220   IF WKNXP > PMYMEM-SZE
001230     THEN
001240*- メモリーサイズを超えたらＡＢＥＮＤ
001250       MOVE -1              TO PMYMEM-COD
001251       STRING WK000MYNAME SPACE ',ABNORMAL END.'
001252                            INTO PMYMEM-MSG
001253       GO TO C00-EXIT
001260   END-IF.
001270*- 引数の値をカレントポインターからセットする。
001280   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
001290                       TO PMYMEM-MEM(PMYMEM-CNT:PMYMEM-VLL).
001330*- カレントポインターを戻り値とする。
001340   MOVE PMYMEM-CNT          TO PMYMEM-VLI.
001350*- 次のポインターをカレントポインターにする。
001360   MOVE WKNXP               TO PMYMEM-CNT.
001380   MOVE ZERO                TO PMYMEM-COD.
001390   STRING WK000MYNAME SPACE ',NORMAL END.  '
001400                            INTO PMYMEM-MSG.
001410 C00-EXIT.
001411   EXIT.
001420 D00-GET SECTION.
001430*--------------------------------------------------------------
001440*- GET VALUE FROM MY-MEMORY.
001450*--------------------------------------------------------------
001460 D00-010.
001470   MOVE PMYMEM-VLI          TO WKNXP.
001490   MOVE ZERO                TO WKCNT.
001500   INSPECT PMYMEM-MEM(WKNXP:PMYMEM-SZE)
001510     TALLYING WKCNT FOR CHARACTERS BEFORE HIGH-VALUE.
001520   MOVE SPACE               TO PMYMEM-VAL.
001524   EVALUATE PMYMEM-MEM(WKNXP:1)
001525   WHEN '1'
001526     MOVE PMYMEM-MEM(WKNXP:WKCNT)
001527                            TO WK-OPD
001528     MOVE PIN100(WK-POS:WK-LEN)
001529                            TO PMYMEM-VAL(1:WK-LEN)
001530     MOVE WK-LEN            TO PMYMEM-VLL
001531   WHEN '2'
001532     MOVE PMYMEM-MEM(WKNXP:WKCNT)
001533                            TO WK-OPD
001534     MOVE PIN200(WK-POS:WK-LEN)
001535                            TO PMYMEM-VAL(1:WK-LEN)
001536     MOVE WK-LEN            TO PMYMEM-VLL
001537   WHEN '3'
001538     MOVE PMYMEM-MEM(WKNXP:WKCNT)
001539                            TO WK-OPD
001540     MOVE PIN300(WK-POS:WK-LEN)
001541                            TO PMYMEM-VAL(1:WK-LEN)
001542     MOVE WK-LEN            TO PMYMEM-VLL
001543   WHEN 'O'
001544     MOVE PMYMEM-MEM(WKNXP:WKCNT)
001545                            TO WK-OPD
001546     MOVE POUT00(WK-POS:WK-LEN)
001547                            TO PMYMEM-VAL(1:WK-LEN)
001548     MOVE WK-LEN            TO PMYMEM-VLL
001549   WHEN 'L'
001550*-   頭1桁が"L"なので、それをスキップするため＋１
001551     ADD 1                  TO WKNXP
001552*-   頭1桁が"L"なので、それ分長さを短くするため－１
001553     ADD -1                 TO WKCNT
001554     MOVE PMYMEM-MEM(WKNXP:WKCNT)
001555                            TO PMYMEM-VAL(1:WKCNT)
001556     MOVE WKCNT             TO PMYMEM-VLL
001558   END-EVALUATE.
001560 D00-EXIT.
001570   MOVE 'D00-010.'          TO WK000SECTION.
001580   MOVE ZERO                TO PMYMEM-COD.
001590   STRING WK000MYNAME SPACE ',NORMAL END.  '
001600                            INTO PMYMEM-MSG.
001610   EXIT.
001620 S00-INIT SECTION.
001630*--------------------------------------------------------------
001640*- 初期化。
001650*--------------------------------------------------------------
001660 S00-010.
001670   MOVE 'S00-010.'          TO WK000SECTION.
001680   MOVE -1                  TO PMYMEM-COD.
001690   STRING WK000MYNAME SPACE ',ABNORMAL END.'
001700                            INTO PMYMEM-MSG.
001710 S00-EXIT.
001720   EXIT.
