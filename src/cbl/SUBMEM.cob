000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. SUBMEM.
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
000130     05   FILLER            PIC X(8)  VALUE 'SUBMEM__'.
000140     05   WK000SECTION      PIC X(8).
000150*--------------------------------------------------------------
000160*- WORK
000170*--------------------------------------------------------------
000180 01       WK                SYNC.
000190   03     WKNXP             PIC 9(5).
000200   03     WKCNT             PIC 9(5).
000210 LINKAGE SECTION.
000220*--------------------------------------------------------------
000230*- パラメータ引数。
000000*-
000240*- I:FNC:機能名称。
000000*-       'INIT':自身が管理するメモリエリアの初期化。
000000*-       'SETD':VALの文字列を自身が管理する文字列領域PMEM-STR
000000*-              にセットし、そのポインタをPVLにセットして返す。
000000*-              VALから文字列を切り出すとき、1文字目から
000000*-              HIGH-VALUEが見つかるまでを切り出す。
000000*-       'SETL':VALの文字列を自身が管理する文字列領域PMEM-STR
000000*-              にセットし、そのポインタをPVLにセットして返す。
000000*-              VALから文字列を切り出すとき、1文字目から
000000*-              VLLまでを切り出す。
000000*-       'GET_':自身が管理する文字列領域PMEM-STRからPVLに指定
000000*-              されたポインタからHIGH-VALUEまでの文字列を
000000*-              切り出し、VALにセットして返す。
000000*-
000250*- I:MEMARA-SZE:文字列領域のバイト数。FNC='INIT'時に必要。
000000*-
000260*- I:VLL:SETする値引数の文字列長。FNC='SET_'時に必要。
000270*- O:VLL:GETされる値引数の文字列長。FNC='GET_'時に返す。
000000*-
000280*- I:PVL:文字列先頭ポインタ。
000290*-       FNC='GET_'時に指定すると、目的の文字列が取得できる。
000300*- O:PVL:文字列先頭ポインタ。
000310*-       FNC='SET_'時に、PMEM-VALをPMEM-STRに設定した後に、
000310*-       PMEM-STRのポインタを返す。
000000*-
000330 01       PMEM-PRM.
000390   03     PMEM-COD          PIC S9(1).
000400   03     PMEM-MSG          PIC X(80).
000340   03     PMEM-FNC          PIC X(4).
000350   03     PMEM-MEMARA-SZE   PIC 9(5).
000360   03     PMEM-VLL          PIC 9(5).
000370   03     PMEM-PVL          PIC 9(5).
000650*--------------------------------------------------------------
000660*- 値受け渡しエリア。
000430*- I:VALの値をVLLの長さ分メモリにセット。
000440*- O:メモリの値をVLLの長さ分VALにセット。
000450*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000460*- 領域を取っている前提。
000320*--------------------------------------------------------------
000481 01       PMEM-VAL.
000481   03     FILLER            PIC X(1).
000650*--------------------------------------------------------------
000660*- メモリエリア。
000510*- SZE:メモリエリアの全体のバイト数。
000520*- PNXT:文字列領域の空き開始ポインタ。
000000*-      SZE 9(5), PNXT 9(5)の次なので、初期値は11となる。
000520*- STR:文字列領域。
000680*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、メモリ領域
000690*- 十分に取っている前提。
000700*--------------------------------------------------------------
000710 01       PMEM-MEMARA.
000610   03     PMEM-SZE          PIC 9(5).
000620   03     PMEM-PNXT         PIC 9(5).
000710   03     PMEM-STR          PIC X(1).
000740*--------------------------------------------------------------
000750 PROCEDURE DIVISION USING PMEM-PRM
000000                          PMEM-VAL
000780                          PMEM-MEMARA.
000790 A00-MAIN SECTION.
000800*--------------------------------------------------------------
000810*-
000820*--------------------------------------------------------------
000830 A00-010.
000840   MOVE 'A00-010.'          TO WK000SECTION.
000850   PERFORM S00-INIT.
000860   EVALUATE PMEM-FNC
000870   WHEN 'INIT' PERFORM B00-INIT
000880   WHEN 'SETD' PERFORM C00-SET-BY-DELIMITER
000880   WHEN 'SETL' PERFORM D00-SET-BY-LENGTH
000890   WHEN 'GET_' PERFORM E00-GET
000900   WHEN OTHER
000910        MOVE 'A00-010.'     TO WK000SECTION
000920        MOVE -1             TO PMEM-COD
000930        STRING WK000MYNAME SPACE ',ABNORMAL END.'
000940                            INTO PMEM-MSG
000950   END-EVALUATE.
000963 A00-EXIT.
000970   MOVE ZERO TO RETURN-CODE.
000980   GOBACK.
000990 B00-INIT SECTION.
001000*--------------------------------------------------------------
001010*- CLEAR MEMORY.
001020*--------------------------------------------------------------
001030 B00-010.
001040   MOVE 'B00-010.'          TO WK000SECTION.
001080   MOVE ALL HIGH-VALUE      TO PMEM-MEMARA(1:PMEM-MEMARA-SZE).
001070   MOVE PMEM-MEMARA-SZE     TO PMEM-SZE.
001050   MOVE 1                   TO PMEM-PNXT.
001100   MOVE ZERO                TO PMEM-COD.
001110   STRING WK000MYNAME SPACE ',NORMAL END.  '
001120                            INTO PMEM-MSG.
001220   IF PMEM-PNXT > PMEM-SZE
001230     THEN
001240*- メモリーサイズを超えたらＡＢＥＮＤ
001250       MOVE -1              TO PMEM-COD
001251       STRING WK000MYNAME SPACE ',ABNORMAL END.'
001252                            INTO PMEM-MSG
001253       GO TO B00-EXIT
001260   END-IF.
001090 B00-EXIT.
001130   EXIT.
001140 C00-SET-BY-DELIMITER SECTION.
001150*--------------------------------------------------------------
001160*- SET VALUE TO MY-MEMORY.
001170*--------------------------------------------------------------
001180 C00-010.
001190   MOVE 'C00-010.'          TO WK000SECTION.
000505   DISPLAY '>>PMEM-VAL:(' PMEM-VAL(1:PMEM-SZE) ')'.
001100   MOVE ZERO                TO WKCNT.
001500   INSPECT PMEM-VAL(1:PMEM-SZE)
001510     TALLYING WKCNT FOR CHARACTERS BEFORE HIGH-VALUE.
000000   MOVE WKCNT               TO PMEM-VLL.
000505   DISPLAY '>>WKCNT:(' WKCNT ')'.
000000   PERFORM D00-SET-BY-LENGTH.
001410 C00-EXIT.
001411   EXIT.
001140 D00-SET-BY-LENGTH SECTION.
001150*--------------------------------------------------------------
001160*- SET VALUE TO MY-MEMORY.
001170*--------------------------------------------------------------
001180 D00-010.
001190   MOVE 'D00-010.'          TO WK000SECTION.
001200*- 次のポインターを計算する
001210   COMPUTE WKNXP = PMEM-PNXT + PMEM-VLL + 1.
001220   IF WKNXP > PMEM-SZE
001230     THEN
001240*- メモリーサイズを超えたらＡＢＥＮＤ
001250       MOVE -1              TO PMEM-COD
001251       STRING WK000MYNAME SPACE ',ABNORMAL END.'
001252                            INTO PMEM-MSG
001253       GO TO D00-EXIT
001260   END-IF.
001270*- 引数の値をカレントポインターからセットする。
001280   MOVE PMEM-VAL(1:PMEM-VLL)
001290                            TO PMEM-STR(PMEM-PNXT:PMEM-VLL).
001330*- カレントポインターを戻り値とする。
001340   MOVE PMEM-PNXT           TO PMEM-PVL.
001350*- 次のポインターをカレントポインターにする。
001360   MOVE WKNXP               TO PMEM-PNXT.
001380   MOVE ZERO                TO PMEM-COD.
001390   STRING WK000MYNAME SPACE ',NORMAL END.  '
001400                            INTO PMEM-MSG.
001410 D00-EXIT.
001411   EXIT.
001420 E00-GET SECTION.
001430*--------------------------------------------------------------
001440*- GET VALUE FROM MY-MEMORY.
001450*--------------------------------------------------------------
001460 E00-010.
001470   MOVE PMEM-PVL            TO WKNXP.
001490   MOVE ZERO                TO WKCNT.
001500   INSPECT PMEM-STR(WKNXP:PMEM-SZE)
001510     TALLYING WKCNT FOR CHARACTERS BEFORE HIGH-VALUE.
001520   MOVE SPACE               TO PMEM-VAL(1:WKCNT).
001554   MOVE PMEM-STR(WKNXP:WKCNT)
001555                            TO PMEM-VAL(1:WKCNT)
001556   MOVE WKCNT               TO PMEM-VLL.
001560 E00-EXIT.
001570   MOVE 'E00-010.'          TO WK000SECTION.
001580   MOVE ZERO                TO PMEM-COD.
001590   STRING WK000MYNAME SPACE ',NORMAL END.  '
001600                            INTO PMEM-MSG.
001610   EXIT.
001620 S00-INIT SECTION.
001630*--------------------------------------------------------------
001640*- 初期化。
001650*--------------------------------------------------------------
001660 S00-010.
001670   MOVE 'S00-010.'          TO WK000SECTION.
001680   MOVE -1                  TO PMEM-COD.
001690   STRING WK000MYNAME SPACE ',ABNORMAL END.'
001700                            INTO PMEM-MSG.
001710 S00-EXIT.
001720   EXIT.
