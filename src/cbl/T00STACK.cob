000020 IDENTIFICATION DIVISION.
000030 PROGRAM-ID.   T00STACK.
000040 ENVIRONMENT    DIVISION.
000041 DATA DIVISION.
000070 WORKING-STORAGE SECTION.
000120*--------------------------------------------------------------
000130*- スタックパラメータ引数。
000148*--------------------------------------------------------------
000149 01       PSTACK-PRM        SYNC.
000340   COPY   RSTACK-PRM.
000174*--------------------------------------------------------------
000175*- 値受渡引数。
000176*- I:VALの値をVLLの長さ分メモリにセット。
000177*- O:メモリの値をVLLの長さ分VALにセット。
000178*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000179*- 領域を取っている前提。
000180*--------------------------------------------------------------
000181 01       PSTACK-VAL        SYNC.
000182   03     FILLER            PIC X(100).
000183*--------------------------------------------------------------
000184*- メモリコントロールエリア。
000185*- SZE:メモリのバイト数。
000186*- CNT:空きメモリの開始アドレス。
000187*- ADRIDX:使用済メモリ開始アドレス管理テーブルのカレントIDX。
000188*- ADRTBL:使用済メモリ開始アドレス管理テーブル。
000189*- "OCCURS 1"は、ちょっとトリッキーですが、呼び出し側で、
000190*- 十分な領域を取っている前提。
000193*--------------------------------------------------------------
000194 01       PSTACK-CTR        SYNC.
000195   03     FILLER            PIC X(100).
000196*--------------------------------------------------------------
000197*- メモリエリア。
000198*- MEM:メモリ領域。
000199*- "X(1)"は、ちょっとトリッキーですが、呼び出し側で、十分な
000200*- 領域を取っている前提。
000201*--------------------------------------------------------------
000202 01       PSTACK-MEM        SYNC.
000203   03     FILLER            PIC X(100).
000204 PROCEDURE DIVISION.   
000205 A00-MAIN SECTION.
000206*--------------------------------------------------------------
000207*- SUBSTACKのテストメインルーチン
000208*--------------------------------------------------------------
000215 A00-010.
000559   MOVE SPACE               TO PSTACK-PRM.
000560   MOVE 'INIT'              TO PSTACK-FNC.
000561   MOVE 100                 TO PSTACK-MEM-SZE.
000562   CALL 'SUBSTACK'       USING PSTACK-PRM
000563                               PSTACK-VAL
000564                               PSTACK-CTR
000565                               PSTACK-MEM.
000566*-
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE 'HELLO'             TO PSTACK-VAL.
000570   MOVE 5                   TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000613*-
000614   MOVE SPACE               TO PSTACK-PRM.
000615   MOVE 'PUSH'              TO PSTACK-FNC.
000616   MOVE 'WORLD!'            TO PSTACK-VAL.
000617   MOVE 6                   TO PSTACK-VLL.
000618   CALL 'SUBSTACK'       USING PSTACK-PRM
000619                               PSTACK-VAL
000620                               PSTACK-CTR
000621                               PSTACK-MEM.
000622   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000623   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000624   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000625   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000626*- WORLD!のはず
000634   MOVE SPACE               TO PSTACK-PRM.
000635   MOVE 'POP_'              TO PSTACK-FNC.
000638   CALL 'SUBSTACK'       USING PSTACK-PRM
000639                               PSTACK-VAL
000640                               PSTACK-CTR
000641                               PSTACK-MEM.
000642   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000643   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000644   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000645   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000646*- HELLOのはず
000654   MOVE SPACE               TO PSTACK-PRM.
000655   MOVE 'PEEK'              TO PSTACK-FNC.
000656   CALL 'SUBSTACK'       USING PSTACK-PRM
000657                               PSTACK-VAL
000658                               PSTACK-CTR
000659                               PSTACK-MEM.
000660   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000661   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000662   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000663   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000664*- HELLOのはず
000672   MOVE SPACE               TO PSTACK-PRM.
000673   MOVE 'PEEK'              TO PSTACK-FNC.
000674   CALL 'SUBSTACK'       USING PSTACK-PRM
000675                               PSTACK-VAL
000676                               PSTACK-CTR
000677                               PSTACK-MEM.
000678   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000679   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000680   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000681   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000690*- HELLOのはず
000698   MOVE SPACE               TO PSTACK-PRM.
000699   MOVE 'POP_'              TO PSTACK-FNC.
000700   CALL 'SUBSTACK'       USING PSTACK-PRM
000701                               PSTACK-VAL
000702                               PSTACK-CTR
000703                               PSTACK-MEM.
000704   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000705   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000706   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000707   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000734*- 空文字が返る。
000735   MOVE SPACE               TO PSTACK-PRM.
000736   MOVE 'POP_'              TO PSTACK-FNC.
000737   CALL 'SUBSTACK'       USING PSTACK-PRM
000738                               PSTACK-VAL
000739                               PSTACK-CTR
000740                               PSTACK-MEM.
000741   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000742   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000743   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000744   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000734*- 空文字が返る。
000790   MOVE SPACE               TO PSTACK-PRM.
000791   MOVE 'POP_'              TO PSTACK-FNC.
000792   CALL 'SUBSTACK'       USING PSTACK-PRM
000793                               PSTACK-VAL
000794                               PSTACK-CTR
000795                               PSTACK-MEM.
000796   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000797   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000798   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000799   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000566*-time 1. next pointer is 22.
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE '11111111111111111111' TO PSTACK-VAL.
000570   MOVE 20                  TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000566*-time 2. next poniter is 43.
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE '22222222222222222222' TO PSTACK-VAL.
000570   MOVE 20                  TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000566*-time 3. next pointer is 64.
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE '33333333333333333333' TO PSTACK-VAL.
000570   MOVE 20                  TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000566*-time 4. next pointer is 85.
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE '44444444444444444444' TO PSTACK-VAL.
000570   MOVE 20                  TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000566*-time 5. next pointer is 105. out of memory abend.
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE '55555555555555555555' TO PSTACK-VAL.
000570   MOVE 20                  TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000566*-time 6. time 5 is abend. not execute.
000567   MOVE SPACE               TO PSTACK-PRM.
000568   MOVE 'PUSH'              TO PSTACK-FNC.
000569   MOVE '55555555555555555555' TO PSTACK-VAL.
000570   MOVE 20                  TO PSTACK-VLL.
000571   CALL 'SUBSTACK'       USING PSTACK-PRM
000572                               PSTACK-VAL
000573                               PSTACK-CTR
000574                               PSTACK-MEM.
000589   DISPLAY 'PRM:(' PSTACK-PRM ')'.
000590   DISPLAY 'VAL:(' PSTACK-VAL(1:PSTACK-VLL) ')'.
000591   DISPLAY 'CTR:(' PSTACK-CTR ')'.
000592   DISPLAY 'MEM:(' PSTACK-MEM ')'.
000800 A00-EXIT.
000801   MOVE ZERO TO RETURN-CODE.
000802   GOBACK.
000810
