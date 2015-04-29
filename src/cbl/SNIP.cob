000020 IDENTIFICATION DIVISION.
000030 PROGRAM-ID.   SNIP.
000040 ENVIRONMENT    DIVISION.
000138 DATA DIVISION.
000144 WORKING-STORAGE SECTION.
000148 01       WK001             SYNC.
000169   03     WK001VAL1         PIC X(10) VALUE '"12345678"'.
000173   03     WK001VAL2         PIC X(10) VALUE '123 567890'.
000175   03     WK001VAL3         PIC X(10) VALUE '123 ******'.
000176   03     WK001VAL4         PIC X(10) VALUE '****56****'.
000177   03     WK001WK           PIC X(10).
000178 01       WK                SYNC.
000179   03     WKI               PIC S9(2) COMP VALUE ZERO.
000180   03     WKJ               PIC S9(2) COMP VALUE ZERO.
000181 PROCEDURE DIVISION.   
000182 A00-MAIN SECTION.
000183*--------------------------------------------------------------
000184*-
000185*--------------------------------------------------------------
000190 A00-010.
000582*- ダブルクォートでくくられた文字列長を求める
000583   MOVE ZERO                TO WKI.
000584   INSPECT WK001VAL1 TALLYING WKI FOR CHARACTERS
000585   AFTER '"' BEFORE '"'. 
000586   DISPLAY WKI.
000587*- 前方一致検索
000588   DISPLAY '(' WK001VAL2 ')?(' WK001VAL3 ')'.
000589*-- 1).最初の"*"までの文字列長を求める。
000590   MOVE ZERO                TO WKI.
000591   INSPECT WK001VAL3  TALLYING WKI
000601     FOR CHARACTERS BEFORE '*'.
000604*-- 2).比較
000605   DISPLAY '(' WK001VAL2(1:WKI) ')=(' WK001VAL3(1:WKI) ')'.
000609 A00-EXIT.
000610   MOVE ZERO TO RETURN-CODE.
000611   GOBACK.
000620
