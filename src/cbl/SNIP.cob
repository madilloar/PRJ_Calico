000020 IDENTIFICATION DIVISION.
000030 PROGRAM-ID.   SNIP.
000040 ENVIRONMENT    DIVISION.
000138 DATA DIVISION.
000144 WORKING-STORAGE SECTION.
000148 01       WK001             SYNC.
000169   03     WK001VAL1         PIC X(10) VALUE 'ABCDEFGHIJ'.
000177   03     WK001WK           PIC X(10).
000178 01       WK                SYNC.
000179   03     WKH               PIC 9(8) BINARY VALUE ZERO.
000179   03     WKI               PIC 9(5) COMP VALUE ZERO.
000180   03     WKJ               PIC 9(5) COMP VALUE ZERO.
000179   03     WKK               PIC 9(5) COMP VALUE ZERO.
000180   03     WKL               PIC 9(5) COMP VALUE ZERO.
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
000000*--------------------------------------------------------------
000000*- 文字列ハッシュ値計算ワーク
000000*- 文字列のハッシュ値の計算は、先頭10文字だけで計算する。
000000*--------------------------------------------------------------
000000 01       PHSH-PRM          SYNC.
000000   03     PHSH-VAL          PIC 9(8) BINARY VALUE ZERO.
000000   03     PHSH-CHRIDX       PIC 9(1) BINARY VALUE ZERO.
000000   03     PHSH-CHR.
000000     05   PHSH-CHRVAL       PIC 9(2) BINARY VALUE ZERO.
000000   03     PHSH-STR          PIC X(10).
000000*--------------------------------------------------------------
000000*- ハッシュバケットINDEX計算ワーク
000000*- ハッシュバケットのサイズは素数がよいらしい。
000000*--------------------------------------------------------------
000000 01       PBKT-PRM          SYNC.
000000   03     PBKT-SZE          PIC 9(5).
000000   03     PBKT-RECSZE      PIC 9(5).
000000   03     PBKT-REC.
000000     05   PBKT-PBF          PIC 9(5).
000000     05   PBKT-PKY          PIC 9(5).
000000     05   PBKT-PVL          PIC 9(5).
000000   03     PBKT-ARYSZE       PIC 9(4).
000000   03     PBKT-DMY          PIC 9(4).
000000   03     PBKT-IDX          PIC 9(4).
000181 PROCEDURE DIVISION.
000182 A00-MAIN SECTION.
000183*--------------------------------------------------------------
000184*-
000185*--------------------------------------------------------------
000190 A00-010.
000000*---
000000*- 文字列のハッシュ値を求める。
000000*---
000000   MOVE ZERO               TO PHSH-PRM.
000000   MOVE SPACE              TO PHSH-STR.
000000   MOVE 'ABCDEFGHIJKLM'    TO PHSH-STR.
000000   PERFORM S10-GET-HASH-VAL.
000000   DISPLAY 'HASHVAL(' PHSH-VAL ')'.
000000*---
000000*- 文字列のハッシュバケットINDEXを求める。
000000*---
000000   MOVE ZERO               TO PHSH-PRM.
000000   MOVE SPACE              TO PHSH-STR.
000000   MOVE 'ABCDEFGHIJKLM'    TO PHSH-STR.
000000   MOVE 7                  TO PBKT-ARYSZE.
000000   PERFORM S20-GET-BAKETS-INDEX.
000000   DISPLAY 'BAKETS_IDX(' PBKT-IDX '),HASHVAL(' PHSH-VAL ')'.
000000*---
000000*- ハッシュバケットのアロケート。
000000*---
000000   MOVE SPACE               TO PMEM-PRM.
000000   MOVE 'INIT'              TO PMEM-FNC.
000000   MOVE 100                 TO PMEM-MEMARA-SZE.
000000   CALL 'SUBMYMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000000                               PMEM-MEMARA.
000000   MOVE ZERO                TO PBKT-PRM.
000000   MOVE 5                   TO PBKT-ARYSZE.
000000   MOVE 15                  TO PBKT-RECSZE.
000000   PERFORM S30-ALLOCATE-BAKETS.
000609 A00-EXIT.
000610   MOVE ZERO TO RETURN-CODE.
000611   GOBACK.
000000 S10-GET-HASH-VAL SECTION.
000000*--------------------------------------------------------------
000000*- 文字列のハッシュ値を求める。
000000*-
000000*- PARAMETER:
000000*-   PHSH-STR PIC X(10).
000000*-     ハッシュ値計算対象の文字列。10文字までしか計算しない。
000000*-
000000*- RETURN:
000000*-   PHSH-VAL PIC 9(8) BINARY.
000000*-     ハッシュ値。
000000*-
000000*-   全ての文字で計算した方が衝突が少ないのだが、
000000*-   長い文字列だと、ハッシュ値の計算に時間がかるので、
000000*-   1から10文字目まででハッシュ値を計算する。
000000*-   どうせ、この後のハッシュバケットの添え字を計算する時に
000000*-   「ハッシュ値 MOD バケット数 => バケット添え字」
000000*-   の計算をするので、この時にも添え字の衝突が起きるため。
000000*--------------------------------------------------------------
000000 S10-010.
000000   MOVE ZERO               TO PHSH-VAL.
000000   PERFORM VARYING PHSH-CHRIDX FROM 1 BY 1
000000   UNTIL PHSH-CHRIDX > 10
000000     MOVE PHSH-STR(PHSH-CHRIDX:1) TO PHSH-CHR
000000     COMPUTE PHSH-VAL = PHSH-VAL * 31 + PHSH-CHRVAL
000000*- DEBUG:
000000     DISPLAY 'HASH_VALUE:' PHSH-VAL ',CHAR:' PHSH-CHRVAL
000000   END-PERFORM.
000000 S10-EXIT.
000000   EXIT.
000000 S20-GET-BAKETS-INDEX SECTION.
000000*--------------------------------------------------------------
000000*- ハッシュバケットの添え字を求める。
000000*-
000000*- PARAMETER:
000000*-   PHSH-PRM.
000000*-   PBKT-ARYSZE PIC 9(4).
000000*-
000000*- RETURN:
000000*-   PBKT-IDX PIC 9(4).
000000*--------------------------------------------------------------
000000 S20-010.
000000   PERFORM S10-GET-HASH-VAL.
000000   DIVIDE PHSH-VAL BY PBKT-ARYSZE GIVING PBKT-DMY
000000     REMAINDER PBKT-IDX.
000000 S20-EXIT.
000000   EXIT.
000000 S30-ALLOCATE-BAKETS SECTION.
000000*--------------------------------------------------------------
000000*- ハッシュバケットの領域確保。
000000*--------------------------------------------------------------
000000 S30-010.
000000   COMPUTE PBKT-SZE = PBKT-ARYSZE * PBKT-RECSZE.
000000   MOVE SPACE               TO PMEM-PRM.
000000   MOVE 'SETL'              TO PMEM-FNC.
000000   MOVE ZERO                TO PMEM-VAL(1:PBKT-SZE).
000000   MOVE PBKT-SZE            TO PMEM-VLL.
000000   CALL 'SUBMYMEM'       USING PMEM-PRM
000000                               PMEM-VAL
000000                               PMEM-MEMARA.
000000   DISPLAY 'PRM:(' PMEM-PRM ')'.
000000   DISPLAY 'VAL:(' PMEM-VAL ')'.
000000   DISPLAY 'MEM:(' PMEM-MEMARA ')'.
000000 S30-EXIT.
000000   EXIT.

