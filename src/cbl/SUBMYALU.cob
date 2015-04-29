000010*>
000020 IDENTIFICATION DIVISION.
000030 PROGRAM-ID.   SUBMYALU.
000040 ENVIRONMENT    DIVISION.
000050 CONFIGURATION  SECTION.
000060 DATA DIVISION.
000070 WORKING-STORAGE SECTION.
000080*--------------------------------------------------------------
000090*- その他ワーク。
000100*--------------------------------------------------------------
000110 01       WK000             SYNC.
000120   03     WK000MYNAME.
000130     05   FILLER            PIC X(11) VALUE 'PROGRAM-ID:'.
000140     05   FILLER            PIC X(8)  VALUE 'SUBMYALU'.
000150     05   WK000SECTION      PIC X(8).
000160 01       WKB00             SYNC.
000170   03     WKB00I            PIC 9(5).
000180   03     WKB00J            PIC 9(5).
000190   03     WKB00K            PIC 9(5).
000200 01       WKC00             SYNC.
000210   03     WKC00IS-END       PIC 9(5).
000220   03     WKC00LEFT-9       PIC 9(5).
000230   03     WKC00RIGHT-9      PIC 9(5).
000240   03     WKC00LEFT-X       PIC X(100).
000250   03     WKC00RIGHT-X      PIC X(100).
000260   03     WKC00RIGH2-X      PIC X(100).
000270   03     WKC00LEFT-LL      PIC 9(5).
000280   03     WKC00RIGHT-LL     PIC 9(5).
000290   03     WKC00RIGH2-LL     PIC 9(5).
000300 LINKAGE SECTION.
000310*--------------------------------------------------------------
000133*- 仮想コード:仮想コードモジュールCALLパラメータ。
000330*--------------------------------------------------------------
000340 01  PVCODE-PRM.
000201   COPY   RVCODE-PRM.
000132*--------------------------------------------------------------
000133*- スタック:スタックモジュールCALLパラメータ。
000134*--------------------------------------------------------------
000500 01       PSTACK-PRM.
000340   COPY   RSTACK-PRM.
000144*--------------------------------------------------------------
000145*- スタック:右辺と左辺データを格納したアドレス情報(9(5))を、
000145*- POPやPUSHするときの受け渡しエリア。
000146*--------------------------------------------------------------
000580 01       PSTACK-VAL.
000590   03     FILLER            PIC X(5).
000149*--------------------------------------------------------------
000150*- スタック:スタック内部制御情報。
000151*--------------------------------------------------------------
000600 01       PSTACK-CTR.
000340   COPY   RSTACK-CTR.
000161*--------------------------------------------------------------
000162*- スタック：内部メモリエリア。呼び出し側で十分なサイズが取得
000162*- されている前提でX(1)となっている。
000178*--------------------------------------------------------------
000660 01       PSTACK-MEM.
000670   03     FILLER            PIC X(1).
000680*--------------------------------------------------------------
000690*- メモリ:メモリモジュールCALLパラメータ。
000700*--------------------------------------------------------------
000710 01       PMYMEM-PRM.
000340   COPY   RMYMEM-PRM.
000144*--------------------------------------------------------------
000145*- メモリ:定数値やアドレス情報を受け渡しするエリア。
000146*--------------------------------------------------------------
000790 01       PMYMEM-VAL.
000800   03     FILLER            PIC X(1).
000161*--------------------------------------------------------------
000162*- メモリ：内部メモリエリア。呼び出し側で十分なサイズが取得
000162*- されている前提でX(1)となっている。
000178*--------------------------------------------------------------
000840 01       PMYMEM-MEM.
000850   03     FILLER            PIC X(1).
000860*--------------------------------------------------------------
000870*- 仮想コード。
000880*--------------------------------------------------------------
000890 01       PVCODEAREA.
000340   COPY   RVCODEAREA.
001000*--------------------------------------------------------------
001001*- IN1
001002*--------------------------------------------------------------
001003 01       PIN100.
001004   03     FILLER            PIC X(1).
001005*--------------------------------------------------------------
001006*- IN2
001007*--------------------------------------------------------------
001008 01       PIN200.
001009   03     FILLER            PIC X(1).
001010*--------------------------------------------------------------
001011*- IN3
001012*--------------------------------------------------------------
001013 01       PIN300.
001014   03     FILLER            PIC X(1).
001015*--------------------------------------------------------------
001016*- OUT
001017*--------------------------------------------------------------
001018 01       POUT00.
001019   03     FILLER            PIC X(1).
001020*--------------------------------------------------------------
001021*-
001022*--------------------------------------------------------------
001030 PROCEDURE DIVISION USING   PVCODE-PRM
001040                            PSTACK-PRM
001050                            PSTACK-VAL
001060                            PSTACK-CTR
001070                            PSTACK-MEM
001080                            PMYMEM-PRM
001090                            PMYMEM-VAL
001110                            PMYMEM-MEM
001120                            PVCODEAREA
001121                            PIN100
001122                            PIN200
001123                            PIN300
001124                            POUT00.
001130 A00-MAIN SECTION.
001140*--------------------------------------------------------------
001150*-
001160*--------------------------------------------------------------
001170
001180 A00-010.
001190   MOVE 'A00-010.'          TO WK000SECTION.
001200   PERFORM C00-EXECUTE.
001210 A00-EXIT.
001220   MOVE ZERO TO RETURN-CODE.
001230   GOBACK.
001240 C00-EXECUTE SECTION.
001250*--------------------------------------------------------------
001260*- 仮想コードを実行する。
001270*--------------------------------------------------------------
001280 C00-010.
001290   MOVE 'C00-010.'          TO WK000SECTION.
001300*-
001310   PERFORM VARYING VCODE-PC FROM 1 BY 1
001320   UNTIL VCODE-PC > VCODE-TBL-SZE
001330     EVALUATE VCODE-OPR(VCODE-PC)
001340     WHEN 'PUSHV'
001350       PERFORM C10-PUSHV
001360     WHEN 'PUSHL'
001370       PERFORM C15-PUSHL
001380     WHEN 'LT___'
001390       PERFORM C31-LT
001400     WHEN 'LE___'
001410       PERFORM C32-LE
001420     WHEN 'GT___'
001430       PERFORM C33-GT
001440     WHEN 'GE___'
001450       PERFORM C34-GE
001460     WHEN 'EQ___'
001470       PERFORM C35-EQ
001480     WHEN 'NOTEQ'
001490       PERFORM C36-NE
001500     WHEN 'IN___'
001510       PERFORM C41-IN
001520     WHEN 'ISBLK'
001530       PERFORM C42-IS-BLANK
001540     WHEN 'LIKE_'
001550       PERFORM C43-LIKE
001560     WHEN 'BTWN_'
001570       PERFORM C44-BETWEEN
001580     WHEN 'NOT__'
001590       PERFORM C50-NOT
001600     WHEN 'AND__'
001610       PERFORM C60-AND
001620     WHEN 'OR___'
001630       PERFORM C70-OR
001640     WHEN 'JPZ__'
001650       PERFORM C80-JPZ
001660     WHEN 'PUT__'
001670       PERFORM C90-PUT
001680     END-EVALUATE
001690   END-PERFORM
001700*-
001710   MOVE ZERO                TO PVCODE-COD.
001720   STRING WK000MYNAME SPACE ',NORMAL END.  '
001730                            INTO PVCODE-MSG.
001740 C00-EXIT.
001750     EXIT.
001760 C10-PUSHV SECTION.
001770*--------------------------------------------------------------
001780*- PUSHV
001790*--------------------------------------------------------------
001800 C10-010.
001810   MOVE 'C10-010.'          TO WK000SECTION.
001820*-
001830   PERFORM S30-PUSH.
001840*-
001850   MOVE ZERO                TO PVCODE-COD.
001860   STRING WK000MYNAME SPACE ',NORMAL END.  '
001870                            INTO PVCODE-MSG.
001880 C10-EXIT.
001890     EXIT.
001900 C15-PUSHL SECTION.
001910*--------------------------------------------------------------
001920*- PUSHL
001930*--------------------------------------------------------------
001940 C15-010.
001950   MOVE 'C15-010.'          TO WK000SECTION.
001960*-
001970   PERFORM S30-PUSH.
001980*-
001990   MOVE ZERO                TO PVCODE-COD.
002000   STRING WK000MYNAME SPACE ',NORMAL END.  '
002010                            INTO PVCODE-MSG.
002020 C15-EXIT.
002030     EXIT.
002040 C31-LT SECTION.
002050*--------------------------------------------------------------
002060*- LT
002070*--------------------------------------------------------------
002080 C31-010.
002090   MOVE 'C31-010.'          TO WK000SECTION.
002100*-
002110   PERFORM S40-GET2ITEM.
002120   DISPLAY 'L:(' WKC00LEFT-X ')'.
002130   DISPLAY 'R:(' WKC00RIGHT-X ')'.
002140   DISPLAY '< '.
002150   IF WKC00LEFT-X        < WKC00RIGHT-X
002160    THEN
002170     PERFORM S10-TRUE
002180    ELSE
002190     PERFORM S20-FALSE
002200   END-IF.
002210*-
002220   MOVE ZERO                TO PVCODE-COD.
002230   STRING WK000MYNAME SPACE ',NORMAL END.  '
002240                            INTO PVCODE-MSG.
002250 C31-EXIT.
002260   EXIT.
002270 C32-LE SECTION.
002280*--------------------------------------------------------------
002290*- LE
002300*--------------------------------------------------------------
002310 C32-010.
002320   MOVE 'C32-010.'          TO WK000SECTION.
002330*-
002340   PERFORM S40-GET2ITEM.
002350   DISPLAY 'L:(' WKC00LEFT-X ')'.
002360   DISPLAY 'R:(' WKC00RIGHT-X ')'.
002370   DISPLAY '<='.
002380   IF WKC00LEFT-X           <= WKC00RIGHT-X
002390    THEN
002400     PERFORM S10-TRUE
002410    ELSE
002420     PERFORM S20-FALSE
002430   END-IF.
002440*-
002450   MOVE ZERO                TO PVCODE-COD.
002460   STRING WK000MYNAME SPACE ',NORMAL END.  '
002470                            INTO PVCODE-MSG.
002480 C32-EXIT.
002490   EXIT.
002500 C33-GT SECTION.
002510*--------------------------------------------------------------
002520*- GT
002530*--------------------------------------------------------------
002540 C33-010.
002550   MOVE 'C33-010.'          TO WK000SECTION.
002560*-
002570   PERFORM S40-GET2ITEM.
002580   DISPLAY 'L:(' WKC00LEFT-X ')'.
002590   DISPLAY 'R:(' WKC00RIGHT-X ')'.
002600   DISPLAY '> '.
002610   IF WKC00LEFT-X        > WKC00RIGHT-X
002620    THEN
002630     PERFORM S10-TRUE
002640    ELSE
002650     PERFORM S20-FALSE
002660   END-IF.
002670*-
002680   MOVE ZERO                TO PVCODE-COD.
002690   STRING WK000MYNAME SPACE ',NORMAL END.  '
002700                            INTO PVCODE-MSG.
002710 C33-EXIT.
002720   EXIT.
002730 C34-GE SECTION.
002740*--------------------------------------------------------------
002750*- GE
002760*--------------------------------------------------------------
002770 C34-010.
002780   MOVE 'C34-010.'          TO WK000SECTION.
002790*-
002800   PERFORM S40-GET2ITEM.
002810   DISPLAY 'L:(' WKC00LEFT-X ')'.
002820   DISPLAY 'R:(' WKC00RIGHT-X ')'.
002830   DISPLAY '>='.
002840   IF WKC00LEFT-X           >= WKC00RIGHT-X
002850    THEN
002860     PERFORM S10-TRUE
002870    ELSE
002880     PERFORM S20-FALSE
002890   END-IF.
002900*-
002910   MOVE ZERO                TO PVCODE-COD.
002920   STRING WK000MYNAME SPACE ',NORMAL END.  '
002930                            INTO PVCODE-MSG.
002940 C34-EXIT.
002950   EXIT.
002960 C35-EQ SECTION.
002970*--------------------------------------------------------------
002980*- EQ
002990*--------------------------------------------------------------
003000 C35-010.
003010   MOVE 'C35-010.'          TO WK000SECTION.
003020*-
003030   PERFORM S40-GET2ITEM.
003040   DISPLAY 'L:(' WKC00LEFT-X ')'.
003050   DISPLAY 'R:(' WKC00RIGHT-X ')'.
003060   DISPLAY '=='.
003070   IF WKC00LEFT-X            = WKC00RIGHT-X
003080    THEN
003090     PERFORM S10-TRUE
003100    ELSE
003110     PERFORM S20-FALSE
003120   END-IF.
003130*-
003140   MOVE ZERO                TO PVCODE-COD.
003150   STRING WK000MYNAME SPACE ',NORMAL END.  '
003160                            INTO PVCODE-MSG.
003170 C35-EXIT.
003180   EXIT.
003190 C36-NE SECTION.
003200*--------------------------------------------------------------
003210*- NE
003220*--------------------------------------------------------------
003230 C36-010.
003240   MOVE 'C36-010.'          TO WK000SECTION.
003250*-
003260   PERFORM S40-GET2ITEM.
003270   DISPLAY 'L:(' WKC00LEFT-X ')'.
003280   DISPLAY 'R:(' WKC00RIGHT-X ')'.
003290   DISPLAY '!='.
003300   IF WKC00LEFT-X            = WKC00RIGHT-X
003310    THEN
003320     PERFORM S20-FALSE
003330    ELSE
003340     PERFORM S10-TRUE
003350   END-IF.
003360*-
003370   MOVE ZERO                TO PVCODE-COD.
003380   STRING WK000MYNAME SPACE ',NORMAL END.  '
003390                            INTO PVCODE-MSG.
003400 C36-EXIT.
003410   EXIT.
003420 C41-IN SECTION.
003430*--------------------------------------------------------------
003440*- IN SENTENCE.
003450*--------------------------------------------------------------
003460 C41-010.
003470   MOVE 'C41-010.'          TO WK000SECTION.
003480*- 右辺の要素数を取りだす
003490   MOVE 'POP_'              TO PSTACK-FNC.
003500   CALL 'SUBSTACK'       USING PSTACK-PRM
003510                               PSTACK-VAL
003520                               PSTACK-CTR
003530                               PSTACK-MEM.
003540   MOVE PSTACK-VAL(1:PSTACK-VLL)
003550                            TO WKC00RIGHT-9.
003560   MOVE 'GET_'              TO PMYMEM-FNC.
003570   MOVE WKC00RIGHT-9        TO PMYMEM-VLI.
003580   CALL 'SUBMYMEM'       USING PMYMEM-PRM
003590                               PMYMEM-VAL
003610                               PMYMEM-MEM
003611                               PIN100
003612                               PIN200
003613                               PIN300
003614                               POUT00.
003620   MOVE ZERO                TO WKC00RIGHT-9.
003630   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
003640                            TO WKB00K.
003650*- 左辺を取りだす
003660   COMPUTE WKB00J = PSTACK-ADRIDX - WKB00K.
003670   MOVE WKB00J              TO PSTACK-VLI.
003680   MOVE 'GET_'              TO PSTACK-FNC.
003690   CALL 'SUBSTACK'       USING PSTACK-PRM
003700                               PSTACK-VAL
003710                               PSTACK-CTR
003720                               PSTACK-MEM
003730   MOVE PSTACK-VAL(1:PSTACK-VLL)
003740                            TO WKC00LEFT-9.
003750   MOVE 'GET_'              TO PMYMEM-FNC.
003760   MOVE WKC00LEFT-9         TO PMYMEM-VLI.
003770   CALL 'SUBMYMEM'       USING PMYMEM-PRM
003780                               PMYMEM-VAL
003800                               PMYMEM-MEM
003801                               PIN100
003802                               PIN200
003803                               PIN300
003804                               POUT00.
003810   MOVE SPACE               TO WKC00LEFT-X.
003820   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
003830                            TO WKC00LEFT-X.
003840   MOVE PMYMEM-VLL          TO WKC00LEFT-LL.
003850*- 右辺を一つずつ取りだし、左辺と比べる
003860   MOVE ZERO                TO WKC00IS-END.
003870   COMPUTE WKB00J = PSTACK-ADRIDX - WKB00K + 1.
003880   PERFORM VARYING WKB00I FROM WKB00J BY 1
003890   UNTIL WKB00I > PSTACK-ADRIDX
003900   OR    WKC00IS-END = 1
003910     MOVE 'GET_'            TO PSTACK-FNC
003920     MOVE WKB00I            TO PSTACK-VLI
003930     CALL 'SUBSTACK'     USING PSTACK-PRM
003940                               PSTACK-VAL
003950                               PSTACK-CTR
003960                               PSTACK-MEM
003970     MOVE PSTACK-VAL(1:PSTACK-VLL)
003980                            TO WKC00RIGHT-9
003990     MOVE 'GET_'            TO PMYMEM-FNC
004000     MOVE WKC00RIGHT-9      TO PMYMEM-VLI
004010     CALL 'SUBMYMEM'     USING PMYMEM-PRM
004020                               PMYMEM-VAL
004040                               PMYMEM-MEM
004041                               PIN100
004042                               PIN200
004043                               PIN300
004044                               POUT00
004050     MOVE SPACE             TO WKC00RIGHT-X
004060     MOVE PMYMEM-VAL(1:PMYMEM-VLL)
004070                            TO WKC00RIGHT-X
004080     MOVE PMYMEM-VLL        TO WKC00RIGHT-LL
004090
004100     DISPLAY 'L:(' WKC00LEFT-X ')'
004110     DISPLAY 'R:(' WKC00RIGHT-X ')'
004120     DISPLAY 'IN'
004130     IF WKC00LEFT-X          = WKC00RIGHT-X
004140      THEN
004150       MOVE 1               TO WKC00IS-END
004160     END-IF
004170   END-PERFORM.
004180*- 一通り評価し終わったのでスタックをクリア
004190*- +1するのは左辺の分もＰＯＰするため
004200   ADD 1                    TO WKB00K.
004210   PERFORM VARYING WKB00I FROM WKB00K BY -1
004220   UNTIL WKB00I = ZERO
004230     MOVE 'POP_'            TO PSTACK-FNC
004240     CALL 'SUBSTACK'     USING PSTACK-PRM
004250                               PSTACK-VAL
004260                               PSTACK-CTR
004270                               PSTACK-MEM
004280   END-PERFORM.
004290*- 結果をスタックにPUSH
004300   IF WKC00IS-END            = 1
004310    THEN
004320      PERFORM S10-TRUE
004330    ELSE
004340      PERFORM S20-FALSE
004350   END-IF.
004360*-
004370   MOVE ZERO                TO PVCODE-COD.
004380   STRING WK000MYNAME SPACE ',NORMAL END.  '
004390                            INTO PVCODE-MSG.
004400 C41-EXIT.
004410   EXIT.
004420 C42-IS-BLANK SECTION.
004430*--------------------------------------------------------------
004440*- IS BLANK
004450*--------------------------------------------------------------
004460 C42-010.
004470   MOVE 'C42-010.'          TO WK000SECTION.
004480*-
004490   MOVE 'POP_'              TO PSTACK-FNC.
004500   CALL 'SUBSTACK'       USING PSTACK-PRM
004510                               PSTACK-VAL
004520                               PSTACK-CTR
004530                               PSTACK-MEM.
004540   MOVE PSTACK-VAL(1:PSTACK-VLL)
004550                            TO WKC00LEFT-9.
004560   MOVE 'GET_'              TO PMYMEM-FNC.
004570   MOVE WKC00LEFT-9         TO PMYMEM-VLI.
004580   CALL 'SUBMYMEM'       USING PMYMEM-PRM
004590                               PMYMEM-VAL
004610                               PMYMEM-MEM
004611                               PIN100
004612                               PIN200
004613                               PIN300
004614                               POUT00.
004620   MOVE SPACE               TO WKC00LEFT-X.
004630   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
004640                            TO WKC00LEFT-X.
004650   MOVE PMYMEM-VLL          TO WKC00LEFT-LL.
004660   DISPLAY 'L:(' WKC00LEFT-X ')'.
004670   DISPLAY 'R:(SPACE)'.
004680   DISPLAY 'IS BLANK'.
004690   IF WKC00LEFT-X            = SPACE
004700    THEN
004710     PERFORM S10-TRUE
004720    ELSE
004730     PERFORM S20-FALSE
004740   END-IF.
004750*-
004760   MOVE ZERO                TO PVCODE-COD.
004770   STRING WK000MYNAME SPACE ',NORMAL END.  '
004780                            INTO PVCODE-MSG.
004790 C42-EXIT.
004800   EXIT.
004810 C43-LIKE SECTION.
004820*--------------------------------------------------------------
004830*- LIKE
004840*--------------------------------------------------------------
004850 C43-010.
004860   MOVE 'C43-010.'          TO WK000SECTION.
004870*-
004880   PERFORM S40-GET2ITEM.
004890   INSPECT WKC00RIGHT-X REPLACING ALL '*'
004900                            BY HIGH-VALUE.
004910
004920   DISPLAY 'L:(' WKC00LEFT-X ')'.
004930   DISPLAY 'R:(' WKC00RIGHT-X ')'.
004940   DISPLAY 'LIKE'.
004950
004960   MOVE ZERO                TO WKC00IS-END.
004970   PERFORM VARYING WKB00I FROM 1 BY 1
004980   UNTIL WKB00I > WKC00RIGHT-LL
004990   OR    WKC00IS-END = 1
005000     IF WKC00RIGHT-X(WKB00I:1)
005010                         NOT = HIGH-VALUE
005020      AND WKC00LEFT-X(WKB00I:1)
005030                         NOT = WKC00RIGHT-X(WKB00I:1)
005040      THEN
005050       MOVE 1             TO WKC00IS-END
005060     END-IF
005070   END-PERFORM.
005080   IF WKC00IS-END            = ZERO
005090    THEN
005100     PERFORM S10-TRUE
005110    ELSE
005120     PERFORM S20-FALSE
005130   END-IF.
005140*-
005150   MOVE ZERO                TO PVCODE-COD.
005160   STRING WK000MYNAME SPACE ',NORMAL END.  '
005170                            INTO PVCODE-MSG.
005180 C43-EXIT.
005190   EXIT.
005200 C44-BETWEEN SECTION.
005210*--------------------------------------------------------------
005220*- BETWEEN SENTENCE.
005230*--------------------------------------------------------------
005240 C44-010.
005250   MOVE 'C44-010.'          TO WK000SECTION.
005260*- 右辺を2つ取りだし、左辺と比べる
005270   MOVE 'POP_'              TO PSTACK-FNC.
005280   CALL 'SUBSTACK'       USING PSTACK-PRM
005290                               PSTACK-VAL
005300                               PSTACK-CTR
005310                               PSTACK-MEM
005320   MOVE PSTACK-VAL(1:PSTACK-VLL)
005330                            TO WKC00RIGHT-9.
005340   MOVE 'GET_'              TO PMYMEM-FNC.
005350   MOVE WKC00RIGHT-9         TO PMYMEM-VLI.
005360   CALL 'SUBMYMEM'       USING PMYMEM-PRM
005370                               PMYMEM-VAL
005390                               PMYMEM-MEM
005391                               PIN100
005392                               PIN200
005393                               PIN300
005394                               POUT00.
005400   MOVE SPACE               TO WKC00RIGHT-X.
005410   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
005420                            TO WKC00RIGHT-X.
005430   MOVE PMYMEM-VLL          TO WKC00RIGHT-LL.
005440*-
005450   MOVE 'POP_'              TO PSTACK-FNC
005460   CALL 'SUBSTACK'       USING PSTACK-PRM
005470                               PSTACK-VAL
005480                               PSTACK-CTR
005490                               PSTACK-MEM.
005500   MOVE PSTACK-VAL(1:PSTACK-VLL)
005510                            TO WKC00RIGHT-9.
005520   MOVE 'GET_'              TO PMYMEM-FNC.
005530   MOVE WKC00RIGHT-9        TO PMYMEM-VLI.
005540   CALL 'SUBMYMEM'       USING PMYMEM-PRM
005550                               PMYMEM-VAL
005570                               PMYMEM-MEM
005571                               PIN100
005572                               PIN200
005573                               PIN300
005574                               POUT00.
005580   MOVE SPACE               TO WKC00RIGH2-X.
005590   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
005600                            TO WKC00RIGH2-X.
005610   MOVE PMYMEM-VLL          TO WKC00RIGH2-LL.
005620*- 左辺を取りだす
005630   MOVE 'POP_'              TO PSTACK-FNC
005640   CALL 'SUBSTACK'       USING PSTACK-PRM
005650                               PSTACK-VAL
005660                               PSTACK-CTR
005670                               PSTACK-MEM.
005680   MOVE PSTACK-VAL(1:PSTACK-VLL)
005690                            TO WKC00LEFT-9.
005700   MOVE 'GET_'              TO PMYMEM-FNC.
005710   MOVE WKC00LEFT-9         TO PMYMEM-VLI.
005720   CALL 'SUBMYMEM'       USING PMYMEM-PRM
005730                               PMYMEM-VAL
005750                               PMYMEM-MEM
005751                               PIN100
005752                               PIN200
005753                               PIN300
005754                               POUT00.
005760   MOVE SPACE               TO WKC00LEFT-X.
005770   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
005780                            TO WKC00LEFT-X.
005790   MOVE PMYMEM-VLL          TO WKC00LEFT-LL.
005800*-
005810   DISPLAY 'R2:(' WKC00RIGH2-X ')'.
005820   DISPLAY 'L :(' WKC00LEFT-X ')'.
005830   DISPLAY 'R1:(' WKC00RIGHT-X ')'.
005840   DISPLAY 'BETWEEN'.
005850   IF   WKC00RIGH2-X <= WKC00LEFT-X
005860    AND WKC00LEFT-X  <= WKC00RIGHT-X
005870    THEN
005880      PERFORM S10-TRUE
005890    ELSE
005900      PERFORM S20-FALSE
005910   END-IF.
005920*-
005930   MOVE ZERO                TO PVCODE-COD.
005940   STRING WK000MYNAME SPACE ',NORMAL END.  '
005950                            INTO PVCODE-MSG.
005960 C41-EXIT.
005970   EXIT.
005980 C50-NOT SECTION.
005990*--------------------------------------------------------------
006000*- NOT
006010*--------------------------------------------------------------
006020 C50-010.
006030   MOVE 'C50-010.'          TO WK000SECTION.
006040*-
006050   MOVE 'POP_'              TO PSTACK-FNC.
006060   CALL 'SUBSTACK'       USING PSTACK-PRM
006070                               PSTACK-VAL
006080                               PSTACK-CTR
006090                               PSTACK-MEM.
006100   MOVE PSTACK-VAL(1:PSTACK-VLL)
006110                            TO WKC00RIGHT-9.
006120   DISPLAY 'R:(' WKC00RIGHT-9 ')'.
006130   DISPLAY 'NOT'.
006140   IF WKC00RIGHT-9           = 1
006150    THEN
006160     PERFORM S20-FALSE
006170    ELSE
006180     PERFORM S10-TRUE
006190   END-IF.
006200*-
006210   MOVE ZERO                TO PVCODE-COD.
006220   STRING WK000MYNAME SPACE ',NORMAL END.  '
006230                            INTO PVCODE-MSG.
006240 C50-EXIT.
006250   EXIT.
006260 C60-AND SECTION.
006270*--------------------------------------------------------------
006280*- AND
006290*--------------------------------------------------------------
006300 C60-010.
006310   MOVE 'C60-010.'          TO WK000SECTION.
006320*-
006330   PERFORM S50-POP2STACK.
006340   DISPLAY 'L:(' WKC00LEFT-9 ')'.
006350   DISPLAY 'R:(' WKC00RIGHT-9 ')'.
006360   DISPLAY 'AND'.
006370   IF WKC00LEFT-9        = 1
006380    AND WKC00RIGHT-9     = 1
006390    THEN
006400     PERFORM S10-TRUE
006410    ELSE
006420     PERFORM S20-FALSE
006430   END-IF.
006440*-
006450   MOVE ZERO                TO PVCODE-COD.
006460   STRING WK000MYNAME SPACE ',NORMAL END.  '
006470                            INTO PVCODE-MSG.
006480 C60-EXIT.
006490   EXIT.
006500 C70-OR SECTION.
006510*--------------------------------------------------------------
006520*- OR
006530*--------------------------------------------------------------
006540 C70-010.
006550   MOVE 'C70-010.'          TO WK000SECTION.
006560*-
006570   PERFORM S50-POP2STACK.
006580   DISPLAY 'L:(' WKC00LEFT-9 ')'.
006590   DISPLAY 'R:(' WKC00RIGHT-9 ')'.
006600   DISPLAY 'OR_'.
006610   IF WKC00LEFT-9        = 1
006620    OR WKC00RIGHT-9     = 1
006630    THEN
006640     PERFORM S10-TRUE
006650    ELSE
006660     PERFORM S20-FALSE
006670   END-IF.
006680*-
006690   MOVE ZERO                TO PVCODE-COD.
006700   STRING WK000MYNAME SPACE ',NORMAL END.  '
006710                            INTO PVCODE-MSG.
006720 C70-EXIT.
006730   EXIT.
006740 C80-JPZ SECTION.
006750*--------------------------------------------------------------
006760*- JPZ
006770*--------------------------------------------------------------
006780 C80-010.
006790   MOVE 'C80-010.'          TO WK000SECTION.
006800*-
006810   MOVE 'POP_'              TO PSTACK-FNC.
006820   CALL 'SUBSTACK'       USING PSTACK-PRM
006830                               PSTACK-VAL
006840                               PSTACK-CTR
006850                               PSTACK-MEM.
006860   MOVE PSTACK-VAL(1:PSTACK-VLL)
006870                            TO WKC00RIGHT-9.
006880   DISPLAY 'R:(' WKC00RIGHT-9 ')'.
006890   DISPLAY 'JPZ'.
006900   IF WKC00RIGHT-9           = ZERO
006910    THEN
006920     MOVE  VCODE-TBL-SZE    TO VCODE-PC
006930   END-IF.
006940*-
006950   MOVE ZERO                TO PVCODE-COD.
006960   STRING WK000MYNAME SPACE ',NORMAL END.  '
006970                            INTO PVCODE-MSG.
006980 C80-EXIT.
006990   EXIT.
007000 C90-PUT SECTION.
007010*--------------------------------------------------------------
007020*- PUT
007030*--------------------------------------------------------------
007040 C90-010.
007050   MOVE 'C90-010.'          TO WK000SECTION.
007060*-
007061   MOVE SPACE               TO POUT00.
007070   MOVE ZERO                TO WKB00J.
007071   MOVE 1                   TO WKB00K.
007073   MOVE ZERO                TO WKC00IS-END.
007080   PERFORM VARYING WKB00I FROM 1 BY 1
007090   UNTIL WKC00IS-END = 1
007100     MOVE 'POP_'            TO PSTACK-FNC
007110     CALL 'SUBSTACK'     USING PSTACK-PRM
007120                               PSTACK-VAL
007130                               PSTACK-CTR
007140                               PSTACK-MEM
007150     IF PSTACK-VLL           = ZERO
007160      THEN
007170       MOVE 1               TO WKC00IS-END
007180      ELSE
007190       MOVE PSTACK-VAL(1:PSTACK-VLL)
007200                            TO WKC00LEFT-9
007210       MOVE 'GET_'          TO PMYMEM-FNC
007220       MOVE WKC00LEFT-9     TO PMYMEM-VLI
007230       CALL 'SUBMYMEM'   USING PMYMEM-PRM
007240                               PMYMEM-VAL
007260                               PMYMEM-MEM
007261                               PIN100
007262                               PIN200
007263                               PIN300
007264                               POUT00
007270       MOVE SPACE           TO WKC00LEFT-X
007280       MOVE PMYMEM-VAL(1:PMYMEM-VLL)
007290                            TO WKC00LEFT-X
007300       MOVE PMYMEM-VLL      TO WKC00LEFT-LL
007310*- ちょっとトリッキーなコード
007311*- スタックから取り出した値を1行の文字列にしている
007316       COMPUTE WKB00K = WKC00LEFT-LL + 1
007317       STRING POUT00(WKB00J:1)
007318              WKC00LEFT-X(1:WKC00LEFT-LL)
007319                          INTO POUT00(WKB00J:WKB00K)
007320       COMPUTE WKB00J = WKB00J + WKC00LEFT-LL
007321     END-IF
007330   END-PERFORM.
007341*-
007350   MOVE ZERO                TO PVCODE-COD.
007360   STRING WK000MYNAME SPACE ',NORMAL END.  '
007370                            INTO PVCODE-MSG.
007380 C90-EXIT.
007390   EXIT.
007400 S10-TRUE SECTION.
007410*--------------------------------------------------------------
007420*- TRUE.
007430*--------------------------------------------------------------
007440 S10-010.
007450   MOVE 'S10-010.'          TO WK000SECTION.
007460*-
007461   DISPLAY 'TRUE!'.
007470   MOVE 'PUSH'              TO PSTACK-FNC.
007480   MOVE '1'                 TO PSTACK-VAL.
007490   MOVE 1                   TO PSTACK-VLL.
007500   CALL 'SUBSTACK'       USING PSTACK-PRM
007510                               PSTACK-VAL
007520                               PSTACK-CTR
007530                               PSTACK-MEM.
007540*-
007550   MOVE ZERO                TO PVCODE-COD.
007560   STRING WK000MYNAME SPACE ',NORMAL END.  '
007570                            INTO PVCODE-MSG.
007580 S10-EXIT.
007590     EXIT.
007600 S20-FALSE SECTION.
007610*--------------------------------------------------------------
007620*- FALSE
007630*--------------------------------------------------------------
007640 S20-010.
007650   MOVE 'S20-010.'          TO WK000SECTION.
007660*-
007670   DISPLAY 'FALSE!'.
007671   MOVE 'PUSH'              TO PSTACK-FNC.
007680   MOVE '0'                 TO PSTACK-VAL.
007690   MOVE 1                   TO PSTACK-VLL.
007700   CALL 'SUBSTACK'       USING PSTACK-PRM
007710                               PSTACK-VAL
007720                               PSTACK-CTR
007730                               PSTACK-MEM.
007740*-
007750   MOVE ZERO                TO PVCODE-COD.
007760   STRING WK000MYNAME SPACE ',NORMAL END.  '
007770                            INTO PVCODE-MSG.
007780 S20-EXIT.
007790     EXIT.
007800 S30-PUSH SECTION.
007810*--------------------------------------------------------------
007820*- PUSH
007830*--------------------------------------------------------------
007840 S30-010.
007850   MOVE 'S30-010.'          TO WK000SECTION.
007860*-
007870   MOVE 'PUSH'              TO PSTACK-FNC.
007880   MOVE VCODE-ADR(VCODE-PC) TO PSTACK-VAL.
007895   MOVE 5                   TO PSTACK-VLL.
007900   CALL 'SUBSTACK'       USING PSTACK-PRM
007910                               PSTACK-VAL
007920                               PSTACK-CTR
007930                               PSTACK-MEM.
007940*-
007950   MOVE ZERO                TO PVCODE-COD.
007960   STRING WK000MYNAME SPACE ',NORMAL END.  '
007970                            INTO PVCODE-MSG.
007980 S30-EXIT.
007990     EXIT.
008000 S40-GET2ITEM SECTION.
008010*--------------------------------------------------------------
008020*- GET 2 ITEM.
008030*--------------------------------------------------------------
008040 S40-010.
008050   MOVE 'S40-010.'          TO WK000SECTION.
008060*-
008070   MOVE 'POP_'              TO PSTACK-FNC.
008080   CALL 'SUBSTACK'       USING PSTACK-PRM
008090                               PSTACK-VAL
008100                               PSTACK-CTR
008110                               PSTACK-MEM.
008120   MOVE PSTACK-VAL(1:PSTACK-VLL)
008130                            TO WKC00RIGHT-9.
008140   MOVE 'GET_'              TO PMYMEM-FNC.
008150   MOVE WKC00RIGHT-9        TO PMYMEM-VLI.
008160   CALL 'SUBMYMEM'       USING PMYMEM-PRM
008170                               PMYMEM-VAL
008190                               PMYMEM-MEM
008191                               PIN100
008192                               PIN200
008193                               PIN300
008194                               POUT00.
008200   MOVE SPACE               TO WKC00RIGHT-X.
008210   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
008220                            TO WKC00RIGHT-X.
008230   MOVE PMYMEM-VLL          TO WKC00RIGHT-LL.
008240*-
008250   MOVE 'POP_'              TO PSTACK-FNC.
008260   CALL 'SUBSTACK'       USING PSTACK-PRM
008270                               PSTACK-VAL
008280                               PSTACK-CTR
008290                               PSTACK-MEM.
008300   MOVE PSTACK-VAL(1:PSTACK-VLL)
008310                            TO WKC00LEFT-9.
008320   MOVE 'GET_'              TO PMYMEM-FNC.
008330   MOVE WKC00LEFT-9         TO PMYMEM-VLI.
008340   CALL 'SUBMYMEM'       USING PMYMEM-PRM
008350                               PMYMEM-VAL
008370                               PMYMEM-MEM
008371                               PIN100
008372                               PIN200
008373                               PIN300
008374                               POUT00.
008380   MOVE SPACE               TO WKC00LEFT-X.
008390   MOVE PMYMEM-VAL(1:PMYMEM-VLL)
008400                            TO WKC00LEFT-X.
008410   MOVE PMYMEM-VLL          TO WKC00LEFT-LL.
008420*-
008430   MOVE ZERO                TO PVCODE-COD.
008440   STRING WK000MYNAME SPACE ',NORMAL END.  '
008450                            INTO PVCODE-MSG.
008460 S40-EXIT.
008470     EXIT.
008480 S50-POP2STACK SECTION.
008490*--------------------------------------------------------------
008500*- POP 2 STACK.
008510*--------------------------------------------------------------
008520 S50-010.
008530   MOVE 'S50-010.'          TO WK000SECTION.
008540*-
008550   MOVE 'POP_'              TO PSTACK-FNC.
008560   CALL 'SUBSTACK'       USING PSTACK-PRM
008570                               PSTACK-VAL
008580                               PSTACK-CTR
008590                               PSTACK-MEM.
008600   MOVE PSTACK-VAL(1:PSTACK-VLL)
008610                            TO WKC00RIGHT-9.
008620*-
008630   MOVE 'POP_'              TO PSTACK-FNC.
008640   CALL 'SUBSTACK'       USING PSTACK-PRM
008650                               PSTACK-VAL
008660                               PSTACK-CTR
008670                               PSTACK-MEM.
008680   MOVE PSTACK-VAL(1:PSTACK-VLL)
008690                            TO WKC00LEFT-9.
008700*-
008710   MOVE ZERO                TO PVCODE-COD.
008720   STRING WK000MYNAME SPACE ',NORMAL END.  '
008730                            INTO PVCODE-MSG.
008740 S50-EXIT.
008750     EXIT.

