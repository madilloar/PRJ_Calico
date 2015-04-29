000020 IDENTIFICATION DIVISION.
000030 PROGRAM-ID.   TEST-MAIN.
000040 ENVIRONMENT    DIVISION.
000138 INPUT-OUTPUT               SECTION.
000139 FILE-CONTROL.
000140   SELECT F1INPUT ASSIGN TO
000141   '../data/pchecktbl.txt'
000142   STATUS FST.
000143 DATA DIVISION.
000144 FILE                       SECTION.
000145 FD  F1INPUT.
000146 01  F1RECORD               PIC X(18).
000147 WORKING-STORAGE SECTION.
000148*--------------------------------------------------------------
000158*- ���̑����[�N
000168*--------------------------------------------------------------
000178 01  FST                    PIC X(02).
000186*--------------------------------------------------------------
000187*- ���z�R�[�h���W���[��CALL�p���[���[�^�B
000188*--------------------------------------------------------------
000189 01       PVCODE-PRM.
000201   COPY   RVCODE-PRM.
000207*--------------------------------------------------------------
000208*- IN1�̂���
000209*--------------------------------------------------------------
000210 01       IN100             SYNC.
000211   03     FILLER            PIC X(10) VALUE '123456789 '.
000217*--------------------------------------------------------------
000218*- IN2�̂���
000219*--------------------------------------------------------------
000220 01       IN200             SYNC.
000221   03     FILLER            PIC X(10) VALUE '123456789 '.
000227*--------------------------------------------------------------
000228*- IN3�̂���
000229*--------------------------------------------------------------
000230 01       IN300             SYNC.
000231   03     FILLER            PIC X(10) VALUE '123456789 '.
000232*--------------------------------------------------------------
000233*- OUT�̂���
000234*--------------------------------------------------------------
000235 01       OUT00             SYNC.
000236   03     FILLER            PIC X(100).
000237 PROCEDURE DIVISION.   
000238 A00-MAIN SECTION.
000239*--------------------------------------------------------------
000240*-
000241*--------------------------------------------------------------
000242 A00-010.
000243   MOVE SPACE         TO PVCODE-PRM.
000244   MOVE 'INIT'        TO PVCODE-FUNC.
000245   CALL 'SUBVCODE' USING PVCODE-PRM
000246                         IN100
000247                         IN200
000248                         IN300
000249                         OUT00.
000250*- FILE READ
000559   OPEN  INPUT  F1INPUT.
000560   PERFORM UNTIL FST NOT = '00'
000561     READ F1INPUT
000562       END
000563         CONTINUE
000564       NOT END
000566         MOVE SPACE         TO PVCODE-PRM
000567         MOVE 'LOAD'        TO PVCODE-FUNC
000568         MOVE F1RECORD      TO PVCODE
000569         CALL 'SUBVCODE' USING PVCODE-PRM
000570                               IN100
000571                               IN200
000572                               IN300
000573                               OUT00
000574     END-READ
000575   END-PERFORM.
000576   CLOSE F1INPUT.
000577   MOVE SPACE         TO PVCODE-PRM.
000578   MOVE 'EXEC'        TO PVCODE-FUNC.
000579   CALL 'SUBVCODE' USING PVCODE-PRM
000580                         IN100
000581                         IN200
000582                         IN300
000583                         OUT00.
000584   DISPLAY 'RES:(' OUT00 ')'.
000585*-DEBUG:
000586   MOVE 'DUMP'        TO PVCODE-FUNC.
000587   CALL 'SUBVCODE' USING PVCODE-PRM
000588                         IN100
000589                         IN200
000590                         IN300
000591                         OUT00.
000592 A00-EXIT.
000593   MOVE ZERO TO RETURN-CODE.
000594   GOBACK.
000600