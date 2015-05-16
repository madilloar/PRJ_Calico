000010 IDENTIFICATION DIVISION.
000020 PROGRAM-ID. SUBMEM.
000030 ENVIRONMENT    DIVISION.
000040 CONFIGURATION  SECTION.
000050 DATA DIVISION.
000060 WORKING-STORAGE SECTION.
000070*--------------------------------------------------------------
000080*- ABEND���̎肪����p�B
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
000230*- �p�����[�^�����B
000000*-
000240*- I:FNC:�@�\���́B
000000*-       'INIT':���g���Ǘ����郁�����G���A�̏������B
000000*-       'SETD':VAL�̕���������g���Ǘ����镶����̈�PMEM-STR
000000*-              �ɃZ�b�g���A���̃|�C���^��PVL�ɃZ�b�g���ĕԂ��B
000000*-              VAL���當�����؂�o���Ƃ��A1�����ڂ���
000000*-              HIGH-VALUE��������܂ł�؂�o���B
000000*-       'SETL':VAL�̕���������g���Ǘ����镶����̈�PMEM-STR
000000*-              �ɃZ�b�g���A���̃|�C���^��PVL�ɃZ�b�g���ĕԂ��B
000000*-              VAL���當�����؂�o���Ƃ��A1�����ڂ���
000000*-              VLL�܂ł�؂�o���B
000000*-       'GET_':���g���Ǘ����镶����̈�PMEM-STR����PVL�Ɏw��
000000*-              ���ꂽ�|�C���^����HIGH-VALUE�܂ł̕������
000000*-              �؂�o���AVAL�ɃZ�b�g���ĕԂ��B
000000*-
000250*- I:MEMARA-SZE:������̈�̃o�C�g���BFNC='INIT'���ɕK�v�B
000000*-
000260*- I:VLL:SET����l�����̕����񒷁BFNC='SET_'���ɕK�v�B
000270*- O:VLL:GET�����l�����̕����񒷁BFNC='GET_'���ɕԂ��B
000000*-
000280*- I:PVL:������擪�|�C���^�B
000290*-       FNC='GET_'���Ɏw�肷��ƁA�ړI�̕����񂪎擾�ł���B
000300*- O:PVL:������擪�|�C���^�B
000310*-       FNC='SET_'���ɁAPMEM-VAL��PMEM-STR�ɐݒ肵����ɁA
000310*-       PMEM-STR�̃|�C���^��Ԃ��B
000000*-
000330 01       PMEM-PRM.
000390   03     PMEM-COD          PIC S9(1).
000400   03     PMEM-MSG          PIC X(80).
000340   03     PMEM-FNC          PIC X(4).
000350   03     PMEM-MEMARA-SZE   PIC 9(5).
000360   03     PMEM-VLL          PIC 9(5).
000370   03     PMEM-PVL          PIC 9(5).
000650*--------------------------------------------------------------
000660*- �l�󂯓n���G���A�B
000430*- I:VAL�̒l��VLL�̒������������ɃZ�b�g�B
000440*- O:�������̒l��VLL�̒�����VAL�ɃZ�b�g�B
000450*- "X(1)"�́A������ƃg���b�L�[�ł����A�Ăяo�����ŁA�\����
000460*- �̈������Ă���O��B
000320*--------------------------------------------------------------
000481 01       PMEM-VAL.
000481   03     FILLER            PIC X(1).
000650*--------------------------------------------------------------
000660*- �������G���A�B
000510*- SZE:�������G���A�̑S�̂̃o�C�g���B
000520*- PNXT:������̈�̋󂫊J�n�|�C���^�B
000000*-      SZE 9(5), PNXT 9(5)�̎��Ȃ̂ŁA�����l��11�ƂȂ�B
000520*- STR:������̈�B
000680*- "X(1)"�́A������ƃg���b�L�[�ł����A�Ăяo�����ŁA�������̈�
000690*- �\���Ɏ���Ă���O��B
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
001240*- �������[�T�C�Y�𒴂�����`�a�d�m�c
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
001200*- ���̃|�C���^�[���v�Z����
001210   COMPUTE WKNXP = PMEM-PNXT + PMEM-VLL + 1.
001220   IF WKNXP > PMEM-SZE
001230     THEN
001240*- �������[�T�C�Y�𒴂�����`�a�d�m�c
001250       MOVE -1              TO PMEM-COD
001251       STRING WK000MYNAME SPACE ',ABNORMAL END.'
001252                            INTO PMEM-MSG
001253       GO TO D00-EXIT
001260   END-IF.
001270*- �����̒l���J�����g�|�C���^�[����Z�b�g����B
001280   MOVE PMEM-VAL(1:PMEM-VLL)
001290                            TO PMEM-STR(PMEM-PNXT:PMEM-VLL).
001330*- �J�����g�|�C���^�[��߂�l�Ƃ���B
001340   MOVE PMEM-PNXT           TO PMEM-PVL.
001350*- ���̃|�C���^�[���J�����g�|�C���^�[�ɂ���B
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
001640*- �������B
001650*--------------------------------------------------------------
001660 S00-010.
001670   MOVE 'S00-010.'          TO WK000SECTION.
001680   MOVE -1                  TO PMEM-COD.
001690   STRING WK000MYNAME SPACE ',ABNORMAL END.'
001700                            INTO PMEM-MSG.
001710 S00-EXIT.
001720   EXIT.
