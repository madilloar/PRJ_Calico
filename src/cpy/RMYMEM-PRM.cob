000220*--------------------------------------------------------------
000230*- �������T�u���[�`��CALL�p�����[�^�B
000240*- I:FNC:�@�\���́B
000250*- I:MEM-SZE:�������̃o�C�g���BFNC='INIT'���ɕK�v�B
000260*- I:VLL:SET����l�����̕����񒷁BFNC='SET_'���ɕK�v�B
000270*- O:VLL:GET�����l�����̕����񒷁BFNC='GET_'���ɕԂ��B
000280*- I:VLI:�g�p�σ������J�n�A�h���X�Ǘ��e�[�u����INDEX�B
000290*-       FNC='GET_'���Ɏw�肷��ƁA�ړI�̕����񂪎擾�ł���B
000300*- O:VLI:FNC='SET_'���ɁA�g�p�σ������J�n�A�h���X�Ǘ�
000310*-       �e�[�u����INDEX��Ԃ��B
000320*--------------------------------------------------------------
000340   03     PMYMEM-FNC        PIC X(4).
000350   03     PMYMEM-MEM-SZE    PIC 9(5).
000360   03     PMYMEM-VLL        PIC 9(5).
000370   03     PMYMEM-VLI        PIC 9(5).
000380   03     PMYMEM-STS.
000390     05   PMYMEM-COD        PIC S9(1).
000400     05   PMYMEM-MSG        PIC X(80).