000223*--------------------------------------------------------------
000224*- 仮想コード:パラメータで受け取った仮想コードを格納するエリア
000225*--------------------------------------------------------------
000227   03     VCODE-PC          PIC 9(5) VALUE ZERO.
000228   03     VCODE-TBL-SZE     PIC 9(5) VALUE ZERO.
000229   03     VCODE-TBL         OCCURS 500.
000230     05   VCODE.
000231      07  VCODE-OPR         PIC X(5).
000232      07  FILLER            PIC X(1).
000233      07  VCODE-OPD.
000234*- STACKがXタイプなので、ADRは9(5)とした
000235       09 VCODE-ADR         PIC 9(5).
