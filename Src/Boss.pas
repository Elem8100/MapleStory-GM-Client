unit Boss;

interface

uses
  Windows, types, SysUtils, StrUtils,  Generics.Collections,
  WZIMGFile, Global, Math, Footholds, LadderRopes, Mob2;

procedure DropBoss;

implementation
  uses MapleMap;

procedure DropBoss;
var
  I: Integer;
begin
  case TMap.ID.ToInteger   of
    // �𧯤�
    102020500: TMob.Drop('3220000',250, 2050);
    // �Ŧ�Ĩۣ��
    100020301: TMob.Drop('8220007',170, -800);
    // �߬ɫ��F
    103020320: TMob.Drop('5090000',400, 250);
    // ���_��
    104010200: TMob.Drop('2220000',250, 170);
    //  6130101:
    100020101: TMob.Drop('6130101', -800, 100);
    // �L��Ĩۣ��
    100020401: TMob.Drop('6300005', 150, -700);
    // �h�A���s
    103030400: TMob.Drop('6220000', -500, 0);
    // �L�͵U��
    101040300: TMob.Drop('5220002', 300, -500);
    // ���p���N�ԩM���Z���
    106021401, 106021500: TMob.Drop('3300005', 0, -100);
    // �u�\�h�L�`�q�O�x
    102040600: TMob.Drop('9400725', -150, -250);
    // �`�z�j��
    106021600, 106021700: TMob.Drop('3300008', 0, -100);
    // ���~��
    120030500: TMob.Drop('5220000', -1000, 0);
    // ���F�Х�
    200080000: TMob.Drop('9400726', -1000, 0);
    // ���贵
    200101500: TMob.Drop('6160003', 500, 100);
    // ���F��
    230020100: TMob.Drop('4220000', -100, 500);
    // ���Ⱝ�� + �ŦⰭ�� + ��Ⱝ��
    222010401:
    begin
      TMob.Drop('7130400', 500, 0);
      TMob.Drop('7130401', 500, 0);
      TMob.Drop('7130402', 500, 0);
    end;
    //�Щx�j��
    921133000: TMob.Drop('8210006', 500, -300);
    //�Z�L����
    702060000:  TMob.Drop('9600026', 0, 400);
    //�ּw��
    250010304:  TMob.Drop('9300200', -400, 300);
    //�}�S�M����
    261010102, 261020401:  TMob.Drop('8090000', 0, 0);
    //�_����
    261030000:  TMob.Drop('8220002', -600, -600);
    //����ǤH
    211040500:  TMob.Drop('8220001', 0, 400);
    //�Թϴ�����
    220080001:
    begin
      TMob.Drop('8500000', -400, -600);
      TMob.Drop('8500001', -400, -600);
      TMob.Drop('8500002', -400, -600);
      TMob.Drop('8500003', -400, -600);
      TMob.Drop('8500004', -400, -600);
    end;
    //�p�륨�H,�Щx�j�� ,�Щx�ܿ�,�Z�p��
    211070100, 211070200 :
    begin
      TMob.Drop('8840000', 0, -300);
      TMob.Drop('8840003', 0, -300);
      TMob.Drop('8840005', 0, -300);
      TMob.Drop('8840004', 0, -300);
    end;
    //�̹B����
    211041400:  TMob.Drop('6090000', 1300, 0);
    //���Ǻ� ,�������
    551030200:
    begin
      TMob.Drop('9420542', -350, 500);
      TMob.Drop('9420547', -350, 500);
    end;
    //�¹D����,�¹D�v��, �¹D�O��
    801040100:
    begin
      TMob.Drop('9400300', 700, 0);
      TMob.Drop('9400112', 700, 0);
      TMob.Drop('9400113', 700, 0);
    end;
    //����,�Ѭ�����
    800040410:
    begin
      TMob.Drop('9400407', 180, 0);
      TMob.Drop('9400409', 650, 0);
    end;
    //�֥d��
    270050100: TMob.Drop('9410070', 0, -220);
    //���մ�
    271040100: TMob.Drop('8850011', -150, -100);
    //Zakum
    {
    280030000, 280030001,280030100:
    begin
      TMob.Drop('8800000', -50, -300);
      for I := 8800010 downto 8800003 do
       TMob.Drop(inttostr(I), -50, -300);
    end;
    }
    //Horntail
    240060200, 240060201:
    begin
      for I := 8810003 to 8810006 do
        TMob.Drop(inttostr(I), 80, 200);
      for I := 8810008 to 8810009 do
        TMob.Drop(inttostr(I), 80, 200);
      TMob.Drop('8810002', 80, 200);
      TMob.Drop('8810007', 80, 200);
    end;
    //balrog
    105100300, 105100400:
    begin
      TMob.Drop('8830004', 400, -200);
      TMob.Drop('8830005', 400, -200);
      TMob.Drop('8830000', 400, -200);
    end;

  end;

end;


end.
