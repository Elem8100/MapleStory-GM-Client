unit Boss;

interface

uses
  Windows, types, SysUtils, StrUtils, AsphyreSprite, Generics.Collections,
  WZIMGFile, Global, Math, Footholds, LadderRopes, Mob2;

procedure DropBoss;

implementation
  uses MapleMap;

procedure DropBoss;
var
  I: Integer;
begin
  case TMap.ID.ToInteger   of
    // ¾ð§¯¤ý
    102020500: TMob.Drop('3220000',250, 2050);
    // ÂÅ¦âÄ¨Û£¤ý
    100020301: TMob.Drop('8220007',170, -800);
    // ­ß¬É«ÕÆF
    103020320: TMob.Drop('5090000',400, 250);
    // ¬õÄ_¤ý
    104010200: TMob.Drop('2220000',250, 170);
    //  6130101:
    100020101: TMob.Drop('6130101', -800, 100);
    // íL«ÍÄ¨Û£¤ý
    100020401: TMob.Drop('6300005', 150, -700);
    // ªh¿A¥¨Æs
    103030400: TMob.Drop('6220000', -500, 0);
    // íL«ÍµU¤ý
    101040300: TMob.Drop('5220002', 300, -500);
    // ¸¯¹p³·¦N©Ô©M¥øÃZ°ê¤ý
    106021401, 106021500: TMob.Drop('3300005', 0, -100);
    // ¾uÅ\¤h§LÁ`¥q¥O©x
    102040600: TMob.Drop('9400725', -150, -250);
    // Á`²z¤j¦Ú
    106021600, 106021700: TMob.Drop('3300008', 0, -100);
    // ¥¨©~ÃÉ
    120030500: TMob.Drop('5220000', -1000, 0);
    // ºëÆF±Ð¥À
    200080000: TMob.Drop('9400726', -1000, 0);
    // Á§¦è´µ
    200101500: TMob.Drop('6160003', 500, 100);
    // ¤õ°F´ß
    230020100: TMob.Drop('4220000', -100, 500);
    // ¶À¦â°­©Ç + ÂÅ¦â°­©Ç + ºñ¦â°­©Ç
    222010401:
    begin
      TMob.Drop('7130400', 500, 0);
      TMob.Drop('7130401', 500, 0);
      TMob.Drop('7130402', 500, 0);
    end;
    //±Ð©xÀjº¸
    921133000: TMob.Drop('8210006', 500, -300);
    //ªZªL§¯¹¬
    702060000:  TMob.Drop('9600026', 0, 400);
    //ªÖ¼wºµ
    250010304:  TMob.Drop('9300200', -400, 300);
    //­}¯S©M¬¥¨Ì
    261010102, 261020401:  TMob.Drop('8090000', 0, 0);
    //©_¬ü©Ô
    261030000:  TMob.Drop('8220002', -600, -600);
    //³·¤ò©Ç¤H
    211040500:  TMob.Drop('8220001', 0, 400);
    //©Ô¹Ï´µ¤§ÄÁ
    220080001:
    begin
      TMob.Drop('8500000', -400, -600);
      TMob.Drop('8500001', -400, -600);
      TMob.Drop('8500002', -400, -600);
      TMob.Drop('8500003', -400, -600);
      TMob.Drop('8500004', -400, -600);
    end;
    //¤p¶ë¥¨¤H,±Ð©xÀjº¸ ,±Ð©xµÜ¿Õ,¤Z¹p®¦
    211070100, 211070200 :
    begin
      TMob.Drop('8840000', 0, -300);
      TMob.Drop('8840003', 0, -300);
      TMob.Drop('8840005', 0, -300);
      TMob.Drop('8840004', 0, -300);
    end;
    //¤Ì¹B¦º¯«
    211041400:  TMob.Drop('6090000', 1300, 0);
    //®õ°Çºµ ,«½«½·à¤ý
    551030200:
    begin
      TMob.Drop('9420542', -350, 500);
      TMob.Drop('9420547', -350, 500);
    end;
    //¶Â¹Dªø¦Ñ,¶Â¹D®v·Ý, ¶Â¹D«OÃð
    801040100:
    begin
      TMob.Drop('9400300', 700, 0);
      TMob.Drop('9400112', 700, 0);
      TMob.Drop('9400113', 700, 0);
    end;
    //ÃÊßï,¤Ñ¬ÓÃÊßï
    800040410:
    begin
      TMob.Drop('9400407', 180, 0);
      TMob.Drop('9400409', 650, 0);
    end;
    //¥Ö¥d³î
    270050100: TMob.Drop('9410070', 0, -220);
    //¦è®æ¿Õ´µ
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
