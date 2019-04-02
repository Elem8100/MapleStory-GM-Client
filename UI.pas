unit UI;

interface

uses
  Windows, SysUtils, StrUtils, WZArchive, Generics.Collections,
  WZIMGFile, WZDirectory, Classes, Graphics,
  BassHandler, Global, ExtCtrls, StdCtrls, PNGImage;

var
  UIImages: TDictionary<string, TPNGImage>;

procedure InitUI;

implementation

uses MainUnit;

procedure InitUI;
var
  Iter, Iter2, Iter3, Iter4: TWZIMGEntry;
  I: Integer;
begin
  for Iter in UIWz.GetImgFile('StatusBar2.img').Root.Children do
  begin
    for Iter2 in Iter.Children do
    begin
      if Iter2.DataType = mdtCanvas then
        UIImages.Add('StatusBar2/' + Iter.Name + '/' + Iter2.Name, Iter2.Canvas.DumpPNG);

      for Iter3 in Iter2.Children do
      begin
        if Iter3.DataType = mdtCanvas then
          UIImages.Add('StatusBar2/' + Iter.Name + '/' + Iter2.Name + '/' +  Iter3.Name, Iter3.Canvas.DumpPNG);

        for Iter4 in Iter3.Children do
        begin
          if Iter4.DataType = mdtCanvas then
            UIImages.Add('StatusBar2/' + Iter.Name + '/' + Iter2.Name + '/' + Iter3.Name + '/' + Iter4.Name, Iter4.Canvas.DumpPNG);
        end;

      end;
    end;
  end;

  for I := 0 to MainForm.ComponentCount - 1 do
    if (MainForm.Components[I] is TImage) then
      with TImage(MainForm.Components[I]) do
      begin
        if UIImages.ContainsKey(Hint) then
          Picture.Assign(UIImages[Hint]);
      end;

end;

initialization

UIImages := TDictionary<string, TPNGImage>.Create;

end.
