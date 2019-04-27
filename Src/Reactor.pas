unit Reactor;

interface

uses
  Windows, SysUtils, StrUtils, AsphyreSprite, WZArchive, Generics.Collections,
  WZIMGFile, WZDirectory, Classes, Global;

type

  TReactor = class(TSpriteEX)
  private
    ID: string;
  public
    procedure DoMove(const Movecount: Single); override;
    procedure DoDraw; override;
  end;

  procedure CreateReactor;

implementation

procedure CreateReactor;
var
  I: Integer;
  ReactorID, ImagePath: string;
  Iter, Iter2, Iter3, Entry, Child: TWZIMGEntry;
  HasLoad: TList<string>;
begin
  HasLoad := TList<string>.Create;
  if ImgFile.Child['reactor'] = nil then Exit;
  for Iter in ImgFile.Child['reactor'].Children do
  begin
    ReactorID := Iter.Get('id', '');
    Entry :=ReactorWz.GetImgFile(ReactorID + '.img').Root.Get('info/link');
    if Entry <> nil then
      ReactorID := Entry.Data;
    if not HasLoad.Contains(ReactorID) then
    begin
      HasLoad.Add(ReactorID);
      Entry := ReactorWz.GetImgFile(ReactorID + '.img').Root;
      for Iter2 in Entry.Children do
      begin
        for Iter3 in Iter2.Children do
        begin
          if Iter3.DataType = mdtUOL then
            Child := Iter2.Get(Iter3.Data)
          else
            Child := Iter3;

          if Child.DataType = mdtCanvas then
          begin
            ImagePath := ReactorID + Child.Parent.Name + '/' + Child.Name;
            WzData.AddOrSetValue(ReactorID + Iter2.Name + '/' + Iter3.Name, ImagePath);
          end;

          if (Child.DataType = mdtCanvas) and (not HasLoad.Contains(ImagePath))  then
          begin
            HasLoad.Add(ImagePath);
            DumpCanvas(ImagePath, Child);
          end;
        end;
      end;

    end;

    with TReactor.Create(SpriteEngine) do
    begin
      ID := ReactorID;
      X := Iter.Get('x', '');
      Y := Iter.Get('y', '');
      Z := 0;
      ImageLib := Images;
      ImageName := WzData[ID + '0/0'];
      Width := PatternWidth;
      Height := PatternHeight;
      Offset.X := -WzData[ImageName + 'origin.x'];
      Offset.Y := -WzData[ImageName + 'origin.y'];
    end;
  end;
  HasLoad.Free;
end;

procedure TReactor.DoMove(const Movecount: Single);
begin
  inherited;

 // Offset.X := -WzData[ImageName + 'origin.x'];
 // Offset.Y := -WzData[ImageName + 'origin.y'];
end;

procedure TReactor.DoDraw;
begin

    inherited;

end;

end.
