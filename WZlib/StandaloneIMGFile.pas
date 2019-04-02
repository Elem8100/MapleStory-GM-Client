unit StandaloneIMGFile;

interface

uses WZDirectory, WZIMGFile, WZReader, VirtualTrees;

function LoadIMGFile(const FileName: string; VST: TVirtualStringTree = nil): TWZIMGFile;

implementation

function LoadIMGFile(const FileName: string; VST: TVirtualStringTree = nil): TWZIMGFile;
var
  Reader: TWZReader;
  F: TWZFile;
begin
  Reader := TWZReader.Create(FileName);
  F := TWZFile.Create(Reader.FileName, 0, 0, nil);
  F.Offset := 0;
  if VST <> nil then
  begin
    F.VST := VST;
    F.Node := VST.AddChild(nil, F);
  end;
  Result := TWZIMGFile.Create(Reader, F);
  F.Parsed := True;
  F.IMGFile := Result;
end;

end.
