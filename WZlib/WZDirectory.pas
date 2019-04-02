unit WZDirectory;

interface

uses SysUtils, Generics.Collections;

type
  TWZEntry = class
  protected
    FName: string;
    FSize: Integer;
    FChecksum: Integer;
    FOffset: Cardinal;
    FParent: TWZEntry;
  public
    constructor Create(const Name: string; Size, Checksum, Offset: Integer; Parent: TWZEntry);

    function GetPath: string;

    property Name: string read FName write FName;
    property Size: Integer read FSize write FSize;
    property Checksum: Integer read FChecksum write FChecksum;
    property Offset: Cardinal read FOffset write FOffset;
    property Parent: TWZEntry read FParent write FParent;
  end;

  TWZFile = class(TWZEntry)
  private
    FIMGFile: TObject;
  public
    destructor Destroy; override;

    property IMGFile: TObject read FIMGFile write FIMGFile;
  end;

  TWZDirectory = class(TWZEntry)
  private
    FArchive: TObject; { TWZArchive }
    FFiles: TList<TWZFile>;
    FSubDirs: TList<TWZDirectory>;
    FEntries: TList<TWZEntry>;    // Files and SubDirs together

    function GetEntry(const Name: string): TWZEntry;
  public
    constructor Create(const Name: string; Size, Checksum, Offset: Integer; Parent: TWZEntry);
    destructor Destroy; override;

    procedure AddFile(FileEntry: TWZFile);
    procedure AddDirectory(Dir: TWZDirectory);

    property Archive: TObject { TWZArchive } read FArchive write FArchive;
    property Entry[const Name: string]: TWZEntry read GetEntry;
    property Entries: TList<TWZEntry> read FEntries;
    property Files: TList<TWZFile> read FFiles;
    property SubDirs: TList<TWZDirectory> read FSubDirs;
  end;

implementation

uses WZArchive;

{ TWZEntry }

constructor TWZEntry.Create(const Name: string; Size, Checksum, Offset: Integer; Parent: TWZEntry);
begin
  FName := Name;
  FSize := Size;
  FChecksum := Checksum;
  FOffset := Offset;
  FParent := Parent;
end;

function TWZEntry.GetPath: string;
begin
  if (FParent = nil) or ((FParent is TWZDirectory) and (TWZDirectory(FParent).Archive <> nil)) then
    Result := Name
  else
    Result := FParent.GetPath + '/' + FName;
end;

{ TWZDirectory }

constructor TWZDirectory.Create(const Name: string; Size, Checksum, Offset: Integer; Parent: TWZEntry);
begin
  inherited Create(Name, Size, Checksum, Offset, Parent);

  FFiles := TList<TWZFile>.Create;
  FSubDirs := TList<TWZDirectory>.Create;
  FEntries := TList<TWZEntry>.Create;
end;

destructor TWZDirectory.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    FEntries[i].Free;

  FFiles.Free;
  FSubDirs.Free;
  FEntries.Free;

  inherited;
end;

function TWZDirectory.GetEntry(const Name: string): TWZEntry;
begin
  for Result in FEntries do
    if Result.Name = Name then
      Exit;

  Result := nil;
end;

procedure TWZDirectory.AddFile(FileEntry: TWZFile);
begin
  FFiles.Add(FileEntry);
  FEntries.Add(FileEntry);
end;

procedure TWZDirectory.AddDirectory(Dir: TWZDirectory);
begin
  FSubDirs.Add(Dir);
  FEntries.Add(Dir);
end;

{ TWZFile }

destructor TWZFile.Destroy;
begin
  if FIMGFile <> nil then
    FIMGFile.Free;

  inherited;
end;

end.

