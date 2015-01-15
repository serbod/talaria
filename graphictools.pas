unit GraphicTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Clipbrd, LCLIntf, LCLType;


{ Return file type from file name extension, one of:
  picture, archive, executable, audio, video }
function GetFileType(AFileName: string): string;

{ Return bytes string with preview data from AFileName file
  if AFileName='' then get preview from clipboard
  AImageSize is desired width and height
  AFileInfo returns file type and properties: "picture bitmap 640x480"
  }
function GetFilePreview(AFileName: string; AImageSize: TPoint; var AFileInfo: string): AnsiString;

{ Open picture file from AFileName and resize it to given ABitmap
  original image resolution info written to AImageInfo }
function GetThumbnailFromFile(AFileName: string;
  ABitmap: TBitmap; var AImageInfo: string): boolean;

{ Open picture from clipboard and resize it to given ABitmap
  original image resolution info written to AImageInfo }
function GetThumbnailFromClipboard(ABitmap: TBitmap; var AImageInfo: string): boolean;

implementation

function GetFileType(AFileName: string): string;
var
  sExt: string;
begin
  sExt:='|'+LowerCase(ExtractFileExt(AFileName))+'|';
  if Pos(sExt, '|.bmp|.jpg|.jpeg|.png|')>0 then Result:='picture'
  else if Pos(sExt, '|.zip|.rar|.7z|.gz|.tar|')>0 then Result:='archive'
  else if Pos(sExt, '|.exe|.bat|.com|')>0 then Result:='executable'
  else if Pos(sExt, '|.wav|.mp3|.midi|.ogg|')>0 then Result:='audio'
  else if Pos(sExt, '|.avi|.mkv|.mpg|.mp4|.flw|.wmv')>0 then Result:='video'
  else Result:='unknown';
end;

function GetFilePreview(AFileName: string; AImageSize: TPoint;
  var AFileInfo: string): AnsiString;
var
  ssData: TStringStream;
  sFileType, sImgInfo: string;
  bmp: TBitmap;
  pic: TPicture;
  Done: boolean;
begin
  Result:='';
  AFileInfo:='';
  ssData:=TStringStream.Create('');

  if AFileName='' then sFileType:='picture'
  else sFileType:=GetFileType(AFileName);

  if sFileType='picture' then
  begin
    bmp:=TBitmap.Create();
    bmp.Height:=AImageSize.y;
    bmp.Width:=AImageSize.x;
    try
      sImgInfo:='';
      Done:=False;
      if AFileName='' then // clipboard
        Done:=GetThumbnailFromClipboard(bmp, sImgInfo)
      else
        Done:=GetThumbnailFromFile(AFileName, bmp, sImgInfo);
      if Done then
      begin
        bmp.SaveToStream(ssData);
        AFileInfo:=sFileType+' bitmap '+sImgInfo;
      end;

    finally
      FreeAndNil(bmp);
    end;
  end;
  Result:=ssData.DataString;
  FreeAndNil(ssData);
end;

function ResizePicture(Pic: TPicture; ABitmap: TBitmap): Boolean;
var
  r: TRect;
  k, kx, ky: Real;
begin
  Result:=False;
  if not Assigned(Pic) then Exit;
  if not Assigned(ABitmap) then Exit;
  if (ABitmap.Height=0) or (ABitmap.Width=0) then Exit;

  r:=Rect(0, 0, ABitmap.Width, ABitmap.Height);

  // calculate aspect ratio and clip frame size
  ky:=Pic.Height / ABitmap.Height;
  kx:=Pic.Width / ABitmap.Width;
  k:=kx;
  if ky>k then k:=ky;
  if k>0 then
  begin
    r.Right:=Round(Pic.Width / k);
    r.Bottom:=Round(Pic.Height / k);
    // align to center
    if r.BottomRight.x < ABitmap.Width then
    begin
      r.Left:=(ABitmap.Width-r.BottomRight.x) div 2;
      r.Right:=r.Right+r.Left;
    end;
    if r.BottomRight.y < ABitmap.Height then
    begin
      r.Top:=(ABitmap.Height-r.BottomRight.y) div 2;
      r.Bottom:=r.Bottom+r.Top;
    end;
  end;

  ABitmap.Canvas.StretchDraw(r, Pic.Graphic);

  Result:=True;
end;

{ Open picture file from AFileName and resize it to given ABitmap
  original image resolution info written to AImageInfo }
function GetThumbnailFromFile(AFileName: string; ABitmap: TBitmap;
  var AImageInfo: string): boolean;
var
  Pic: TPicture;
begin
  Result:=False;
  if not Assigned(ABitmap) then Exit;
  if (ABitmap.Height=0) or (ABitmap.Width=0) then Exit;

  Pic:=TPicture.Create();
  try
    Pic.LoadFromFile(AFileName);
    AImageInfo:=IntToStr(Pic.Width)+'x'+IntToStr(Pic.Height);

    Result:=ResizePicture(Pic, ABitmap);
  finally
    FreeAndNil(Pic);
  end;
end;


{ Open picture from clipboard and resize it to given ABitmap
  original image resolution info written to AImageInfo }
function GetThumbnailFromClipboard(ABitmap: TBitmap; var AImageInfo: string): boolean;
var
  Pic: TPicture;
begin
  Result:=False;
  if not Assigned(ABitmap) then Exit;
  if (ABitmap.Height=0) or (ABitmap.Width=0) then Exit;

  Pic:=TPicture.Create();
  try
    if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
      Pic.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap))
    else if Clipboard.HasFormat(PredefinedClipboardFormat(pcfDelphiBitmap)) then
      Pic.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfDelphiBitmap));

    if (Pic.Height>0) and (Pic.Width>0) then
    begin
      AImageInfo:=IntToStr(Pic.Width)+'x'+IntToStr(Pic.Height); // !!!
      Result:=ResizePicture(Pic, ABitmap);
    end;
  finally
    FreeAndNil(Pic);
  end;
end;


end.

