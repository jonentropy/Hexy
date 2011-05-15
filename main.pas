//
// Copyright 2011 Tristan Linnell
//    tris@canthack.org
//
// main.pas - Main form for Hexy
//   binary to hex header file conversion utility
//

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, LCLIntf;

type

  { TForm1 }

  TForm1 = class(TForm)
    lblTAL: TLabel;
    lblStatus: TLabel;
    lblInstructions: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure lblTALClick(Sender: TObject);
    procedure lblTALMouseEnter(Sender: TObject);
    procedure lblTALMouseLeave(Sender: TObject);
  private
    procedure ConvertToHex(filename: string);
    function ReplaceIllegalChars(Input: String): String;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  DeveloperURL: string = 'http://github.com/tristan2468';
  SourceHeader: string = '//Auto-created by Hexy binary to hex array conversion utility' + LineEnding +
                         '//https://github.com/tristan2468/Hexy' + LineEnding +
                         '// on ';

implementation

{ TForm1 }

procedure TForm1.ConvertToHex(filename: string);
var
  Fin:     file;
  i, j: integer;
  Outstring: string;
  First:   boolean;
  Outfile: TextFile;
  FileSizeValue: int64;
  DataArray: array of byte;
begin
  First := True;

  try
    if (Filename <> '') and FileExists(Filename) then
    begin
      lblStatus.Caption := '';

      lblStatus.Caption := 'Reading ' + ExtractFileName(Filename) + '...';
      ProgressBar1.Position := 0;
      FileSizeValue := FileSize(Filename);

      if filesizevalue > 0 then
        ProgressBar1.Max := FileSizeValue;

      SetLength(DataArray, FileSizeValue);

      AssignFile(fin, filename);

      try
        FileMode := fmOpenRead;
        Reset(fin, 1);
        BlockRead(fin, DataArray[0], FileSizeValue);
      finally
        CloseFile(fin);
      end;

      Outstring := SourceHeader + DateTimeToStr(Now) + LineEnding + LineEnding + #9 +
                   'unsigned byte ' + ReplaceIllegalChars(ChangeFileExt(ExtractFilename(Filename), '')) + '[' +
                   IntToStr(FileSizeValue) + '] = {' + LineEnding + #9#9;
      i := 0;
      for j := Low(DataArray) to High(DataArray) do
      begin

        if not First then
        begin
          Outstring := Outstring + ',';
        end;

        if i > 15 then
        begin
          i := 0;
          Outstring := Outstring + LineEnding + #9#9;
        end;

        Inc(i);
        Outstring := Outstring + '0x' + IntToHex(DataArray[j], 2);

        if j mod 32 = 0 then
        begin
          ProgressBar1.StepBy(32);
          Application.ProcessMessages;
        end;

        First := False;
      end;

      SetLength(DataArray, 0);

      Outstring := Outstring + '};';

      lblStatus.Caption := 'Writing...';
      ProgressBar1.Position := ProgressBar1.Max;
      Application.ProcessMessages;

      AssignFile(Outfile, ChangeFileExt(filename, '.h'));
      try
        Rewrite(Outfile);
        Writeln(Outfile, Outstring);
        Flush(Outfile);
      finally
        CloseFile(Outfile);
      end;

      lblStatus.Caption := 'Done!';
    end;

  except
    on E:Exception do
      ShowMessage(E.ClassName + ' exception occurred with the message: ' + E.Message);
  end;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: integer;
begin
  for i := Low(Filenames) to High(Filenames) do
    ConvertToHex(FileNames[i]);
end;

procedure TForm1.lblTALClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  OpenURL(DeveloperURL);
  Sleep(300);
  Screen.Cursor := crDefault;
end;

procedure TForm1.lblTALMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderline, fsBold];
  Screen.Cursor := crHandPoint;
end;

procedure TForm1.lblTALMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
  Screen.Cursor := crDefault;
end;

function TForm1.ReplaceIllegalChars(Input: string): string;
begin
  Result := Input;

  if (Length(Result) > 0) and (Result[1] in ['0'..'9']) then
    Result := '_' + Result;

  Result := StringReplace(Result, '-', '_', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
end;

initialization
  {$I main.lrs}
end.
