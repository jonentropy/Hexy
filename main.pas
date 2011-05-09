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
  StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    lblStatus: TLabel;
    lblInstructions: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    procedure ConvertToHex(filename: string);
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

const
  SourceHeader: string = '//Auto-created by Hexy' + LineEnding +
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
                   'unsigned int ' + ChangeFileExt(ExtractFilename(Filename), '') + '[] = {';
      i := 0;
      for j := Low(DataArray) to High(DataArray) do
      begin

        if not First then
        begin
          Outstring := Outstring + ',';
        end;

        Inc(i);
        if i > 15 then
        begin
          i := 0;
          Outstring := Outstring + LineEnding + #9#9;
        end;

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

initialization
  {$I main.lrs}
end.
