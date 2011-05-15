//
// Copyright 2011 Tristan Linnell
//    tris@canthack.org
//
// hexy.lpr - Main project file for Hexy
//   binary to hex header file conversion utility
//

program hexy;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, main;

{$R *.res}

begin
  Application.Title:='Hexy';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
