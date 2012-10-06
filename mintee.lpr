{
*	The MIT License
*
*	Copyright (c) 2012 Georgios Migdos <cyberpython@gmail.com>
*
*	Permission is hereby granted, free of charge, to any person obtaining a copy
*	of this software and associated documentation files (the "Software"), to deal
*	in the Software without restriction, including without limitation the rights
*	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*	copies of the Software, and to permit persons to whom the Software is
*	furnished to do so, subject to the following conditions:
*
*	The above copyright notice and this permission notice shall be included in
*	all copies or substantial portions of the Software.
*
*	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*	THE SOFTWARE.
}
program mintee;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Process;

type

  { TMintee }

  TMintee = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMintee }

procedure TMintee.DoRun;
const
  READ_BYTES = 2048;
var
  ErrorMsg          : String;
  Command           : String;
  Buf               : Array[1..1024] of UnicodeChar;
  TmpStr            : String;
  Proc              : TProcess;
  BytesRead         : Integer;
  TotalBytesRead    : LongInt;
  StdOutFile        : TextFile;
  StdOutFname       : String;
  LongOpt           : Boolean;
  ShortOpt          : Boolean;
  WriteOutFile      : Boolean;
  WriteStdOut       : Boolean;
  AppendToFiles     : Boolean;
begin

  CaseSensitiveOptions := False;

  // quick check parameters
  ErrorMsg:=CheckOptions('o:a','help out: no-stdout');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('o', 'out') then
     WriteOutFile := True
  else
    WriteOutFile := False;

  if HasOption('no-stdout') then
    WriteStdOut := False
  else
    WriteStdOut := True;

  if HasOption('a') then
    AppendToFiles := True
  else
    AppendToFiles := False;

  // check that a command has been specified
  LongOpt := True;
  ShortOpt := False;
  if( (LeftStr(ParamStr(ParamCount),1)=OptionChar) OR ((FindOptionIndex('o', ShortOpt)+1)=ParamCount)
                                            OR ((FindOptionIndex('out', LongOpt)+1)=ParamCount))then
  begin
    WriteLn('No command specified!');
    WriteHelp;
    Terminate;
    Exit;
  end;

  TotalBytesRead := 0;

  StdOutFname := '';

  Command := ParamStr(ParamCount);

  if WriteOutFile then
  begin
       StdOutFname := GetOptionValue('o', 'out');
       AssignFile(StdOutFile, StdOutFname);
       if AppendToFiles then
         Append(StdOutFile)
       else
         Rewrite(StdOutFile);
  end;

  Proc := TProcess.Create(nil);
  Proc.CommandLine := Command;
  Proc.Options := [poUsePipes];
  Proc.Execute;

  try
  begin
    while Proc.Running do
    begin
         BytesRead := Proc.Output.Read( Buf, READ_BYTES);
         if(BytesRead>0) then
         begin
             Inc(TotalBytesRead, BytesRead);
             SetString(TmpStr, @Buf, BytesRead);
             if WriteStdOut then Write(TmpStr);
             if WriteOutFile then Write(StdOutFile, TmpStr);
         end
         else Sleep(100);
    end;

    repeat
         BytesRead := Proc.Output.Read( Buf, READ_BYTES);
         if(BytesRead>0) then
         begin
             Inc(TotalBytesRead, BytesRead);
             SetString(TmpStr, @Buf, BytesRead);
             if WriteStdOut then Write(TmpStr);
             if WriteOutFile then Write(StdOutFile, TmpStr);
         end;
    until BytesRead <= 0;

  end;
  finally
      if WriteOutFile then CloseFile(StdOutFile);
      Proc.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TMintee.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMintee.Destroy;
begin
  inherited Destroy;
end;

procedure TMintee.WriteHelp;
begin
  WriteLn('Usage: mintee [OPTIONS] <CMD>');
  WriteLn('Execute command <CMD> and copy the command''s output to a file as well as to the standard output.');
  WriteLn();
  WriteLn('  -a, --append            append instead of overwriting files');
  WriteLn('  -o <file>, --out=<file> copy from the command''s stdout to <file>');
  WriteLn('  --no-stdout             do not write to stdout');
  WriteLn();
  WriteLn('Copyright © 2012 Georgios Migdos <cyberpython@gmail.com>');
end;

var
  Application: TMintee;

begin
  Application:=TMintee.Create(nil);
  Application.Title:='mintee';
  Application.Run;
  Application.Free;
end.

