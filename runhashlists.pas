program runhashlists;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, pointerhash, stringhash
  { you can add units after this };

type

  { TAppPerlHash }

  TAppPerlHash = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TAppPerlHash }

procedure TAppPerlHash.DoRun;
var
  hshmap: TPLObjectHashList;
  strmap: TPLStringHashList;
  psvl: PAnsiString;
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

(*   *)
  hshmap := TPLObjectHashList.Create;
  strmap := TPLStringHashList.Create;

  hshmap.setValue('en', strmap);

  strmap.setValue('ab', 'cd');
  strmap.setValue('ef', 'gh');
  strmap.setValue('ij', 'kl');

  strmap := TPLStringHashList.Create;

  hshmap.setValue('es', strmap);

  strmap.setValue('bog', 'bog_value');
  strmap.setValue('gob', 'gob_value');
  strmap.setValue('key6', 'value6');

  Write('Key: ', chr(39), 'gob', chr(39), ': ', chr(39));
  psvl := strmap.getValue('gob');
  if psvl <> nil then Write(psvl^);
  WriteLn(chr(39));

  Write('Key: ', chr(39), 'bog', chr(39), ': ', chr(39));
  psvl := strmap.getValue('bog');
  if psvl <> nil then Write(psvl^);
  WriteLn(chr(39));

  Write('Key: ', chr(39), 'bob', chr(39), ': ', chr(39));
  psvl := strmap.getValue('bob');
  if psvl <> nil then Write(psvl^);
  WriteLn(chr(39));

  WriteLn('hsh map (cnt: ', chr(39), hshmap.getCount(), chr(39), '):');

  if hshmap.moveFirst() then
  begin
    repeat  //until not hshmap.moveNext();
      strmap := TPLStringHashList(hshmap.getCurrentValue());

      WriteLn('str map ', chr(39), hshmap.getCurrentKey(), chr(39)
        , ' (cnt: ', chr(39), strmap.getCount(), chr(39), '):');

      if strmap.moveFirst() then
      begin
        repeat
          Write('; key: ', chr(39), strmap.getCurrentKey(), chr(39)
            , ' => ', chr(39), PAnsiString(strmap.getCurrentValue())^, chr(39));

        until not strmap.moveNext();

        WriteLn('');
      end;  //if strmap.moveFirst() then
    until not hshmap.moveNext();

  end;  //if hshmap.moveFirst() then

  if hshmap.moveFirst() then
  begin
    TPLStringHashList(hshmap.getCurrentValue()).Free;

    while hshmap.moveNext() do
    begin
      TPLStringHashList(hshmap.getCurrentValue()).Free;
    end;
  end;  //if hshmap.moveFirst() then

  hshmap.Free;


  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TAppPerlHash.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TAppPerlHash.Destroy;
begin
  inherited Destroy;
end;

procedure TAppPerlHash.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TAppPerlHash;
begin
  Application:=TAppPerlHash.Create(nil);
  Application.Title:='App Perl Hash';
  Application.Run;
  Application.Free;
end.

