program demohashlists;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , pointerhash, stringhash;


type
  { TDemoHashLists }
  TDemoHashLists = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


{ TDemoHashLists }
procedure TDemoHashLists.DoRun;
var
  hshmap: TPLPointerHashList;
  strmap: TPLStringHashList;
  itr: TPLPtrHashListIterator;
  sky, svl, srmky1, srmky2, srmky3, srmky4: String;
  ErrorMsg: String;
  psvl: PAnsiString;
  iky: Integer;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hd', ['help', 'debug']);

  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;  //if ErrorMsg<>'' then

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;  //if HasOption('h', 'help') then

(*   *)
  hshmap := TPLPointerHashList.Create(3, 2);
  strmap := TPLStringHashList.Create(3, 2);

  hshmap.setValue('en', strmap);

  strmap.setValue('ab', 'cd');
  strmap.setValue('ef', 'gh');
  strmap.setValue('ij', 'kl');

  strmap := TPLStringHashList.Create(2, 2);

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

  strmap.setValue('trying a somwhat very long key 1', 'inserting its somewhat very long value 1');

  strmap := TPLStringHashList.Create;

  strmap.GrowFactor := 2;

  hshmap.setValue('iterator', strmap);

  for iky := 1 to 10 do
  begin
    sky := 'key' + IntToStr(iky);
    svl := 'value' + IntToStr(iky);
    strmap.setValue(sky, svl);
  end;  //for iky := 1 to 10 do

  WriteLn('hsh map (cnt: ', chr(39), hshmap.Count, chr(39), '):');

  // parse parameters
  if HasOption('d', 'debug') then
  begin
    WriteLn('Sleeping 30 s ...');

    Sleep(30000);

    WriteLn('Sleeping 30 s done.');
  end;  //if HasOption('d', 'debug') then

  if hshmap.moveFirst() then
  begin
    repeat  //until not hshmap.moveNext();
      strmap := TPLStringHashList(hshmap.getCurrentValue());

      WriteLn('str map ', chr(39), hshmap.getCurrentKey(), chr(39)
        , ' (cnt: ', chr(39), strmap.Count, chr(39), '):');

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


  strmap := TPLStringHashList(hshmap['iterator']);
  itr := strmap.Iterator;
  srmky1 := 'key2';
  srmky2 := 'key3';
  srmky3 := 'key8';
  srmky4 := 'key9';

  if itr <> Nil then
  begin
    repeat  //until not itr.Next;
      if (itr.Key = srmky1)
        or (itr.Key = srmky2)
        or (itr.Key = srmky3)
        or (itr.Key = srmky4) then
      begin
        WriteLn('key: '#39 + itr.Key + #39'; key removing ...');
        //Remove the selected Keys and move on
        strmap.removeKey(itr.Key);
      end;
    until not itr.Next;

  end;  //if itr <> Nil then


  if hshmap.moveFirst() then
  begin
    TPLStringHashList(hshmap.getCurrentValue()).Free;

    while hshmap.moveNext() do
    begin
      TPLStringHashList(hshmap.getCurrentValue()).Free;
    end;
  end;  //if hshmap.moveFirst() then

  hshmap.Free;


  // stop program loop
  Terminate;
end;

constructor TDemoHashLists.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TDemoHashLists.Destroy;
begin
  inherited Destroy;
end;

procedure TDemoHashLists.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TDemoHashLists;
begin
  Application:=TDemoHashLists.Create(nil);
  Application.Title:='Demo Hash Lists';
  Application.Run;
  Application.Free;
end.

