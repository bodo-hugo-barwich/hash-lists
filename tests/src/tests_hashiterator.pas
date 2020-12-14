unit tests_hashiterator;

{$mode objfpc}{$H+}

interface

uses
  TestFramework
  , pointerhash;

type
  TTestsHashListIterator = class(TTestCase)
  protected
    lsthshobjs: TPLPointerHashList;
    procedure SetUp; override;
    procedure Teardown; override;
  published
    (*
    Test Adding 20 Keys and their Values with the Default Property Method (which should grow the List more than twice)
    And Iterating over the Keys from the Beginning and Counting the found Keys.
    The counted number must match the inserted number.
    *)
    procedure TestIterateForward;
    (*
    Test Adding 20 Keys and their Values with the Default Property Method (which should grow the List more than twice)
    And Iterating over the Keys from the End and Counting the found Keys.
    The counted number must match the inserted number.
    *)
    procedure TestIterateBackward;
    procedure TestResetOnAdd;
    (*
    Test Adding 20 Keys and their Values with the Default Property Method (which should grow the List more than twice)
    And Iterating over the Keys from the Beginning and Removing the matching Keys.
    The number of removed Keys must match the requested number of Keys
    and the number of remaining Keys must match the difference between the inserted Keys and the removed Keys.
    *)
    procedure TestRemoveForward;
    (*
    Test Adding 20 Keys and their Values with the Default Property Method (which should grow the List more than twice)
    And Iterating over the Keys from the End and Removing the matching Keys.
    The number of removed Keys must match the requested number of Keys
    and the number of remaining Keys must match the difference between the inserted Keys and the removed Keys.
    *)
    procedure TestRemoveBackward;
  end;

procedure RegisterTests;



implementation

uses
  Classes, SysUtils;



{ here we register all our test classes }
procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestsHashListIterator.Suite);
end;



(*==========================================================================*)
(* Class TTestsHashListIterator Implementation *)



//----------------------------------------------------------------------------
//Initialization Methods


{
  New Hash Table List is created
}
procedure TTestsHashListIterator.SetUp;
begin
  Self.lsthshobjs := TPLPointerHashList.Create();
end;

{
  Free used Memory
}
procedure TTestsHashListIterator.Teardown;
var
  psvl: PAnsiString;
begin
  if self.lsthshobjs.moveFirst then
  begin
    repeat  //until not self.lsthshobjs.moveNext();
      psvl := self.lsthshobjs.getCurrentValue();

      if psvl <> nil then Dispose(psvl);
    until not self.lsthshobjs.moveNext();
  end;  //if self.lsthshobjs.moveFirst then

  self.lsthshobjs.Free;
end;



//----------------------------------------------------------------------------
//Test Methods


procedure TTestsHashListIterator.TestIterateForward;
var
  itr: TPLPtrHashListIterator;
  sky: String;
  psvl: PAnsiString;
  iky, ikycnt, ikymxcnt, ikyttlcnt: Integer;
begin
  WriteLn('TTestsHashListIterator.TestIterateForward: go ...');

  ikymxcnt := 20;

  Self.lsthshobjs.GrowFactor := 2;
  Self.lsthshobjs.LoadFactor := 3;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    Self.lsthshobjs[sky] := psvl;
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  WriteLn('TTestsHashListIterator.TestIterateForward: cap: '#39, Self.lsthshobjs.Capacity, #39);

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  itr := Self.lsthshobjs.Iterator;

  Check(itr <> Nil, 'TPLPointerHashList.Iterator : failed!');

  CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key First : failed! '
    + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

  psvl := itr.Value;

  if psvl <> Nil then
  begin
    WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
    CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value First : failed! '
      + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
  end
  else
    Check(psvl <> Nil, 'TPLPtrHashListIterator.Value First : failed! '
      + 'Returned Value: '#39'NIL'#39);

  iky := 1;
  ikycnt := 1;

  while itr.Next do
  begin
    inc(iky);
    inc(ikycnt);

    CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

    psvl := itr.Value;

    if psvl <> Nil then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
      CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
    end
    else
      Check(psvl <> Nil, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

  end;  //while itr.Next do

  CheckEquals(ikycnt, ikyttlcnt, 'MV - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt) + #39);
end;

procedure TTestsHashListIterator.TestIterateBackward;
var
  itr: TPLPtrHashListIterator;
  sky: String;
  psvl: PAnsiString;
  iky, ikycnt, ikymxcnt, ikyttlcnt: Integer;
begin
  WriteLn('TTestsHashListIterator.TestIterateBackward: go ...');

  ikymxcnt := 20;

  Self.lsthshobjs.GrowFactor := 2;
  Self.lsthshobjs.LoadFactor := 3;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    Self.lsthshobjs[sky] := psvl;
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  WriteLn('TTestsHashListIterator.TestIterateBackward: cap: '#39, Self.lsthshobjs.Capacity, #39);

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  itr := Self.lsthshobjs.GetIterator(itpLast);

  Check(itr <> Nil, 'TPLPointerHashList.GetIterator() Last : failed!');

  CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key Last : failed! '
    + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

  psvl := itr.Value;

  if psvl <> Nil then
  begin
    WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
    CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value Last : failed! '
      + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
  end
  else
    Check(psvl <> Nil, 'TPLPtrHashListIterator.Value Last : failed! '
      + 'Returned Value: '#39'NIL'#39);

  iky := 1;
  ikycnt := 1;

  while itr.Previous do
  begin
    inc(iky);
    inc(ikycnt);

    CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

    psvl := itr.Value;

    if psvl <> Nil then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
      CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
    end
    else
      Check(psvl <> Nil, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

  end;  //while itr.Previous do

  CheckEquals(ikycnt, ikyttlcnt, 'MV - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt) + #39);
end;

procedure TTestsHashListIterator.TestResetOnAdd;
var
  itr: TPLPtrHashListIterator;
  psvl: PAnsiString;
  sky, sfstky: String;
  iky, ikymxcnt, ikyttlcnt: Integer;
begin
  WriteLn('TTestsHashListIterator.TestResetOnAdd: go ...');

  ikymxcnt := 10;

  Self.lsthshobjs.LoadFactor := 2;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    Self.lsthshobjs[sky] := psvl;
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  WriteLn('TTestsHashListIterator.TestResetOnAdd: cap: '#39, Self.lsthshobjs.Capacity, #39);

  itr := Self.lsthshobjs.Iterator;

  Check(itr <> Nil, 'TPLPointerHashList.Iterator : failed!');

  sfstky := itr.Key;

  CheckNotEquals('', sfstky, 'TPLPtrHashListIterator.Key First No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + sfstky + chr(39));

  psvl := itr.Value;

  if psvl <> Nil then
  begin
    WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
    CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value First No. 1 : failed! '
      + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
  end
  else
    Check(psvl <> Nil, 'TPLPtrHashListIterator.Value First No. 1 : failed! '
      + 'Returned Value: '#39'NIL'#39);

  for iky := 1 to 3 do
  begin
    Check(itr.Next <> False, 'TPLPtrHashListIterator.Next() No. ' + IntToStr(iky) + '  : failed! '
      + 'Returned Value: '#39'FALSE'#39);
  end;  //for iky := 1 to 3 do

  sky := 'key' + IntToStr(ikymxcnt + 1);

  New(psvl);
  psvl^ := 'value' + IntToStr(ikymxcnt + 1);

  Self.lsthshobjs[sky] := psvl;

  WriteLn('TTestsHashListIterator.TestResetOnAdd: cap: '#39, Self.lsthshobjs.Capacity, #39);

  CheckEquals(sfstky, itr.Key, 'INS - TPLPtrHashListIterator.Reset() failed! TPLPtrHashListIterator.Key is: '#39
     + itr.Key + ' / ' + sfstky + #39);
end;

procedure TTestsHashListIterator.TestRemoveForward;
var
  itr: TPLPtrHashListIterator;
  sky, srmky1, srmky2, srmky3, srmky4, srmky5: String;
  psvl: PAnsiString;
  iky, ikycnt, ikymxcnt, ikyttlcnt: Integer;
  irmcnt, irmttlcnt: Integer;
begin
  WriteLn('TTestsHashListIterator.TestRemoveForward: go ...');

  srmky1 := 'key2';
  srmky2 := 'key3';
  srmky3 := 'key8';
  srmky4 := 'key9';
  srmky5 := 'key19';
  ikymxcnt := 20;
  irmttlcnt := 5;

  Self.lsthshobjs.GrowFactor := 2;
  Self.lsthshobjs.LoadFactor := 3;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    Self.lsthshobjs[sky] := psvl;
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  itr := Self.lsthshobjs.Iterator;
  iky := 0;
  ikycnt := 0;
  irmcnt := 0;

  Check(itr <> Nil, 'TPLPointerHashList.Iterator : failed!');

  repeat  //until not itr.Next;
    inc(iky);
    inc(ikycnt);

    CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

    psvl := itr.Value;

    if psvl <> Nil then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
      CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
    end
    else
      Check(psvl <> Nil, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

    if (itr.Key = srmky1)
      or (itr.Key = srmky2)
      or (itr.Key = srmky3)
      or (itr.Key = srmky4)
      or (itr.Key = srmky5) then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; key removing ...');
      //Free the Value first
      Dispose(psvl);
      //Remove the selected Keys and move on
      Self.lsthshobjs.removeKey(itr.Key);
      inc(irmcnt);
    end;

  until not itr.Next;

  CheckEquals(ikycnt, ikyttlcnt, 'MV1 - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt) + #39);
  CheckEquals(irmcnt, irmttlcnt, 'RM - Count failed! Count is: '#39
     + IntToStr(irmcnt) + ' / ' + IntToStr(irmttlcnt) + #39);

  WriteLn('TTestsHashListIterator.TestRemoveForward: cap: '#39, Self.lsthshobjs.Capacity, #39);

  Check(itr.First = True, 'TPLPointerHashList.Iterator First : failed!');

  iky := 0;
  ikycnt := 0;

  repeat  //until not itr.Next;
    inc(iky);
    inc(ikycnt);

    CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

    psvl := itr.Value;

    if psvl <> Nil then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
      CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
    end
    else
      Check(psvl <> Nil, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

    if (itr.Key = srmky1)
      or (itr.Key = srmky2)
      or (itr.Key = srmky3)
      or (itr.Key = srmky4)
      or (itr.Key = srmky5) then
    begin
      Check(False, 'TPLPointerHashList.removeKey() failed! '
        + 'Key: '#39 + itr.Key + #39' was not removed.');
    end;

  until not itr.Next;

  CheckEquals(ikycnt, ikyttlcnt - irmttlcnt, 'MV2 - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt - irmttlcnt) + #39);

end;

procedure TTestsHashListIterator.TestRemoveBackward;
var
  itr: TPLPtrHashListIterator;
  sky, srmky1, srmky2, srmky3, srmky4, srmky5: String;
  psvl: PAnsiString;
  iky, ikycnt, ikymxcnt, ikyttlcnt: Integer;
  irmcnt, irmttlcnt: Integer;
begin
  WriteLn('TTestsHashListIterator.TestRemoveBackward: go ...');

  srmky1 := 'key2';
  srmky2 := 'key3';
  srmky3 := 'key8';
  srmky4 := 'key9';
  srmky5 := 'key19';
  ikymxcnt := 20;
  irmttlcnt := 5;

  Self.lsthshobjs.GrowFactor := 2;
  Self.lsthshobjs.LoadFactor := 3;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    Self.lsthshobjs[sky] := psvl;
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  itr := Self.lsthshobjs.GetIterator(itpLast);
  iky := 0;
  ikycnt := 0;
  irmcnt := 0;

  Check(itr <> Nil, 'TPLPointerHashList.GetIterator Last : failed!');

  repeat  //until not itr.Previous;
    inc(iky);
    inc(ikycnt);

    CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

    psvl := itr.Value;

    if psvl <> Nil then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
      CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
    end
    else
      Check(psvl <> Nil, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

    if (itr.Key = srmky1)
      or (itr.Key = srmky2)
      or (itr.Key = srmky3)
      or (itr.Key = srmky4)
      or (itr.Key = srmky5) then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; key removing ...');
      //Free the Value first
      Dispose(psvl);
      //Remove the selected Keys and move on
      Self.lsthshobjs.removeKey(itr.Key);
      inc(irmcnt);
    end;

  until not itr.Previous;

  CheckEquals(ikycnt, ikyttlcnt, 'MV1 - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt) + #39);
  CheckEquals(irmcnt, irmttlcnt, 'RM - Count failed! Count is: '#39
     + IntToStr(irmcnt) + ' / ' + IntToStr(irmttlcnt) + #39);

  WriteLn('TTestsHashListIterator.TestRemoveBackward: cap: '#39, Self.lsthshobjs.Capacity, #39);

  Check(itr.Last = True, 'TPLPointerHashList.Iterator Last : failed!');

  iky := 0;
  ikycnt := 0;

  repeat  //until not itr.Next;
    inc(iky);
    inc(ikycnt);

    CheckNotEquals('', itr.Key, 'TPLPtrHashListIterator.Key No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + itr.Key + chr(39));

    psvl := itr.Value;

    if psvl <> Nil then
    begin
      WriteLn('key: '#39 + itr.Key + #39'; value: '#39 + psvl^ + #39);
      CheckNotEquals('', psvl^, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + psvl^ + chr(39));
    end
    else
      Check(psvl <> Nil, 'TPLPtrHashListIterator.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

    if (itr.Key = srmky1)
      or (itr.Key = srmky2)
      or (itr.Key = srmky3)
      or (itr.Key = srmky4)
      or (itr.Key = srmky5) then
    begin
      Check(False, 'TPLPointerHashList.removeKey() failed! '
        + 'Key: '#39 + itr.Key + #39' was not removed.');
    end;

  until not itr.Previous;

  CheckEquals(ikycnt, ikyttlcnt - irmttlcnt, 'MV2 - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt - irmttlcnt) + #39);

end;

end.

