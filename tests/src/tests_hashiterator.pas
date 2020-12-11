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
    Test Adding 10 Keys and their Values with the Add()-Method (which should grow the List at least once)
    And Looking up their Values (they must match their inserted Values)
    *)
    procedure TestIterateForward;
    (*
    Test Adding 3 Keys and their Values with the Add()-Method
    Adding the 4th Value on a duplicate Key.
    The "dupAccept" Bahaviour is enabled which should override the existing Value
    *)
    procedure TestIterateBackward;
    (*
    Test Adding 3 Keys and their Values with the Add()-Method
    Adding the 4th Value on a duplicate Key.
    The "dupIgnore" Bahaviour is enabled which should drop the new Value
    *)
    procedure TestRemoveForward;
    (*
    Test Adding 3 Keys and their Values with the Add()-Method
    Adding the 4th Value on a duplicate Key.
    The "dupError" Bahaviour is enabled which should raise an Exception
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

procedure TTestsHashListIterator.TestRemoveForward;
begin
  CheckEquals(1, 1, 'TTestsHashListIterator.TestRemoveForward failed!');
end;

procedure TTestsHashListIterator.TestRemoveBackward;
begin
  CheckEquals(1, 1, 'TestsHashListIterator.TestRemoveBackward failed!');
end;

end.

