unit tests_objecthash;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, objecthash;

type
  //==========================================================================
  // Class TStringObject Declaration


  TStringObject = class
  protected
    skey: String;
    svalue: String;
  public
    constructor Create; overload;
    constructor Create(const skeyname: String); overload;
    constructor Create(const skeyname: String; const skeyvalue: String); overload;
    property Key: String read skey write skey;
    property Value: String read svalue write svalue;
  end;



  //==========================================================================
  // Class TTestsObjectHashList Declaration


  TTestsObjectHashList = class(TTestCase)
  protected
    lsthshobjs: TPLObjectHashList;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    (*
    Test Adding 10 Keys and their Values with the setValue()-Method (which should grow the List at least once)
    The first 5 Elements will be inserted and looked up with the Default Property
    And Looking up their Values (they must match their inserted Values)
    *)
    procedure TestInsertCheckElements;
    (*
    Test Adding 3 Keys and their Values for a valid List Test
    And Iterating to the First Key (this is not necessary the first inserted key)
    It should return the defined Key and its Value
    *)
    procedure TestCheckFirstElement;
    (*
    Test Adding 3 Keys and their Values for a valid List Test
    Using the Default Property
    And Iterating to the First Key and to the Next Key (they are not necessary
    the first and second inserted keys)
    It should return the defined Keys and their Values
    *)
    procedure TestCheckNextElement;
    (*
    Test Adding 10 Keys and their Values using the Default Property
    And Iterating from the Beginning and Removing 4 Keys
    The number of counted Keys must match the number of inserted Keys
    The number of removed Keys must match the requested number of Keys
    and the number of remaining Keys must match the difference between the inserted Keys and the removed Keys.
    Memory does not need to be freed but must not leak either.
    *)
    procedure TestRemoveCountElements;
    (*
    Clear Function Test. Inserting 10 Elements and Calling the Clear Method.
    This Test will grow the List once.
    The List must be empty and the Capacity must be reset to the minimum.
    The Memory must be freed correctly. (Values will be freed automatically)
    *)
    procedure TestInsertClear;
  end;

 procedure RegisterTests;


implementation
 
uses
  Classes, SysUtils;


{ here we register all our test classes }
procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestsObjectHashList.Suite);
end;



//==========================================================================
// Class TStringObject Implementation


constructor TStringObject.Create;
begin
  Self.skey := '';
  Self.svalue := '';
end;

constructor TStringObject.Create(const skeyname: String);
begin
  Self.skey := skeyname;
  Self.svalue := '';
end;

constructor TStringObject.Create(const skeyname: String; const skeyvalue: String);
begin
  Self.skey := skeyname;
  Self.svalue := skeyvalue;
end;



//==========================================================================
// Class TTestsObjectHashList Implementation


procedure TTestsObjectHashList.SetUp;
begin
  Self.lsthshobjs := TPLObjectHashList.Create;

  Self.lsthshobjs.Owned := True;
end;

procedure TTestsObjectHashList.TearDown;
begin
  if Self.lsthshobjs <> Nil then Self.lsthshobjs.Free;

end;

procedure TTestsObjectHashList.TestInsertCheckElements;
var
  elm: TStringObject;
begin
  //WriteLn('TTestsObjectHashList.TestInsertCheckElements: go ...');

  elm := TStringObject.Create('key1', 'value1');
  Self.lsthshobjs['key1'] := elm;

  elm := TStringObject.Create('key2', 'value2');
  Self.lsthshobjs['key2'] := elm;

  elm := TStringObject.Create('key3', 'value3');
  Self.lsthshobjs['key3'] := elm;

  elm := TStringObject.Create('key4', 'value4');
  Self.lsthshobjs['key4'] := elm;

  elm := TStringObject.Create('key5', 'value5');
  Self.lsthshobjs['key5'] := elm;

  elm := TStringObject.Create('key6', 'value6');
  Self.lsthshobjs.setValue('key6', elm);

  elm := TStringObject.Create('key7', 'value7');
  Self.lsthshobjs.setValue('key7', elm);

  elm := TStringObject.Create('key8', 'value8');
  self.lsthshobjs.setValue('key8', elm);

  elm := TStringObject.Create('key9', 'value9');
  self.lsthshobjs.setValue('key9', elm);

  elm := TStringObject.Create('key10', 'value10');
  self.lsthshobjs.setValue('key10', elm);

  elm := TStringObject(Self.lsthshobjs['key1']);

  CheckEquals('value1', elm.Value, 'LKP - key '  + chr(39) + 'key1' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs['key2']);

  CheckEquals('value2', elm.Value, 'LKP - key '  + chr(39) + 'key2' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs['key3']);

  CheckEquals('value3', elm.Value, 'LKP - key '  + chr(39) + 'key3' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs['key4']);

  CheckEquals('value4', elm.Value, 'LKP - key '  + chr(39) + 'key4' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs['key5']);

  CheckEquals('value5', elm.Value, 'LKP - key '  + chr(39) + 'key5' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs.getValue('key6'));

  CheckEquals('value6', elm.Value, 'LKP - key '  + chr(39) + 'key6' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs.getValue('key7'));

  CheckEquals('value7', elm.Value, 'LKP - key '  + chr(39) + 'key7' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs.getValue('key8'));

  CheckEquals('value8', elm.Value, 'LKP - key '  + chr(39) + 'key8' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs.getValue('key9'));

  CheckEquals('value9', elm.Value, 'LKP - key '  + chr(39) + 'key9' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

  elm := TStringObject(Self.lsthshobjs.getValue('key10'));

  CheckEquals('value10', elm.Value, 'LKP - key '  + chr(39) + 'key10' + chr(39)
    + ' failed! Value is: ' + chr(39) + elm.Value + chr(39));

end;

procedure TTestsObjectHashList.TestCheckFirstElement;
var
  elm: TStringObject;
  sky: String;
begin
  elm := TStringObject.Create('first_key', 'first_value');
  Self.lsthshobjs.setValue('first_key', elm);

  elm := TStringObject.Create('next_key', 'next_value');
  self.lsthshobjs.setValue('next_key', elm);

  elm := TStringObject.Create('last_key', 'last_value');
  self.lsthshobjs.setValue('last_key', elm);

  Check(self.lsthshobjs.moveFirst() = True, 'TPLObjectHashList.moveFirst(): failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('first_key', sky, 'TPLObjectHashList.getCurrentKey(): failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  elm := TStringObject(self.lsthshobjs.getCurrentValue());

  CheckEquals('first_value', elm.Value, 'TPLObjectHashList.getCurrentValue(): failed! '
    + 'Returned Key: ' + chr(39) + elm.Value + chr(39));

end;

procedure TTestsObjectHashList.TestCheckNextElement;
var
  elm: TStringObject;
  sky: String;
begin
  Self.lsthshobjs['first_key'] := TStringObject.Create('first_key', 'first_value');
  Self.lsthshobjs['next_key'] := TStringObject.Create('next_key', 'next_value');
  Self.lsthshobjs['last_key'] := TStringObject.Create('last_key', 'last_value');

  Check(self.lsthshobjs.moveNext() = True, 'TPLObjectHashList.moveNext() No. 1 : failed!');

  sky := Self.lsthshobjs.getCurrentKey();

  CheckEquals('first_key', sky, 'TPLObjectHashList.getCurrentKey() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  elm := TStringObject(Self.lsthshobjs.getCurrentValue());

  CheckEquals('first_value', elm.Value, 'TPLObjectHashList.getCurrentValue() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + elm.Value + chr(39));

  Check(self.lsthshobjs.moveNext() = True, 'TPLObjectHashList.moveNext() No. 2 : failed!');

  sky := Self.lsthshobjs.getCurrentKey();

  CheckEquals('next_key', sky, 'TPLObjectHashList.getCurrentKey() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  elm := TStringObject(Self.lsthshobjs.getCurrentValue());

  CheckEquals('next_value', elm.Value, 'TPLObjectHashList.getCurrentValue() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + elm.Value + chr(39));

  Check(self.lsthshobjs.moveNext() = True, 'TPLObjectHashList.moveNext() No. 3 : failed!');

  sky := Self.lsthshobjs.getCurrentKey();

  CheckEquals('last_key', sky, 'TPLObjectHashList.getCurrentKey() No. 3 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  elm := TStringObject(Self.lsthshobjs.getCurrentValue());

  CheckEquals('last_value', elm.Value, 'TPLObjectHashList.getCurrentValue() No. 3 : failed! '
    + 'Returned Key: ' + chr(39) + elm.Value + chr(39));
end;

procedure TTestsObjectHashList.TestRemoveCountElements;
var
  elm: TStringObject;
  sky, svl, srmky1, srmky2, srmky3, srmky4: String;
  iky, ikycnt, ikymxcnt, ikyttlcnt: Integer;
  irmcnt, irmttlcnt: Integer;
begin
  //WriteLn('TTestsObjectHashList.TestRemoveCountElements: go ...');

  srmky1 := 'key2';
  srmky2 := 'key3';
  srmky3 := 'key8';
  srmky4 := 'key9';
  ikymxcnt := 10;
  irmttlcnt := 4;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);
    svl := 'value' + IntToStr(iky);
    Self.lsthshobjs[sky] := TStringObject.Create(sky, svl);
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  //WriteLn('TTestsObjectHashList.TestRemoveCountElements: cap: '#39, Self.lsthshobjs.Capacity, #39);

  Check(Self.lsthshobjs.moveFirst() = True, 'TPLObjectHashList.moveFirst() No. 1 : failed!');

  iky := 0;
  ikycnt := 0;
  irmcnt := 0;

  repeat  //until not Self.lsthshobjs.moveNext();
    inc(iky);
    inc(ikycnt);

    sky := Self.lsthshobjs.getCurrentKey();
    elm := TStringObject(Self.lsthshobjs.getCurrentValue());

    if elm <> Nil then
    begin
      //WriteLn('key: '#39 + elm.Key + #39'; value: '#39 + elm.Value + #39);
      CheckNotEquals('', elm.Value, 'TStringObject.Value No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + elm.Value + chr(39));
    end
    else
      Check(elm <> Nil, 'TPLObjectHashList.getCurrentValue() No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

    if (sky = srmky1)
      or (sky = srmky2)
      or (sky = srmky3)
      or (sky = srmky4) then
    begin
      //WriteLn('key: '#39 + sky + #39'; key removing ...');
      //Remove the selected Keys and move on
      Self.lsthshobjs.removeKey(sky);
      inc(irmcnt);
    end;
  until not Self.lsthshobjs.moveNext();

  CheckEquals(ikycnt, ikyttlcnt, 'MV1 - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt) + #39);
  CheckEquals(irmcnt, irmttlcnt, 'RM - Count failed! Count is: '#39
     + IntToStr(irmcnt) + ' / ' + IntToStr(irmttlcnt) + #39);

  //WriteLn('TTestsObjectHashList.TestRemoveCountElements: cap: '#39, Self.lsthshobjs.Capacity, #39);

  Check(Self.lsthshobjs.moveFirst() = True, 'TPLObjectHashList.moveFirst() No. 2 : failed!');

  iky := 0;
  ikycnt := 0;

  repeat  //until not Self.lsthshobjs.moveNext();
    inc(iky);
    inc(ikycnt);

    sky := Self.lsthshobjs.getCurrentKey();
    elm := TStringObject(Self.lsthshobjs.getCurrentValue());

    CheckNotEquals('', sky, 'TPLObjectHashList.getCurrentKey() No. ' + IntToStr(iky) + ' : failed! '
      + 'Returned Key: ' + chr(39) + sky + chr(39));

    if elm <> Nil then
    begin
      //WriteLn('key: '#39 + elm.Key + #39'; value: '#39 + elm.Value + #39);
      CheckNotEquals('', elm.Value, 'TPLObjectHashList.getCurrentValue() No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: ' + chr(39) + elm.Value + chr(39));
    end
    else
      Check(elm <> Nil, 'TPLObjectHashList.getCurrentValue() No. ' + IntToStr(iky) + ' : failed! '
        + 'Returned Value: '#39'NIL'#39);

    if (sky = srmky1)
      or (sky = srmky2)
      or (sky = srmky3)
      or (sky = srmky4) then
    begin
      Check(False, 'TPLPointerHashList.removeKey() failed! '
        + 'Key: '#39 + sky + #39' was not removed.');
    end;

  until not Self.lsthshobjs.moveNext();

  CheckEquals(ikycnt, ikyttlcnt - irmttlcnt, 'MV2 - Count failed! Count is: '#39
     + IntToStr(ikycnt) + ' / ' + IntToStr(ikyttlcnt - irmttlcnt) + #39);

end;

procedure TTestsObjectHashList.TestInsertClear;
var
  sky, svl: String;
  iky, imincap, ikymxcnt, ikyttlcnt: Integer;
begin
  //WriteLn('TTestsObjectHashList.TestInsertClear: go ...');

  ikymxcnt := 10;

  Self.lsthshobjs.LoadFactor := 2;

  //WriteLn('TTestsObjectHashList.TestInsertClear: cap 0: '#39, Self.lsthshobjs.Capacity, #39);

  imincap := Self.lsthshobjs.GrowFactor * Self.lsthshobjs.LoadFactor;

  for iky := 1 to ikymxcnt do
  begin
    sky := 'key' + IntToStr(iky);
    svl := 'value' + IntToStr(iky);
    Self.lsthshobjs[sky] := TStringObject.Create(sky, svl);
  end;  //for iky := 1 to ikymxcnt do

  ikyttlcnt := Self.lsthshobjs.Count;

  CheckEquals(ikymxcnt, ikyttlcnt, 'INS - Count failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / ' + IntToStr(ikymxcnt) + #39);

  //WriteLn('TTestsObjectHashList.TestInsertClear: cap 1: '#39, Self.lsthshobjs.Capacity, #39);

  Self.lsthshobjs.Clear();

  ikyttlcnt := Self.lsthshobjs.Count;

  CheckEquals(0, ikyttlcnt, 'DEL - TPLObjectHashList.Clear() failed! Count is: '#39
     + IntToStr(ikyttlcnt) + ' / 0'#39);
  CheckEquals(imincap, Self.lsthshobjs.Capacity, 'DEL - TPLObjectHashList.Clear() failed! Capacity is: '#39
     + IntToStr(Self.lsthshobjs.Capacity) + ' / ' + IntToStr(imincap) + #39);

  //WriteLn('TTestsObjectHashList.TestInsertClear: cap 2: '#39, Self.lsthshobjs.Capacity, #39);

end;


end.

