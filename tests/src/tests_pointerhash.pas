unit tests_pointerhash;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, Generics.Collections, pointerhash;

type
  TPMap = Specialize TFastHashMap<String, Pointer>;
  TTestsPointerHashList = class(TTestCase)
  protected
    mpobjs: TPMap;
    lsthshobjs: TPLPointerHashList;
    procedure SetUp; override;
    procedure Teardown; override;
  published
    procedure TestInsertCheckElements;
    procedure TestCheckFirstElement;
    procedure TestCheckNextElement;
    procedure TestMapInsertCheck1000Elements;
    procedure TestInsertCheck1000Elements;
    procedure TestMapInsertCheck5000Elements;
    procedure TestInsertCheck5000Elements;
  end;

 procedure RegisterTests;






implementation

uses
  SysUtils, DateUtils;


{ here we register all our test classes }
procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestsPointerHashList.Suite);
end;


procedure TTestsPointerHashList.SetUp;
begin
  self.mpobjs:= TPMap.Create;
  self.lsthshobjs := TPLPointerHashList.Create();
end;

procedure TTestsPointerHashList.Teardown;
var
  sky: String;
  psvl: PAnsiString;
begin
  for sky in self.mpobjs.Keys do
  begin
    psvl := self.mpobjs[sky];

    if psvl <> nil then
      Dispose(psvl);

  end;  //for sky in self.mpobjs.Keys do

  self.mpobjs.Free;


  if self.lsthshobjs.moveFirst then
  begin
    repeat  //until not self.lsthshobjs.moveNext();
      psvl := self.lsthshobjs.getCurrentValue();

      if psvl <> nil then Dispose(psvl);
    until not self.lsthshobjs.moveNext();
  end;  //if self.lsthshobjs.moveFirst then

  self.lsthshobjs.Free;
end;

(*
Test Adding 10 Keys and their Values (which should grow the List at least once)
And Looking up their Values (they must match their inserted Values)
*)
procedure TTestsPointerHashList.TestInsertCheckElements;
var
  psvl: PAnsiString;
begin
  New(psvl);
  psvl^ := 'value1';

  self.lsthshobjs.setValue('key1', psvl);

  New(psvl);
  psvl^ := 'value2';

  self.lsthshobjs.setValue('key2', psvl);

  New(psvl);
  psvl^ := 'value3';

  self.lsthshobjs.setValue('key3', psvl);

  New(psvl);
  psvl^ := 'value4';

  self.lsthshobjs.setValue('key4', psvl);

  New(psvl);
  psvl^ := 'value5';

  self.lsthshobjs.setValue('key5', psvl);

  New(psvl);
  psvl^ := 'value6';

  self.lsthshobjs.setValue('key6', psvl);

  New(psvl);
  psvl^ := 'value7';

  self.lsthshobjs.setValue('key7', psvl);

  New(psvl);
  psvl^ := 'value8';

  self.lsthshobjs.setValue('key8', psvl);

  New(psvl);
  psvl^ := 'value9';

  self.lsthshobjs.setValue('key9', psvl);

  New(psvl);
  psvl^ := 'value10';

  self.lsthshobjs.setValue('key10', psvl);

  psvl := self.lsthshobjs.getValue('key1');

  CheckEquals('value1', psvl^, 'key '  + chr(39) + 'key1' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key2');

  CheckEquals('value2', psvl^, 'key '  + chr(39) + 'key2' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key3');

  CheckEquals('value3', psvl^, 'key '  + chr(39) + 'key3' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key4');

  CheckEquals('value4', psvl^, 'key '  + chr(39) + 'key4' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key5');

  CheckEquals('value5', psvl^, 'key '  + chr(39) + 'key5' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key6');

  CheckEquals('value6', psvl^, 'key '  + chr(39) + 'key6' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key7');

  CheckEquals('value7', psvl^, 'key '  + chr(39) + 'key7' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key8');

  CheckEquals('value8', psvl^, 'key '  + chr(39) + 'key8' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key9');

  CheckEquals('value9', psvl^, 'key '  + chr(39) + 'key9' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

  psvl := self.lsthshobjs.getValue('key10');

  CheckEquals('value10', psvl^, 'key '  + chr(39) + 'key10' + chr(39)
    + ': value lookup failed! It is: ' + chr(39) + psvl^ + chr(39));

end;

(*
Test Adding 3 Keys and their Values for a valid List Test
And Iterating to the First Key (this is not necessary the first inserted key)
It should return the defined Key and its Value
*)
procedure TTestsPointerHashList.TestCheckFirstElement;
var
  sky: String;
  psvl: PAnsiString;
begin
  New(psvl);
  psvl^ := 'first_value';

  self.lsthshobjs.setValue('first_key', psvl);

  New(psvl);
  psvl^ := 'next_value';

  self.lsthshobjs.setValue('next_key', psvl);

  New(psvl);
  psvl^ := 'last_value';

  self.lsthshobjs.setValue('last_key', psvl);

  Check(self.lsthshobjs.moveFirst() = True, 'TPLPointerHashList.moveFirst(): failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('first_key', sky, 'TPLPointerHashList.getCurrentKey(): failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := self.lsthshobjs.getCurrentValue();

  CheckEquals('first_value', psvl^, 'TPLPointerHashList.getCurrentValue(): failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

end;

(*
Test Adding 3 Keys and their Values for a valid List Test
And Iterating to the First Key and to the Next Key (they are not necessary
the first and second inserted keys)
It should return the defined Keys and their Values
*)
procedure TTestsPointerHashList.TestCheckNextElement;
var
  sky: String;
  psvl: PAnsiString;
begin
  New(psvl);
  psvl^ := 'first_value';

  self.lsthshobjs.setValue('first_key', psvl);

  New(psvl);
  psvl^ := 'next_value';

  self.lsthshobjs.setValue('next_key', psvl);

  New(psvl);
  psvl^ := 'last_value';

  self.lsthshobjs.setValue('last_key', psvl);

  Check(self.lsthshobjs.moveNext() = True, 'TPLPointerHashList.moveNext() No. 1 : failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('first_key', sky, 'TPLPointerHashList.getCurrentKey() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := self.lsthshobjs.getCurrentValue();

  CheckEquals('first_value', psvl^, 'TPLPointerHashList.getCurrentValue() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

  Check(self.lsthshobjs.moveNext() = True, 'TPLPointerHashList.moveNext() No. 2 : failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('next_key', sky, 'TPLPointerHashList.getCurrentKey() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := self.lsthshobjs.getCurrentValue();

  CheckEquals('next_value', psvl^, 'TPLPointerHashList.getCurrentValue() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

end;

(*
Reference Performance Test against the Standard TFPGMap
Adding 1000 Keys and their Values
Looking Up all their Values
It should return the defined Keys and their Values
*)
procedure TTestsPointerHashList.TestMapInsertCheck1000Elements;
var
  sky, schk: String;
  psvl: PAnsiString;
  tmstrt, tmend : TDateTime;
  tmins: Double;
  DT : TDateTime;
  TS: TTimeStamp;
  MS : Comp;
  ilkplmt, iinslmt: Integer;
  iky: Integer;
begin
  WriteLn('TestMapInsertCheck1000Elements: do ...');

  TS:=DateTimeToTimeStamp(Now);
  Writeln ('INS - Start - Now in days since 1/1/0001      : ',TS.Date);
  Writeln ('INS - Start - Now in millisecs since midnight : ',TS.Time);
  MS:=TimeStampToMSecs(TS);
  Writeln ('Now in millisecs since 1/1/0001 : ',MS);
  MS:=MS-1000*3600*2;
  TS:=MSecsToTimeStamp(MS);
  DT:=TimeStampToDateTime(TS);
  Writeln ('Now minus 1 day : ',DateTimeToStr(DT));


  iinslmt := 2;

  tmstrt := Now;

  for iky := 1 to 1000 do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    self.mpobjs.Add(sky, psvl);
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('INS - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('INS Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < iinslmt, 'INS Operation: Operation slower than '
    + chr(39) + IntToStr(iinslmt) + chr(39) +' ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');


  ilkplmt := 2;

  tmstrt := Now;

  self.mpobjs.Capacity := 1000;

  for iky := 1 to 1000 do
  begin
    sky := 'key' + IntToStr(iky);
    schk := 'value' + IntToStr(iky);

    psvl := self.mpobjs[sky];

    if psvl <> nil then
    begin
      if psvl^ <> schk then
        CheckEquals(schk, psvl^, 'LKP - key '  + chr(39) + sky + chr(39)
          + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

    end
    else  //Value is nil
    begin
      Check(psvl <> nil, 'LKP - key '  + chr(39) + sky + chr(39)
        + ' failed! Value is: ' + chr(39) + 'nil' + chr(39));
    end;  //if psvl <> nil then
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('LKP - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('LKP Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < ilkplmt, 'LKP Operation: Operation slower than '
    + chr(39) + IntToStr(ilkplmt) + chr(39) + ' ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');

end;

procedure TTestsPointerHashList.TestInsertCheck1000Elements;
var
  sky, schk: String;
  psvl: PAnsiString;
  tmstrt, tmend : TDateTime;
  tmins: Double;
  TS: TTimeStamp;
  iky: Integer;
begin
  WriteLn('TestInsertCheck1000Elements: do ...');

  TS:=DateTimeToTimeStamp(Now);
  Writeln ('INS - Start - Now in millisecs since midnight : ',TS.Time);


  tmstrt := Now;

  self.lsthshobjs.LoadFactor := 2;
  self.lsthshobjs.Limit := 1000;

  for iky := 1 to 1000 do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    self.lsthshobjs.setValue(sky, psvl);

  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('INS - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('INS Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < 2, 'INS Operation: Operation slower than 2 ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');


  tmstrt := Now;

  for iky := 1 to 1000 do
  begin
    sky := 'key' + IntToStr(iky);
    schk := 'value' + IntToStr(iky);

    psvl := self.lsthshobjs.getValue(sky);

    if psvl <> nil then
    begin
      if psvl^ <> schk then
        CheckEquals(schk, psvl^, 'LKP - key '  + chr(39) + sky + chr(39)
          + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

    end
    else  //Key Lookup failed
    begin
      Check(psvl <> nil, 'LKP - key '  + chr(39) + sky + chr(39)
        + ' failed! Value is: ' + chr(39) + 'nil' + chr(39));
    end; //if psvl <> nil then
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('LKP - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('LKP Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < 2, 'LKP Operation: Operation slower than 2 ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');

end;


(*
Reference Performance Test against the Standard TFPGMap
Adding 5000 Keys and their Values
Looking Up all their Values
It should return the defined Keys and their Values
*)
procedure TTestsPointerHashList.TestMapInsertCheck5000Elements;
var
  sky, schk: String;
  psvl: PAnsiString;
  tmstrt, tmend : TDateTime;
  tmins: Double;
  TS: TTimeStamp;
  iky: Integer;
begin
  WriteLn('TestMapInsertCheck5000Elements: do ...');

  TS:=DateTimeToTimeStamp(Now);
  Writeln ('INS - Start - Now in millisecs since midnight : ',TS.Time);


  tmstrt := Now;

  self.mpobjs.Capacity := 5000;

  for iky := 1 to 5000 do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    self.mpobjs.Add(sky, psvl);
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('INS - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('INS Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < 7, 'INS Operation: Operation slower than 7 ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');


  tmstrt := Now;

  for iky := 1 to 5000 do
  begin
    sky := 'key' + IntToStr(iky);
    schk := 'value' + IntToStr(iky);

    psvl := self.mpobjs[sky];

    if psvl <> nil then
    begin
      if psvl^ <> schk then
        CheckEquals(schk, psvl^, 'LKP - key '  + chr(39) + sky + chr(39)
          + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

    end
    else  //Value is nil
    begin
      Check(psvl <> nil, 'LKP - key '  + chr(39) + sky + chr(39)
        + ' failed! Value is: ' + chr(39) + 'nil' + chr(39));
    end;  //if psvl <> nil then
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('LKP - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('LKP Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < 4, 'LKP Operation: Operation slower than 4 ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');

end;

procedure TTestsPointerHashList.TestInsertCheck5000Elements;
var
  sky, schk: String;
  psvl: PAnsiString;
  tmstrt, tmend : TDateTime;
  tmins: Double;
  TS: TTimeStamp;
  iky: Integer;
begin
  WriteLn('TestInsertCheck5000Elements: do ...');

  TS:=DateTimeToTimeStamp(Now);
  Writeln ('INS - Start - Now in millisecs since midnight : ',TS.Time);


  tmstrt := Now;

  self.lsthshobjs.LoadFactor := 2;
  self.lsthshobjs.Limit := 5000;

  for iky := 1 to 5000 do
  begin
    sky := 'key' + IntToStr(iky);

    New(psvl);
    psvl^ := 'value' + IntToStr(iky);

    self.lsthshobjs.setValue(sky, psvl);
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('INS - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('INS Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < 7, 'INS Operation: Operation slower than 7 ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');


  tmstrt := Now;

  for iky := 1 to 5000 do
  begin
    sky := 'key' + IntToStr(iky);
    schk := 'value' + IntToStr(iky);

    psvl := self.lsthshobjs.getValue(sky);

    if psvl <> nil then
    begin
      if psvl^ <> schk then
        CheckEquals(schk, psvl^, 'LKP - key '  + chr(39) + sky + chr(39)
          + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

    end
    else  //Key Lookup failed
    begin
      Check(psvl <> nil, 'LKP - key '  + chr(39) + sky + chr(39)
        + ' failed! Value is: ' + chr(39) + 'nil' + chr(39));
    end; //if psvl <> nil then
  end;  //for iky := 1 to 1000 do

  tmend := Now;
  tmins := MilliSecondSpan(tmend, tmstrt);

  TS:=DateTimeToTimeStamp(Now);
  WriteLn ('LKP - End - Now in millisecs since midnight : ',TS.Time);

  WriteLn('LKP Operation completed in ', chr(39), FloatToStr(tmins), chr(39), ' ms.');

  Check(tmins < 4, 'LKP Operation: Operation slower than 4 ms! It took: '
    + chr(39) + FloatToStr(tmins) + chr(39) + ' ms.');

end;

end.

