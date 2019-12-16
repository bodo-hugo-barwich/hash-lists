unit tests_pointerhash;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, pointerhash;

type
  TTestsPointerHashList = class(TTestCase)
  protected
    lsthshobjs: TPLPointerHashList;
    procedure SetUp; override;
    procedure Teardown; override;
  published
    procedure TestInsertCheckElements;
    procedure TestCheckFirstElement;
    procedure TestCheckNextElement;
  end;

 procedure RegisterTests;






implementation

uses
  SysUtils;


{ here we register all our test classes }
procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestsPointerHashList.Suite);
end;


procedure TTestsPointerHashList.SetUp;
begin
  self.lsthshobjs := TPLObjectHashList.Create();
end;

procedure TTestsPointerHashList.Teardown;
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

  Check(self.lsthshobjs.moveFirst() = True, 'TPLObjectHashList.moveFirst(): failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('first_key', sky, 'TPLObjectHashList.getCurrentKey(): failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := self.lsthshobjs.getCurrentValue();

  CheckEquals('first_value', psvl^, 'TPLObjectHashList.getCurrentValue(): failed! '
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

  Check(self.lsthshobjs.moveNext() = True, 'TPLObjectHashList.moveNext() No. 1 : failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('first_key', sky, 'TPLObjectHashList.getCurrentKey() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := self.lsthshobjs.getCurrentValue();

  CheckEquals('first_value', psvl^, 'TPLObjectHashList.getCurrentValue() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

  Check(self.lsthshobjs.moveNext() = True, 'TPLObjectHashList.moveNext() No. 2 : failed!');

  sky := self.lsthshobjs.getCurrentKey();

  CheckEquals('next_key', sky, 'TPLObjectHashList.getCurrentKey() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := self.lsthshobjs.getCurrentValue();

  CheckEquals('next_value', psvl^, 'TPLObjectHashList.getCurrentValue() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

end;


end.

