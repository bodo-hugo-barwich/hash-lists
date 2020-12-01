unit tests_stringhash;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, stringhash;



type
  //==========================================================================
  // Class TTestsObjectHashList Declaration


  TTestsStringHashList = class(TTestCase)
  protected
    lsthshstrs: TPLStringHashList;
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
  end;

procedure RegisterTests;

implementation

uses
 Classes, SysUtils;


{ here we register all our test classes }
procedure RegisterTests;
begin
 TestFramework.RegisterTest(TTestsStringHashList.Suite);
end;




//==========================================================================
// Class TTestsObjectHashList Implementation


procedure TTestsStringHashList.SetUp;
begin
  Self.lsthshstrs := TPLStringHashList.Create;

end;

procedure TTestsStringHashList.TearDown;
begin
  if Self.lsthshstrs <> Nil then Self.lsthshstrs.Free;

end;

procedure TTestsStringHashList.TestInsertCheckElements;
var
  psvl: PAnsiString;
begin
  //WriteLn('TTestsStringHashList.TestInsertCheckElements: go ...');

  New(psvl);
  psvl^ := 'value1';

  Self.lsthshstrs['key1'] := psvl;

  New(psvl);
  psvl^ := 'value2';

  Self.lsthshstrs['key2'] := psvl;

  New(psvl);
  psvl^ := 'value3';

  Self.lsthshstrs['key3'] := psvl;

  New(psvl);
  psvl^ := 'value4';

  Self.lsthshstrs['key4'] := psvl;

  New(psvl);
  psvl^ := 'value5';

  Self.lsthshstrs['key5'] := psvl;

  Self.lsthshstrs.setValue('key6', 'value6');
  Self.lsthshstrs.setValue('key7', 'value7');
  self.lsthshstrs.setValue('key8', 'value8');
  self.lsthshstrs.setValue('key9', 'value9');
  self.lsthshstrs.setValue('key10', 'value10');

  psvl := PAnsiString(Self.lsthshstrs['key1']);

  CheckEquals(psvl^, 'value1', 'LKP - key '  + chr(39) + 'key1' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs['key2']);

  CheckEquals(psvl^, 'value2', 'LKP - key '  + chr(39) + 'key2' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs['key3']);

  CheckEquals(psvl^, 'value3', 'LKP - key '  + chr(39) + 'key3' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs['key4']);

  CheckEquals(psvl^, 'value4', 'LKP - key '  + chr(39) + 'key4' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs['key5']);

  CheckEquals(psvl^, 'value5', 'LKP - key '  + chr(39) + 'key5' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getValue('key6'));

  CheckEquals(psvl^, 'value6', 'LKP - key '  + chr(39) + 'key6' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getValue('key7'));

  CheckEquals(psvl^, 'value7', 'LKP - key '  + chr(39) + 'key7' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getValue('key8'));

  CheckEquals(psvl^, 'value8', 'LKP - key '  + chr(39) + 'key8' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getValue('key9'));

  CheckEquals(psvl^, 'value9', 'LKP - key '  + chr(39) + 'key9' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getValue('key10'));

  CheckEquals(psvl^, 'value10', 'LKP - key '  + chr(39) + 'key10' + chr(39)
    + ' failed! Value is: ' + chr(39) + psvl^ + chr(39));

end;

procedure TTestsStringHashList.TestCheckFirstElement;
var
  psvl: PAnsiString;
  sky: String;
begin
  Self.lsthshstrs.setValue('first_key', 'first_value');
  self.lsthshstrs.setValue('next_key', 'next_value');
  self.lsthshstrs.setValue('last_key', 'last_value');

  Check(self.lsthshstrs.moveFirst() = True, 'TPLStringHashList.moveFirst(): failed!');

  sky := self.lsthshstrs.getCurrentKey();

  CheckEquals(sky, 'first_key', 'TPLStringHashList.getCurrentKey(): failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := PAnsiString(self.lsthshstrs.getCurrentValue());

  CheckEquals(psvl^, 'first_value', 'TPLStringHashList.getCurrentValue(): failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

end;

procedure TTestsStringHashList.TestCheckNextElement;
var
  psvl: PAnsiString;
  sky: String;
begin
  New(psvl);
  psvl^ := 'first_value';

  Self.lsthshstrs['first_key'] := psvl;

  New(psvl);
  psvl^ := 'next_value';

  Self.lsthshstrs['next_key'] := psvl;

  New(psvl);
  psvl^ := 'last_value';

  Self.lsthshstrs['last_key'] := psvl;

  Check(self.lsthshstrs.moveNext() = True, 'TPLStringHashList.moveNext() No. 1 : failed!');

  sky := Self.lsthshstrs.getCurrentKey();

  CheckEquals(sky, 'first_key', 'TPLStringHashList.getCurrentKey() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getCurrentValue());

  CheckEquals(psvl^, 'first_value', 'TPLStringHashList.getCurrentValue() No. 1 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

  Check(self.lsthshstrs.moveNext() = True, 'TPLStringHashList.moveNext() No. 2 : failed!');

  sky := Self.lsthshstrs.getCurrentKey();

  CheckEquals('next_key', sky, 'TPLStringHashList.getCurrentKey() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getCurrentValue());

  CheckEquals(psvl^, 'next_value', 'TPLStringHashList.getCurrentValue() No. 2 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));

  Check(self.lsthshstrs.moveNext() = True, 'TPLStringHashList.moveNext() No. 3 : failed!');

  sky := Self.lsthshstrs.getCurrentKey();

  CheckEquals(sky, 'last_key', 'TPLStringHashList.getCurrentKey() No. 3 : failed! '
    + 'Returned Key: ' + chr(39) + sky + chr(39));

  psvl := PAnsiString(Self.lsthshstrs.getCurrentValue());

  CheckEquals(psvl^, 'last_value', 'TPLStringHashList.getCurrentValue() No. 3 : failed! '
    + 'Returned Key: ' + chr(39) + psvl^ + chr(39));
end;

end.

