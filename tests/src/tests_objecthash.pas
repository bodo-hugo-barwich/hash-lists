unit tests_objecthash;

{$mode objfpc}{$H+}

interface

uses
  TestFramework, pointerhash;

type
  TTestsObjectHashList = class(TTestCase)
  protected
    lsthshobjs: TPLObjectHashList;
    procedure SetUp; override;
    procedure Teardown; override;
  published
    procedure TestInsertCheckElements;
  end;

 procedure RegisterTests;






implementation

uses
  SysUtils;


{ here we register all our test classes }
procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestsObjectHashList.Suite);
end;


procedure TTestsObjectHashList.SetUp;
begin
  self.lsthshobjs := TPLObjectHashList.Create();
end;

procedure TTestsObjectHashList.Teardown;
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

(*Test Adding 7 Keys and their Values (which should grow the List twice)
And Looking up their Values
*)
procedure TTestsObjectHashList.TestInsertCheckElements;
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

end;


end.

