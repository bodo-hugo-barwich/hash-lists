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
  // Class TStringObject Declaration


  TTestsObjectHashList = class(TTestCase)
  protected
    lsthshobjs: TPLObjectHashList;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestInsertCheckElements;
    procedure TestCheckFirstElement;
    procedure TestCheckNextElement;
  end;

 procedure RegisterTests;


implementation
 
uses
  Classes, SysUtils, DateUtils;


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

end;

procedure TTestsObjectHashList.TearDown;
begin
  if Self.lsthshobjs <> Nil then Self.lsthshobjs.Free;

end;

procedure TTestsObjectHashList.TestInsertCheckElements;
var
  eml: TStringObject;
begin
  eml := TStringObject.Create('key1', 'value1');
  self.lsthshobjs['key1'] := eml;

  eml := TStringObject.Create('key2', 'value2');
  self.lsthshobjs['key2'] := eml;

  eml := TStringObject.Create('key3', 'value3');
  self.lsthshobjs['key3'] := eml;

  eml := TStringObject.Create('key4', 'value4');
  self.lsthshobjs['key4'] := eml;

  eml := TStringObject.Create('key5', 'value5');
  self.lsthshobjs['key5'] := eml;

  eml := TStringObject.Create('key6', 'value6');
  self.lsthshobjs.setValue('key6', eml);

  eml := TStringObject.Create('key7', 'value7');
  self.lsthshobjs.setValue('key7', eml);

  eml := TStringObject.Create('key8', 'value8');
  self.lsthshobjs.setValue('key8', eml);

  eml := TStringObject.Create('key9', 'value9');
  self.lsthshobjs.setValue('key9', eml);

  eml := TStringObject.Create('key10', 'value10');
  self.lsthshobjs.setValue('key10', eml);

  eml := TStringObject(Self.lsthshobjs['key1']);

  CheckEquals('value1', eml.Value, 'LKP - key '  + chr(39) + 'key1' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs['key2']);

  CheckEquals('value2', eml.Value, 'LKP - key '  + chr(39) + 'key2' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs['key3']);

  CheckEquals('value3', eml.Value, 'LKP - key '  + chr(39) + 'key3' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs['key4']);

  CheckEquals('value4', eml.Value, 'LKP - key '  + chr(39) + 'key4' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs['key5']);

  CheckEquals('value5', eml.Value, 'LKP - key '  + chr(39) + 'key5' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs.getValue('key6'));

  CheckEquals('value6', eml.Value, 'LKP - key '  + chr(39) + 'key6' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs.getValue('key7'));

  CheckEquals('value7', eml.Value, 'LKP - key '  + chr(39) + 'key7' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs.getValue('key8'));

  CheckEquals('value8', eml.Value, 'LKP - key '  + chr(39) + 'key8' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs.getValue('key9'));

  CheckEquals('value9', eml.Value, 'LKP - key '  + chr(39) + 'key9' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

  eml := TStringObject(Self.lsthshobjs.getValue('key10'));

  CheckEquals('value10', eml.Value, 'LKP - key '  + chr(39) + 'key10' + chr(39)
    + ' failed! Value is: ' + chr(39) + eml.Value + chr(39));

end;

procedure TTestsObjectHashList.TestCheckFirstElement;
begin

end;

procedure TTestsObjectHashList.TestCheckNextElement;
begin

end;

end.

