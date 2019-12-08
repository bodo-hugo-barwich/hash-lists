unit tests_objecthash;

{$mode objfpc}{$H+}

interface

uses
  TestFramework;

type
  TTestsObjectHashList = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure Teardown; override; // Close the Comm port here
  published
    procedure TestInsertCheckElements;
    procedure TestOne;
    procedure TestNoError;
    procedure TestThree;
    procedure TestFour;
  end;

 procedure RegisterTests;






implementation

uses
  SysUtils, pointerhash;


{ here we register all our test classes }
procedure RegisterTests;
begin
  TestFramework.RegisterTest(TTestsObjectHashList.Suite);
end;


procedure TTestsObjectHashList.SetUp;

procedure TTestsObjectHashList.TestInsertCheckElements;
var

  psvl: PAnsiString;
begin

end;


end.

