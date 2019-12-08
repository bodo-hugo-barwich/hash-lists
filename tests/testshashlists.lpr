program testshashlists;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, tests_objecthash;


begin
  // Register all tests
  tests_objecthash.RegisterTests;

  RunRegisteredTests;
end.
