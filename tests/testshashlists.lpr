program testshashlists;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, tests_pointerhash, tests_objecthash;


begin
  // Register all tests
  tests_pointerhash.RegisterTests;

  RunRegisteredTests;
end.
