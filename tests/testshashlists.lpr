program testshashlists;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, tests_objecthash;


begin
  // Register all tests
  sample_tests.RegisterTests;

  RunRegisteredTests;
end.
