program testshashlists;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner, tests_pointerhash;


begin
  // Register all tests
  tests_pointerhash.RegisterTests;

  RunRegisteredTests;
end.
