program testshashlists;

{$mode objfpc}{$H+}

uses
  Classes, TextTestRunner
  , tests_pointerhash, tests_hashiterator, tests_objecthash, tests_stringhash;


begin
  // Register all tests
  tests_pointerhash.RegisterTests;
  tests_hashiterator.RegisterTests;
  tests_objecthash.RegisterTests;
  tests_stringhash.RegisterTests;

  RunRegisteredTests;
end.
