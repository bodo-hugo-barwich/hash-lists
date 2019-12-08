unit stringhash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pointerhash;

type
  TPLStringNodeList = class(TPLHashNodeList)
  public
    destructor Destroy; override;
    function addNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): Integer; overload;
  end;

  TPLStringHashList = class(TPLObjectHashList)
  protected
    procedure extendList(brebuild: Boolean = True); override;
  public
    procedure setValue(const skey: String; svalue: String); overload;
  end;


implementation
  (*==========================================================================*)
  // Class TPLStringNodeList



  destructor TPLStringNodeList.Destroy;
  var
    ind: Integer;
  begin
    for ind := 0 to self.imaxcount - 1 do
    begin
      if self.arrnodes[ind] <> nil then
      begin
        if self.arrnodes[ind]^.pvalue <> nil then
        begin
          //Free the String Data
          Dispose(PAnsiString(self.arrnodes[ind]^.pvalue));
          self.arrnodes[ind]^.pvalue := nil;
        end;
      end;  //if self.arrnodes[ind] <> nil then
    end; //for ind := 0 to self.imaxcount - 1 do

    inherited Destroy;
  end;

  function TPLStringNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): Integer;
  var
    psvl: PAnsiString;
  begin
    //Create New String Pointer
    New(psvl);
    psvl^ := psvalue^;

    Result := inherited addNode(ihash, pskey, psvl);
  end;


  (*==========================================================================*)
  // Class TPLStringHashList


  procedure TPLStringHashList.extendList(brebuild: Boolean = True);
  var
    ibkt: Integer;
  begin
    self.ibucketcount := self.ibucketcount + self.igrowfactor;
    self.imaxkeycount := (self.ibucketcount * 3) - 1;

    SetLength(self.arrbuckets, self.ibucketcount);

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      if self.arrbuckets[ibkt] = nil then self.arrbuckets[ibkt] := TPLStringNodeList.Create(ibkt);
    end;

    if brebuild then
      //Reindex the Nodes
      self.rebuildList();

  end;


  procedure TPLStringHashList.setValue(const skey: String; svalue: String);
  var
    plstnd: PPLHashNode;
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    //Build the Hash Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    plstnd := TPLStringNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh);

    if plstnd = nil then
    begin
      //Add a New Node

      if self.ikeycount = self.imaxkeycount then
      begin
        self.extendList();

        //Recompute Bucket Index
        ibktidx := ihsh mod self.ibucketcount;
      end;  //if self.ikeycount = self.imaxkeycount then

      TPLStringNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, PAnsiString(@svalue));

      inc(self.ikeycount);
    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      PAnsiString(plstnd^.pvalue)^ := svalue;
    end;  //if plstnd = nil then
  end;

end.

