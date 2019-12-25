unit stringhash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pointerhash;

type
  TPLStringNodeList = class(TPLPointerNodeList)
  public
    destructor Destroy; override;
    function addNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): PPLHashNode; overload;
    function removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean; override; overload;
    function removeNode(pskey: PAnsiString): Boolean; override; overload;
    procedure Clear; override;
  end;

  TPLStringHashList = class(TPLPointerHashList)
  protected
    procedure extendList(brebuild: Boolean = True); override;
  public
    procedure setLimitCount(ilimit: Integer); override;
    procedure setValue(const skey: String; svalue: String); overload;
  end;


implementation


uses
  math;



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

  function TPLStringNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): PPLHashNode;
  var
    psvl: PAnsiString;
  begin
    //Create New String Pointer
    New(psvl);
    psvl^ := psvalue^;

    Result := inherited addNode(ihash, pskey, psvl);
  end;

  function TPLStringNodeList.removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean;
  var
    plstnd: PPLHashNode;
  begin
    Result := False;

    plstnd := self.searchNode(ihash, pskey);

    if plstnd <> nil then
    begin
      self.unsetIndex(plstnd^.inodeindex);
      Dispose(PAnsiString(plstnd^.pvalue));

      Result := inherited removeNode(plstnd);
    end;  //if plstnd <> nil then
  end;

  function TPLStringNodeList.removeNode(pskey: PAnsiString): Boolean;
  var
    plstnd: PPLHashNode;
  begin
    Result := False;

    plstnd := self.searchNode(pskey);

    if plstnd <> nil then
    begin
      self.unsetIndex(plstnd^.inodeindex);
      Dispose(PAnsiString(plstnd^.pvalue));

      Result := inherited removeNode(plstnd);
    end;  //if plstnd <> nil then
  end;

  procedure TPLStringNodeList.Clear;
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

    //Do the Clearing of the Nodes
    inherited Clear;
  end;



  (*==========================================================================*)
  // Class TPLStringHashList


  procedure TPLStringHashList.setLimitCount(ilimit: Integer);
  var
    ibkt: Integer;
    brbld: Boolean = False;
  begin
    if ilimit > self.imaxkeycount then
    begin
      //Set ilimit as Max. Key Count
      self.imaxkeycount := ilimit;

      //Will the Bucket Count increase
      if floor(self.imaxkeycount / self.iloadfactor) > self.ibucketcount then
      begin
        self.ibucketcount := ceil(self.imaxkeycount / self.iloadfactor);

        //Forcing a uneven Bucket Count
        if (self.ibucketcount mod 2) = 0 then
          dec(self.ibucketcount);

        SetLength(self.arrbuckets, self.ibucketcount);

        for ibkt := 0 to self.ibucketcount - 1 do
        begin
          if self.arrbuckets[ibkt] = nil then
          begin
            self.arrbuckets[ibkt] := TPLPointerNodeList.Create(ibkt);

            TPLStringNodeList(self.arrbuckets[ibkt]).GrowFactor := self.iloadfactor;

            brbld := True;
          end;  //if self.arrbuckets[ibkt] = nil then
        end;  //for ibkt := 0 to self.ibucketcount - 1 do

        if brbld then
          //Reindex the Nodes
          self.rebuildList();

      end;  //if floor(self.imaxkeycount / self.iloadfactor) > self.ibucketcount then
    end; //if ilimit > self.imaxkeycount then
  end;

  procedure TPLStringHashList.extendList(brebuild: Boolean = True);
  var
    ibkt: Integer;
  begin
    self.ibucketcount := self.ibucketcount + self.igrowfactor;
    self.imaxkeycount := (self.ibucketcount * self.iloadfactor) - 1;

    SetLength(self.arrbuckets, self.ibucketcount);

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      //Create the Buckets as TPLStringNodeList
      if self.arrbuckets[ibkt] = nil then
      begin
        self.arrbuckets[ibkt] := TPLStringNodeList.Create(ibkt);

        TPLStringNodeList(self.arrbuckets[ibkt]).GrowFactor := self.iloadfactor;
      end;
    end;  //for ibkt := 0 to self.ibucketcount - 1 do

    if brebuild then
      //Reindex the Nodes
      self.rebuildList();

  end;


  procedure TPLStringHashList.setValue(const skey: String; svalue: String);
  var
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    //Build the Hash Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    if self.psearchednode <> nil then
    begin
      if not ((self.psearchednode^.ihash = ihsh)
        and (self.psearchednode^.skey = skey)) then
        self.psearchednode := nil;

    end;  //if self.psearchednode <> nil then

    if self.psearchednode = nil then
      self.psearchednode := TPLStringNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

    if self.psearchednode = nil then
    begin
      //Add a New Node

      if self.ikeycount = self.imaxkeycount then
      begin
        self.extendList();

        //Recompute Bucket Index
        ibktidx := ihsh mod self.ibucketcount;
      end;  //if self.ikeycount = self.imaxkeycount then

      self.psearchednode := TPLStringNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, PAnsiString(@svalue));

      inc(self.ikeycount);
    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      PAnsiString(self.psearchednode^.pvalue)^ := svalue;
    end;  //if self.psearchednode = nil then
  end;

end.

