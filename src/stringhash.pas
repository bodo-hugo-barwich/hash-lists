unit stringhash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , pointerhash;

type
  TPLStringNodeList = class(TPLPointerNodeList)
  public
    destructor Destroy; override;
    function addNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): PPLHashNode; overload;
    function removeNode(pnode: PPLHashNode): Boolean; override;
    procedure Clear; override;
  end;

  TPLStringHashList = class(TPLPointerHashList)
  protected
    procedure extendList(brebuild: Boolean = True); override;
  public
    procedure setCapacity(icapacity: Integer); override;
    procedure Add(const skey: String; svalue: String); overload;
    procedure setValue(const skey: String; svalue: String); overload;
    procedure removeKey(const skey: String); override;
    procedure Clear(); override;
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

function TPLStringNodeList.removeNode(pnode: PPLHashNode): Boolean;
begin
  Result := False;

  if pnode <> nil then
  begin
    self.unsetIndex(pnode^.inodeindex);
    Dispose(PAnsiString(pnode^.pvalue));

    Result := inherited removeNode(pnode);
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


  procedure TPLStringHashList.setCapacity(icapacity: Integer);
  var
    ibkt: Integer;
    brbld: Boolean = False;
  begin
    if icapacity > self.imaxkeycount then
    begin
      //Set icapacity as Max. Key Count
      self.imaxkeycount := icapacity;

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
          Self.rebuildList(0, Self.ibucketcount - 1, Self.ibucketcount);

      end;  //if floor(self.imaxkeycount / self.iloadfactor) > self.ibucketcount then
    end; //if icapacity > self.imaxkeycount then
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
      Self.rebuildList(0, Self.ibucketcount - 1, Self.ibucketcount);

  end;

procedure TPLStringHashList.Add(const skey: String; svalue: String);
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

    if Self.nodeiterator <> Nil then
      //Reset the Iterator
      Self.nodeiterator.Reset;

  end
  else  //The Key is already in the List
  begin
    case Self.eduplicatekeys of
      //Update the Node Value
      dupAccept: PAnsiString(self.psearchednode^.pvalue)^ := svalue;
      dupError: RaiseListException('Key ' + chr(39) + skey + chr(39) + ': key does already exist!');
      dupIgnore: ;
    end;  //case Self.eduplicatekeys of
  end;  //if self.psearchednode = nil then
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

    if Self.nodeiterator <> Nil then
      //Reset the Iterator
      Self.nodeiterator.Reset;

  end
  else  //The Key is already in the List
  begin
    //Update the Node Value

    PAnsiString(self.psearchednode^.pvalue)^ := svalue;
  end;  //if self.psearchednode = nil then
end;

procedure TPLStringHashList.removeKey(const skey: String);
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

  if Self.psearchednode = nil then
    Self.psearchednode := TPLStringNodeList(Self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

  if Self.psearchednode <> nil then
  begin
    if (Self.nodeiterator <> Nil)
      and (Self.nodeiterator.PNode = Self.psearchednode) then
      //Move the Iterator to a former Node
      Self.nodeiterator.Return;

    if TPLStringNodeList(self.arrbuckets[ibktidx]).removeNode(self.psearchednode) then
    begin
      self.psearchednode := nil;
      dec(Self.ikeycount);
    end;
  end;  //if self.psearchednode <> nil then
end;

procedure TPLStringHashList.Clear();
var
  ibkt: Integer;
begin
  for ibkt := 0 to self.ibucketcount - 1 do
  begin
    TPLStringNodeList(self.arrbuckets[ibkt]).Clear;

    if ibkt >= self.igrowfactor then
      self.arrbuckets[ibkt].Free;

  end;  //for ibkt := 0 to self.ibucketcount - 1 do

  //Shrink the List to its initial Size
  SetLength(self.arrbuckets, self.igrowfactor);

  //Reset Counter
  Self.ibucketcount := Self.igrowfactor;
  Self.ikeycount := 0;
  Self.imaxkeycount := Self.ibucketcount * Self.iloadfactor;

  if Self.nodeiterator <> Nil then
    //Reset the Iterator
    Self.nodeiterator.Reset;

end;

end.

