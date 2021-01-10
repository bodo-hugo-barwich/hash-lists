unit stringhash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , pointerhash;

type
  TPLStringNodeList = class(TPLPointerNodeList)
  protected
    procedure Initialize(ibucket, igrowth: Integer); override;
    procedure SetOwned(bisowned: Boolean);
    function IsValuesOwned: Boolean; inline;
  public
    destructor Destroy; override;
    function AddNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): PPLHashNode; overload;
    function removeNode(pnode: PPLHashNode): Boolean; override;
    procedure Clear; override;
    property Owned: Boolean read IsValuesOwned write setOwned;
  end;

  TPLStringHashList = class(TPLPointerHashList)
  protected
    bowned: Boolean;
    procedure Initialize(icapacity: Integer; iload: Integer); override;
    procedure SetCapacity(icapacity: Integer); override;
    procedure SetOwned(bisowned: Boolean);
    procedure ExtendList(brebuild: Boolean = True); override;
  public
    procedure Add(const skey: String; svalue: String); overload;
    procedure setValue(const skey: String; svalue: String); overload;
    procedure removeKey(const skey: String); override;
    procedure Clear(); override;
    property Owned: Boolean read bowned write SetOwned;
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



//----------------------------------------------------------------------------
//Administration Methods


procedure TPLStringNodeList.Initialize(ibucket, igrowth: Integer);
begin
  inherited Initialize(ibucket, igrowth);

  Self.SetOwned(True);
end;

procedure TPLStringNodeList.SetOwned(bisowned: Boolean);
var
  stlstopts: TListOptions;
begin
  stlstopts:= Self.stoptions;

  if bisowned then
    Include(stlstopts, oplValuesOwned)
  else
    Exclude(stlstopts, oplValuesOwned);

  Self.stoptions := stlstopts;
end;

function TPLStringNodeList.AddNode(ihash: Cardinal; pskey: PAnsiString; psvalue: PAnsiString): PPLHashNode;
var
  psvl: PAnsiString;
begin
  if Self.IsValuesOwned then
  begin
    //Create New String Pointer
    New(psvl);
    psvl^ := psvalue^;
  end
  else
    psvl := psvalue;

  Result := inherited addNode(ihash, pskey, psvl);
end;

function TPLStringNodeList.removeNode(pnode: PPLHashNode): Boolean;
begin
  Result := False;

  if pnode <> nil then
  begin
    self.unsetIndex(pnode^.inodeindex);

    if Self.IsValuesOwned then
      Dispose(PAnsiString(pnode^.pvalue));

    Result := inherited removeNode(pnode);
  end;  //if plstnd <> nil then
end;

procedure TPLStringNodeList.Clear;
var
  ind: Integer;
begin
  if Self.IsValuesOwned then
    //List has Ownership of Values and
    //Values need to be freed
    for ind := 0 to self.imaxcount - 1 do
      if self.arrnodes[ind] <> nil then
      begin
        if self.arrnodes[ind]^.pvalue <> nil then
        begin
          //Free the String Data
          Dispose(PAnsiString(self.arrnodes[ind]^.pvalue));
          self.arrnodes[ind]^.pvalue := nil;
        end;
      end;  //if self.arrnodes[ind] <> nil then

  //Do the Clearing of the Nodes
  inherited Clear;
end;



//----------------------------------------------------------------------------
//Consultation Methods


function TPLStringNodeList.IsValuesOwned: Boolean;
begin
  Result := (oplValuesOwned in Self.stoptions);
end;




  (*==========================================================================*)
  // Class TPLStringHashList



//----------------------------------------------------------------------------
//Administration Methods


procedure TPLStringHashList.Initialize(icapacity: Integer; iload: Integer);
begin
  //Do the Base Initialization
  inherited Initialize(icapacity, iload);

  //Enable Ownership
  Self.SetOwned(True);
end;

procedure TPLStringHashList.SetCapacity(icapacity: Integer);
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

          TPLStringNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
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

procedure TPLStringHashList.SetOwned(bisowned: Boolean);
var
  ibkt: Integer;
begin
  self.bowned := bisowned;

  for ibkt := 0 to self.ibucketcount - 1 do
  begin
    TPLStringNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
  end;  //for ibkt := 0 to self.ibucketcount - 1 do
end;

procedure TPLStringHashList.ExtendList(brebuild: Boolean = True);
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

      TPLStringNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
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

