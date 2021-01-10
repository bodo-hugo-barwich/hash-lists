unit objecthash;

{$mode objfpc}{$H+}

interface


uses
  Classes, pointerhash;



type
  TPLObjectNodeList = class(TPLPointerNodeList)
  protected
    procedure Initialize(ibucket, igrowth: Integer); override;
    procedure SetOwned(bisowned: Boolean);
    function IsValuesOwned: Boolean; inline;
  public
    destructor Destroy; override;
    function addNode(ihash: Cardinal; pskey: PAnsiString; value: TObject): PPLHashNode; overload;
    function removeNode(pnode: PPLHashNode): Boolean; override;
    procedure Clear; override;
    property Owned: Boolean read IsValuesOwned write setOwned;
  end;

  TPLObjectHashList = class(TPLPointerHashList)
  protected
    bowned: Boolean;
    procedure Initialize(icapacity: Integer; iload: Integer); override;
    procedure setCapacity(icapacity: Integer); override;
    procedure SetOwned(bisowned: Boolean);
    procedure extendList(brebuild: Boolean = True); override;
  public
    procedure Add(const skey: String; value: TObject); overload;
    procedure setValue(const skey: String; value: TObject); overload;
    procedure removeKey(const skey: String); override;
    procedure Clear(); override;
    property Owned: Boolean read bowned write setOwned;
  end;


implementation


uses
  math;



(*==========================================================================*)
(* Class TPLObjectNodeList *)


destructor TPLObjectNodeList.Destroy;
var
  ind: Integer;
begin
  if Self.IsValuesOwned then
  begin
    for ind := 0 to self.imaxcount - 1 do
    begin
      if self.arrnodes[ind] <> nil then
      begin
        if self.arrnodes[ind]^.pvalue <> nil then
        begin
          //Free the added Data Object
          TObject(self.arrnodes[ind]^.pvalue).Free;
          self.arrnodes[ind]^.pvalue := nil;
        end;
      end;  //if self.arrnodes[ind] <> nil then
    end; //for ind := 0 to self.imaxcount - 1 do
  end; //if self.bowned then

  inherited Destroy;
end;



//----------------------------------------------------------------------------
//Administration Methods


procedure TPLObjectNodeList.Initialize(ibucket, igrowth: Integer);
begin
  inherited Initialize(ibucket, igrowth);

  Self.setOwned(True);
end;

procedure TPLObjectNodeList.SetOwned(bisowned: Boolean);
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

  function TPLObjectNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; value: TObject): PPLHashNode;
  begin
    Result := inherited addNode(ihash, pskey, value);
  end;

function TPLObjectNodeList.removeNode(pnode: PPLHashNode): Boolean;
begin
  Result := False;

  if pnode <> nil then
  begin
    self.unsetIndex(pnode^.inodeindex);

    if Self.IsValuesOwned then
      TObject(pnode^.pvalue).Free;

    Result := inherited removeNode(pnode);
  end;  //if plstnd <> nil then
end;

procedure TPLObjectNodeList.Clear;
var
  ind: Integer;
begin
  if Self.IsValuesOwned then
  begin
    for ind := 0 to self.imaxcount - 1 do
    begin
      if self.arrnodes[ind] <> nil then
      begin
        if self.arrnodes[ind]^.pvalue <> nil then
        begin
          //Free the added Data Object
          TObject(self.arrnodes[ind]^.pvalue).Free;
          self.arrnodes[ind]^.pvalue := nil;
        end;
      end;  //if self.arrnodes[ind] <> nil then
    end; //for ind := 0 to self.imaxcount - 1 do
  end;  //if self.bowned then

  //Do the Clearing of the Nodes
  inherited Clear;
end;



//----------------------------------------------------------------------------
//Consultation Methods


function TPLObjectNodeList.IsValuesOwned: Boolean;
begin
  Result := (oplValuesOwned in Self.stoptions);
end;




(*==========================================================================*)
(* Class TPLObjectHashList *)



//----------------------------------------------------------------------------
//Administration Methods


procedure TPLObjectHashList.Initialize(icapacity: Integer; iload: Integer);
begin
  //Do the Base Initialization
  inherited Initialize(icapacity, iload);

  //Enable Ownership
  Self.setOwned(True);
end;

  procedure TPLObjectHashList.setCapacity(icapacity: Integer);
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

            TPLObjectNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
            TPLObjectNodeList(self.arrbuckets[ibkt]).GrowFactor := self.iloadfactor;

            brbld := True;
          end;  //if self.arrbuckets[ibkt] = nil then
        end;  //for ibkt := 0 to self.ibucketcount - 1 do

        if brbld then
          //Reindex the Nodes
          self.rebuildList(0, Self.ibucketcount - 1, Self.ibucketcount);

      end;  //if floor(self.imaxkeycount / self.iloadfactor) > self.ibucketcount then
    end; //if icapacity > self.imaxkeycount then
  end;

procedure TPLObjectHashList.SetOwned(bisowned: Boolean);
var
  ibkt: Integer;
begin
  self.bowned := bisowned;

  for ibkt := 0 to self.ibucketcount - 1 do
  begin
    TPLObjectNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
  end;  //for ibkt := 0 to self.ibucketcount - 1 do
end;


  procedure TPLObjectHashList.extendList(brebuild: Boolean = True);
  var
    ibkt: Integer;
  begin
    self.ibucketcount := self.ibucketcount + self.igrowfactor;
    self.imaxkeycount := (self.ibucketcount * self.iloadfactor) - 1;

    SetLength(self.arrbuckets, self.ibucketcount);

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      //Create the Buckets as TPLObjectNodeList
      if self.arrbuckets[ibkt] = nil then
      begin
        self.arrbuckets[ibkt] := TPLObjectNodeList.Create(ibkt);

        TPLObjectNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
        TPLObjectNodeList(self.arrbuckets[ibkt]).GrowFactor := self.iloadfactor;
      end;  //if self.arrbuckets[ibkt] = nil then
    end;  //for ibkt := 0 to self.ibucketcount - 1 do

    if brebuild then
      //Reindex the Nodes
      Self.rebuildList(0, Self.ibucketcount - 1, Self.ibucketcount);

  end;

procedure TPLObjectHashList.Add(const skey: String; value: TObject);
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
    self.psearchednode := TPLObjectNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

  if self.psearchednode = nil then
  begin
    //Add a New Node

    if self.ikeycount = self.imaxkeycount then
    begin
      self.extendList();

      //Recompute Bucket Index
      ibktidx := ihsh mod self.ibucketcount;
    end;  //if self.ikeycount = self.imaxkeycount then

    self.psearchednode := TPLObjectNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, TObject(value));

    inc(self.ikeycount);

    if Self.nodeiterator <> Nil then
      //Reset the Iterator
      Self.nodeiterator.Reset;

  end
  else  //The Key is already in the List
  begin
    case Self.eduplicatekeys of
      //Update the Node Value
      dupAccept: TObject(self.psearchednode^.pvalue) := value;
      dupError: RaiseListException('Key ' + chr(39) + skey + chr(39) + ': key does already exist!');
      dupIgnore: ;
    end;  //case Self.eduplicatekeys of
  end;  //if self.psearchednode = nil then
end;

  procedure TPLObjectHashList.setValue(const skey: String; value: TObject);
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
      self.psearchednode := TPLObjectNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

    if self.psearchednode = nil then
    begin
      //Add a New Node

      if self.ikeycount = self.imaxkeycount then
      begin
        self.extendList();

        //Recompute Bucket Index
        ibktidx := ihsh mod self.ibucketcount;
      end;  //if self.ikeycount = self.imaxkeycount then

      self.psearchednode := TPLObjectNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, TObject(value));

      inc(self.ikeycount);

      if Self.nodeiterator <> Nil then
        //Reset the Iterator
        Self.nodeiterator.Reset;

    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      TObject(self.psearchednode^.pvalue) := value;
    end;  //if self.psearchednode = nil then
  end;

procedure TPLObjectHashList.removeKey(const skey: String);
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
    Self.psearchednode := TPLObjectNodeList(Self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

  if Self.psearchednode <> nil then
  begin
    if (Self.nodeiterator <> Nil)
      and (Self.nodeiterator.PNode = Self.psearchednode) then
      //Move the Iterator to a former Node
      Self.nodeiterator.Return;

    if TPLObjectNodeList(self.arrbuckets[ibktidx]).removeNode(self.psearchednode) then
    begin
      self.psearchednode := nil;
      dec(Self.ikeycount);
    end;
  end;  //if self.psearchednode <> nil then
end;

procedure TPLObjectHashList.Clear();
var
  ibkt: Integer;
begin
  for ibkt := 0 to self.ibucketcount - 1 do
  begin
    TPLObjectNodeList(self.arrbuckets[ibkt]).Clear;

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

