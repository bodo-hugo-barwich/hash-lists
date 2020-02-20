unit objecthash;

{$mode objfpc}{$H+}

interface


uses
  Classes, pointerhash;



type
  TPLObjectNodeList = class(TPLPointerNodeList)
  protected
    bowned: Boolean;
  public
    constructor Create; override; overload;
    constructor Create(iindex: Integer); override; overload;
    constructor Create(iindex: Integer; ifactor: Integer); override; overload;
    destructor Destroy; override;
    procedure setOwned(bisowned: Boolean);
    function addNode(ihash: Cardinal; pskey: PAnsiString; value: TObject): PPLHashNode; overload;
    function removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean; override; overload;
    function removeNode(pskey: PAnsiString): Boolean; override; overload;
    procedure Clear; override;
    property Owned: Boolean read bowned write setOwned;
  end;

  TPLObjectHashList = class(TPLPointerHashList)
  protected
    bowned: Boolean;
    procedure extendList(brebuild: Boolean = True); override;
  public
    procedure setOwned(bisowned: Boolean);
    procedure setCapacity(icapacity: Integer); override;
    procedure Add(const skey: String; value: TObject); overload;
    procedure setValue(const skey: String; value: TObject); overload;
    property Owned: Boolean read bowned write setOwned;
  end;


implementation


uses
  math;



  (*==========================================================================*)
  (* Class TPLObjectNodeList *)


  constructor TPLObjectNodeList.Create;
  begin
    inherited Create;

    self.bowned := True;
  end;

  constructor TPLObjectNodeList.Create(iindex: Integer);
  begin
    inherited Create(iindex);

    self.bowned := True;
  end;

  constructor TPLObjectNodeList.Create(iindex: Integer; ifactor: Integer);
  begin
    inherited Create(iindex, ifactor);

    self.bowned := True;
  end;

  destructor TPLObjectNodeList.Destroy;
  var
    ind: Integer;
  begin
    if self.bowned then
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

  procedure TPLObjectNodeList.setOwned(bisowned: Boolean);
  begin
    self.bowned := bisowned;
  end;

  function TPLObjectNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; value: TObject): PPLHashNode;
  begin
    Result := inherited addNode(ihash, pskey, value);
  end;

  function TPLObjectNodeList.removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean;
  var
    plstnd: PPLHashNode;
  begin
    Result := False;

    plstnd := self.searchNode(ihash, pskey);

    if plstnd <> nil then
    begin
      self.unsetIndex(plstnd^.inodeindex);
      TObject(plstnd^.pvalue).Free;

      Result := inherited removeNode(plstnd);
    end;  //if plstnd <> nil then
  end;

  function TPLObjectNodeList.removeNode(pskey: PAnsiString): Boolean;
  var
    plstnd: PPLHashNode;
  begin
    Result := False;

    plstnd := self.searchNode(pskey);

    if plstnd <> nil then
    begin
      self.unsetIndex(plstnd^.inodeindex);
      TObject(plstnd^.pvalue).Free;

      Result := inherited removeNode(plstnd);
    end;  //if plstnd <> nil then
  end;

  procedure TPLObjectNodeList.Clear;
  var
    ind: Integer;
  begin
    if self.bowned then
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



  (*==========================================================================*)
  (* Class TPLObjectHashList *)


  procedure TPLObjectHashList.setOwned(bisowned: Boolean);
  var
    ibkt: Integer;
  begin
    self.bowned := bisowned;

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      TPLObjectNodeList(self.arrbuckets[ibkt]).Owned := self.bowned;
    end;  //for ibkt := 0 to self.ibucketcount - 1 do
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
      self.rebuildList();

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
    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      TObject(self.psearchednode^.pvalue) := value;
    end;  //if self.psearchednode = nil then
  end;

end.

