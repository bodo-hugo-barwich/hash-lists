unit pointerhash;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  PPLHashNode = ^TPLHashNode;
  TPLHashNode = packed record
    ibucketindex: Integer;
    inodeindex: Integer;
    ihash: Cardinal;
    skey: String;
    pvalue: Pointer;
  end;

  TPLHashNodeList = class
  protected
    arrnodes: array of PPLHashNode;
    ibucketindex: Integer;
    ilastindex: Integer;
    inextindex: Integer;
    imaxcount: Integer;
    igrowfactor: Integer;
    procedure extendList();
  public
    constructor Create; overload;
    constructor Create(iindex: Integer); overload;
    destructor Destroy; override;
    function addNode(pnode: PPLHashNode = nil): Integer; overload;
    function addNode(ihash: Cardinal; pskey: PAnsiString; ppointer: Pointer): Integer; overload;
    procedure setValue(ihash: Cardinal; ppointer: Pointer); overload;
    procedure setValue(pskey: PAnsiString; ppointer: Pointer); overload;
    procedure unsetIndex(iindex: Integer);
    function getNode(iindex: Integer): PPLHashNode; overload;
    function searchNode(ihash: Cardinal): PPLHashNode; overload;
    function searchNode(pskey: PAnsiString): PPLHashNode; overload;
    function searchValue(ihash: Cardinal): Pointer; overload;
    function searchValue(pskey: PAnsiString): Pointer; overload;
    function searchIndex(ihash: Cardinal): Integer; overload;
    function searchIndex(pskey: PAnsiString): Integer; overload;
    function getLastIndex(): Integer;
    function getCount(): Integer;
  end;


  TPLObjectHashList = class
  protected
    arrbuckets: array of TObject;
    pcurrentnode: PPLHashNode;
    ikeycount: Integer;
    imaxkeycount: Integer;
    ibucketcount: Integer;
    igrowfactor: Integer;
    iloadfactor: Integer;
    procedure extendList(brebuild: Boolean = True); virtual;
    procedure rebuildList();
    function computeHash(pskey: PAnsiString): Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure setValue(const skey: String; ppointer: Pointer); virtual;
    function getValue(const skey: String): Pointer; virtual;
    function hasKey(const skey: String): Boolean;
    function moveFirst(): Boolean;
    function moveNext(): Boolean;
    function getCurrentKey(): String;
    function getCurrentValue(): Pointer; virtual;
    function getCount(): Integer;
  end;

implementation
  (*==========================================================================*)
  (* Class TPLHashNodeList *)


  constructor TPLHashNodeList.Create;
  begin
    self.ibucketindex := -1;
    self.ilastindex := 0;
    self.inextindex := 0;
    self.imaxcount := 3;
    self.igrowfactor := 2;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  constructor TPLHashNodeList.Create(iindex: Integer);
  begin
    self.ibucketindex := iindex;
    self.ilastindex := 0;
    self.inextindex := 0;
    self.imaxcount := 3;
    self.igrowfactor := 2;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  destructor TPLHashNodeList.Destroy;
  var
    ind: Integer;
  begin
    for ind := 0 to self.imaxcount - 1 do
    begin
      if self.arrnodes[ind] <> nil then
      begin
        Dispose(self.arrnodes[ind]);
      end;
    end; //for ind := 0 to self.imaxcount - 1 do

    SetLength(self.arrnodes, 0);

    inherited Destroy;
  end;

  procedure TPLHashNodeList.extendList();
  begin
    self.imaxcount := self.imaxcount + self.igrowfactor;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  function TPLHashNodeList.addNode(pnode: PPLHashNode = nil): Integer;
  begin
    if self.inextindex >= self.imaxcount then self.extendList;

    if pnode = nil then
    begin
      New(pnode);

      //Initialize Pointer
      pnode^.pvalue := nil;
    end; //if pnode = nil then

    self.ilastindex := self.inextindex;
    self.arrnodes[self.ilastindex] := pnode;

    pnode^.ibucketindex := self.ibucketindex;
    pnode^.inodeindex := self.ilastindex;

    Result := self.ilastindex;

    inc(self.inextindex);
  end;

  function TPLHashNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; ppointer: Pointer): Integer;
  var
    plstnd: PPLHashNode;
  begin
    //Create New Node
    New(plstnd);

    //Initialize Values
    plstnd^.ihash := ihash;
    plstnd^.skey := pskey^;
    plstnd^.pvalue := ppointer;

    Result := self.addNode(plstnd);
  end;

  procedure TPLHashNodeList.setValue(ihash: Cardinal; ppointer: Pointer);
  var
    plstnd: PPLHashNode;
  begin
    plstnd := self.searchNode(ihash);

    if plstnd <> nil then plstnd^.pvalue := ppointer;
  end;

  procedure TPLHashNodeList.setValue(pskey: PAnsiString; ppointer: Pointer);
  var
    plstnd: PPLHashNode;
  begin
    plstnd := self.searchNode(pskey);

    if plstnd <> nil then plstnd^.pvalue := ppointer;
  end;

  procedure TPLHashNodeList.unsetIndex(iindex: Integer);
  begin
    if (iindex > -1)
      and (iindex < self.imaxcount) then
    begin
      self.arrnodes[iindex] := nil;
    end;
  end;

  function TPLHashNodeList.getNode(iindex: Integer): PPLHashNode;
  begin
    Result := nil;

    if (iindex > -1)
      and (iindex <= self.ilastindex) then
    begin
      Result := self.arrnodes[iindex];
    end;
  end;

  function TPLHashNodeList.searchNode(ihash: Cardinal): PPLHashNode;
  var
    ind: Integer;
  begin
    Result := nil;
    ind := 0;

    repeat  //while Result = nil and ind = self.inodelast;
      if self.arrnodes[ind] <> nil then
      begin
        if self.arrnodes[ind]^.ihash = ihash then Result := self.arrnodes[ind];
      end;

      inc(ind);
    until (Result <> nil)
      or (ind > self.ilastindex);

  end;

  function TPLHashNodeList.searchNode(pskey: PAnsiString): PPLHashNode;
  var
    ind: Integer;
  begin
    Result := nil;
    ind := 0;

    repeat  //while Result = nil and ind = self.inodelast;
      if self.arrnodes[ind] <> nil then
      begin
        if self.arrnodes[ind]^.skey = pskey^ then Result := self.arrnodes[ind];
      end;

      inc(ind);
    until (Result <> nil)
      or (ind > self.ilastindex);

  end;

  function TPLHashNodeList.searchValue(ihash: Cardinal): Pointer;
  var
    plstnd: PPLHashNode;
  begin
    Result := nil;
    plstnd := self.searchNode(ihash);

    if plstnd <> nil then Result := plstnd^.pvalue;
  end;

  function TPLHashNodeList.searchValue(pskey: PAnsiString): Pointer;
  var
    plstnd: PPLHashNode;
  begin
    Result := nil;
    plstnd := self.searchNode(pskey);

    if plstnd <> nil then Result := plstnd^.pvalue;
  end;

  function TPLHashNodeList.searchIndex(ihash: Cardinal): Integer;
  var
    plstnd: PPLHashNode;
  begin
    Result := -1;
    plstnd := self.searchNode(ihash);

    if plstnd <> nil then Result := plstnd^.inodeindex;
  end;

  function TPLHashNodeList.searchIndex(pskey: PAnsiString): Integer;
  var
    plstnd: PPLHashNode;
  begin
    Result := -1;
    plstnd := self.searchNode(pskey);

    if plstnd <> nil then Result := plstnd^.inodeindex;
  end;

  function TPLHashNodeList.getLastIndex(): Integer;
  begin
    Result := self.ilastindex;
  end;

  function TPLHashNodeList.getCount(): Integer;
  begin
    Result := self.ilastindex + 1;
  end;



  (*==========================================================================*)
  (* Class TPLObjectHashList *)


  constructor TPLObjectHashList.Create;
  begin
    self.ikeycount := 0;
    self.imaxkeycount := 0;
    self.ibucketcount := 0;
    self.igrowfactor := 3;
    self.iloadfactor := 3;
    self.pcurrentnode := nil;

    //Create the Buckets
    self.extendList(False);
  end;

  destructor TPLObjectHashList.Destroy;
  var
    ibkt: Integer;
  begin
    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      self.arrbuckets[ibkt].Free;
    end;

    SetLength(self.arrbuckets, 0);

    inherited Destroy;
  end;

  procedure TPLObjectHashList.extendList(brebuild: Boolean = True);
  var
    ibkt: Integer;
  begin
    self.ibucketcount := self.ibucketcount + self.igrowfactor;

    //Forcing a uneven Bucket Count
    if (self.ibucketcount mod 2) = 0 then
      dec(self.ibucketcount);

    self.imaxkeycount := self.ibucketcount * self.iloadfactor;

    SetLength(self.arrbuckets, self.ibucketcount);

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      if self.arrbuckets[ibkt] = nil then self.arrbuckets[ibkt] := TPLHashNodeList.Create(ibkt);
    end;

    if brebuild then
      //Reindex the Nodes
      self.rebuildList();

  end;

(*
  # Return the hashed value of a string: $hash = perlhash("key")
  # (Defined by the PERL_HASH macro in hv.h)
  sub perlhash
    {
      $hash = 0;
      foreach (split //, shift) {
          $hash = $hash*33 + ord($_);
      }
      return $hash;
  }

  /* djb2
   * This algorithm was first reported by Dan Bernstein
   * many years ago in comp.lang.c
   */
  unsigned long hash(unsigned char *str)
  {
      unsigned long hash = 5381;
      int c;
      while (c = *str++) hash = ((hash << 5) + hash) + c; // hash*33 + c
      return hash;
  }

  /* This algorithm was created for the sdbm (a reimplementation of ndbm)
   * database library and seems to work relatively well in scrambling bits
   */
  static unsigned long sdbm(unsigned char *str)
  {
      unsigned long hash = 0;
      int c;
      while (c = *str++) hash = c + (hash << 6) + (hash << 16) - hash;
      return hash;
  }
*)
  function TPLObjectHashList.computeHash(pskey: PAnsiString): Cardinal;
  var
    arrbts: TBytes;
    ibt, ibtcnt: Integer;
  begin
    Result := 0;

    if pskey <> nil then
    begin
      arrbts := BytesOf(pskey^);
      ibtcnt := Length(arrbts);

      for ibt := 0 to ibtcnt -1 do
      begin
        Result := arrbts[ibt] + (Result << 6) + (Result << 16) - Result;
      end;
    end;  //if pskey <> nil then
  end;

  procedure TPLObjectHashList.setValue(const skey: String; ppointer: Pointer);
  var
    plstnd: PPLHashNode;
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    //Build the Hash Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    plstnd := TPLHashNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh);

    if plstnd = nil then
    begin
      //Add a New Node

      if self.ikeycount = self.imaxkeycount then
      begin
        self.extendList();

        //Recompute Bucket Index
        ibktidx := ihsh mod self.ibucketcount;
      end;  //if self.ikeycount = self.imaxkeycount then

      TPLHashNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, ppointer);

      inc(self.ikeycount);
    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      plstnd^.pvalue := ppointer;
    end;  //if plstnd = nil then
  end;

  procedure TPLObjectHashList.rebuildList();
  var
    plstnd: PPLHashNode;
    ibktnwidx, ibktidx, indidx, indlstidx: Integer;
  begin
    for ibktidx := 0 to self.ibucketcount -1 do
    begin
      indlstidx := TPLHashNodeList(self.arrbuckets[ibktidx]).getLastIndex();

      for indidx := 0 to indlstidx do
      begin
        plstnd := TPLHashNodeList(self.arrbuckets[ibktidx]).getNode(indidx);
        ibktnwidx := -1;

        if plstnd <> nil then
        begin
          ibktnwidx := plstnd^.ihash mod self.ibucketcount;
        end;

        if (ibktnwidx > -1)
          and (ibktnwidx <> ibktidx) then
        begin
          TPLHashNodeList(self.arrbuckets[ibktidx]).unsetIndex(indidx);
          TPLHashNodeList(self.arrbuckets[ibktnwidx]).addNode(plstnd);
        end;  //if (ibktnwidx > -1) and (ibktnwidx <> ibktidx) then
      end; //for indidx := 0 to indlstidx do
    end;  //for ibktidx := 0 to self.ibucketcount -1 do
  end;

  function TPLObjectHashList.getValue(const skey: String): Pointer;
  var
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    //Compute Bucket Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    //Search the Hash within the Bucket
    Result := TPLHashNodeList(self.arrbuckets[ibktidx]).searchValue(ihsh);
  end;

  function TPLObjectHashList.hasKey(const skey: String): Boolean;
  var
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    Result := False;

    //Compute Bucket Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    if TPLHashNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh) <> nil then
      Result := True;

  end;

  function TPLObjectHashList.moveFirst(): Boolean;
  var
    ibkt, indidx, indlstidx: Integer;
  begin
    Result := False;
    self.pcurrentnode := nil;
    ibkt := 0;

    repeat //until (self.pcurrentnode <> nil) or (ibkt >= self.ibucketcount);
      indlstidx := TPLHashNodeList(self.arrbuckets[ibkt]).getLastIndex();
      indidx := 0;

      repeat
        self.pcurrentnode := TPLHashNodeList(self.arrbuckets[ibkt]).getNode(indidx);

        inc(indidx);
      until (self.pcurrentnode <> nil)
        or (indidx > indlstidx);

      inc(ibkt);
    until (self.pcurrentnode <> nil)
      or (ibkt >= self.ibucketcount);

    if self.pcurrentnode <> nil then
      Result := True;

  end;

  function TPLObjectHashList.moveNext(): Boolean;
  var
    plstnd: PPLHashNode;
    ibktidx, indidx, indlstidx: Integer;
  begin
    Result := False;

    if self.pcurrentnode <> nil then
    begin
      plstnd := nil;
      ibktidx := self.pcurrentnode^.ibucketindex;
      indidx := self.pcurrentnode^.inodeindex;

      if ibktidx < self.ibucketcount then
      begin
        repeat  //until (plstnd <> nil) or (ibktidx >= self.ibucketcount);
          indlstidx := TPLHashNodeList(self.arrbuckets[ibktidx]).getLastIndex();

          repeat  //until (plstnd <> nil) or (indidx > indlstidx);
            inc(indidx);

            plstnd := TPLHashNodeList(self.arrbuckets[ibktidx]).getNode(indidx);
          until (plstnd <> nil)
            or (indidx > indlstidx);

          if plstnd = nil then
          begin
            //Check the Next Bucket
            inc(ibktidx);
            indidx := -1;
          end;  //if plstnd = nil then

        until (plstnd <> nil)
          or (ibktidx >= self.ibucketcount);

        self.pcurrentnode := plstnd;

        if self.pcurrentnode <> nil then
          Result := True;

      end;  //if ibktidx < self.ibucketcount then
    end
    else  //The Current Node is not set
    begin
      Result := self.moveFirst();
    end;  //if self.pcurrentnode <> nil then
  end;

  function TPLObjectHashList.getCurrentKey(): String;
  begin
    Result := '';

    if self.pcurrentnode <> nil then
      Result := self.pcurrentnode^.skey;

  end;

  function TPLObjectHashList.getCurrentValue(): Pointer;
  begin
    Result := nil;

    if self.pcurrentnode <> nil then
      Result := self.pcurrentnode^.pvalue;

  end;

  function TPLObjectHashList.getCount(): Integer;
  begin
    Result := self.ikeycount;
  end;

end.

