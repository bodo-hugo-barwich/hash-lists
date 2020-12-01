unit pointerhash;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  PPLHashNode = ^TPLHashNode;
  TPLHashNode = record
    ibucketindex: Integer;
    inodeindex: Integer;
    ihash: Cardinal;
    skey: String;
    pvalue: Pointer;
  end;

  TPLPointerNodeList = class
  protected
    arrnodes: array of PPLHashNode;
    ibucketindex: Integer;
    ilastindex: Integer;
    inextindex: Integer;
    inodecount: Integer;
    imaxcount: Integer;
    igrowfactor: Integer;
    procedure extendList;
  public
    constructor Create; virtual; overload;
    constructor Create(iindex: Integer); virtual; overload;
    constructor Create(iindex: Integer; ifactor: Integer); virtual; overload;
    destructor Destroy; override;
    procedure setGrowFactor(ifactor: Integer);
    function addNode(pnode: PPLHashNode = nil): PPLHashNode; overload;
    function addNode(ihash: Cardinal; pskey: PAnsiString; ppointer: Pointer): PPLHashNode; overload;
    procedure setValue(ihash: Cardinal; pskey: PAnsiString; ppointer: Pointer); overload;
    procedure setValue(pskey: PAnsiString; ppointer: Pointer); overload;
    procedure unsetIndex(iindex: Integer);
    function removeNode(pnode: PPLHashNode = nil): Boolean; overload;
    function removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean; virtual; overload;
    function removeNode(pskey: PAnsiString): Boolean; virtual; overload;
    procedure reindexList;
    procedure Pack;
    procedure Clear; virtual;
    function getNode(iindex: Integer): PPLHashNode; overload;
    function searchNode(ihash: Cardinal; pskey: PAnsiString): PPLHashNode; overload;
    function searchNode(pskey: PAnsiString): PPLHashNode; overload;
    function searchValue(ihash: Cardinal; pskey: PAnsiString): Pointer; overload;
    function searchValue(pskey: PAnsiString): Pointer; overload;
    function searchIndex(ihash: Cardinal; pskey: PAnsiString): Integer; overload;
    function searchIndex(pskey: PAnsiString): Integer; overload;
    function getLastIndex(): Integer;
    property GrowFactor: Integer read igrowfactor write setGrowFactor;
  end;


  TPLPointerHashList = class
  protected
    arrbuckets: array of TObject;
    pcurrentnode: PPLHashNode;
    psearchednode: PPLHashNode;
    ikeycount: Integer;
    imaxkeycount: Integer;
    ibucketcount: Integer;
    igrowfactor: Integer;
    iloadfactor: Integer;
    eduplicatekeys: TDuplicates;
    procedure Init(icapacity: Integer; iload: Integer); virtual;
    procedure extendList(brebuild: Boolean = True); virtual;
    procedure rebuildList(istartindex, iendindex, icount: Integer);
    class function computeHash(pskey: PAnsiString): Cardinal;
    class procedure RaiseListException(const serrormessage: String);
  public
    constructor Create; overload;
    constructor Create(icapacity: Integer); overload;
    constructor Create(icapacity: Integer; iload: Integer); overload;
    destructor Destroy; override;
    procedure setLoadFactor(ifactor: Integer);
    procedure setGrowFactor(ifactor: Integer);
    procedure setCapacity(icapacity: Integer); virtual;
    function Add(const skey: String; ppointer: Pointer): Boolean; virtual;
    procedure setValue(const skey: String; ppointer: Pointer); virtual;
    procedure removeKey(const skey: String); virtual;
    procedure Clear(); virtual;
    function getValue(const skey: String): Pointer; virtual;
    function hasKey(const skey: String): Boolean;
    function moveFirst(): Boolean;
    function moveNext(): Boolean;
    function getCurrentKey(): String;
    function getCurrentValue(): Pointer; virtual;
    property KeyData[const skey: String]: Pointer read getValue write setValue; default;
    property Duplicates: TDuplicates read eduplicatekeys write eduplicatekeys;
    property LoadFactor: Integer read iloadfactor write setLoadFactor;
    property GrowFactor: Integer read igrowfactor write setGrowFactor;
    property Capacity: Integer read imaxkeycount write setCapacity;
    property Count: Integer read ikeycount;
  end;

implementation

  uses
    sysutils, math;



  (*==========================================================================*)
  (* Class TPLPointerNodeList *)


  constructor TPLPointerNodeList.Create;
  begin
    self.ibucketindex := -1;
    self.ilastindex := 0;
    self.inextindex := 0;
    self.inodecount := 0;
    self.imaxcount := 3;
    self.igrowfactor := 2;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  constructor TPLPointerNodeList.Create(iindex: Integer);
  begin
    self.ibucketindex := iindex;
    self.ilastindex := 0;
    self.inextindex := 0;
    self.inodecount := 0;
    self.imaxcount := 3;
    self.igrowfactor := 2;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  constructor TPLPointerNodeList.Create(iindex: Integer; ifactor: Integer);
  begin
    self.ibucketindex := iindex;
    self.ilastindex := 0;
    self.inextindex := 0;
    self.inodecount := 0;
    self.imaxcount := ifactor;
    self.igrowfactor := ifactor;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  destructor TPLPointerNodeList.Destroy;
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

  procedure TPLPointerNodeList.setGrowFactor(ifactor: Integer);
  begin
    self.igrowfactor := ifactor;
  end;

  procedure TPLPointerNodeList.extendList;
  begin
    self.imaxcount := self.imaxcount + self.igrowfactor;

    SetLength(self.arrnodes, self.imaxcount);
  end;

  function TPLPointerNodeList.addNode(pnode: PPLHashNode = nil): PPLHashNode;
  begin
    if self.inextindex >= self.imaxcount then
    begin
      if self.inodecount < self.imaxcount - self.igrowfactor then
      begin
        //Make Space by Reindexing
        self.reindexList;
      end
      else
      begin
        //Get more Space by Growing
        self.extendList;
      end;
    end;  //if self.inextindex >= self.imaxcount then

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

    Result := pnode;

    inc(self.inextindex);
    inc(self.inodecount);
  end;

  function TPLPointerNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; ppointer: Pointer): PPLHashNode;
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

  procedure TPLPointerNodeList.setValue(ihash: Cardinal; pskey: PAnsiString; ppointer: Pointer);
  var
    plstnd: PPLHashNode;
  begin
    plstnd := self.searchNode(ihash, pskey);

    if plstnd <> nil then plstnd^.pvalue := ppointer;
  end;

  procedure TPLPointerNodeList.setValue(pskey: PAnsiString; ppointer: Pointer);
  var
    plstnd: PPLHashNode;
  begin
    plstnd := self.searchNode(pskey);

    if plstnd <> nil then plstnd^.pvalue := ppointer;
  end;

  procedure TPLPointerNodeList.unsetIndex(iindex: Integer);
  begin
    if (iindex > -1)
      and (iindex < self.imaxcount) then
    begin
      self.arrnodes[iindex] := nil;

      dec(self.inodecount);
    end; //if (iindex > -1) and (iindex < self.imaxcount) then
  end;

  function TPLPointerNodeList.removeNode(pnode: PPLHashNode): Boolean;
  begin
    Result := False;

    if pnode <> nil then
    begin
      self.unsetIndex(pnode^.inodeindex);
      Dispose(pnode);

      Result := True;
    end;  //if pnode <> nil then
  end;


  function TPLPointerNodeList.removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean;
  begin
    Result := self.removeNode(self.searchNode(ihash, pskey));
  end;

  function TPLPointerNodeList.removeNode(pskey: PAnsiString): Boolean;
  begin
    Result := self.removeNode(self.searchNode(pskey));
  end;

  procedure TPLPointerNodeList.reindexList;
  var
    plwnd, phgnd: PPLHashNode;
    ifridx, ilstidx: Integer;
    ind: Integer;
  begin
    plwnd := nil;
    ilstidx := self.ilastindex;
    ind := 0;

    repeat  //until ind > ilstidx;
      if self.arrnodes[ind] <> nil then
      begin
        //Keep track of the highest Node from the Beginning
        plwnd := self.arrnodes[ind];
        ifridx := -1;
      end
      else  //if self.arrnodes[ind] <> nil then
        ifridx := ind;

      if (ifridx > -1) then
      begin
        if (ifridx < ilstidx) then
        begin
          repeat
            phgnd := self.arrnodes[ilstidx];

            dec(ilstidx);
          until (phgnd <> nil)
            or (ilstidx <= ifridx);

          if phgnd <> nil then
          begin
            //Move the Node to the Free Index
            self.arrnodes[phgnd^.inodeindex] := nil;
            self.arrnodes[ifridx] := phgnd;

            phgnd^.inodeindex := ifridx;

            //The highest Node from the End is the new highest Node from the Beginning
            plwnd := phgnd;

          end;  //if phgnd <> nil then
        end; //if (ifridx < ilstidx) then
      end; //if (ifridx > -1) then

      inc(ind);
    until ind > ilstidx;

    if plwnd <> nil then
    begin
      //Set the Last Index according to the highest Node from the Beginning
      self.ilastindex := plwnd^.inodeindex;
      self.inextindex := plwnd^.inodeindex + 1;
    end
    else  //The List is empty
    begin
      self.inextindex := 0;
      self.ilastindex := 0;
    end;  //if plwnd <> nil then
  end;

  procedure TPLPointerNodeList.Pack;
  begin
    //Reorder the Nodes
    self.reindexList;

    //Shrink the List to its needed Size
    SetLength(self.arrnodes, self.inodecount);
  end;

  procedure TPLPointerNodeList.Clear;
  var
     ind: Integer;
  begin
    for ind := 0 to self.imaxcount - 1 do
    begin
     if self.arrnodes[ind] <> nil then
     begin
       Dispose(self.arrnodes[ind]);
       self.arrnodes[ind] := nil;
     end;
    end; //for ind := 0 to self.imaxcount - 1 do

    //Shrink the List to its initial Size
    SetLength(self.arrnodes, self.igrowfactor);
  end;

  function TPLPointerNodeList.getNode(iindex: Integer): PPLHashNode;
  begin
    Result := nil;

    if (iindex > -1)
      and (iindex <= self.ilastindex) then
    begin
      Result := self.arrnodes[iindex];
    end;
  end;

  function TPLPointerNodeList.searchNode(ihash: Cardinal; pskey: PAnsiString): PPLHashNode;
  var
    ind: Integer;
  begin
    Result := nil;
    ind := 0;

    repeat  //while Result = nil and ind = self.inodelast;
      if self.arrnodes[ind] <> nil then
      begin
        if (self.arrnodes[ind]^.ihash = ihash)
          and (self.arrnodes[ind]^.skey = pskey^) then
          Result := self.arrnodes[ind];

      end;  //if self.arrnodes[ind] <> nil then

      inc(ind);
    until (Result <> nil)
      or (ind > self.ilastindex);

  end;

  function TPLPointerNodeList.searchNode(pskey: PAnsiString): PPLHashNode;
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

  function TPLPointerNodeList.searchValue(ihash: Cardinal; pskey: PAnsiString): Pointer;
  var
    plstnd: PPLHashNode;
  begin
    Result := nil;
    plstnd := self.searchNode(ihash, pskey);

    if plstnd <> nil then Result := plstnd^.pvalue;
  end;

  function TPLPointerNodeList.searchValue(pskey: PAnsiString): Pointer;
  var
    plstnd: PPLHashNode;
  begin
    Result := nil;
    plstnd := self.searchNode(pskey);

    if plstnd <> nil then Result := plstnd^.pvalue;
  end;

  function TPLPointerNodeList.searchIndex(ihash: Cardinal; pskey: PAnsiString): Integer;
  var
    plstnd: PPLHashNode;
  begin
    Result := -1;
    plstnd := self.searchNode(ihash, pskey);

    if plstnd <> nil then Result := plstnd^.inodeindex;
  end;

  function TPLPointerNodeList.searchIndex(pskey: PAnsiString): Integer;
  var
    plstnd: PPLHashNode;
  begin
    Result := -1;
    plstnd := self.searchNode(pskey);

    if plstnd <> nil then Result := plstnd^.inodeindex;
  end;

  function TPLPointerNodeList.getLastIndex(): Integer;
  begin
    Result := self.ilastindex;
  end;



  (*==========================================================================*)
  (* Class TPLPointerHashList *)



  //----------------------------------------------------------------------------
  //Constructors


  constructor TPLPointerHashList.Create;
  begin
    Self.Init(9, 3);
  end;

  constructor TPLPointerHashList.Create(icapacity: Integer);
  begin
    Self.Init(icapacity, 3);
  end;

  constructor TPLPointerHashList.Create(icapacity: Integer; iload: Integer);
  begin
    Self.Init(icapacity, iload);
  end;

  destructor TPLPointerHashList.Destroy;
  var
    ibkt: Integer;
  begin
    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      Self.arrbuckets[ibkt].Free;
    end;

    SetLength(Self.arrbuckets, 0);

    inherited Destroy;
  end;



  //----------------------------------------------------------------------------
  //Administration Methods


  procedure TPLPointerHashList.Init(icapacity: Integer; iload: Integer);
  begin
    Self.ikeycount := 0;
    Self.imaxkeycount := 0;
    Self.igrowfactor := 3;
    Self.iloadfactor := iload;
    Self.ibucketcount := ceil(icapacity / Self.iloadfactor) - Self.igrowfactor;
    Self.pcurrentnode := nil;
    Self.psearchednode := nil;

    Self.eduplicatekeys := dupAccept;

    if Self.ibucketcount < 0 then
      Self.ibucketcount := 0;

    //Create the Buckets
    self.extendList(False);
  end;

  procedure TPLPointerHashList.setLoadFactor(ifactor: Integer);
  var
    ibkt: Integer;
  begin
    self.iloadfactor := ifactor;

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      TPLPointerNodeList(self.arrbuckets[ibkt]).GrowFactor := self.iloadfactor;
    end;  //for ibkt := 0 to self.ibucketcount - 1 do
  end;

  procedure TPLPointerHashList.setGrowFactor(ifactor: Integer);
  begin
    self.igrowfactor := ifactor;
  end;

  procedure TPLPointerHashList.setCapacity(icapacity: Integer);
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

        for ibkt := 0 to Self.ibucketcount - 1 do
        begin
          if self.arrbuckets[ibkt] = nil then
          begin
            self.arrbuckets[ibkt] := TPLPointerNodeList.Create(ibkt, self.iloadfactor);

            brbld := True;
          end;  //if self.arrbuckets[ibkt] = nil then
        end;  //for ibkt := 0 to self.ibucketcount - 1 do

        if brbld then
          //Reindex the Nodes
          self.rebuildList(0, Self.ibucketcount - 1, Self.ibucketcount);

      end;  //if floor(self.imaxkeycount / self.iloadfactor) > self.ibucketcount then
    end; //if icapacity > self.imaxkeycount then
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

  Both Algorithms create an Integer Overflow Error and Range Check Error
  Enabling Overflow Check with crash the Programm
  Thus the generated Hash is not guaranteed to be unique.

*)
  class function TPLPointerHashList.computeHash(pskey: PAnsiString): Cardinal;
  var
    pchr, pchrend: PChar;
  begin
    {$Q-}{$R-}

    Result := 0;

    if pskey <> nil then
    begin
      pchr := PChar(pskey^);
      pchrend := pchr + Length(pskey^);

      while pchr < pchrend do
      begin
        //Result := ((Result << 5) + Result) + ord(pchr^);
        Result := ord(pchr^) + (Result << 6) + (Result << 16) - Result;

        inc(pchr);
      end;  //while pchr < pchrend do
    end;  //if pskey <> nil then

    {$Q+}{$R+}
  end;

class procedure TPLPointerHashList.RaiseListException(const serrormessage: String);
begin
  raise EStringListError.Create(serrormessage);
end;

function TPLPointerHashList.Add(const skey: String; ppointer: Pointer): Boolean;
var
  ihsh: Cardinal;
  ibktidx: Integer;
begin
  Result := False;

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
    self.psearchednode := TPLPointerNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

  if self.psearchednode = nil then
  begin
    //Add a New Node

    if self.ikeycount = self.imaxkeycount then
    begin
      self.extendList();

      //Recompute Bucket Index
      ibktidx := ihsh mod self.ibucketcount;
    end;  //if self.ikeycount = self.imaxkeycount then

    self.psearchednode := TPLPointerNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, ppointer);

    inc(self.ikeycount);

    //The Value was successfully added
    Result := True;
  end
  else  //The Key is already in the List
  begin
    case Self.eduplicatekeys of
      //Update the Node Value
      dupAccept:
      begin
        self.psearchednode^.pvalue := ppointer;

        //The Value was successfully added
        Result := True;
      end;
      dupError: RaiseListException('Key ' + chr(39) + skey + chr(39) + ': key does already exist!');
      dupIgnore: ;
    end;  //case Self.eduplicatekeys of
  end;  //if self.psearchednode = nil then
end;


  procedure TPLPointerHashList.setValue(const skey: String; ppointer: Pointer);
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
      self.psearchednode := TPLPointerNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

    if self.psearchednode = nil then
    begin
      //Add a New Node

      if self.ikeycount = self.imaxkeycount then
      begin
        self.extendList();

        //Recompute Bucket Index
        ibktidx := ihsh mod self.ibucketcount;
      end;  //if self.ikeycount = self.imaxkeycount then

      self.psearchednode := TPLPointerNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, ppointer);

      inc(self.ikeycount);
    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      self.psearchednode^.pvalue := ppointer;
    end;  //if self.psearchednode = nil then
  end;

  procedure TPLPointerHashList.removeKey(const skey: String);
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

    if self.psearchednode <> nil then
    begin
      if TPLPointerNodeList(self.arrbuckets[ibktidx]).removeNode(self.psearchednode) then
      begin
        self.psearchednode := nil;

        dec(Self.ikeycount);
      end;  //if TPLPointerNodeList(self.arrbuckets[ibktidx]).removeNode(self.psearchednode) then
    end
    else  //Searched Node does not match
    begin
      if TPLPointerNodeList(self.arrbuckets[ibktidx]).removeNode(ihsh, @skey) then
        dec(Self.ikeycount);

    end;  //if self.psearchednode <> nil then
  end;

  procedure TPLPointerHashList.extendList(brebuild: Boolean = True);
  var
    ibkt: Integer;
  begin
    if Self.ibucketcount = 0 then
      Self.ibucketcount := Self.igrowfactor
    else if Self.ibucketcount < 256 / Self.iloadfactor then
      Self.ibucketcount := Self.ibucketcount * Self.igrowfactor
    else
      inc(Self.ibucketcount, Self.ibucketcount DIV Self.igrowfactor);

    //Forcing a uneven Bucket Count
    if (self.ibucketcount mod 2) = 0 then
      dec(self.ibucketcount);

    self.imaxkeycount := self.ibucketcount * self.iloadfactor;

    //Extend the Bucket List
    SetLength(self.arrbuckets, self.ibucketcount);

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      //Create missing Node Lists
      if self.arrbuckets[ibkt] = nil then
      begin
        self.arrbuckets[ibkt] := TPLPointerNodeList.Create(ibkt, self.iloadfactor);
      end;  //if self.arrbuckets[ibkt] = nil then
    end;  //for ibkt := 0 to self.ibucketcount - 1 do

    if brebuild then
      //Reindex the Nodes
      self.rebuildList(0, Self.ibucketcount - 1, Self.ibucketcount);

  end;

  procedure TPLPointerHashList.rebuildList(istartindex, iendindex, icount: Integer);
  var
    plstnd: PPLHashNode;
    ibktnwidx, ibktidx, indidx, indlstidx: Integer;
  begin
    for ibktidx := istartindex to iendindex do
    begin
      indlstidx := TPLPointerNodeList(self.arrbuckets[ibktidx]).getLastIndex();

      for indidx := 0 to indlstidx do
      begin
        plstnd := TPLPointerNodeList(self.arrbuckets[ibktidx]).getNode(indidx);
        ibktnwidx := -1;

        if plstnd <> nil then
        begin
          ibktnwidx := plstnd^.ihash mod icount;
        end;

        if (ibktnwidx > -1)
          and (ibktnwidx <> ibktidx) then
        begin
          TPLPointerNodeList(self.arrbuckets[ibktidx]).unsetIndex(indidx);
          TPLPointerNodeList(self.arrbuckets[ibktnwidx]).addNode(plstnd);
        end;  //if (ibktnwidx > -1) and (ibktnwidx <> ibktidx) then
      end; //for indidx := 0 to indlstidx do
    end;  //for ibktidx := istartindex to iendindex do
  end;

  procedure TPLPointerHashList.Clear();
  var
    ibkt: Integer;
  begin
    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      TPLPointerNodeList(self.arrbuckets[ibkt]).Clear;

      if ibkt >= self.igrowfactor then
        self.arrbuckets[ibkt].Free;

    end;  //for ibkt := 0 to self.ibucketcount - 1 do

    //Shrink the List to its initial Size
    SetLength(self.arrbuckets, self.igrowfactor);
  end;

  function TPLPointerHashList.getValue(const skey: String): Pointer;
  var
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    Result := nil;

    //Compute Bucket Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    if self.psearchednode <> nil then
    begin
      if not ((self.psearchednode^.ihash = ihsh)
        and (self.psearchednode^.skey = skey)) then
        self.psearchednode := nil;

    end; //if self.psearchednode <> nil then

    if self.psearchednode = nil then
      //Search the Hash within the Bucket
      self.psearchednode := TPLPointerNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

    if self.psearchednode <> nil then
      Result := self.psearchednode^.pvalue;

  end;

  function TPLPointerHashList.hasKey(const skey: String): Boolean;
  var
    ihsh: Cardinal;
    ibktidx: Integer;
  begin
    Result := False;

    //Compute Bucket Index
    ihsh := computeHash(@skey);
    ibktidx := ihsh mod self.ibucketcount;

    if self.psearchednode <> nil then
    begin
      if not ((self.psearchednode^.ihash = ihsh)
        and (self.psearchednode^.skey = skey)) then
        self.psearchednode := nil;

    end; //if self.psearchednode <> nil then

    if self.psearchednode = nil then
      //Search the Hash within the Bucket
      self.psearchednode := TPLPointerNodeList(self.arrbuckets[ibktidx]).searchNode(ihsh, @skey);

    if self.psearchednode <> nil then
      Result := True;

  end;

  function TPLPointerHashList.moveFirst(): Boolean;
  var
    ibkt, indidx, indlstidx: Integer;
  begin
    Result := False;
    self.pcurrentnode := nil;
    ibkt := 0;

    repeat //until (self.pcurrentnode <> nil) or (ibkt >= self.ibucketcount);
      indlstidx := TPLPointerNodeList(self.arrbuckets[ibkt]).getLastIndex();
      indidx := 0;

      repeat
        self.pcurrentnode := TPLPointerNodeList(self.arrbuckets[ibkt]).getNode(indidx);

        inc(indidx);
      until (self.pcurrentnode <> nil)
        or (indidx > indlstidx);

      inc(ibkt);
    until (self.pcurrentnode <> nil)
      or (ibkt >= self.ibucketcount);

    if self.pcurrentnode <> nil then
      Result := True;

  end;

  function TPLPointerHashList.moveNext(): Boolean;
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
          indlstidx := TPLPointerNodeList(self.arrbuckets[ibktidx]).getLastIndex();

          repeat  //until (plstnd <> nil) or (indidx > indlstidx);
            inc(indidx);

            plstnd := TPLPointerNodeList(self.arrbuckets[ibktidx]).getNode(indidx);
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



  //----------------------------------------------------------------------------
  //Consultation Methods


  function TPLPointerHashList.getCurrentKey(): String;
  begin
    Result := '';

    if self.pcurrentnode <> nil then
      Result := self.pcurrentnode^.skey;

  end;

  function TPLPointerHashList.getCurrentValue(): Pointer;
  begin
    Result := nil;

    if self.pcurrentnode <> nil then
      Result := self.pcurrentnode^.pvalue;

  end;

end.

