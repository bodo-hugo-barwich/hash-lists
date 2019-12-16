unit objecthash;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, pointerhash;

type
  TPLObjectNodeList = class(TPLPointerNodeList)
  protected
    bowned: Boolean;
  public
    constructor Create; override; overload;
    constructor Create(iindex: Integer); override; overload;
    destructor Destroy; override;
    procedure setOwned(bisowned: Boolean);
    function addNode(ihash: Cardinal; pskey: PAnsiString; value: TObject): Integer; overload;
    function removeNode(ihash: Cardinal; pskey: PAnsiString): Boolean; override; overload;
    function removeNode(pskey: PAnsiString): Boolean; override; overload;
    procedure Clear; override;
    function isOwned(): Boolean;
  end;

  TPLObjectHashList = class(TPLPointerHashList)
  protected
    bowned: Boolean;
    procedure extendList(brebuild: Boolean = True); override;
  public
    procedure setValue(const skey: String; value: TObject); overload;
  end;


implementation
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

  function TPLObjectNodeList.addNode(ihash: Cardinal; pskey: PAnsiString; value: TObject): Integer;
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

  function TPLObjectNodeList.isOwned(): Boolean;
  begin
    Result := self.bowned;
  end;



  (*==========================================================================*)
  (* Class TPLObjectHashList *)


  procedure TPLObjectHashList.extendList(brebuild: Boolean = True);
  var
    ibkt: Integer;
  begin
    self.ibucketcount := self.ibucketcount + self.igrowfactor;
    self.imaxkeycount := (self.ibucketcount * 3) - 1;

    SetLength(self.arrbuckets, self.ibucketcount);

    for ibkt := 0 to self.ibucketcount - 1 do
    begin
      //Create the Buckets as TPLObjectNodeList
      if self.arrbuckets[ibkt] = nil then self.arrbuckets[ibkt] := TPLObjectNodeList.Create(ibkt);
    end;

    if brebuild then
      //Reindex the Nodes
      self.rebuildList();

  end;


  procedure TPLObjectHashList.setValue(const skey: String; value: TObject);
  var
    ihsh: Cardinal;
    ibktidx, indidx: Integer;
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

      indidx := TPLObjectNodeList(self.arrbuckets[ibktidx]).addNode(ihsh, @skey, TObject(value));
      self.psearchednode := TPLObjectNodeList(self.arrbuckets[ibktidx]).getNode(indidx);

      inc(self.ikeycount);
    end
    else  //The Key is already in the List
    begin
      //Update the Node Value

      TObject(self.psearchednode^.pvalue) := value;
    end;  //if self.psearchednode = nil then
  end;

end.

