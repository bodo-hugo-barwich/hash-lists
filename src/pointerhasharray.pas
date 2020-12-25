unit pointerhasharray;

{$mode objfpc}{$H+}

interface

uses
  Classes
  , pointerhash;

type
  //==========================================================================
  // Class TPLPointerNodeList Declaration


  TPLPtrNodeArrayList = class
  protected
    arrnodes: array of PPLHashNode;
    ilastindex: Integer;
    inextindex: Integer;
    inodecount: Integer;
    imaxcount: Integer;
    igrowfactor: Integer;
    bnodesowned: Boolean;
    procedure SetGrowFactor(ifactor: Integer);
    procedure SetNode(iindex: Integer; phashnode: PPLHashNode);
    procedure ExtendList(iminimumcapacity: Integer = -1);
    function GetNode(iindex: Integer): PPLHashNode;
  public
    constructor Create; virtual; overload;
    constructor Create(icapacity: Integer); virtual; overload;
    constructor Create(icapacity: Integer; ifactor: Integer); virtual; overload;
    destructor Destroy; override;
    procedure SetNodesOwned(bisowned: Boolean); inline;
    function Add(pnode: PPLHashNode = Nil): PPLHashNode; virtual; overload;
    function Add(ihash: Cardinal; const skey: String; pvalue: Pointer): Integer; virtual; overload;
    procedure SetValue(iindex: Integer; pvalue: Pointer); virtual;
    function GetLastIndex: Integer; inline;
    function IsNodesOwned: Boolean; inline;
    property GrowFactor: Integer read igrowfactor write SetGrowFactor;
    property PNode[iindex: Integer]: PPLHashNode read GetNode write SetNode; default;
    property Count: Integer read inodecount;
    property Capacity: Integer read imaxcount;
  end;


implementation

uses
  SysUtils, math;



//==========================================================================
// Class TPLPointerNodeList Implementation



//----------------------------------------------------------------------------
//Constructors


constructor TPLPtrNodeArrayList.Create;
begin
  Self.ilastindex := 0;
  Self.inextindex := 0;
  Self.inodecount := 0;
  Self.imaxcount := 4;
  Self.igrowfactor := 4;
  Self.bnodesowned := True;

  SetLength(Self.arrnodes, Self.imaxcount);
end;

constructor TPLPtrNodeArrayList.Create(icapacity: Integer);
begin
  Self.Create;

  Self.ExtendList(icapacity);
end;

constructor TPLPtrNodeArrayList.Create(icapacity: Integer; ifactor: Integer);
begin
  Self.Create;

  Self.SetGrowFactor(ifactor);

  Self.ExtendList(icapacity);
end;

destructor TPLPtrNodeArrayList.Destroy;
var
  ind: Integer;
begin
  if Self.bnodesowned then
    for ind := 0 to Self.imaxcount - 1 do
      if self.arrnodes[ind] <> nil then
        Dispose(self.arrnodes[ind]);

  SetLength(self.arrnodes, 0);

  inherited Destroy;
end;



//----------------------------------------------------------------------------
//Administration Methods


procedure TPLPtrNodeArrayList.SetGrowFactor(ifactor: Integer);
begin
  if ifactor < 2 then
    ifactor := 2;

  Self.igrowfactor := ifactor;
end;

procedure TPLPtrNodeArrayList.SetNodesOwned(bisowned: Boolean);
begin
  Self.bnodesowned := bisowned;
end;

procedure TPLPtrNodeArrayList.SetValue(const iindex: Integer; pvalue: Pointer);
var
  pnd: PPLHashNode;
begin
  pnd := Self.GetNode(iindex);

  if pnd <> Nil then
    pnd^.pvalue := pvalue;

end;

procedure TPLPtrNodeArrayList.SetNode(iindex: Integer; phashnode: PPLHashNode);
var
  pnd: PPLHashNode;
begin
  pnd := Self.GetNode(iindex);

  if pnd <> Nil then
  begin
    if Self.bnodesowned then
      Dispose(pnd);

    Self.arrnodes[iindex] := phashnode;
  end;  //if pnd <> Nil then
end;

procedure TPLPtrNodeArrayList.ExtendList(iminimumcapacity: Integer = -1);
begin
  if iminimumcapacity = -1 then
  begin
    //No Minimum Capacity is requested

    if Self.imaxcount < 256 then
      Self.imaxcount := Self.imaxcount * Self.igrowfactor
    else
      inc(Self.imaxcount, Self.imaxcount DIV Self.igrowfactor);

  end
  else  //A Capacity is requested
  begin
    //Align the requested Capacity to the Grow Factor
    Self.imaxcount := ((iminimumcapacity DIV Self.igrowfactor) + 1) * Self.igrowfactor;
  end; //if iminimumcapacity = -1 then

  //Extend the Node List
  SetLength(Self.arrnodes, Self.imaxcount);
end;

function TPLPtrNodeArrayList.Add(pnode: PPLHashNode = Nil): PPLHashNode;
begin
  if self.inextindex >= self.imaxcount then
    //Get more Space by Growing
    Self.ExtendList();

  if pnode = nil then
  begin
    New(pnode);

    //Initialize Node Structure
    pnode^.ihash := 0;
    pnode^.skey := '';
    pnode^.pvalue := nil;
    pnode^.ibucketindex := -1;
    pnode^.inodeindex := -1;
  end; //if pnode = nil then

  Self.ilastindex := Self.inextindex;
  Self.arrnodes[Self.ilastindex] := pnode;

  pnode^.iinsertindex := Self.ilastindex;

  Result := pnode;

  inc(self.inextindex);
  inc(self.inodecount);
end;

function TPLPtrNodeArrayList.Add(ihash: Cardinal; const skey: String; pvalue: Pointer): Integer;
var
  pnd: PPLHashNode;
begin
  //Create a new Node for the List
  pnd := Self.Add();

  pnd^.ihash:= ihash;
  pnd^.skey := skey;
  pnd^.pvalue := pvalue;

  //Return the Index within the List
  Result := pnd^.iinsertindex;
end;



//----------------------------------------------------------------------------
//Consultation Methods


function TPLPtrNodeArrayList.GetLastIndex: Integer;
begin
  Result := Self.ilastindex;
end;

function TPLPtrNodeArrayList.IsNodesOwned: Boolean;
begin
  Result := Self.bnodesowned;
end;

function TPLPtrNodeArrayList.GetNode(iindex: Integer): PPLHashNode;
begin
  if (iindex > -1)
    and (iindex < Self.ilastindex) then
    Result := Self.arrnodes[iindex]
  else
    Result := Nil;

end;


end.

