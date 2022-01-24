(*------------------------------------------------------------------------------
Class_LetoScriptEnv

Holds the environment for a LetoScript, such a global variables, file handles,
pragmas, and statistics. Abstracting the environment is a prerequisite for
splitting the LetoScript engine into several units, without causing dependency
loop problems (usually leading to implementation-only procedures).

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoScriptEnv;

{$I LetoScript.inc}

interface

uses
  Classes, SysUtils, StrUtils, Variants,
  Header_Leto, Header_LetoScript,
  Class_LetoFile, Class_GffField;

type

  TOutputType		= (
    otInfo, otOutput, otExit, otNotice, otHint, otWarn, otError
  );

  TLetoScriptOutputEvent = procedure(
    S			: String;
    const TextType	: TOutputType
  ) of object;


  TLetoScriptParams	= record
    Warnings		: Boolean;
    Hints		: Boolean;
    Embedded		: Boolean;
  end;


  ELetoScriptError	= class(Exception);
  ELetoScriptExit	= class(Exception);
  ELetoScriptExitStatus = class(Exception);
  ELetoScriptReturn	= class(Exception);


  TLetoScriptValType	= (
    vtValue,
    vtObj,
    vtScalar, vtList, vtArray, vtHash,
    vtKey,
    vtField,
    vtFile
  );
  TLetoScriptValTypeSet	= set of TLetoScriptValType;

  TLetoScriptEachType	= (
    etBoth, etKeys, etValues, etCommas
  );


  TLetoScriptVar	= class;
  TLetoScriptListedVar	= class;
  TLetoScriptVarList	= class;
  TLetoScriptVarTable	= class;
  TLetoScriptVarScope	= class;
  TLetoScriptFldVal	= class;
  TLetoScriptFileVal	= class;
  TLetoScriptEnv	= class;


  TLetoScriptValue	= class

  private

  protected

    FOwner		: TObject;
    FEnv		: TLetoScriptEnv;

    FValType		: TLetoScriptValType;
    FValue		: Variant;
    FDefined		: Boolean;
    FInitialized	: Boolean; // For future use
    FEvaluated		: Boolean; // For future use?

    FEach		: Cardinal;

    FFlags		: TLetoScriptValFlags;

    FLvalue		: TLetoScriptVar;
    FRvalue		: TLetoScriptValue;

    function GetValue: Variant; virtual;
    procedure SetValue(const AValue: Variant); virtual;

    function GetDefined: Boolean; virtual;
    procedure SetDefined(const ADefined: Boolean); virtual;

    function GetLvalue: TLetoScriptVar; virtual;
    procedure SetLvalue(const AValue: TLetoScriptVar); virtual;
    function GetRvalue: TLetoScriptValue; virtual;
    procedure SetRvalue(const AValue: TLetoScriptValue); virtual;

  public

    property Owner: TObject read FOwner write FOwner;
    property Env: TLetoScriptEnv read FEnv write FEnv;

    property Value: Variant read GetValue write SetValue;
    property ValType: TLetoScriptValType read FValType write FValType;
    property Defined: Boolean read GetDefined write SetDefined;
    property Initialized: Boolean read FInitialized write FInitialized;
    property Evaluated: Boolean read FEvaluated write FEvaluated;

    property Flags: TLetoScriptValFlags read FFlags write FFlags;

    property Lvalue: TLetoScriptVar read GetLvalue write SetLvalue;
    property Rvalue: TLetoScriptValue read GetRvalue write SetRvalue;

    property AsVariant: Variant read FValue; { bypasses GetValue }

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv
    ); overload; virtual;
    destructor Destroy; override;

    function AsInt: Integer; virtual;
    function AsReal: Real; virtual;
    function AsBool: Boolean; virtual;
    function AsString: String; virtual;
    function AsChar: Char; virtual;

    function AsVar: TLetoScriptVar; virtual;
    function AsList: TLetoScriptVarList; virtual;
    function AsField: TGffField; virtual;
    function AsFieldVal: TLetoScriptFldVal; virtual;
    function AsFile: TLetoFile; virtual;
    function AsFileVal: TLetoScriptFileVal; virtual;

    function IsNum: Boolean; virtual;
    function IsScalar: Boolean; virtual;
    function IsList: Boolean; virtual;
    function IsField: Boolean; virtual;
    function IsFile: Boolean; virtual;
    function IsScalarVar: Boolean; virtual;
    function IsListVar: Boolean; virtual;
    function IsKeyedVar: Boolean; virtual;
    function IsBlock: Boolean; virtual;

    procedure EachInit; virtual;
    function Each(
      const EachType	: TLetoScriptEachType = etBoth
    ): TLetoScriptValue; virtual;

    procedure CopyFrom(const Source: TLetoScriptValue); overload;

    function Undef: Boolean; virtual;

    procedure Evaluate(const Con: TLetoScriptOpCon = ocScalar); virtual;

    procedure Capture(const Source: TLetoScriptVar); virtual;

  end;


  TLetoScriptVar	= class(TLetoScriptValue)

  private

    FName		: String;

    FScope		: TLetoScriptVarScope;
    FList		: TLetoScriptVarList;
    FInternal		: Boolean;
    { Note: FInternal is also used by TLetoScriptObj in Preprocess. }

  protected

//    function GetDefined: Boolean; virtual; override;
    procedure SetDefined(const ADefined: Boolean); override;

    function GetScope: TLetoScriptVarScope; virtual;
    procedure SetScope(const AScope: TLetoScriptVarScope); virtual;

    function GetList: TLetoScriptVarList; virtual;

    function GetInternal: Boolean; virtual;
    procedure SetInternal(const AInternal: Boolean); virtual;

  public

    property Name: String read FName;

    property Scope: TLetoScriptVarScope read GetScope write SetScope;
    property ParentList: TLetoScriptVarList read GetList;

    property Internal: Boolean read GetInternal write SetInternal;

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv;
      const AName	: String
    ); overload;
    destructor Destroy; override;

    function IsScalarVar: Boolean; override;
    function IsListVar: Boolean; override;

    function Each(
      const EachType	: TLetoScriptEachType = etBoth
    ): TLetoScriptValue; override;

    procedure CopyFrom(const Source: TLetoScriptVar); overload;

    function Undef: Boolean; override;

  end;


  TLetoScriptMaskedVar	= class(TLetoScriptVar)

  private

    FExprLen		: Integer;
    FOutside		: Boolean;

  protected

    procedure SetValue(const AValue: Variant); override;

  public

    Expr		: TLetoScriptValue;
    Offset		: Integer;
    Len			: Integer;
    Replacement		: TLetoScriptValue;

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv;
      const AName	: String;
      const AExpr	: TLetoScriptValue;
      const AOffset	: Integer;
      const ALength	: Integer;
      const AReplace	: TLetoScriptValue
    ); overload;
    destructor Destroy; override;

    procedure Diagnostic(var Err: TLetoScriptError; var Fatal: Boolean);

  end;


  TLetoScriptListedVar	= class(TLetoScriptVar)

  private

    FAddress		: Variant;

    function GetIndex: Cardinal;
    procedure SetIndex(const AIndex: Cardinal);
    function GetKey: String;
    procedure SetKey(const AKey: String);

  public

    Prev		: TLetoScriptListedVar;
    Next		: TLetoScriptListedVar;

    property Index: Cardinal read GetIndex write SetIndex;
    property Key: String read GetKey write SetKey;

    constructor CreateListed(
      const AOwner	: TLetoScriptVarList;
      const AEnv	: TLetoScriptEnv;
      Address		: Variant
    ); overload;
    destructor Destroy; override;

    function IsKeyedVar: Boolean; override;

    procedure CopyFrom(const Source: TLetoScriptListedVar); overload;

  end;

  TLetoScriptVarList	= class(TLetoScriptVar)

  private

    FFirst		: TLetoScriptListedVar;
    FLast		: TLetoScriptListedVar;
    FCount		: Cardinal;

    FListEach		: TLetoScriptListedVar;
    FEachVal		: TLetoScriptVar;

    procedure PushOne(const V: TLetoScriptListedVar);

  protected

  public

    property First: TLetoScriptListedVar read FFirst;
    property Last: TLetoScriptListedVar read FLast;
    property Count: Cardinal read FCount;

    property EachIndex: Cardinal read FEach;

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv;
      const AName	: String;
      const AType	: TLetoScriptValType
    ); overload; virtual;
    destructor Destroy; override;

    function AsInt: Integer; override;
    function AsReal: Real; override;
    function AsBool: Boolean; override;
    function AsString: String; override;
    function AsChar: Char; override;

    function Get(
      const Index	: Cardinal;
      const Autovivify	: Boolean = False
    ): TLetoScriptVar; overload;
    function Get(
      const Key		: String;
      const Autovivify	: Boolean = False
    ): TLetoScriptVar; overload;

    function Add: TLetoScriptVar; overload;
    function Add(const Key: String): TLetoScriptVar; overload;

    function Insert(
      const Index	: Cardinal;
      const Neighbor	: TLetoScriptListedVar
    ): TLetoScriptVar;

    function Unshift(
      const List	: TLetoScriptValue;
      const EachType	: TLetoScriptEachType = etBoth
    ): Integer;
    function Push(
      const List	: TLetoScriptValue;
      const EachType	: TLetoScriptEachType = etBoth
    ): Integer;

    procedure Delete(const V: TLetoScriptListedVar); overload;
    procedure Delete(const Index: Cardinal); overload;
    procedure Delete(const Key: String); overload;

    procedure Reindex(const Len: Integer);

    procedure Clear;

    procedure EachInit; override;
    function Each(
      const EachType	: TLetoScriptEachType = etBoth
    ): TLetoScriptValue; override;

    procedure CopyFrom(const Source: TLetoScriptVarList); overload;

  end;


  TLetoScriptVarTable	= class

  private

    FBuckets		: array[0..26] of TList;

    function GetIndex(const Name: String): Byte;

  public

    Env			: TLetoScriptEnv;

    constructor Create(const AEnv: TLetoScriptEnv);
    destructor Destroy; override;

    function Get(Name: String): TLetoScriptVar;

    function Add(const Name: String; const VT: TLetoScriptValType): TLetoScriptVar;

    procedure AddVar(const V: TLetoScriptVar);

    procedure Delete(Name: String);

    procedure Clear;

  end;

  TLetoScriptVarScope	= class

  private

    FOwner		: TObject;
    FEnv		: TLetoScriptEnv;

    FScalars		: TLetoScriptVarTable;
    FLists		: TLetoScriptVarTable;

    FScalarDefault	: TLetoScriptVar;
    FListDefault	: TLetoScriptVarList;

    function GetTable(const ValType: TLetoScriptValType): TLetoScriptVarTable;

  public

    property Owner: TObject read FOwner write FOwner;
    property Env: TLetoScriptEnv read FEnv write FEnv;

    property ScalarDefault: TLetoScriptVar read FScalarDefault;
    property ListDefault: TLetoScriptVarList read FListDefault;

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv
    );
    destructor Destroy; override;

    function Get(
      const Name	: String;
      const ValType	: TLetoScriptValType;
      const Autovivify	: Boolean = False;
      const Internal	: Boolean = False
    ): TLetoScriptVar; overload;
    function Get(
      Name		: String;
      const Autovivify	: Boolean = False;
      const Internal	: Boolean = False
    ): TLetoScriptVar; overload;

    function Add(
      const Name	: String;
      const ValType	: TLetoScriptValType;
      const Internal	: Boolean = False
    ): TLetoScriptVar;

    procedure Delete(
      const Name	: String;
      const ValType	: TLetoScriptValType
    ); overload;
    procedure Delete(
      Name		: String
    ); overload;

    procedure Clear(const ValType: TLetoScriptValType);
    procedure ClearAll;
    procedure PrepArgScope;

  end;


  { Glue between TGffField and TLetoScriptValue }
  TLetoScriptFldVal	= class(TLetoScriptVar)

  private

    FField		: TGffField;
    FEachVal		: TLetoScriptFldVal;
    FCascadeInt		: Integer;

    function GetField: TGffField;

    function Cascaded: Boolean;
    function NextCascaded(const Current: TGffField): TGffField;

  protected

    function GetValue: Variant; override;

    function GetDefined: Boolean; override;

  public

    property Field: TGffField read GetField write FField;

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv;
      const AField	: TGffField;
      const Path	: String
    ); overload;
    destructor Destroy; override;

    function AsInt: Integer; override;
    function AsReal: Real; override;
    function AsBool: Boolean; override;
    function AsString: String; override;

    function IsNum: Boolean; override;
    function IsList: Boolean; override;

    procedure EachInit; override;
    function Each(
      const EachType	: TLetoScriptEachType = etBoth
    ): TLetoScriptValue; override;

    function Undef: Boolean; override;

  end;

  { Glue between TLetoFile and TLetoScriptValue }
  TLetoScriptFileVal	= class(TLetoScriptVar)

  private

    FEachRes		: TLetoScriptFileVal;
    FEachField		: TLetoScriptFldVal;
    FEachKey		: TLetoScriptValue;

  protected

    function GetValue: Variant; override;

    function GetDefined: Boolean; override;

  public

    LetoFile		: TLetoFile;
    ResMask		: String;

    constructor Create(
      const AOwner	: TObject;
      const AEnv	: TLetoScriptEnv;
      const AFile	: TLetoFile;
      const Handle	: String
    ); overload;
    destructor Destroy; override;

    function AsInt: Integer; override;
    function AsReal: Real; override;
    function AsBool: Boolean; override;
    function AsString: String; override;

    function IsNum: Boolean; override;
    function IsList: Boolean; override;

    procedure EachInit; override;
    function Each(
      const EachType	: TLetoScriptEachType = etBoth
    ): TLetoScriptValue; override;

    function Undef: Boolean; override;

    function SetAttrib(const Attr, Value: String): Boolean;

  end;


  TLetoScriptEnv	= class

  private

    FName		: String;
    FOnOutput		: TLetoScriptOutputEvent;
    FPragmas		: TStringList;
    FHandles		: TStringList;
    FUseHandle		: String;
    FGlobalVars		: TLetoScriptVarScope;
    FSubs		: TStringList;

    FBI_Err		: TLetoScriptVar;
    FErrno		: TLetoScriptError;
    FIoErr		: TLetoError;

    function GetPragma(Name: String): Boolean;
    procedure SetPragma(Name: String; Value: Boolean);

    function GetHandle(const Name: String): TLetoFile;
    procedure SetHandle(const Name: String; const Value: TLetoFile);
    function GetDefHandle: TLetoFile;
    function GetLastHandle: String;

    // TODO 4: ADD: AsInt of $!, error stack in @!
    function GetErrno: TLetoScriptError;
    procedure SetErrno(const Value: TLetoScriptError);
    function GetIoErr: TLetoError;
    procedure SetIoErr(const Value: TLetoError);

  public

    Params		: TLetoScriptParams;
    OutputNL		: Boolean;
    ExprCount		: Cardinal;

    {$IFDEF MSWINDOWS}
    FormatSettings	: TFormatSettings;
    {$ENDIF}

    NilValue		: TLetoScriptValue;
    Last		: TLetoScriptValue;

    property Name: String read FName write FName;

    property OnOutput: TLetoScriptOutputEvent read FOnOutput write FOnOutput;

    property Pragma[Name: String]: Boolean read GetPragma write SetPragma;

    property Handle[const Name: String]: TLetoFile
      read GetHandle write SetHandle;
    property DefaultHandle: TLetoFile read GetDefHandle;
    property UseHandle: String read FUseHandle write FUseHandle;
    property LastHandle: String read GetLastHandle;
    property Handles: TStringList read FHandles;

    property GlobalScope: TLetoScriptVarScope read FGlobalVars;

    property Subs: TStringList read FSubs;

    { Some very common built-ins. }
    property Errno: TLetoScriptError read GetErrno write SetErrno;
    property IoErr: TLetoError read GetIoErr write SetIoErr;

    constructor Create;
    destructor Destroy; override;

    procedure Output(const S: String; const OutputType: TOutputType);

    function CloseHandle(const H: TLetoFile): TLetoScriptError; overload;
    function CloseHandle(const Index: Integer): TLetoScriptError; overload;

    procedure ClearPragmas;
    procedure ClearHandles;
    procedure ClearVars;
    procedure ClearSubs;

  end;


  TLetoScriptLib	= class;

  TLibConst		= record
    Name		: String;
    Value		: String;
  end;

  TLibItemType		= (
    itConst, itUnaryOp, itListOp, itFunc
  );

  TLibItem		= record
    Lib			: TLetoScriptLib;
    Found		: Boolean;
    ItemType		: TLibItemType;
    AsConst		: String;
    AsFn		: Word;
  end;

  TLetoScriptLib	= class

  public

    Env			: TLetoScriptEnv;
    Name		: String;
    Enabled		: Boolean;	// The whole library
    Disabled		: TStringList;	// Individual methods

    constructor Create(
      const AName	: String;
      const AEnv	: TLetoScriptEnv
    ); virtual;
    destructor Destroy; override;

    function CreateFunc(const Syntax: String): Pointer; virtual; abstract;

  end;


  TLetoScriptArgSpecs	= record
    NeedsValue		: Boolean;
    ConstDefault	: Boolean;
    NilValue		: Boolean;
    NoEval		: Boolean;
//    Greedy		: Boolean;
    Optional		: Boolean;
  end;

  TLetoScriptArgType	= (
    atScalar, atList, atField, atFile, atBlock,
    atScalarVar, atListVar
  );

  TLetoScriptArg	= class

  private

    FFunc		: TObject;
    FValue		: TLetoScriptValue;
//    FDefault		: TLetoScriptVar;
    FDefVal		: TLetoScriptValue;
    FDefValSpec		: String;

    WrapFile		: TLetoScriptFileVal;

    function GetValue: TLetoScriptValue;

    function GetDefault: TLetoScriptValue;
    procedure SetDefault(const ADef: TLetoScriptValue);

    function MakeFileWrapper: TLetoScriptValue;

  public

    Env			: TLetoScriptEnv;
    Scope		: TLetoScriptVarScope;
    Con			: TLetoScriptOpCon;
    Evaluated		: Boolean;

    ArgType		: TLetoScriptArgType;
    Name		: String;
    ArgNo		: Byte;

    Specs		: TLetoScriptArgSpecs;

    property Value: TLetoScriptValue read GetValue write FValue;

    property DefaultVal: TLetoScriptValue read GetDefault write SetDefault;
    property DefValSpec: String read FDefValSpec write FDefValSpec;

    constructor Create(const AEnv: TLetoScriptEnv; const AFunc: TObject);
    constructor CreateFrom(const Source: TLetoScriptArg);
    destructor Destroy; override;

    function FullName: String;

    function ValueAssigned: Boolean;

    procedure MakeConstDefault(const AValue: Variant);

    procedure Reset(
      const AScope	: TLetoScriptVarScope;
      const ACon	: TLetoScriptOpCon
    );
    procedure Evaluate;

    function Describe: String;

  end;

  TLetoScriptArgs	= class

  private

    FBlank		: TLetoScriptArg;
    FFunc		: TObject;

    function GetOp: TLetoScriptOp;

    function GetArg(const Name: String): TLetoScriptArg;

    function GetValue(const Name: String): TLetoScriptValue;

  public

    Env			: TLetoScriptEnv;
    Scope		: TLetoScriptVarScope;

    Standards		: TStringList;
    Options		: TStringList;

    LastArgNo		: Byte;
    ListMode		: Boolean;

    property Op: TLetoScriptOp read GetOp;

    property Args[const Name: String]: TLetoScriptArg read GetArg;
    property Values[const Name: String]: TLetoScriptValue read GetValue; default;

    constructor Create(
      const Syntax	: String;
      const AEnv	: TLetoScriptEnv;
      const AFunc	: TObject
    );
    destructor Destroy; override;

    function AddArg(S: String; const List: TStringList): TLetoScriptArg;

    function AddValue(
      const Name	: String;
      const Value	: TLetoScriptValue
    ): TLetoScriptError;

    procedure CopyDefinition(const FromArgs: TLetoScriptArgs);

    function GetByIndex(
      const List	: TStringList;
      const Index	: Integer
    ): TLetoScriptArg;

    function First: TLetoScriptArg;

    procedure Reset;
//    procedure Evaluate;

    procedure Clear;

    function Describe: String;

  end;

  TLetoScriptAttribs	= class

  private

  public

    UnaryOp		: Boolean;
    ListOp		: Boolean;
    ParamOp		: Boolean;
    AttribOp		: Boolean;

    Foldable		: Boolean;

    constructor Create(var Syntax: String);
    destructor Destroy; override;

    function AddAttrib(const Attr: String): Boolean; overload;
    procedure AddAttrib(const A: Char); overload;

    function Describe: String;

  end;


implementation

uses
  Class_LetoScriptObj, Class_LetoScriptLib, Class_LetoScriptSub;


{ TLetoScriptValue }


(*------------------------------------------------------------------------------
property Value

An old note worth keeping here:

For some unfathomable reason (Delphi 7 polymorphism bug, I suspect), removing
this virtual method (making it purely abstract) can cause variant exceptions
(invalid operation) in inheriting classes - such as:

List.Add.Value := 0.625;

Discovered and plugged 2005-02-24 (23beta3).

------------------------------------------------------------------------------*)
function TLetoScriptValue.GetValue: Variant;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.Value
  else
    Result		:= FValue;
end;

procedure TLetoScriptValue.SetValue(const AValue: Variant);
begin
  FValue		:= AValue;
  FDefined		:= True;
end;

(*------------------------------------------------------------------------------
property Defined

------------------------------------------------------------------------------*)
function TLetoScriptValue.GetDefined: Boolean;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.Defined
  else
    Result		:= FDefined;
end;

procedure TLetoScriptValue.SetDefined(const ADefined: Boolean);
begin
  FDefined		:= ADefined;
  { This seems logical - but screws up vars &c, that store name in FValue:
  if not ADefined then
    FValue		:= '';
  }
end;

(*------------------------------------------------------------------------------
property Lvalue

Variables use Lvalue to refer to the actual TLetoScriptVar that should be
modified in an assignment operation. Non-variables may also have Lvalues,
but ultimately an Lvalue should represent something that can be assigned to.

A nil Lvalue is a token that cannot be assigned to, but this is not (necessarily)
the same thing as a const.

------------------------------------------------------------------------------*)
function TLetoScriptValue.GetLvalue: TLetoScriptVar;
begin
  Result		:= FLvalue;

  { Recursive. }
  while Assigned(Result) and Assigned(Result.FLvalue) do
    Result		:= Result.FLvalue;

end;

procedure TLetoScriptValue.SetLvalue(const AValue: TLetoScriptVar);
begin
  if Assigned(FLvalue) and (FLvalue.Owner = self) then begin
    if FRvalue = FLvalue then
      FRvalue		:= nil;
    FLvalue.Free;
  end;

  FLvalue		:= AValue;
end;

(*------------------------------------------------------------------------------
property Rvalue

All TLetoScriptValue objects represent values and, without an Lvalue, are in
essence rvalues. The use of Rvalue here is for an object to point to another
object as its "actual" rvalue. This could be for three reasons:

Complexity: the rvalue of the object is a complex type (a list, a field, etc.)
and this object does not want its type changed to facilitate that data. Instead
this object points to a relevant object that holds the data.

Reference: this object is an actual reference. The Rvalue is the dereference.

State: this object is mutable, and its -current- state must be preserved as
the "value to report" before any further evaluation takes place that could
modify that value. Rvalue is a snapshot of the variable "now", and hence takes
precedence when Value is being looked up. See the Capture method.

For simple data types on non-references, Rvalue is not used. That's what Value
itself is for, after all.

------------------------------------------------------------------------------*)
function TLetoScriptValue.GetRvalue: TLetoScriptValue;
begin
  Result		:= FRvalue;
end;

procedure TLetoScriptValue.SetRvalue(const AValue: TLetoScriptValue);
begin
  if Assigned(FRvalue) and (FRvalue.Owner = self) then begin
    if FLvalue = FRvalue then
      FLvalue		:= nil;
    FRvalue.Free;
  end;

  FRvalue		:= AValue;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptValue.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv
);
begin
  Owner			:= AOwner;
  Env			:= AEnv;

  ValType		:= vtValue;
  FValue		:= '';
  FInitialized		:= False;
  FEvaluated		:= False;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptValue.Destroy;
begin
  Lvalue		:= nil;
  Rvalue		:= nil;

  inherited;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsInt: Integer;
begin
  if not Defined then
    Result		:= 0
  else
    Result		:= StrToIntDef(String(Value), 0);
end;

(*------------------------------------------------------------------------------
AsReal

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsReal: Real;
begin
  if not Defined then
    Result		:= 0
  else
    {$IFDEF MSWINDOWS}
    Result := StrToFloatDef(
      String(Value), 0, Env.FormatSettings
    );
    {$ENDIF}
    {$IFDEF LINUX}
    Result		:= StrToFloatDef(String(Value), 0);
    {$ENDIF}
end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsBool: Boolean;
var
  S			: String;
begin
  S			:= Value;
  Result		:= Defined and (S <> '') and (S <> '0');
end;

(*------------------------------------------------------------------------------
AsString

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsString: String;
begin
  if not Defined then
    Result		:= ''
  else begin
    // TODO 4: CHANGE: Boolean AsString is 1 0, not "true" "false"
    // In the meantime, fix lib functions so that Value := Int()
    {
    S			:= Lowercase(Value);
    try
      B			:= Value;
    except
      B			:= S = 'false';
    end;
    if B and (S = 'true') then
      Result		:= '1'
    else if not B and (S = 'false') then
      Result		:= '0'
    else
      Result		:= S;
    }
    Result		:= Value;
  end;
end;

(*------------------------------------------------------------------------------
AsChar

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsChar: Char;
var
  S			: String;
begin
  S			:= AsString;
  if S <> '' then
    Result		:= S[1]
  else
    Result		:= #0;
end;

(*------------------------------------------------------------------------------
AsVar

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsVar: TLetoScriptVar;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.AsVar
  else if ValType in [vtScalar, vtList, vtArray, vtHash] then
    Result		:= TLetoScriptVar(self)
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
AsList

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsList: TLetoScriptVarList;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.AsList
  else if ValType in [vtList, vtArray, vtHash] then
    Result		:= TLetoScriptVarList(self)
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
AsField

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsField: TGffField;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.AsField
  else if ValType = vtField then
    Result		:= TLetoScriptFldVal(self).Field
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
AsFldVal

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsFieldVal: TLetoScriptFldVal;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.AsFieldVal
  else if ValType = vtField then
    Result		:= TLetoScriptFldVal(self)
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
AsFile

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsFile: TLetoFile;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.AsFile
  else if ValType = vtFile then
    Result		:= TLetoScriptFileVal(self).LetoFile
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
AsFileVal

------------------------------------------------------------------------------*)
function TLetoScriptValue.AsFileVal: TLetoScriptFileVal;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;
  if Assigned(V) then
    Result		:= V.AsFileVal
  else if ValType = vtFile then
    Result		:= TLetoScriptFileVal(self)
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
IsNum

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsNum: Boolean;
begin
  Result :=
    Defined and
    (VarType(Value) and varTypeMask in [
      varSmallInt, varInteger, varSingle, varDouble,
      varShortInt, varByte, varWord, varLongWord, varInt64
    ]);
end;

(*------------------------------------------------------------------------------
IsScalar

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsScalar: Boolean;
begin
  Result		:= not(ValType in [vtList, vtArray, vtHash]);
end;

(*------------------------------------------------------------------------------
IsList

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsList: Boolean;
begin
  Result		:= ValType in [vtList, vtArray, vtHash];
end;

(*------------------------------------------------------------------------------
IsField

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsField: Boolean;
begin
  Result		:= ValType = vtField;
end;

(*------------------------------------------------------------------------------
IsFile

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsFile: Boolean;
begin
  Result		:= ValType = vtFile;
end;

(*------------------------------------------------------------------------------
IsScalarVar

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsScalarVar: Boolean;
begin
  Result		:= False;
end;

(*------------------------------------------------------------------------------
IsListVar

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsListVar: Boolean;
begin
  Result		:= False;
end;

(*------------------------------------------------------------------------------
IsKeyedVar

Allows a TLetoScriptListedVar (in a hash) to announce itself.

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsKeyedVar: Boolean;
begin
  Result		:= False;
end;

(*------------------------------------------------------------------------------
IsBlock

------------------------------------------------------------------------------*)
function TLetoScriptValue.IsBlock: Boolean;
begin
  Result		:= False;
end;

(*------------------------------------------------------------------------------
EachInit

Initializes an Each iteration.

------------------------------------------------------------------------------*)
procedure TLetoScriptValue.EachInit;
begin
  FEach			:= 0;
end;

(*------------------------------------------------------------------------------
Each

------------------------------------------------------------------------------*)
function TLetoScriptValue.Each(
  const EachType	: TLetoScriptEachType
): TLetoScriptValue;
begin
  if FEach = 0 then
    Result		:= self
  else
    Result		:= nil;

  Inc(FEach);

end;

(*------------------------------------------------------------------------------
CopyFrom

------------------------------------------------------------------------------*)
procedure TLetoScriptValue.CopyFrom(const Source: TLetoScriptValue);
begin
  FValue		:= Source.Value;
  FDefined		:= Source.Defined;
  FLvalue		:= Source.Lvalue;
  FRvalue		:= Source.Rvalue;

end;

(*------------------------------------------------------------------------------
Undef

------------------------------------------------------------------------------*)
function TLetoScriptValue.Undef: Boolean;
begin
  Defined		:= False;
  Result		:= True;
end;

(*------------------------------------------------------------------------------
Evaluate

Only some types of value require evaluation before the value is known. For
others that do not inherit this method, the value is static.

------------------------------------------------------------------------------*)
procedure TLetoScriptValue.Evaluate(const Con: TLetoScriptOpCon);
begin

end;

(*------------------------------------------------------------------------------
Capture

Makes a copy of Source and stores it in Rvalue. This is used to save the state
of some variable so that Order of Execution appears more natural. The copy
is owned and freed locally.

The copy does not pursue, but instead copies Source.Rvalue in-tact.

------------------------------------------------------------------------------*)
procedure TLetoScriptValue.Capture(const Source: TLetoScriptVar);
var
  V			: TLetoScriptVar;
begin
  V			:= TLetoScriptVar.Create(self, Source.Env, Source.Name);
  V.FScope		:= Source.FScope;
  V.FList		:= Source.FList;
  V.FInternal		:= Source.FInternal;
  V.FValType		:= Source.FValType;
  V.FValue		:= Source.FValue;
  V.FDefined		:= Source.FDefined;
  V.FLvalue		:= Source.FLvalue;
  V.FRvalue		:= Source.FRvalue;

  Rvalue		:= V;
  
end;


{ TLetoScriptVar }


(*------------------------------------------------------------------------------
property Defined

This does the work instead of Undef, because the caller won't always know or
want to call Undef (e.g., Obj.ScalarAssignment).

------------------------------------------------------------------------------*)
procedure TLetoScriptVar.SetDefined(const ADefined: Boolean);
begin
  // DONE 1: BUG: [27r3] No TLetoScriptVar.SetDefined
  if ADefined then
  else if not Internal and Assigned(FScope) then
    FScope.Delete(Name, ValType)
  else if not Internal and Assigned(FList) then
    FList.Delete( TLetoScriptListedVar(self) );
  inherited;
end;

(*------------------------------------------------------------------------------
property Scope

For a Var, this is the scope that the var is in.

For an Obj, this is an actual scope, hosted at the level of this Obj.

------------------------------------------------------------------------------*)
function TLetoScriptVar.GetScope: TLetoScriptVarScope;
begin
  Result		:= FScope;
end;

procedure TLetoScriptVar.SetScope(const AScope: TLetoScriptVarScope);
begin
  if Assigned(FScope) and (FScope.Owner = self) then
    FScope.Free;

  FScope		:= AScope;
end;

(*------------------------------------------------------------------------------
property ParentList

Named ParentList instead of just List, to avoid confusion. This is a read-only
property that indicates the VarList this Var belongs to. A value of nil means
the var isn't a member of any list.

------------------------------------------------------------------------------*)
function TLetoScriptVar.GetList: TLetoScriptVarList;
begin
  Result		:= FList;
end;

(*------------------------------------------------------------------------------
property Internal

Internal Vars are those such as $_ and @_ that are used frequently for
scratch space or specific, autovivified contextual meaning.

An Internal Var should no-op when asked to Undef. Its FDefined will become
False, but the object instance remains. This is so that the Var is not
constantly re-constructed.

------------------------------------------------------------------------------*)
function TLetoScriptVar.GetInternal: Boolean;
begin
  Result		:= FInternal;
end;

procedure TLetoScriptVar.SetInternal(const AInternal: Boolean);
begin
  FInternal		:= AInternal;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptVar.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv;
  const AName		: String
);
begin
  inherited Create(AOwner, AEnv);

  FName			:= Lowercase(AName);
  ValType		:= vtScalar;
  Value			:= '';

  FInternal		:= False;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptVar.Destroy;
begin
  Scope			:= nil;

  inherited;
end;

(*------------------------------------------------------------------------------
IsScalarVar

------------------------------------------------------------------------------*)
function TLetoScriptVar.IsScalarVar: Boolean;
begin
  Result		:= ValType = vtScalar;
end;

(*------------------------------------------------------------------------------
IsListVar

------------------------------------------------------------------------------*)
function TLetoScriptVar.IsListVar: Boolean;
begin
  Result		:= ValType in [vtList, vtArray, vtHash];
end;

(*------------------------------------------------------------------------------
Each

If this is a scalar, just fetch the value and finish iterating. If this is
a list, call the list's Each.

------------------------------------------------------------------------------*)
function TLetoScriptVar.Each(
  const EachType	: TLetoScriptEachType
): TLetoScriptValue;
begin
  Result		:= self;
  case ValType of
    vtScalar:
      if FEach = 0 then
        FEach		:= 1
      else
        Result		:= nil;
    vtList, vtArray, vtHash:
    begin
      if FEach = 0 then
        AsList.EachInit;
      FEach		:= 1;
      Result		:= AsList.Each(EachType);
    end;
  { In obscure conditions, this could be a vtValue.
    One example is Each'ing on an undefined.
  }
  else
    Result		:= nil;
  end;
end;

(*------------------------------------------------------------------------------
CopyFrom

------------------------------------------------------------------------------*)
procedure TLetoScriptVar.CopyFrom(const Source: TLetoScriptVar);
begin
  inherited CopyFrom(Source);

  FName			:= Source.FName;
  FScope		:= Source.FScope;
  FList			:= Source.FList;
  FInternal		:= Source.FInternal;

end;

(*------------------------------------------------------------------------------
Undef

Calling undef on a variable removes it from its scope.

This is the same as setting Defined to False, but is functional to tell the
caller whether the variable was Internal.

------------------------------------------------------------------------------*)
function TLetoScriptVar.Undef: Boolean;
begin
  Defined		:= False;
  Result		:= not Internal;
end;



{ TLetoScriptMaskedVar }


(*------------------------------------------------------------------------------
property Value

------------------------------------------------------------------------------*)
procedure TLetoScriptMaskedVar.SetValue(const AValue: Variant);
var
  S			: String;
  A, Ins, B		: String;
begin
  if not Assigned(Expr) or not Assigned(Expr.Lvalue) or FOutside then Exit;

  S			:= FValue;
  Ins			:= AValue;

  if Offset > 0 then
    A			:= Copy(S, 1, Offset);
  if Offset + Len < FExprLen then
    B			:= Copy(S, Offset + 1 + Len, FExprLen);

  Expr.Lvalue.Value	:= A + Ins + B;

end;

(*------------------------------------------------------------------------------
constructor

Support for Masked Lvalues added in build 27.

------------------------------------------------------------------------------*)
// DONE 1: ADD: [27] Masked lvalues
constructor TLetoScriptMaskedVar.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv;
  const AName		: String;
  const AExpr		: TLetoScriptValue;
  const AOffset		: Integer;
  const ALength		: Integer;
  const AReplace	: TLetoScriptValue
);
var
  S, Substr		: String;
  I			: Integer;
begin
  inherited Create(AOwner, AEnv, AName);

  Expr			:= AExpr;
  Offset		:= AOffset;
  Len			:= ALength;
  Replacement		:= AReplace;

  S			:= AExpr.AsString;
  FValue		:= S;
  FExprLen		:= Length(S);

  { Negative index wrapping. }
  if Offset < 0 then
    Offset		:= FExprLen + Offset;

  { Default to Length(Expr), negative index wrapping. }
  if Len = 0 then
    Len			:= FExprLen
  else if Len < 0 then
    Len			:= FExprLen - Offset + Len;

  { Note whether the substr is out of bounds. }
  FOutside		:= (Offset > FExprLen) or (Offset + Len < 0);

  { Substr. }
  if FOutside then
    FRvalue		:= Env.NilValue
  else begin
    for I := Offset+1 to Offset + Len do
      if (I > 0) and (I <= FExprLen) then
        Substr		:= Substr + S[I];
    FRvalue		:= TLetoScriptVar.Create(self, Env, AName);
    FRvalue.Value	:= Substr;
  end;

  { Replacement. }
  if Assigned(Replacement) and Replacement.Defined then
    SetValue(Replacement.AsString);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptMaskedVar.Destroy;
begin

  inherited;
end;

(*------------------------------------------------------------------------------
Diagnostic

Test the mask values for bounds or operational errors.

------------------------------------------------------------------------------*)
procedure TLetoScriptMaskedVar.Diagnostic(
  var Err		: TLetoScriptError;
  var Fatal		: Boolean
);
  procedure _Result_(const AErr: TLetoScriptError; const AFatal: Boolean);
  begin
    Err			:= AErr;
    Fatal		:= AFatal;
  end;
begin
  Err			:= LS_Success;
  Fatal			:= False;

  if Assigned(Replacement) and Replacement.Defined then begin
    if not Assigned(Expr.Lvalue) then
      _Result_(Err_LS_ReadOnlyValue, True)
    else if FOutside then
      _Result_(Err_LS_Outside, True);
  end
  else if FOutside then
    _Result_(Err_LS_Outside, False);

end;


{ TLetoScriptListedVar }


(*------------------------------------------------------------------------------
property Index

------------------------------------------------------------------------------*)
function TLetoScriptListedVar.GetIndex: Cardinal;
var
  S			: String;
begin
  S			:= FAddress;
  Result		:= StrToIntDef(S, 0);
end;

procedure TLetoScriptListedVar.SetIndex(const AIndex: Cardinal);
begin
  FAddress		:= AIndex;
end;

(*------------------------------------------------------------------------------
property Key

------------------------------------------------------------------------------*)
function TLetoScriptListedVar.GetKey: String;
begin
  Result		:= FAddress;
end;

procedure TLetoScriptListedVar.SetKey(const AKey: String);
begin
  FAddress		:= AKey;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptListedVar.CreateListed(
  const AOwner		: TLetoScriptVarList;
  const AEnv		: TLetoScriptEnv;
  Address		: Variant
);
begin
  inherited Create(AOwner, AEnv, '');
  // DONE 1: BUG: [27r3] CreateListed sets FList (sponsors bug in undef, possibly elsewhere)
  FList			:= AOwner;
  FAddress		:= Address;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptListedVar.Destroy;
begin

  inherited;
end;

(*------------------------------------------------------------------------------
IsKeyedVar

Why yes, yes I am.

------------------------------------------------------------------------------*)
function TLetoScriptListedVar.IsKeyedVar: Boolean;
begin
  Result		:= Assigned(FList) and (FList.ValType = vtHash);
end;

(*------------------------------------------------------------------------------
CopyFrom

------------------------------------------------------------------------------*)
procedure TLetoScriptListedVar.CopyFrom(const Source: TLetoScriptListedVar);
begin
  inherited CopyFrom(Source);
  FAddress		:= Source.FAddress;
end;


{ TLetoScriptVarList }


(*------------------------------------------------------------------------------
PushOne

Does the work for overloaded Add.

------------------------------------------------------------------------------*)
procedure TLetoScriptVarList.PushOne(const V: TLetoScriptListedVar);
begin

  { Setting FList shouldn't be needed any more, CreateListed does it. }
//  V.FList		:= self;

  Inc(FCount);

  if not Assigned(FFirst) then
    FFirst		:= V;
  if Assigned(FLast) then begin
    FLast.Next		:= V;
    V.Prev		:= FLast;
  end;

  FLast			:= V;

end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptVarList.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv;
  const AName		: String;
  const AType		: TLetoScriptValType
);
begin
  inherited Create(AOwner, AEnv, AName);

  ValType		:= AType;
  FCount		:= 0;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptVarList.Destroy;
begin
  Clear;

  if Assigned(FEachVal) then
    FreeAndNil(FEachVal);

  inherited;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoScriptVarList.AsInt: Integer;
begin
  Result		:= FCount;
end;

(*------------------------------------------------------------------------------
AsReal

------------------------------------------------------------------------------*)
function TLetoScriptVarList.AsReal: Real;
begin
  Result		:= FCount;
end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoScriptVarList.AsBool: Boolean;
begin
  Result		:= FCount > 0;
end;

(*------------------------------------------------------------------------------
AsString

------------------------------------------------------------------------------*)
function TLetoScriptVarList.AsString: String;
begin
  Result		:= '';
end;

(*------------------------------------------------------------------------------
AsChar

------------------------------------------------------------------------------*)
function TLetoScriptVarList.AsChar: Char;
begin
  Result		:= #0;
end;

(*------------------------------------------------------------------------------
Get

------------------------------------------------------------------------------*)
function TLetoScriptVarList.Get(
  const Index		: Cardinal;
  const Autovivify	: Boolean
): TLetoScriptVar;
var
  V			: TLetoScriptListedVar;
begin
  Result		:= nil;
  V			:= nil;

  if not Assigned(FFirst) or not Assigned(FLast) then

  else if (Index div 2) < (FLast.Index div 2) then begin
    V			:= FFirst;
    while Assigned(V) and (Index > V.Index) do
      V			:= V.Next;
  end
  else begin
    V			:= FLast;
    while Assigned(V) and (Index < V.Index) do
      V			:= V.Prev;
  end;

  if Assigned(V) and (V.Index = Index) then
    Result		:= V;

  if not Assigned(Result) and Autovivify then
    Result		:= Insert(Index, V);

end;

// TODO 3: CHANGE: Optimize VarList hashes
function TLetoScriptVarList.Get(
  const Key		: String;
  const Autovivify	: Boolean
): TLetoScriptVar;
var
  K			: String;
  V			: TLetoScriptListedVar;
begin
  Result		:= nil;
  K			:= Lowercase(Key);

  V			:= FFirst;
  while Assigned(V) and (K <> Lowercase(V.Key)) do
    V			:= V.Next;

  if Assigned(V) then
    Result		:= V
  else if Autovivify then begin
    V			:= TLetoScriptListedVar.CreateListed(self, Env, Key);
    PushOne(V);
    Result		:= V;
  end;

end;

(*------------------------------------------------------------------------------
Add

The Key version does a lookup on that key, and if it already exists, simply
returns that var. Otherwise, this arbitrarily adds a new var, pushes it,
and returns it.

------------------------------------------------------------------------------*)
function TLetoScriptVarList.Add: TLetoScriptVar;
var
  I			: Cardinal;
  V			: TLetoScriptListedVar;
begin
  if Assigned(FLast) then
    I			:= FLast.Index + 1
  else
    I			:= 0;

  V			:= TLetoScriptListedVar.CreateListed(self, Env, I);
  PushOne(V);
  Result		:= V;

end;

function TLetoScriptVarList.Add(const Key: String): TLetoScriptVar;
var
  V			: TLetoScriptListedVar;
begin
  ValType		:= vtHash;

  Result		:= Get(Key);
  if Assigned(Result) then Exit;

  V			:= TLetoScriptListedVar.CreateListed(self, Env, Key);
  PushOne(V);
  Result		:= V;

end;

(*------------------------------------------------------------------------------
Insert

Designed for arrays, supports sparse. This handles either inserting at the
end (just an add), or inserting between as an absolute index (no
re-indexing). To re-index, use Unshift.

------------------------------------------------------------------------------*)
function TLetoScriptVarList.Insert(
  const Index		: Cardinal;
  const Neighbor	: TLetoScriptListedVar
): TLetoScriptVar;
var
  V			: TLetoScriptListedVar;
begin
  if not Assigned(Neighbor) then begin
    Result		:= Add;
    TLetoScriptListedVar(Result).Index := Index;
    Exit;
  end;

  V			:= TLetoScriptListedVar.CreateListed(self, Env, Index);
//  V.FList		:= self;
  Inc(FCount);

  { Insert after }
  if Index > Neighbor.Index then begin
    V.Prev		:= Neighbor;
    V.Next		:= Neighbor.Next;
    if Assigned(V.Next) then
      V.Next.Prev	:= V
    else
      FLast		:= V;
    Neighbor.Next	:= V;
  end

  { Insert before }
  else begin
    V.Prev		:= Neighbor.Prev;
    V.Next		:= Neighbor;
    if Assigned(V.Prev) then
      V.Prev.Next	:= V
    else
      FFirst		:= V;
    Neighbor.Prev	:= V;
  end;

  Result		:= V;

end;

(*------------------------------------------------------------------------------
Unshift

Inserts into index 0, and re-indexes all elements by +1, maintaining the
exact sparseness of the array.

The List argument is the value or values to unshift. These are copied in-tact
and in order. E.g., if the current items are 1, 2, 3, calling Unshift('a', 'b')
results in 'a', 'b', 1, 2, 3.

The result of the function is the new number of elements in the list.

------------------------------------------------------------------------------*)
// DONE 1: BUG: [27] Corrected Unshift, Push... Each, keys, values
function TLetoScriptVarList.Unshift(
  const List		: TLetoScriptValue;
  const EachType	: TLetoScriptEachType
): Integer;
var
  E			: TLetoScriptValue;
  Prev, Item		: TLetoScriptListedVar;
  Len			: Integer;
begin

  Item			:= nil;
  Prev			:= nil;
  Len			:= 0;
  Result		:= Count;

  { Copy }
  List.EachInit;
  E			:= List.Each(EachType);
  while Assigned(E) do begin
    if (EachType = etBoth) and
      ( (vfKeyValue in List.Flags) or (vfKey in List.Flags) )
    then
      ValType		:= vtHash;
    if vfKeyValue in List.Flags then begin
      Prev.CopyFrom(E);
      E			:= List.Each(EachType);
      Continue;
    end
    else if vfKey in List.Flags then
      Item		:= TLetoScriptListedVar.CreateListed(self, Env, E.AsString)
    else if E.IsKeyedVar then
      Item		:= TLetoScriptListedVar.CreateListed(self, Env, TLetoScriptListedVar(E).Key )
    else
      Item		:= TLetoScriptListedVar.CreateListed(self, Env, Len);
    Item.CopyFrom(E);
    Item.Prev		:= Prev;
    if Assigned(Prev) then
      Prev.Next		:= Item;
    Prev		:= Item;
    E			:= List.Each(EachType);
    Inc(Len);
  end;

  if not Assigned(Item) then Exit;

  { Reindex }
  Reindex(Len);

  { Link }
  if not Assigned(FLast) then
    FLast		:= Item;
  if Assigned(FFirst) then begin
    FFirst.Prev		:= Item;
    Item.Next		:= FFirst;
  end;
  while Assigned(Item.Prev) do
    Item		:= Item.Prev;
  FFirst		:= Item;

  Inc(FCount, Len);
  Result		:= Count;

end;

(*------------------------------------------------------------------------------
Push

------------------------------------------------------------------------------*)
function TLetoScriptVarList.Push(
  const List		: TLetoScriptValue;
  const EachType	: TLetoScriptEachType
): Integer;
var
  E			: TLetoScriptValue;
  Prev, Item		: TLetoScriptListedVar;
  I			: Integer;
begin

  Item			:= nil;
  Prev			:= FLast;

  List.EachInit;
  E			:= List.Each(EachType);
  while Assigned(E) do begin
    if (EachType = etBoth) and
      ( (vfKeyValue in List.Flags) or (vfKey in List.Flags) )
    then
      ValType		:= vtHash;
    if vfKeyValue in List.Flags then begin
      Prev.CopyFrom(E);
      E			:= List.Each(EachType);
      Continue;
    end
    else if vfKey in List.Flags then
      Item		:= TLetoScriptListedVar.CreateListed(self, Env, E.AsString)
    else if E.IsKeyedVar then
      Item		:= TLetoScriptListedVar.CreateListed(self, Env, TLetoScriptListedVar(E).Key)
    else begin
      // DONE 1: BUG: [27r3] Push on a sparse array yielding incorrect index
      if Assigned(FLast) then
        I		:= StrToIntDef(FLast.FAddress, FCount-1)+1
      else
        I		:= FCount;
      Item		:= TLetoScriptListedVar.CreateListed(self, Env, I);
    end;
    Item.CopyFrom(E);
    Item.Prev		:= Prev;
    if Assigned(Prev) then
      Prev.Next		:= Item
    else
      FFirst		:= Item;
    Prev		:= Item;
    E			:= List.Each(EachType);
    Inc(FCount);
  end;

  if Assigned(Item) then
    FLast		:= Item;

  Result		:= FCount;

end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------*)
procedure TLetoScriptVarList.Delete(const V: TLetoScriptListedVar);
begin
  if not Assigned(V) then Exit;

  if Assigned(V.Prev) then
    V.Prev.Next		:= V.Next
  else
    FFirst		:= V.Next;

  if Assigned(V.Next) then
    V.Next.Prev		:= V.Prev
  else
    FLast		:= V.Prev;

  // DONE 1: BUG: [27r3] VarList.Delete should not reindex - caller must reindex
  { Do not reindex here. Caller must reindex, and only a shift operation
    should ever do that. }
//  Reindex(-1);

  V.Free;
  Dec(FCount);

end;

procedure TLetoScriptVarList.Delete(const Index: Cardinal);
begin
  Delete( TLetoScriptListedVar(Get(Index)) );
end;

procedure TLetoScriptVarList.Delete(const Key: String);
begin
  Delete( TLetoScriptListedVar(Get(Key)) );
end;

(*------------------------------------------------------------------------------
Reindex

------------------------------------------------------------------------------*)
procedure TLetoScriptVarList.Reindex(const Len: Integer);
var
  V			: TLetoScriptListedVar;
  S			: String;
  I			: Integer;
begin
  if ValType = vtHash then Exit;

  V			:= FFirst;
  while Assigned(V) do begin
    S			:= V.FAddress;
    if TryStrToInt(S, I) then
      V.FAddress	:= I + Len;
    V			:= V.Next;
  end;

end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoScriptVarList.Clear;
var
  V, Next		: TLetoScriptListedVar;
begin
  V			:= FFirst;
  while Assigned(V) do begin
    Next		:= V.Next;
    V.Free;
    V			:= Next;
  end;

  FFirst		:= nil;
  FLast			:= nil;

  FCount		:= 0;

end;

(*------------------------------------------------------------------------------
EachInit

------------------------------------------------------------------------------*)
procedure TLetoScriptVarList.EachInit;
begin
  inherited;

  if not Assigned(FEachVal) then
    FEachVal		:= TLetoScriptListedVar.Create(self, Env, '');

  FListEach		:= FFirst;
  FEach			:= 1;

end;

(*------------------------------------------------------------------------------
Each

This cycles through all the vars in this list, and returns nil after it
reaches the last var. Use EachInit to start again at the beginning of
the list.

------------------------------------------------------------------------------*)
function TLetoScriptVarList.Each(
  const EachType	: TLetoScriptEachType
): TLetoScriptValue;
begin
  Result		:= FListEach;
  FFlags		:= FFlags - [vfKey, vfKeyValue];

  if not Assigned(Result) then
    Exit

  { etKeys }
  else if EachType = etKeys then begin
    FEachVal.CopyFrom(FListEach);
    FEachVal.Value	:= FListEach.FAddress;
    Result		:= FEachVal;
    FListEach		:= FListEach.Next;
  end

  else if (ValType = vtArray) or (EachType <> etBoth) then begin
    Inc(FEach);
    FListEach		:= FListEach.Next;
  end

  { etBoth => Key }
  else if FEach = 1 then begin
    Include(FFlags, vfKey);
    FEachVal.CopyFrom(FListEach);
    FEachVal.Value	:= FListEach.FAddress;
    Result		:= FEachVal;
    FEach		:= 2;
  end

  { etBoth => Value }
  else if FEach = 2 then begin
    Include(FFlags, vfKeyValue);
    FEach		:= 1;
    FListEach		:= FListEach.Next;
  end;

end;

(*------------------------------------------------------------------------------
CopyFrom

------------------------------------------------------------------------------*)
procedure TLetoScriptVarList.CopyFrom(const Source: TLetoScriptVarList);
var
  Item, Copied		: TLetoScriptListedVar;
begin

  Item			:= Source.First;

  while Assigned(Item) do begin
    Copied		:= TLetoScriptListedVar.CreateListed(self, Env, '');
    Copied.CopyFrom(Item);
    Item		:= Item.Next;
  end;

end;


{ TLetoScriptVarTable }


(*------------------------------------------------------------------------------
GetIndex

------------------------------------------------------------------------------*)
function TLetoScriptVarTable.GetIndex(const Name: String): Byte;
var
  C			: Char;
begin
  C			:= UpCase(Name[1]);

  if C in ['A'..'Z'] then
    Result		:= Ord(C) - 65
  else
    Result		:= High(FBuckets);

end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptVarTable.Create(const AEnv: TLetoScriptEnv);
var
  I			: Integer;
begin
  Env			:= AEnv;

  for I := 0 to High(FBuckets) do
    FBuckets[I]		:= TList.Create;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptVarTable.Destroy;
var
  I			: Byte;
  L			: Integer;
begin
  for I := 0 to High(FBuckets) do begin
    for L := 0 to FBuckets[I].Count-1 do
      TLetoScriptVar(FBuckets[I][L]).Free;
    FBuckets[I].Free;
  end;

  inherited;
end;

(*------------------------------------------------------------------------------
Get

------------------------------------------------------------------------------*)
function TLetoScriptVarTable.Get(Name: String): TLetoScriptVar;
var
  I			: Byte;
  L			: Integer;
  V			: TLetoScriptVar;
begin
  Result		:= nil;
  Name			:= Lowercase(Name);
  I			:= GetIndex(Name);

  for L := FBuckets[I].Count-1 downto 0 do begin
    V			:= TLetoScriptVar(FBuckets[I][L]);
    if V.Name = Name then begin
      Result		:= V;
      Exit;
    end;
  end;

end;

(*------------------------------------------------------------------------------
Add

------------------------------------------------------------------------------*)
function TLetoScriptVarTable.Add(
  const Name		: String;
  const VT		: TLetoScriptValType
): TLetoScriptVar;
begin
  // DONE 1: BUG: [26] New lists with accessors exception
  if VT = vtScalar then
    Result		:= TLetoScriptVar.Create(nil, Env, Name)
  else
    Result		:= TLetoScriptVarList.Create(nil, Env, Name, VT);
  FBuckets[GetIndex(Name)].Add(Result);
end;

(*------------------------------------------------------------------------------
AddVar

------------------------------------------------------------------------------*)
procedure TLetoScriptVarTable.AddVar(const V: TLetoScriptVar);
begin
  FBuckets[GetIndex(V.Name)].Add(V);
end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------*)
procedure TLetoScriptVarTable.Delete(Name: String);
var
  I			: Byte;
  L			: Integer;
  V			: TLetoScriptVar;
begin
  Name			:= Lowercase(Name);
  I			:= GetIndex(Name);

  for L := FBuckets[I].Count-1 downto 0 do begin
    V			:= TLetoScriptVar(FBuckets[I][L]);
    if V.Name = Name then begin
      V.Free;
      FBuckets[I].Delete(L);
      Exit;
    end;
  end;

end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoScriptVarTable.Clear;
var
  I			: Byte;
  L			: Integer;
begin
  for I := 0 to High(FBuckets) do begin
    for L := 0 to FBuckets[I].Count-1 do
      TLetoScriptVar(FBuckets[I][L]).Free;
    FBuckets[I].Clear;
  end;
end;


{ TLetoScriptVarScope }


(*------------------------------------------------------------------------------
GetTable

------------------------------------------------------------------------------*)
function TLetoScriptVarScope.GetTable(
  const ValType		: TLetoScriptValType
): TLetoScriptVarTable;
begin
  if ValType = vtScalar then
    Result		:= FScalars
  else
    Result		:= FLists;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptVarScope.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv
);
begin
  FOwner		:= AOwner;
  FEnv			:= AEnv;

  FScalars		:= TLetoScriptVarTable.Create(AEnv);
  FLists		:= TLetoScriptVarTable.Create(AEnv);

  { Initialize Defaults. }
  ClearAll;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptVarScope.Destroy;
begin
  ClearAll;

  FreeAndNil(FScalars);
  FreeAndNil(FLists);

  inherited;
end;

(*------------------------------------------------------------------------------
Get

------------------------------------------------------------------------------*)
function TLetoScriptVarScope.Get(
  const Name		: String;
  const ValType		: TLetoScriptValType;
  const Autovivify	: Boolean;
  const Internal	: Boolean
): TLetoScriptVar;
begin
  if Name = '_' then begin
    if ValType = vtScalar then
      Result		:= FScalarDefault
    else
      Result		:= FListDefault;
    Exit;
  end;

  Result		:= GetTable(ValType).Get(Name);
  if not Assigned(Result) and Autovivify then
    Result		:= Add(Name, ValType, Internal);

end;

function TLetoScriptVarScope.Get(
  Name			: String;
  const Autovivify	: Boolean;
  const Internal	: Boolean
): TLetoScriptVar;
var
  V			: TLetoScriptValType;
begin
  if Name[1] = '$' then
    V			:= vtScalar
  else
    V			:= vtList;
  System.Delete(Name, 1, 1);
  Result		:= Get(Name, V, Autovivify, Internal);
end;

(*------------------------------------------------------------------------------
Add

------------------------------------------------------------------------------*)
function TLetoScriptVarScope.Add(
  const Name		: String;
  const ValType		: TLetoScriptValType;
  const Internal	: Boolean
): TLetoScriptVar;
begin
  if ValType = vtScalar then
    Result		:= TLetoScriptVar.Create(nil, Env, Name)
  else
    Result		:= TLetoScriptVarList.Create(nil, Env, Name, ValType);
  Result.FScope		:= self;
  Result.Internal	:= Internal;
  GetTable(ValType).AddVar(Result);
end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------*)
procedure TLetoScriptVarScope.Delete(
  const Name		: String;
  const ValType		: TLetoScriptValType
);
begin
  GetTable(ValType).Delete(Name);
end;

procedure TLetoScriptVarScope.Delete(Name: String);
var
  V			: TLetoScriptValType;
begin
  if Name[1] = '$' then
    V			:= vtScalar
  else
    V			:= vtList;
  System.Delete(Name, 1, 1);
  Delete(Name, V);
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoScriptVarScope.Clear(const ValType: TLetoScriptValType);
begin
  case ValType of

    vtScalar:
    begin
      FScalars.Clear;
      FScalarDefault	:= Add('_', vtScalar, True);
      FScalarDefault.Value := 'Hello, world!';
    end;

    vtList:
    begin
      FLists.Clear;
//      FListDefault	:= TLetoScriptVarList( Add('_', vtArray, True) );
    end;

  end;

end;

(*------------------------------------------------------------------------------
ClearAll

Clears both tables.

------------------------------------------------------------------------------*)
procedure TLetoScriptVarScope.ClearAll;
begin
  Clear(vtScalar);
  Clear(vtList);
end;

(*------------------------------------------------------------------------------
PrepArgScope

Called when the scope is being used for a context that may receive arguments,
such as a sub. ClearAll, and create ListDefault.

------------------------------------------------------------------------------*)
procedure TLetoScriptVarScope.PrepArgScope;
begin
  ClearAll;
  FListDefault		:= TLetoScriptVarList( Add('_', vtArray, True) );
end;


{ TLetoScriptFldVal }


(*------------------------------------------------------------------------------
property Field

Inserts logic for cascading.

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.GetField: TGffField;
begin
  if Cascaded and Assigned(FField) and Assigned(FField.FirstChild) then
    Result		:= FField.FirstChild
  else
    Result		:= FField;
end;

(*------------------------------------------------------------------------------
Cascaded

The special syntax /List// is used in loops to iterate through two generations
of children rather than just the first generation, which on a list is only
a bunch of Structs. Cascading gives access to the meaningful information in
each of the Structs, and is a shortcut for having to nest an extra loop.

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.Cascaded: Boolean;
begin
  Result		:= RightStr(Name, 2) = '//';
end;

(*------------------------------------------------------------------------------
NextCascaded

Works through a List's grandchildren, skipping immediate (Struct) children.

For Structs, this simply works through all immediate children.

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.NextCascaded(const Current: TGffField): TGffField;
begin

  // DONE 1: CHANGE: [27] Field-cascading functionality redefined
  if FField.VarType = gffStruct then begin

    if FEach = 0 then
      Result		:= FField.FirstChild
    else if Assigned(Current) then
      Result		:= Current.NextSibling
    else
      Result		:= nil;

    Inc(FEach);

  end

  else if FField.VarType = gffList then begin

    Result		:= FField.ChildOfIndex(FEach);

    if not Assigned(Result) then Exit;

    Result		:= Result.ChildOfIndex(FCascadeInt);

    if not Assigned(Result) then begin
      FCascadeInt	:= 0;
      Inc(FEach);
      Result		:= NextCascaded(nil);
    end else
      Inc(FCascadeInt);

  end

  else
    Result		:= nil;

end;

(*------------------------------------------------------------------------------
property Value

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.GetValue: Variant;
var
  F			: TGffField;
begin
  // Do the other props need this too...?
  if Cascaded then
    F			:= FField.FirstChild
  else
    F			:= FField;

  if not Assigned(F) then begin
    Result		:= '';
    Exit;
  end;

  case F.VarType of
    gffLocString:
      Result		:= F.AsLocString.ByIndex[FEach];
    gffVoid:
      if FEach < Cardinal(Length(F.Data.AsVoid))-1 then
        Result		:= F.Data.AsVoid[FEach]
      else
        Result		:= #0;
  else
    Result		:= F.AsVariant[0];
  end;

end;

(*------------------------------------------------------------------------------
property Defined

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.GetDefined: Boolean;
begin
  Result		:= Assigned(FField) and FField.IsValid;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptFldVal.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv;
  const AField		: TGffField;
  const Path		: String
);
begin
  inherited Create(AOwner, AEnv, Path);

  ValType		:= vtField;
  Field			:= AField;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptFldVal.Destroy;
begin
  if Assigned(FEachVal) then
    FreeAndNil(FEachVal);

  inherited;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.AsInt: Integer;
var
  F			: TGffField;
begin
  {
  if FValue = '//' then
    F			:= Field.FirstChild
  else
    F			:= Field;

  if not Assigned(F) then begin
    Result		:= 0;
    Exit;
  end;
  }
  F			:= FField;

  case F.VarType of
    gffResRef, gffString:
      Result		:= StrToIntDef(F.AsString, 0);
    gffLocString, gffStruct, gffList:
      Result		:= F.Count;
    gffVoid:
      Result		:= Length(F.Data.AsVoid);
  else
    Result		:= F.AsInt;
  end;
end;

(*------------------------------------------------------------------------------
AsReal

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.AsReal: Real;
var
  F			: TGffField;
begin
  {
  if FValue = '//' then
    F			:= Field.FirstChild
  else
    F			:= Field;

  if not Assigned(F) then begin
    Result		:= 0;
    Exit;
  end;
  }
  F			:= FField;

  case F.VarType of
    gffResRef, gffString:
      Result		:= StrToIntDef(F.AsString, 0);
    gffLocString, gffStruct, gffList:
      Result		:= F.Count;
    gffVoid:
      Result		:= Length(F.Data.AsVoid);
  else
    Result		:= F.AsDouble;
  end;
end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.AsBool: Boolean;
var
  F			: TGffField;
begin
  {
  if FValue = '//' then
    F			:= Field.FirstChild
  else
    F			:= Field;

  if not Assigned(F) then begin
    Result		:= False;
    Exit;
  end;
  }
  F			:= FField;

  case F.VarType of
    gffResRef, gffString:
      Result		:= F.AsString <> '';
    gffLocString, gffStruct, gffList:
      Result		:= F.Count > 0;
    gffVoid:
      Result		:= Length(F.Data.AsVoid) > 0;
  else
    Result		:= F.AsInt <> 0;
  end;
end;

(*------------------------------------------------------------------------------
AsString

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.AsString: String;
var
  F			: TGffField;
begin
  {
  if FValue = '//' then
    F			:= Field.FirstChild
  else
    F			:= Field;

  if not Assigned(F) then begin
    Result		:= '';
    Exit;
  end;
  }
  F			:= FField;

  case F.VarType of
    gffLocString:
      Result		:= F.AsLocString.ByIndex[FEach];
    gffVoid:
      if FEach < Cardinal(Length(F.Data.AsVoid))-1 then
        Result		:= Char(F.Data.AsVoid[FEach])
      else
        Result		:= #0;
  else
    Result		:= F.AsString;
  end;
end;

(*------------------------------------------------------------------------------
IsNum

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.IsNum: Boolean;
var
  F			: TGffField;
begin
  F			:= FField;

  Result :=
    Assigned(F) and
    not(F.VarType in [gffResRef, gffString, gffLocString]);
end;

(*------------------------------------------------------------------------------
IsList

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.IsList: Boolean;
var
  F			: TGffField;
begin
  F			:= FField;

  Result :=
    Assigned(F) and
    (F.VarType in [gffStruct, gffList]);
end;

(*------------------------------------------------------------------------------
EachInit

------------------------------------------------------------------------------*)
procedure TLetoScriptFldVal.EachInit;
begin
  inherited;

  if
    (FField.VarType in [gffLocString, gffVoid, gffStruct, gffList]) and
    not Assigned(FEachVal)
  then
    FEachVal		:= TLetoScriptFldVal.Create(self, Env, FField, '');

  FEach			:= 0;
  FCascadeInt		:= 0;

end;

(*------------------------------------------------------------------------------
Each

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.Each(
  const EachType	: TLetoScriptEachType
): TLetoScriptValue;
begin
  Result		:= FEachVal;
  if Assigned(Result) and not(FField.VarType in [gffStruct, gffList]) then
    Result.FEach	:= FEach;

  case FField.VarType of

    gffLocString:
      if FEach > FEachVal.FField.AsLocString.Count-1 then
        Result		:= nil;

    gffVoid:
      if FEach > Cardinal(Length(FEachVal.FField.Data.AsVoid))-1 then
        Result		:= nil;

    gffStruct, gffList:
      if Cascaded then
        FEachVal.Field	:= NextCascaded(FEachVal.Field)
      else if FEach = 0 then
        FEachVal.Field	:= FField.FirstChild
      else
        FEachVal.Field	:= FEachVal.FField.NextSibling;

  else
    if FEach = 0 then
      Result		:= self
    else
      Result		:= nil;

  end;

  if not Cascaded then
    Inc(FEach);

end;

(*------------------------------------------------------------------------------
Undef

Calling undef on a Field deletes the Field from the GFF.

------------------------------------------------------------------------------*)
function TLetoScriptFldVal.Undef: Boolean;
begin
  // Is there such a thing as an "internal" Field...?
  Result		:= True;
  FreeAndNil(FField);
  FEachVal		:= nil;
end;


{ TLetoScriptFileVal }


(*------------------------------------------------------------------------------
property Value

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.GetValue: Variant;
begin
  if not Assigned(LetoFile) then
    Result		:= ''
  else if LetoFile.IsErf and Assigned(LetoFile.Erf.Opened) then
    Result		:= LetoFile.CurrResName + '.' + LetoFile.CurrResType
  else
    Result		:= LetoFile.FileName;
end;

(*------------------------------------------------------------------------------
property Defined

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.GetDefined: Boolean;
begin
  Result		:= Assigned(LetoFile) and LetoFile.Loaded;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptFileVal.Create(
  const AOwner		: TObject;
  const AEnv		: TLetoScriptEnv;
  const AFile		: TLetoFile;
  const Handle		: String
);
begin
  { Caution: Do not rely on Handle (Name), it may be blank! }
  inherited Create(AOwner, AEnv, Handle);

  ValType		:= vtFile;
  LetoFile		:= AFile;

  FEachKey		:= TLetoScriptValue.Create(self, Env);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptFileVal.Destroy;
begin
  if Assigned(FEachRes) then
    FreeAndNil(FEachRes);
  if Assigned(FEachField) then
    FreeAndNil(FEachField);

  FreeAndNil(FEachKey);

  inherited;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.AsInt: Integer;
begin
  if not Assigned(LetoFile) then begin
    Result		:= -1;
    Exit;
  end;

  case LetoFile.FileType of
    ftERF..ft_ERF:
      Result		:= LetoFile.Erf.Count;
    ftGFF..ft_GFF:
      Result		:= LetoFile.Gff.Root.Count;
  else
    Result		:= 0;
  end;
end;

(*------------------------------------------------------------------------------
AsReal

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.AsReal: Real;
begin
  if not Assigned(LetoFile) then begin
    Result		:= -1;
    Exit;
  end;

  case LetoFile.FileType of
    ftERF:
      Result		:= LetoFile.Erf.Count;
    ftGFF:
      Result		:= LetoFile.Gff.Root.Count;
  else
    Result		:= 0;
  end;
end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.AsBool: Boolean;
begin
  { A successfully opened file is always True. }
  Result		:= Assigned(LetoFile) and LetoFile.Loaded;
end;

(*------------------------------------------------------------------------------
AsString

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.AsString: String;
begin
  Result		:= GetValue;
end;

(*------------------------------------------------------------------------------
IsNum

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.IsNum: Boolean;
begin
  { Although it can AsInt, a file-handle is not truly numerical. }
  Result		:= False;
end;

(*------------------------------------------------------------------------------
IsList

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.IsList: Boolean;
begin
  { These file types can be Each'ed over... }
  Result :=
    Assigned(LetoFile) and
    (LetoFile.FileType in [ftERF..ft_ERF, ftGFF..ft_GFF]);
end;

(*------------------------------------------------------------------------------
EachInit

------------------------------------------------------------------------------*)
procedure TLetoScriptFileVal.EachInit;
begin
  if not Assigned(LetoFile) then Exit;

  inherited;

  if ResMask = '' then
    ResMask		:= '*.*';

  case LetoFile.FileType of
    ftERF..ft_ERF:
      if not Assigned(FEachRes) then
        FEachRes :=
          TLetoScriptFileVal.Create(self, Env, LetoFile, Name)
      else
        FEachRes.LetoFile := LetoFile;
    ftGFF..ft_GFF:
      if not Assigned(FEachField) then
        FEachField :=
          TLetoScriptFldVal.Create(self, Env, LetoFile.Root.FirstChild, '')
      else
        FEachField.Field := LetoFile.Root.FirstChild;
  end;

end;

(*------------------------------------------------------------------------------
Each

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.Each(
  const EachType	: TLetoScriptEachType
): TLetoScriptValue;
begin
  Result		:= nil;

  if not Assigned(LetoFile) then Exit;

  case LetoFile.FileType of

    ftERF..ft_ERF:
    begin
      Result		:= FEachRes;
      if FEach = 0 then
        FEachRes.LetoFile.GetFirstResNamed(ResMask)
      else
        FEachRes.LetoFile.GetNextResNamed;
      // Should Result be nil if Error <> Success?
    end;

    // TODO 3: CHANGE: Expand @hash = /field support (will require etKeys support in TLetoScriptFldVal.Each)
    ftGFF..ft_GFF:
    begin
      if not Assigned(FEachField.Field) then
        Exit;
      FFlags		:= FFlags - [vfKey, vfKeyValue];
      if FEach = 3 then begin
        FEach		:= 1;
        FEachField.Field := FEachField.Field.Next;
        if not Assigned(FEachField.Field) then
          Exit;
      end;
      if FEach < 2 then begin
        Include(FFlags, vfKey);
        Result		:= FEachKey;
        FEachKey.Value	:= FEachField.Field.Path;
        if FEach = 0 then
          Inc(FEach);
      end
      else begin
        Include(FFlags, vfKeyValue);
        Result		:= FEachField;
      end;
    end;

  end;

  Inc(FEach);

end;

(*------------------------------------------------------------------------------
Undef

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.Undef: Boolean;
begin
  Result		:=  Env.CloseHandle(LetoFile) = LS_Success;
end;

(*------------------------------------------------------------------------------
SetAttrib

------------------------------------------------------------------------------*)
function TLetoScriptFileVal.SetAttrib(const Attr, Value: String): Boolean;
begin

  Result		:= Assigned(LetoFile) and LetoFile.Loaded;

  if not Result then Exit;

  if (Attr = 'signature') or (Attr = 'header') then
    LetoFile.Signature	:= Value

  else
    Result		:= False;

end;


{ TLetoScriptEnv }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
property Pragma

------------------------------------------------------------------------------*)
function TLetoScriptEnv.GetPragma(Name: String): Boolean;
begin
  Result		:= FPragmas.IndexOf(Name) > -1;
end;

procedure TLetoScriptEnv.SetPragma(Name: String; Value: Boolean);
var
  I			: Integer;
begin
  // Synonyms

  I			:= FPragmas.IndexOf(Name);
  if Value and (I = -1) then
    FPragmas.Add(Name)
  else if not Value and (I <> -1) then
    FPragmas.Delete(I);

end;

(*------------------------------------------------------------------------------
property Handle

------------------------------------------------------------------------------*)
function TLetoScriptEnv.GetHandle(const Name: String): TLetoFile;
var
  I			: Integer;
begin
  if Name = '_' then
    I			:= FHandles.IndexOf(GetLastHandle)
  else
    I			:= FHandles.IndexOf(Name);

  if I = -1 then
    Result		:= nil
  else
    Result		:= TLetoFile(FHandles.Objects[I]);
end;

procedure TLetoScriptEnv.SetHandle(
  const Name		: String;
  const Value		: TLetoFile
);
var
  I			: Integer;
begin
  // Internal use only
  //if not ValidHandle(Name) then Exit;

  { Separate logic for %_, to behave AS UseHandle. }
  if Name = '_' then begin
    UseHandle		:= '';
    if not Assigned(Value) or (FHandles.Count = 0) then
      Exit;
    for I := 0 to FHandles.Count-1 do
      if TLetoFile(FHandles.Objects[I]) = Value then begin
        UseHandle	:= Uppercase(FHandles[I]);
        Exit;
      end;
  end;

  I			:= FHandles.IndexOf(Name);

  if I < 0 then
    FHandles.AddObject(Uppercase(Name), Value)
  else if Assigned(Value) then begin
    TLetoFile(FHandles.Objects[I]).Free;
    FHandles.Objects[I]	:= Value;
  end else begin
    if Uppercase(FHandles[I]) = Uppercase(UseHandle) then
      UseHandle		:= '';
    if not Params.Embedded then
      TLetoFile(FHandles.Objects[I]).Free;
    FHandles.Delete(I);
  end;

end;

(*------------------------------------------------------------------------------
property DefaultHandle

------------------------------------------------------------------------------*)
function TLetoScriptEnv.GetDefHandle: TLetoFile;
var
  I			: Integer;
begin
  I			:= FHandles.IndexOf(GetLastHandle);
  if I = -1 then
    Result		:= nil
  else
    Result		:= TLetoFile(FHandles.Objects[I]);
end;

(*------------------------------------------------------------------------------
property LastHandle

Effectively gets the handle name that _ would point to. (Which could be
UseHandle.)

------------------------------------------------------------------------------*)
function TLetoScriptEnv.GetLastHandle: String;
begin
  // DONE 1: BUG: [27] Retroactive %_
  if UseHandle <> '' then begin
    if FHandles.IndexOf(UseHandle) = -1 then begin
      if FHandles.Count = 0 then
        UseHandle	:= ''
      else
        UseHandle	:= FHandles[FHandles.Count-1];
    end;
    Result		:= UseHandle;
  end
  else if FHandles.Count > 0 then
    Result		:= FHandles[FHandles.Count-1]
  else
    Result		:= '';
end;

(*------------------------------------------------------------------------------
property Errno

------------------------------------------------------------------------------*)
function TLetoScriptEnv.GetErrno: TLetoScriptError;
begin
  Result		:= FErrno;
end;

procedure TLetoScriptEnv.SetErrno(const Value: TLetoScriptError);
begin
  FErrno		:= Value;
  FBI_Err.Value		:= GetLSError(Value);
end;

(*------------------------------------------------------------------------------
property IoErr

This is a TLetoError. Errno is a TLetoScriptError. IoErr is used to capture
the result of a LoadFromStream or SaveToStream at the engine level, which
report their results using TLetoError.

If IoErr is <> Success, then Errno because Err_LS_IO, and checking its string
value will yield IoErr instead.

------------------------------------------------------------------------------*)
function TLetoScriptEnv.GetIoErr: TLetoError;
begin
  Result		:= FIoErr;
end;

procedure TLetoScriptEnv.SetIoErr(const Value: TLetoError);
begin
  FIoErr		:= Value;
  if FIoErr = Success then Exit;

  FErrno		:= Err_LS_IO;
  FBI_Err.Value		:= GetLetoError(Value);
end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptEnv.Create;
const
  InitEmptyScalar	: array[1..1] of String = (
    '@'
  );
  InitZeroScalar	: array[1..1] of String = (
    '.'
  );
  InitLists		: array[1..1] of String = (
    '.'
  );
var
  VS			: TLetoScriptVarScope;
  I			: Integer;
begin
  FPragmas		:= TStringList.Create;
  FHandles		:= TStringList.Create;
  FGlobalVars		:= TLetoScriptVarScope.Create(self, self);
  FSubs			:= TStringList.Create;

  FGlobalVars.PrepArgScope;
  
  OutputNL		:= True;

  NilValue		:= TLetoScriptVar.Create(nil, self);
  NilValue.Defined	:= False;

  {$IFDEF MSWINDOWS}
  FillChar(FormatSettings, SizeOf(TFormatSettings), 0);
  with FormatSettings do begin
    DecimalSeparator	:= '.';
  end;
  {$ENDIF}

  { Builtin vars }
  VS			:= FGlobalVars;
  { Very common built-ins. }
  FBI_Err		:= VS.Add('!', vtScalar, True);
  FBI_Err.Value		:= '';
  { And the rest... }
  for I := Low(InitEmptyScalar) to High(InitEmptyScalar) do
    VS.Add(InitEmptyScalar[I], vtScalar, True).Value := '';
  for I := Low(InitZeroScalar) to High(InitZeroScalar) do
    VS.Add(InitZeroScalar[I], vtScalar, True).Value := 0;
  for I := Low(InitLists) to High(InitLists) do
    VS.Add(InitLists[I], vtArray, True);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptEnv.Destroy;
begin
  if not Params.Embedded then
    ClearHandles;

  FreeAndNil(FHandles);
  FreeAndNil(FPragmas);

  FreeAndNil(FGlobalVars);

  ClearSubs;
  FreeAndNil(FSubs);

  FreeAndNil(NilValue);

  inherited;
end;

(*------------------------------------------------------------------------------
Output

------------------------------------------------------------------------------*)
procedure TLetoScriptEnv.Output(const S: String; const OutputType: TOutputType);
const
  L = {$IFDEF LINUX} 1 {$ENDIF} {$IFDEF MSWINDOWS} 2 {$ENDIF};
begin
  if (S = '') or not Assigned(OnOutput) then Exit;
  OnOutput(S, OutputType);
  OutputNL		:= RightStr(S, L) = sLineBreak;
end;

(*------------------------------------------------------------------------------
CloseHandle

------------------------------------------------------------------------------*)
function TLetoScriptEnv.CloseHandle(const H: TLetoFile): TLetoScriptError;
var
  I, Index		: Integer;
begin
  if Params.Embedded then begin
    Result		:= Err_LS_Embedded;
    Exit;
  end;

  Result		:= Err_LS_NoHandle;

  if not Assigned(H) or (FHandles.Count = 0) then Exit;

  Index			:= -1;

  for I := FHandles.Count-1 downto 0 do
    if TLetoFile(FHandles.Objects[I]) <> H then
      Continue
    else if Index > -1 then
      FHandles.Delete(I)
    else
      Index		:= I;

  if Index > -1 then begin
    Result		:= LS_Success;
    TLetoFile(FHandles.Objects[Index]).Free;
    FHandles.Delete(Index);
  end;

end;

function TLetoScriptEnv.CloseHandle(const Index: Integer): TLetoScriptError;
begin
  Result		:= Err_LS_NoHandle;

  if (Index < 0) or (Index > FHandles.Count-1) then Exit;

  if Uppercase(FHandles[Index]) = Uppercase(UseHandle) then
    UseHandle		:= '';

  Result		:= CloseHandle( TLetoFile(FHandles.Objects[Index]) );

end;

(*------------------------------------------------------------------------------
ClearHandles

------------------------------------------------------------------------------*)
procedure TLetoScriptEnv.ClearHandles;
begin
  while FHandles.Count > 0 do
    CloseHandle(0);
end;

(*------------------------------------------------------------------------------
ClearPragmas

------------------------------------------------------------------------------*)
procedure TLetoScriptEnv.ClearPragmas;
begin
  FPragmas.Clear;
end;

(*------------------------------------------------------------------------------
ClearVars

------------------------------------------------------------------------------*)
procedure TLetoScriptEnv.ClearVars;
begin
  //FVars.Clear;
end;

(*------------------------------------------------------------------------------
ClearSubs

------------------------------------------------------------------------------*)
procedure TLetoScriptEnv.ClearSubs;
var
  I			: Integer;
begin
  for I := 0 to Subs.Count-1 do
    TLetoScriptSub(Subs.Objects[I]).Free
end;


{ TLetoScriptLib }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptLib.Create(
  const AName		: String;
  const AEnv		: TLetoScriptEnv
);
begin
  Name			:= AName;
  Env			:= AEnv;
  Enabled		:= True;

  Disabled		:= TStringList.Create;
  Disabled.Sorted	:= True; { Needed for dupIgnore. }
  Disabled.Duplicates	:= dupIgnore;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptLib.Destroy;
begin
  FreeAndNil(Disabled);

  inherited;
end;


{ TLetoScriptArg }


(*------------------------------------------------------------------------------
property Value

------------------------------------------------------------------------------*)
function TLetoScriptArg.GetValue: TLetoScriptValue;
begin
  Evaluate;
  Result		:= Env.NilValue;
  if Assigned(FValue) then
    Result		:= FValue
  else if Specs.NilValue then
    Exit
  else if Assigned(FDefVal) then
    Result		:= FDefVal
  else if Name = '_' then
    case ArgType of
      atScalar:
        Result		:= Scope.ScalarDefault;
      atList:
        Result		:= Scope.ListDefault;
      atField: ;
        //Result	:= MakeFieldWrapper;
      atFile:
        Result		:= MakeFileWrapper;
    end
  else if FDefValSpec = '$_' then
    Result		:= Scope.ScalarDefault
  else if FDefValSpec = '@_' then begin
    // Pretty ugly hack...
    if Assigned(FFunc) and Assigned( TLetoScriptFunc(FFunc).Token ) then
      Result :=
        TLetoScriptFunc(FFunc).Token.GetNearestArgScope.ListDefault
    else
      Result		:= Env.GlobalScope.ListDefault;
  end
  else if FDefValSpec = '%_' then
    Result		:= MakeFileWrapper;
end;

(*------------------------------------------------------------------------------
property DefaultVal

------------------------------------------------------------------------------*)
function TLetoScriptArg.GetDefault: TLetoScriptValue;
begin
  if not Assigned(FDefVal) then
    FDefVal		:= TLetoScriptValue.Create(self, Env);
  Result		:= FDefVal;
end;

procedure TLetoScriptArg.SetDefault(const ADef: TLetoScriptValue);
begin
  Specs.ConstDefault	:= False;
  if Assigned(FDefVal) and (FDefVal.Owner = self) then
    FreeAndNil(FDefVal);
  FDefVal		:= ADef;
end;

(*------------------------------------------------------------------------------
MakeFileWrapper

------------------------------------------------------------------------------*)
function TLetoScriptArg.MakeFileWrapper: TLetoScriptValue;
begin
  if not Assigned(WrapFile) then
    WrapFile		:= TLetoScriptFileVal.Create(nil, nil);
  WrapFile.LetoFile	:= Env.Handle['_'];
  Result		:= WrapFile;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptArg.Create(
  const AEnv		: TLetoScriptEnv;
  const AFunc		: TObject
);
begin
  Env			:= AEnv;
  FFunc			:= AFunc
//  FDefault		:= TLetoScriptVar.Create(nil, Env, '');
//  FDefault.Defined	:= False;
end;

(*------------------------------------------------------------------------------
CreateFrom

Used by TLetoScriptArgs.CopyDefinition.

------------------------------------------------------------------------------*)
constructor TLetoScriptArg.CreateFrom(const Source: TLetoScriptArg);
begin
  Create(Source.Env, Source.FFunc);

  FValue		:= Source.FValue;

  FDefVal		:= Source.FDefVal;
  FDefValSpec		:= Source.FDefValSpec;

  ArgType		:= Source.ArgType;
  Name			:= Source.Name;
  ArgNo			:= Source.ArgNo;

  Specs.NeedsValue	:= Source.Specs.NeedsValue;
  Specs.ConstDefault	:= Source.Specs.ConstDefault;
  Specs.NilValue	:= Source.Specs.NilValue;
  Specs.NoEval		:= Source.Specs.NoEval;
//  Specs.Greedy		:= Source.Specs.Greedy;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptArg.Destroy;
begin
//  FreeAndNil(FDefault);

  if Assigned(FDefVal) and (FDefVal.Owner = self) then
    FreeAndNil(FDefVal);

  if Assigned(WrapFile) then
    FreeAndNil(WrapFile);

  inherited;
end;

(*------------------------------------------------------------------------------
FullName

Used for output or feedback into argument tokenizer. Gives the argument's
name with its symbol qualifier.

------------------------------------------------------------------------------*)
function TLetoScriptArg.FullName: String;
begin

  case ArgType of
    atScalar:
      Result		:= '$';
    atList:
      Result		:= '@';
    atField:
      Result		:= '/';
    atFile:
      Result		:= '%';
    atBlock:
      Result		:= '&';
    atScalarVar:
      Result		:= 'var $';
    atListVar:
      Result		:= 'var @';
  end;

  Result		:= Result + Name;

end;

(*------------------------------------------------------------------------------
ValueAssigned

------------------------------------------------------------------------------*)
function TLetoScriptArg.ValueAssigned: Boolean;
begin
  Result		:= Assigned(FValue);
end;

(*------------------------------------------------------------------------------
MakeConstDefault

If the default value for the arg is a constant value, this procedure is
called to set the value and toggle the ConstDefault flag, which is used
when checking for foldables.

------------------------------------------------------------------------------*)
procedure TLetoScriptArg.MakeConstDefault(const AValue: Variant);
begin
  Specs.ConstDefault	:= True;
  DefaultVal.Value	:= AValue;
end;

(*------------------------------------------------------------------------------
Reset

------------------------------------------------------------------------------*)
procedure TLetoScriptArg.Reset(
  const AScope		: TLetoScriptVarScope;
  const ACon		: TLetoScriptOpCon
);
begin
  Evaluated		:= False;
  Scope			:= AScope;
  Con			:= ACon;
end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptArg.Evaluate;
begin
  if not Evaluated and ValueAssigned and not Specs.NoEval then begin
    Evaluated		:= True;
    FValue.FFlags	:= FValue.FFlags + [vfEvalEach];
    FValue.Evaluate(Con);
  end;
end;

(*------------------------------------------------------------------------------
Describe

Used by TLetoScriptFunc.Describe.

------------------------------------------------------------------------------*)
function TLetoScriptArg.Describe: String;
var
  S			: String;
  E			: Extended;
begin
  Result		:= '';
//  if Specs.Greedy then
//    Result		:= 'greedy ';
  Result		:= Result + FullName;
  if not Specs.NeedsValue then begin
    if Assigned(FValue) then
      S			:= FValue.AsString
    else if Specs.NilValue then
      S			:= 'undef'
    else if Assigned(FDefVal) then begin
      S			:= FDefVal.AsString;
      if not TryStrToFloat(S, E) then
        S		:= '''' + S + '''';
    end
    else if Name = '_' then
      case ArgType of
        atScalar:
          S		:= '$_';
        atList:
          S		:= '@_';
        atField:
          S		:= '/@';
        atFile:
          S		:= '%_';
      end
    else if FDefValSpec <> '' then
      S			:= FDefValSpec
    else
      S			:= 'undef';
    if (S = 'undef') and Specs.Optional then
      S			:= ''
    else
      S			:= ' = ' + S;
    Result		:= Result + S;
  end;
end;


{ TLetoScriptArgs }


(*------------------------------------------------------------------------------
property Op

------------------------------------------------------------------------------*)
function TLetoScriptArgs.GetOp: TLetoScriptOp;
begin

  if (Standards.Count = 0) and (Options.Count > 0) then
    Result		:= opListOp

  else if Standards.Count = 0 then
    Result		:= opFunc

  else if
    (Standards.Count > 1) or
    (Options.Count > 0) or
    (
      (TLetoScriptArg(Standards.Objects[0]).ArgType = atList) and
      (TLetoScriptArg(Standards.Objects[0]).Name = '_')
    )
  then
    Result		:= opListOp

  else
    Result		:= opUnaryOp;

end;

(*------------------------------------------------------------------------------
property Args

------------------------------------------------------------------------------*)
function TLetoScriptArgs.GetArg(const Name: String): TLetoScriptArg;
var
  I			: Integer;
begin
  Result		:= nil;

  I			:= Standards.IndexOf(Name);
  if I > -1 then
    Result		:= TLetoScriptArg(Standards.Objects[I]);

  if Assigned(Result) then Exit;

  I			:= Options.IndexOf(Name);
  if I > -1 then
    Result		:= TLetoScriptArg(Options.Objects[I]);

end;

(*------------------------------------------------------------------------------
property Values

------------------------------------------------------------------------------*)
function TLetoScriptArgs.GetValue(const Name: String): TLetoScriptValue;
begin
  Result		:= GetArg(Name).Value;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptArgs.Create(
  const Syntax		: String;
  const AEnv		: TLetoScriptEnv;
  const AFunc		: TObject
);
var
  I			: Integer;
  List			: TStringList;
  C			: Char;
  S			: String;
begin
  Env			:= AEnv;
  FFunc			:= AFunc;

  FBlank		:= TLetoScriptArg.Create(Env, FFunc);
  FBlank.ArgType	:= atScalar;

  Standards		:= TStringList.Create;
  Options		:= TStringList.Create;

  LastArgNo		:= 0;
  ListMode		:= False;

  if (Syntax = '()') or (Syntax = '') then Exit;

  List			:= Standards;
  for I := 1 to Length(Syntax) do begin
    C			:= Syntax[I];
    case C of
      ',':
        AddArg(S, List);
      ';':
      begin
        if S <> '' then
          AddArg(S, List);
        List		:= Options;
      end;
      ' ':
        S		:= '';
    else
      S			:= S + C;
    end;
  end;
  if S <> '' then AddArg(S, List);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptArgs.Destroy;
begin
  Clear;

  FreeAndNil(FBlank);

  FreeAndNil(Standards);
  FreeAndNil(Options);

  inherited;
end;

(*------------------------------------------------------------------------------
GetByIndex

------------------------------------------------------------------------------*)
function TLetoScriptArgs.GetByIndex(
  const List		: TStringList;
  const Index		: Integer
): TLetoScriptArg;
begin
  if
    not Assigned(List) or
    (Index < 0) or
    (Index > List.Count-1)
  then
    Result		:= nil
  else
    Result		:= TLetoScriptArg(List.Objects[Index]);

end;

(*------------------------------------------------------------------------------
First

Return a non-nil Arg depending on the function's syntax:

Has formal parameters: the first one.

Single informal: that one.

No formal (only options): FBlank.

------------------------------------------------------------------------------*)
function TLetoScriptArgs.First: TLetoScriptArg;
begin
  if Standards.Count = 0 then
    Result		:= FBlank
  else
    Result		:= TLetoScriptArg(Standards.Objects[0]);
end;

(*------------------------------------------------------------------------------
AddArg

Before actual user args are parsed, the language-level syntax used by the
function implementations must be evaluated. This syntax specifies a parameter's
type, name, semantics, and default value.

------------------------------------------------------------------------------*)
function TLetoScriptArgs.AddArg(
  S			: String;
  const List		: TStringList
): TLetoScriptArg;
var
  M			: Byte;
  I			: Integer;
  C			: Char;
  DefVal		: String;
const
  MODE_TYPE		= 0;
  MODE_NAME		= 1;
  MODE_SEMANTIC		= 2;
  MODE_DEFAULT		= 3;
begin

  Result		:= TLetoScriptArg.Create(Env, FFunc);

  M			:= MODE_TYPE;
  I			:= 1;

  while I <= Length(S) do begin

    C			:= S[I];

    case M of

      MODE_TYPE:
        if C in ['a'..'z', '_'] then begin
          Result.Name	:= Result.Name + C;
          M		:= MODE_NAME;
        end else if C = '=' then
          M		:= MODE_NAME
        else
          case C of
            '%': Result.ArgType := atFile;
            '/': Result.ArgType := atField;
            '&': Result.ArgType := atBlock;
            '$': Result.ArgType := atScalar;
            '@': Result.ArgType := atList;
          end;

      MODE_NAME:
        if C in ['a'..'z', '_'] then
          Result.Name	:= Result.Name + C
        else if C = '=' then
          M		:= MODE_DEFAULT
        else begin
          M		:= MODE_SEMANTIC;
          Continue;
        end;

      MODE_SEMANTIC:
        case C of
          '!': Result.Specs.NoEval := True;
//          '?': Result.Specs.Greedy := True;
          '^':
            if Result.ArgType = atScalar then
              Result.ArgType := atScalarVar
            else if Result.ArgType = atList then
              Result.ArgType := atListVar;
          '=':
            M		:= MODE_DEFAULT;
        end;

      MODE_DEFAULT:
        DefVal		:= DefVal + C;

    end;

    Inc(I);

  end;

  if (M = MODE_DEFAULT) and (DefVal <> '') then begin
    if (DefVal = '$_') or (DefVal = '@_') then
      Result.FDefValSpec := DefVal
    else
      Result.MakeConstDefault(DefVal);
  end;

  Result.Name		:= Lowercase(Result.Name);

  Result.Specs.NeedsValue :=
    (List = Standards) and
    (M <> MODE_DEFAULT) and
    (Result.Name <> '_');

//  if (Result.ArgType = atList) and (Result.Name = '_') then
//    Result.Specs.Greedy	:= True;

//  if Result.Specs.Greedy and (List = Standards) and (List.Count = 0) then
  if (Result.ArgType = atList) and (List = Standards) and (List.Count = 0) then
    ListMode		:= True;

  Result.Specs.Optional := List = Options;

  List.AddObject(Result.Name, Result);

end;

(*------------------------------------------------------------------------------
AddValue

Find the parameter named (or use the next unassigned Standard) and assign it
this value.

------------------------------------------------------------------------------*)
function TLetoScriptArgs.AddValue(
  const Name		: String;
  const Value		: TLetoScriptValue
): TLetoScriptError;
var
  I			: Integer;
  Arg			: TLetoScriptArg;
begin
  Inc(LastArgNo);

  Arg			:= nil;
  Result		:= Err_LS_InvalidArg;

  if Name = '' then begin
    for I := 0 to Standards.Count-1 do
      if not Assigned( TLetoScriptArg(Standards.Objects[I]).FValue ) then begin
        Arg		:= TLetoScriptArg(Standards.Objects[I]);
        Break;
      end;
  end
  else
    Arg			:= GetArg(Name);

  if not Assigned(Arg) then Exit;

  Arg.ArgNo		:= LastArgNo;
  if Assigned(Value) then
    Arg.Value		:= Value
  else
    Arg.Value.Value	:= 1;
  Result		:= LS_Success;

  { Check for List Mode }
  for I := 0 to Standards.Count-1 do begin
    Arg			:= TLetoScriptArg(Standards.Objects[I]);
    if Arg.ValueAssigned then
      Continue
//    else if Arg.Specs.Greedy then
    else if Arg.ArgType = atList then
      ListMode		:= True
    else
      Break;
  end;

end;

(*------------------------------------------------------------------------------
CopyDefinition

Used by subroutines. The subroutine definition declares the syntax - the
"parameters" for the function. Calls to the subroutine supply the "arguments".
The .Args of the fnSub must take the parameters from the actual TLetoScriptSub
as a template, to do meaningful syntax checking.

------------------------------------------------------------------------------*)
procedure TLetoScriptArgs.CopyDefinition(const FromArgs: TLetoScriptArgs);
var
  I			: Integer;
  Arg			: TLetoScriptArg;
begin
  Clear;

  Env			:= FromArgs.Env;
  ListMode		:= FromArgs.ListMode;

  for I := 0 to FromArgs.Standards.Count-1 do begin
    Arg			:= TLetoScriptArg(FromArgs.Standards.Objects[I]);
    Standards.AddObject( Arg.Name, TLetoScriptArg.CreateFrom(Arg) );
  end;

  for I := 0 to FromArgs.Options.Count-1 do begin
    Arg			:= TLetoScriptArg(FromArgs.Options.Objects[I]);
    Options.AddObject( Arg.Name, TLetoScriptArg.CreateFrom(Arg) );
  end;

end;

(*------------------------------------------------------------------------------
Reset

Flag all arguments unevaluated.

------------------------------------------------------------------------------*)
procedure TLetoScriptArgs.Reset;
var
  Con			: TLetoScriptOpCon;
  I			: Integer;
begin

  if Op = opListOp then
    Con			:= ocList
  else
    Con			:= ocScalar;

  for I := 0 to Standards.Count - 1 do
    TLetoScriptArg(Standards.Objects[I]).Reset(Scope, Con);

  for I := 0 to Options.Count - 1 do
    TLetoScriptArg(Options.Objects[I]).Reset(Scope, Con);

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------
procedure TLetoScriptArgs.Evaluate;
var
  Con			: TLetoScriptOpCon;
  I			: Integer;
begin

  if Op = opListOp then
    Con			:= ocList
  else
    Con			:= ocScalar;

  for I := 0 to Standards.Count-1 do
    TLetoScriptArg(Standards.Objects[I]).Evaluate(Scope, Con);

  for I := 0 to Options.Count-1 do
    TLetoScriptArg(Options.Objects[I]).Evaluate(Scope, Con);

end;
*)

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoScriptArgs.Clear;
var
  I			: Integer;
begin
  for I := 0 to Standards.Count-1 do
    TLetoScriptArg(Standards.Objects[I]).Free;
  Standards.Clear;

  for I := 0 to Options.Count-1 do
    TLetoScriptArg(Options.Objects[I]).Free;
  Options.Clear;

end;

(*------------------------------------------------------------------------------
Describe

Used by TLetoScriptFunc.Describe.

------------------------------------------------------------------------------*)
function TLetoScriptArgs.Describe: String;
var
  S, O			: String;
  function _Describe_(const List: TStringList): String;
  var
    I			: Integer;
    Arg			: TLetoScriptArg;
  begin
    Result		:= '';
    for I := 0 to List.Count - 1 do begin
      Arg		:= TLetoScriptArg(List.Objects[I]);
      Result		:= Result + '\n\t' + Arg.Describe;
      if I < List.Count-1 then
        Result		:= Result + ', ';
    end;
  end;
begin

  S			:= _Describe_(Standards);
  O			:= _Describe_(Options);

  Result		:= S;
  if O <> '' then
    Result		:= Result + ';' + O;
  if Result = '' then
    Result		:= '()';

end;


{ TLetoScriptAttribs }


(*------------------------------------------------------------------------------
constructor

The Syntax form is for LetoScript's internal function definitions. These use
a [] section after the parameters to declare attribs as single letters, e.g.

system.var $var^ [P]

Makes the var function a ParamOp (it is only valid in the context of a
parameter list declaration in a sub's definition.

The [] block is stripped from the string, so that the string can be passed
to the constructor for the Args, next.

------------------------------------------------------------------------------*)
constructor TLetoScriptAttribs.Create(var Syntax: String);
var
  P			: Integer;
  S			: String;
begin
  P			:= Pos(' [', Syntax);
  if P = 0 then Exit;

  S			:= Copy(Syntax, P+2, Length(Syntax) - P - 2);
  Syntax		:= Copy(Syntax, 1, P-1);

  for P := 1 to Length(S) do
    AddAttrib(S[P]);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptAttribs.Destroy;
begin

  inherited;
end;

(*------------------------------------------------------------------------------
AddAttrib

------------------------------------------------------------------------------*)
function TLetoScriptAttribs.AddAttrib(const Attr: String): Boolean;
begin

  Result		:= True;

  if       Attr = 'listop' then
    AddAttrib('L')
  else if  Attr = 'unaryop' then
    AddAttrib('U')

  else
    Result		:= False;

end;

procedure TLetoScriptAttribs.AddAttrib(const A: Char);
begin
  case A of
    'F': Foldable	:= True;
    'L': ListOp		:= True;
    'P': ParamOp	:= True;
    'U': UnaryOp	:= True;
  end;

end;

(*------------------------------------------------------------------------------
Describe

Used by TLetoScriptFunc.Describe. Describes *all* attributes, even the
internal ones.

------------------------------------------------------------------------------*)
// TODO 3: CHANGE: Optimize TLetoScriptAttribs.Describe
function TLetoScriptAttribs.Describe: String;
begin

  if Foldable then
    Result		:= Result + 'foldable, ';
  if ListOp then
    Result		:= Result + 'listop, ';
  if ParamOp then
    Result		:= Result + 'paramop, ';
  if UnaryOp then
    Result		:= Result + 'unaryop, ';

  if Result <> '' then
    Result := '[' + Copy(Result, 1, Length(Result)-2) + ']';

end;


(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
