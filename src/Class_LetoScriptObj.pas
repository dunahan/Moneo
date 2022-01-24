(*------------------------------------------------------------------------------
Class_LetoScriptObj

A TLetoScriptObj is a singular object in a LetoScript script. This class
implements the basic behavior for exposed expressions, such as properties and
the mechanics for (simple) operations. Class_LetoScriptLib contains the code
for all of the (complex) functions. Class_LetoScript is the compiler that
builds a tree of TLetoScriptObj's and exposes LetoScript to the rest of
the app.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoScriptObj;

{$I LetoScript.inc}

interface

uses
  SysUtils, Classes, TypInfo, StrUtils, Math, Variants,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  Header_Leto, Header_LetoScript,
  Class_LetoFile, Class_ErfFile, Class_ErfStruct, Class_GffFile, Class_GffField,
  Class_LetoScriptEnv;

type

  TLetoScriptObj	= class(TLetoScriptVar)

  private

    FParent		: TLetoScriptObj;
    FLeft		: TLetoScriptObj;
    FRight		: TLetoScriptObj;
    FBlock		: TLetoScriptObj;
    FExpr		: TLetoScriptObj;
    FItems		: TList;

    FContext		: TLetoScriptOpCon;
    FExplicitContext	: Boolean;

    FOp			: TLetoScriptOp;
    FKind		: TLetoScriptOpKind;
    FPrec		: Byte;

    FFunc		: TObject;

    FLine, FPos		: Cardinal;
    //FLoc		: String;
    FText		: String;

    // TODO 4: CHANGE: Use EStr instead of text
    FEStr1		: String;
    FEStr2		: String;
    FEStr3		: String;

    FLoopContext	: TLetoScriptValue;

    { Local utilites }

    function GetValueKind(const V: Variant): TLetoScriptOpKind;

    { Properties }

    procedure SetLeft(const AValue: TLetoScriptObj);
    procedure SetRight(const AValue: TLetoScriptObj);
    procedure SetBlock(const AValue: TLetoScriptObj);
    procedure SetExpr(const AValue: TLetoScriptObj);

    procedure SetContext(const AValue: TLetoScriptOpCon);

    procedure SetOp(const AValue: TLetoScriptOp);

    function GetNearestOp: TLetoScriptObj;
    function GetLoopContext: TLetoScriptValue;

    { Clutter-saving reusable functions, used by operators }

    function BinaryOp(
      const BinOp	: TLetoScriptOp;
      const L, R	: TLetoScriptValue
    ): Variant;

    function BooleanOp(
      const BoolOp	: TLetoScriptOp;
      const L, R	: TLetoScriptValue
    ): Variant;

    function VarShouldVivify: Boolean;

    procedure Assignment(Lside, Rside: TLetoScriptObj);
    procedure ScalarAssignment(
      const Lval	: TLetoScriptVar;
      const Rside	: TLetoScriptObj
    );
    procedure ListAssignment(
      const EqOp	: TLetoScriptOpKind;
      const Lval	: TLetoScriptVarList;
      Rside		: TLetoScriptObj
    );
    procedure AttribAssignment(const Lside, Rside: TLetoScriptObj);
    procedure MultiAssignment(const Lside, Rside: TLetoScriptObj);

    { Operators }

    procedure Op_Quoted;
    procedure Op_Symbol;
    procedure Op_Reference;
    procedure Op_Attrib;
    procedure Op_Binary;
    procedure Op_Boolean;
    procedure Op_Bitwise;
    procedure Op_Bind;
    procedure Op_Range;
    procedure Op_Conditional;
    procedure Op_Assign;
    procedure Op_List;

  protected

    procedure SetDefined(const ADefined: Boolean); override;

  public

    property Parent: TLetoScriptObj read FParent;
    property Left: TLetoScriptObj read FLeft write SetLeft;
    property Right: TLetoScriptObj read FRight write SetRight;
    property Block: TLetoScriptObj read FBlock write SetBlock;
    property Expr: TLetoScriptObj read FExpr write SetExpr;
    property Items: TList read FItems;

    property Context: TLetoScriptOpCon read FContext write FContext;

    property Op: TLetoScriptOp read FOp write SetOp;
    property Kind: TLetoScriptOpKind read FKind write FKind;
    property Prec: Byte read FPrec;

    property Func: TObject read FFunc write FFunc;

    property Line: Cardinal read FLine write FLine;
    property LinePos: Cardinal read FPos write FPos;
    //property Location: String read FLoc write FLoc;
    property Text: String read FText write FText;

    { Strings reserved for error text. }
    property EStr1: String read FEStr1 write FEStr1;
    property EStr2: String read FEStr2 write FEStr2;
    property EStr3: String read FEStr3 write FEStr3;

    property LoopContext: TLetoScriptValue
      read GetLoopContext write FLoopContext;

    constructor Create(
      const AOwner	: TLetoScriptValue;
      const AEnv	: TLetoScriptEnv;
      const AOp		: TLetoScriptOp
    );
    destructor Destroy; override;

    function AsInt: Integer; override;
    function AsReal: Real; override;
    function AsBool: Boolean; override;
    function AsString: String; override;

    function IsScalar: Boolean; override;
    function IsList: Boolean; override;
    function IsField: Boolean; override;
    function IsFile: Boolean; override;
    function IsScalarVar: Boolean; override;
    function IsListVar: Boolean; override;
    function IsBlock: Boolean; override;

    function Description(const CT: TCaseType = ctLowercase): String;

    function Each(
      const EachType	: TLetoScriptEachType = etBoth
    ): TLetoScriptValue; override;

    procedure Complain(
      const Err		: TLetoScriptError;
      const E1		: String = '';
      const E2		: String = '';
      const E3		: String = '';
      const Fatal	: Boolean = False
    );
    procedure Exception(
      const Err		: TLetoScriptError;
      const E1		: String = '';
      const E2		: String = '';
      const E3		: String = ''
    );

    procedure Clear;

    function WantsBlock: Boolean;
    function WantsExpr: Boolean;

    function CheckSides: Boolean;
    function CheckFoldable: Boolean;
    procedure CheckArgs;
    function BuildArgs(
      const Obj		: TLetoScriptObj;
      const Args	: TLetoScriptArgs
    ): TLetoScriptError;
    function CheckArgType(const Arg: TLetoScriptArg): Boolean;
    function CheckInitialized: Boolean;
    procedure CheckUseless;
    procedure Preprocess;

    procedure InContext(const AContext: TLetoScriptOpCon);
    function AncestorHasFlag(const Flag: TLetoScriptValFlag): Boolean;

    procedure Reset;
    procedure Evaluate(const Con: TLetoScriptOpCon = ocScalar); override;
    function EvalDefined: Boolean;

    function GetNearestScope: TLetoScriptVarScope;
    function GetNearestArgScope: TLetoScriptVarScope;
    function GetVar(Name: String = ''): TLetoScriptVar;

    function EstablishList: TLetoScriptVarList;
    procedure CaptureList(
      const List	: TStringList;
      const FreeList	: Boolean = True
    ); overload;
    procedure CaptureList(
      const V		: TLetoScriptValue;
      const EachType	: TLetoScriptEachType = etBoth
    ); overload;

  end;


implementation

uses
  Class_LetoScript, Class_LetoScriptLib, Class_LetoScriptSub;


{ TLetoScriptObj }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
GetValueKind

Convert a variant's type to an OpKind (opConst).

------------------------------------------------------------------------------*)
function TLetoScriptObj.GetValueKind(const V: Variant): TLetoScriptOpKind;
begin
  case VarType(V) and varTypeMask of

    varBoolean,
    varSmallint, varInteger, varShortInt, varByte, varWord,
    varLongWord, varInt64:
      Result		:= numInt;

    varSingle, varDouble:
      Result		:= numReal;

    varCurrency:
      Result		:= numCurrency;

    varDate:
      Result		:= numDate;

    varOleStr, varStrArg, varString:
      Result		:= quSingle;

  else
    Result		:= numInt;
  end;

end;

(*------------------------------------------------------------------------------
property Left

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetLeft(const AValue: TLetoScriptObj);
begin
  FLeft			:= AValue;
  if Assigned(AValue) then
    AValue.FParent	:= self;
end;

(*------------------------------------------------------------------------------
property Right

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetRight(const AValue: TLetoScriptObj);
begin
  FRight		:= AValue;
  if Assigned(AValue) then
    AValue.FParent	:= self;
end;

(*------------------------------------------------------------------------------
property Block

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetBlock(const AValue: TLetoScriptObj);
begin
  FBlock		:= AValue;
  if Assigned(AValue) then
    AValue.FParent	:= self;
end;

(*------------------------------------------------------------------------------
property Expr

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetExpr(const AValue: TLetoScriptObj);
begin
  FExpr			:= AValue;
  if Assigned(AValue) then
    AValue.FParent	:= self;
end;

(*------------------------------------------------------------------------------
property Context

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetContext(const AValue: TLetoScriptOpCon);
begin
  if FExplicitContext then
    Exit;

  FContext		:= AValue;
  FExplicitContext	:= True;

  // TODO 4: ADD: Fully functional cascading context

  if Assigned(Left) and not Left.FExplicitContext then
    Left.Context	:= AValue;
  if Assigned(Right) and not Right.FExplicitContext then
    Right.Context	:= AValue;
  if Assigned(Expr) and not Expr.FExplicitContext then
    Expr.Context	:= AValue;

end;

(*------------------------------------------------------------------------------
property Op

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetOp(const AValue: TLetoScriptOp);
var
  H			: Cardinal;
  C			: Cardinal;
begin
  FOp			:= AValue;
  H			:= High(Precedence);
  for C := 0 to H do
    if FOp in Precedence[C].Ops then begin
      FPrec		:= H-C;
      Exit;
    end;
  FPrec			:= H+1;
end;

(*------------------------------------------------------------------------------
GetNearestOp

Determines the operator to which this object "applies", which is generally
its parent, unless the parent is merely semantic.

------------------------------------------------------------------------------*)
function TLetoScriptObj.GetNearestOp: TLetoScriptObj;
begin
  Result		:= Parent;
  while Assigned(Result) and (Result.Op in [opComma, opKey, opExpr, opSet]) do
    Result		:= Result.Parent;

end;

(*------------------------------------------------------------------------------
property LoopContext

Retrieve the current context for the nearest loop. (Or nil, if there is
none.)

------------------------------------------------------------------------------*)
function TLetoScriptObj.GetLoopContext: TLetoScriptValue;
begin
  Result		:= FLoopContext;
  if not Assigned(Result) and Assigned(Parent) then
    Result		:= Parent.GetLoopContext;
end;


(*------------------------------------------------------------------------------

		Evaluation templates

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
BinaryOp

The brains of Op_Binary, this is abstracted so that other operations can
re-use it, for instance Op_Assign (where eqPlus re-uses opBinPlus).

------------------------------------------------------------------------------*)
function TLetoScriptObj.BinaryOp(
  const BinOp		: TLetoScriptOp;
  const L, R		: TLetoScriptValue
): Variant;
var
  S			: String;
begin
  try
  case BinOp of

    opUnaMinus:
      if R.IsNum then
        Result		:= R.AsReal * -1
      else begin
        S		:= R.AsString;
        if S = '' then
        else if S[1] = '-' then
          S		:= '+' + S
        else
          S		:= '-' + S;
        Result		:= S;
      end;
    { Unary symbolic + is simply transparent. }
    opUnaPlus:
      Result		:= R.Value;

    opBinExp:
      Result		:= Power(L.AsReal, R.AsReal);
    opBinMult:
      Result		:= L.AsReal * R.AsReal;
    opBinDiv:
      Result		:= L.AsReal / R.AsReal;
    opBinMod:
      Result		:= L.AsReal - Int(L.AsReal / R.AsReal) * R.AsReal;
    opBinRep:
      Result		:= StringRepeat(L.AsString, R.AsInt);

    opBinPlus:
      // DONE 1: CHANGE: [27r2] Looser BinPlus concat requirements.
      // DONE 1: CHANGE: [27r3] Very strict BinPlus BinMinus
      if L.IsNum or R.IsNum then
        Result		:= L.AsReal + R.AsReal
      else
        Result		:= L.AsString + R.AsString;

    opBinMinus:
      if L.IsNum or R.IsNum then
        Result		:= L.AsReal - R.AsReal
      else
        Result		:= StringReplace(L.AsString, R.AsString, '', [rfReplaceAll]);

    // DONE 1: ADD: [27r3] Added literal plus and minus
    opLitPlus:
      Result            := L.AsString + R.AsString;
    opAddSp:
    begin
      if Assigned(L) then
        Result		:= L.AsString + ' '
      else
        Result		:= ' ';
//        Result		:= GetNearestScope.ScalarDefault.AsString + ' ';
      if Assigned(R) then
        Result		:= Result + R.AsString;
    end;

    opLitMinus:
      Result		:= StringReplace(L.AsString, R.AsString, '', [rfReplaceAll]);

    {
    // DONE 1: CHANGE: [27r3] removed +^ and -^
    opRemSp:
    begin
      if Assigned(L) then
        S		:= L.AsString
      else
        S		:= GetNearestScope.ScalarDefault.AsString;
      if Assigned(R) then
        S		:= StringReplace(S, R.AsString, '', [rfReplaceAll]);
      while S[Length(S)] in [' ', #9, #10, #13] do
        System.Delete(S, Length(S), 1);
      Result		:= S;
    end;
    }

  end;
  except
    Exception(Err_LS_InvalidReal);
  end;

end;

(*------------------------------------------------------------------------------
BooleanOp

Makes widely available the functionality of relational (boolean) operators,
in the same way as DoBinaryOp.

------------------------------------------------------------------------------*)
function TLetoScriptObj.BooleanOp(
  const BoolOp		: TLetoScriptOp;
  const L, R		: TLetoScriptValue
): Variant;
var
  Relation		: Boolean;
  LS, RS		: String;
  LL, RL, I		: Integer;
begin

  Relation		:= False;

  { Short-circuit for compound (right-associative) relationships. }
  if
    (Op in [opNumLT..opStrCmp]) and
    (Right.Op in [opNumLT..opStrCmp]) and
    not Right.AsBool
  then begin
    Result		:= 0;
    Exit;
  end;

  case Op of

    opLogNot, opLitNot:
      Relation		:= not R.AsBool;
    opLogAnd, opLitAnd:
      Relation		:= L.AsBool and R.AsBool;
    opLogOr, opLitOr:
      Relation		:= L.AsBool or R.AsBool;
    opLitXor:
      Relation		:= L.AsBool xor R.AsBool;

    opNumLT:
      Relation		:= L.AsReal < R.AsReal;
    opNumGT:
      Relation		:= L.AsReal > R.AsReal;
    opNumLE:
      Relation		:= L.AsReal <= R.AsReal;
    opNumGE:
      Relation		:= L.AsReal >= R.AsReal;
    opNumEq:
      Relation		:= L.AsReal = R.AsReal;
    opNumNE:
      Relation		:= L.AsReal <> R.AsReal;
    opNumCmp:
    begin
      if L.AsReal < R.AsReal then
        Result		:= -1
      else if L.AsReal = R.AsReal then
        Result		:= 0
      else if L.AsReal > R.AsReal then
        Result		:= 1;
      Exit;
    end;

    opStrEq:
      Relation		:= L.AsString = R.AsString;
    opStrEqI:
      Relation		:= Lowercase(L.AsString) = Lowercase(R.AsString);
    opStrNE:
      Relation		:= L.AsString <> R.AsString;
    opStrNEI:
      Relation		:= Lowercase(L.AsString) <> Lowercase(R.AsString);
    opStrLT, opStrGT, opStrLE, opStrGE, opStrCmp:
    begin
      Result		:= 0;
      LS		:= L.AsString;
      RS		:= R.AsString;
      LL		:= Length(LS);
      RL		:= Length(RS);
      if LL < RL then
        I		:= LL
      else
        I		:= RL;
      for I := 1 to I do
        if LS[I] < RS[I] then begin
          Result	:= -1;
          Break;
        end else if LS[I] > RS[I] then begin
          Result	:= 1;
          Break;
        end;
      if Result <> 0 then
      else if LL < RL then
        Result		:= -1
      else if LL > RL then
        Result		:= 1;
      case Op of
        opStrLT:
          Relation	:= Result = -1;
        opStrGT:
          Relation	:= Result = 1;
        opStrLE:
          Relation	:= (Result = -1) or (Result = 0);
        opStrGE:
          Relation	:= (Result = 1) or (Result = 0);
      else
        Exit;
      end;
    end;

  end;

  if Relation then
    Result		:= 1
  else
    Result		:= 0;

end;

(*------------------------------------------------------------------------------
VarShouldVivify

A better solution than AncestorHasFlag(vfVivify).

A var will vivify if it is the direct descendant of:
 - assignment
 - auto-increment / auto-decrement
 - a function that flags itself vfVivify (such as push)

An indirect descendant (the argument of some function that is itself in a
multi-assignment list) is not vivified. Accessors (e.g. $foo[$bar]) are not
vivified.

------------------------------------------------------------------------------*)
function TLetoScriptObj.VarShouldVivify: Boolean;
begin
  Result		:= vfVivify in FFlags;
  if Result or not Assigned(Parent) then
    Exit;

  { Do not vivify accessors }
  if (Parent.Op = opSymbol) and (Parent.Expr = self) then
    Exit;
  if (Kind = symFile) and (Parent.Op = opBind) then
    Exit;

  { Do not vivify function arguments }
  if Parent.Op = opFunc then begin
    if not (vfVivify in Parent.Flags) then
      Exit;
  end;

  { Otherwise, recurse. }
  Result		:= Parent.VarShouldVivify;

end;

(*------------------------------------------------------------------------------
Assignment

Abstracts Op_Assign, so that it can be reused in recursive assignment ops.
Acts as a boilerplate for any kind of lvalue assignment.

Lside is the left side of the assignment, but its type is unknown.

Rside is the right side of the assignment. Its type is also unknown.

Assignment checks the types of Lside and Rside, and calls a particular
assignment function to facilitate the many types of cross-assignment:

ScalarAssignment
  Assignment to a scalar lvalue.

ListAssignment
  Assignment to a list lvalue. Right side is in list context, but this is
  not the same as "list assignment".

AttribAssignment
  Special cases where an attribute can be an lvalue.

MultiAssignment
  Multiple assignments, meaning recursive calls to Assignment. This is
  "list assignment", and the right side is in list context.

The result of this function (Assignment) is Lside, to support cascading Lvalue.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Assignment(
  Lside, Rside		: TLetoScriptObj
);
var
  Lval			: TLetoScriptVar;
begin

  // SALT: Allow my() params to be assigned to
  if Assigned(Lside.Func) and
    (TLetoScriptFunc(Lside.Func).Parent.Name = 'system') and
    (TLetoScriptFunc(Lside.Func).Fn = Word(fnMy)) and
    Assigned(Lside.Expr)
  then
    Lside		:= Lside.Expr;
  // SALT: Allow () = ()
  if (Lside.Op = opExpr) and (Lside.Left.Op = opComma) then
    Lside		:= Lside.Left;

  if Lside.Op = opComma then
    MultiAssignment(Lside, Rside)
  else if Lside.Op = opAttrib then
    AttribAssignment(Lside, Rside);
  if Lside.Op in [opComma, opAttrib] then Exit;

  Lval			:= Lside.Lvalue;

  EStr2			:= Description;
  if not Assigned(Lval) then
    Exception(Err_LS_CantModify, Lside.Description);

  { Note that while FileVals qualify as lists, assignment is always scalar. }
  if (Lside.Kind <> symFile) and Lval.IsList then
    ListAssignment(Kind, Lval.AsList, Rside)
  else
    ScalarAssignment(Lval, Rside);

end;

(*------------------------------------------------------------------------------
ScalarAssignment

Assigns something to a scalar lvalue.

Lval is a scalar lvalue, of any supported type.

Rside is the right side object of some unknown type.

This assignment is done directly to the Lvalue. The caller has responsibility
to capture (Rvalue) or set Value separately.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.ScalarAssignment(
  const Lval		: TLetoScriptVar;
  const Rside		: TLetoScriptObj
);
var
  F			: TGffField;
  S, H			: String;
  B			: Boolean;
  LetoFile		: TLetoFile;
  Filename		: String;
  Erf			: TErfFile;

  function _EqToOp_(const Eq: TLetoScriptOpKind): TLetoScriptOp;
  begin
    Result		:= opNull;
    case Eq of
      eqExp:		Result := opBinExp;
      eqPlus:		Result := opBinPlus;
      eqMinus:		Result := opBinMinus;
      eqMult:		Result := opBinMult;
      eqDiv:		Result := opBinDiv;
      eqMod:		Result := opBinMod;
      eqBitAnd:		Result := opBitAnd;
      eqBitOr:		Result := opBitOr;
      eqBitXor:		Result := opBitXor;
      eqShl:		Result := opShl;
      eqShr:		Result := opShr;
    end;
  end;

  procedure _EqValDef_;
  begin
    Lval.Value		:= Rside.Value;
    Lval.Defined	:= Rside.Defined;
  end;
begin

  case Lval.ValType of

    vtScalar, vtArray, vtHash:
    begin
      case Kind of
        okNone:
          _EqValDef_;
        eqLogAnd:
          if Lval.AsBool then
            _EqValDef_;
        eqLogOr:
          if not Lval.AsBool then
            _EqValDef_;
      else
        Lval.Value	:= BinaryOp(_EqToOp_(Kind), Lval, Rside);
      end;
    end; { vtScalar, ... }

    vtField:
    begin
      F			:= Lval.AsField;
      if not Assigned(F) then Exit;
      case Kind of
        // TODO 3: ADD: Warning for incompatible Right (/Age = 'foo')
        // DONE 1: ADD: [27r2] Warning on ResRef overflow
        okNone:
        begin
          if (F.VarType = gffResRef) and (Length(Rside.AsString) > 16) then
            Complain(Err_LS_ResRefOverflow);
          F.SetValue(Rside.Value);
        end;
        eqLogAnd, eqLogOr:
        begin
          S		:= F.AsString;
          B		:= (S <> '') and (S <> '0');
          if (B and (Kind = eqLogAnd)) or (not B and (Kind = eqLogOr)) then
            F.SetValue(Rside.Value);
        end;
      else
        F.SetValue(BinaryOp(_EqToOp_(Kind), Lval, Rside));
      end;
    end; { vtField }

    vtFile:
    begin
      if Kind <> okNone then
        Exception(Err_LS_BadOverload);
      H			:= Lval.Name;
      { % = '' in % }
      if Rside.Op = opBind then begin
        LetoFile	:= Rside.Right.AsFile;
        if not LetoFile.IsErf then begin
          Complain(Err_LS_NotErfFile, '%');
          Exit;
        end;
        Erf		:= LetoFile.Erf;
        Env.IoErr	:= Erf.Open(Rside.Left.AsString);
        if Env.IoErr = Success then begin
          LetoFile	:= TLetoFile.CreateWrapper(Erf.Opened.Gff, Erf, Erf.Opened);
          Env.Handle[H] := LetoFile;
          TLetoScriptFileVal(Lval).LetoFile := LetoFile;
        end;
        Exit;
      end;
      { % = % }
      if Rside.Kind = symFile then begin
        if not Rside.Defined then
        { %ifo = %module['module.ifo'] }
        // DONE 1: CHANGE: [27r3] %erf['resource'] deprecated
        else if Rside.AsFile.IsErf and Assigned(Rside.Expr) then begin
          {
          Rside.Complain(Err_LS_Deprecated, '%ERF[''resource'']', '''resource'' in %ERF');
          Erf		:= Rside.AsFile.Erf;
          Env.IoErr	:= Erf.Open(Rside.Expr.AsString);
          if Env.IoErr = Success then begin
            LetoFile	:= TLetoFile.CreateWrapper(Erf.Opened.Gff, Erf, Erf.Opened);
            Env.Handle[H] := LetoFile;
            TLetoScriptFileVal(Lval).LetoFile := LetoFile;
          end;
          }
        end
        { %_ = %bic }
        // and %copy = %bic, as a pointer, but this is scheduled for deprecation
        else
          Env.Handle[H] := Rside.AsFile;
        Exit;
      end;
      { % = '...' }
      Filename		:= Rside.AsString;
      LetoFile		:= Lval.AsFile;
      // DONE 1: CHANGE: [27r3] Close handles with undef or blank string
      if not Rside.Defined or (Filename = '') then
        Lval.Undef
      else if (H = '_') or (Assigned(LetoFile) and (Filename[1] <> '>')) then
        Exception(Err_LS_LiveHandle)
      { save }
      else if Assigned(LetoFile) then begin
        System.Delete(Filename, 1, 1);
        Env.IoErr	:= LetoFile.Save(Filename);
      end
      { open }
      else if Env.Params.Embedded then
        { Cannot vivify in an embedded environment. }
        Complain(Err_LS_Embedded)
      else begin
        LetoFile	:= TLetoFile.Create;
        Env.IoErr	:= LetoFile.Open(Filename);
        if Env.IoErr = Success then begin
          Env.Handle[H] := LetoFile;
          TLetoScriptFileVal(Lval).LetoFile := LetoFile;
        end;
      end;
    end; { vtFile }

  end;

end;

(*------------------------------------------------------------------------------
ListAssignment

Assigns something to a list lvalue.

Lval is a list var, either an array or hash. (Or even an array that will be
promoted to a hash by this assignment.)

Rside is the right side object of some unknown type.

As with ScalarAssignment, the assignment is done directly to the Lvalue.

This function compensates for a lot of strange situations, and is still
a work in progress.

Some notes:

@list = /firstname;   # a list of all the LangSpecs in this LocString
@list = /LvlStatList; # all the Fields (paths) beneath LvlStatList
                      # or a hash with paths => values ?
@list = %some_erf;    # all the resnames in the ERF
@list += %some_erf;   # error (use push instead?)

------------------------------------------------------------------------------*)
// DONE 1: CHANGE: [27] Centralized Obj.Each (abstracted ListAssignment, reliable Unshift)
// DONE 1: BUG: [27] @hash1 = @hash2 (and definedness)
procedure TLetoScriptObj.ListAssignment(
  const EqOp		: TLetoScriptOpKind;
  const Lval		: TLetoScriptVarList;
  Rside			: TLetoScriptObj
);
begin
  if not Assigned(Lval) then
    Exit;

  if not(EqOp in [okNone, eqPlus]) then
    Exception(Err_LS_BadOverload);

  if EqOp = okNone then begin
    Lval.Clear;
    Lval.ValType	:= vtArray;
  end;
  if Rside.ValType = vtHash then
    Lval.ValType	:= vtHash;

  if Rside.Op = opExpr then begin
    Rside		:= Rside.Left;
    if not Assigned(Rside) then Exit;
  end;

  Rside.EachInit;
  Lval.Push(Rside);

end;

(*------------------------------------------------------------------------------
AttribAssignment

Assigning to an attribute, e.g.

/FirstName.Lang[2] = 'toto';

Lside is the opAttrib object itself.

Rside is the value to assign the attrib.

Execution is driven by separate functions in relevant units. These functions
should be named SetAttrib and are boolean - False causes a Warning about
invalid attribute assignment, but without specific details.

If Lside isn't even a supported type, an assignment error occurs.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.AttribAssignment(const Lside, Rside: TLetoScriptObj);
var
  Valid			: Boolean;
  L, R			: TLetoScriptObj;
  Field			: TGffField;
  FileVal		: TLetoScriptFileVal;
begin

  if Kind <> okNone then
    Exception(Err_LS_BadOverload);

  Valid			:= True;

  L			:= Lside.Left;
  R			:= Lside.Right;

  case L.Kind of

    symScalar:
      if (R.AsString = 'c') or (R.AsString = 'char') then
        { Mask is already installed. SetValue handles everything. }
        Lside.Lvalue.Value := Rside.Value
      else
        Valid		:= False;

    symField:
    begin
      Field		:= L.AsField; { Field doesn't exist }
      if not Assigned(Field) then Exit;
      if
        (
          Assigned(R.Expr) and
          not Field.SetAttrib(
            Lowercase(R.Text),
            R.Expr.AsString,
            Rside.AsString,
            not R.Expr.Defined
          )
        ) or
        (
          not Assigned(R.Expr) and
          not Field.SetAttrib(Lowercase(R.Text), Rside.AsString)
        )
      then
        Lside.Complain(Err_LS_BadAttrib);
    end;

    symFile:
    begin
      FileVal		:= L.AsFileVal;
      if not Assigned(FileVal) then Exit;
      if not FileVal.SetAttrib(Lowercase(R.Text), Rside.AsString) then
        Lside.Complain(Err_LS_BadAttrib);
    end;

  else
    Valid		:= False;

  end;

  if not Valid then
    Exception(Err_LS_CantModify, 'attribute', Description);

end;

(*------------------------------------------------------------------------------
MultiAssignment

Assigns to multiple lvalues at once, e.g.

($foo, $bar) = (1, 2);
my ($xyzzy, $plugh) = ($foo, $bar);

Lside is a comma list. Its members are unknowns, and passed as-is back into
Assignment, for individual assignment.

Rside is an unknown. It should be a comma list, but may not be. It may not
match the number of items in Lside. Its types may not match, either. Again,
it's up to Assignment to sort these mysteries out.

Iteration continues until one side or the other is exhausted. Any remaining
lvalues are not assigned (but not necessarily unassigned). Any remaining
rvalues are simply discarded, though they will have been evaluated, which
could mean side-effects.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.MultiAssignment(const Lside, Rside: TLetoScriptObj);
var
  L, R			: TLetoScriptValue;
  LObj			: TLetoScriptObj;
begin

  Lside.EachInit;
  Lside.InContext(ocSymList);

  Rside.EachInit;

  L			:= Lside.Each;
  while Assigned(L) do begin
    if L.IsList then
      R			:= Rside.Each(etCommas)
    else
      R			:= Rside.Each;
    if not Assigned(R) then Break;
    LObj		:= TLetoScriptObj(L);
    Assignment(LObj, TLetoScriptObj(R));
    Lvalue		:= LObj;
    L			:= Lside.Each;
  end;

end;

(*------------------------------------------------------------------------------
Op_Quoted

Strings with interpolative data.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Quoted;
var
  S, Val		: String;
  I			: Integer;
  C			: Char;
  Mini			: TLetoScript;
  Token			: TLetoScriptObj;
  E			: TLetoScriptValue;
begin
  S			:= Text;
  I			:= 1;

  // TODO 4: ADD: Another round of updated interpolation support; accessors, attributes?
  // Don't just evaluate symbols, feed the whole stream to Mini in -String Context-

  while I <= Length(S) do begin
    C			:= S[I];

    if
      not(C in ['\', '$', '@', '/', '`'])
      //or ((C = '/') and (Kind <> quPrintn))
    then begin
      Val		:= Val + C;
      Inc(I);
      Continue;
    end;

    Mini		:= TLetoScript.Create(ocString, Env);
    Token		:= TLetoScriptObj.Create(nil, Env, opUnknown);
    Token.FParent	:= self;
    Token.Line		:= Line;
    Token.LinePos	:= LinePos;
    try

    Mini.Stream		:= TStringStream.Create( PChar(@S[I]) );
    Mini.GetLine;
    Mini.GetChr;

    case C of

      { Escaped character. }
      '\':
      begin
        Val		:= Val + Mini.EscapedChr;
        Inc(I);
      end;

      { Symbol interpolation. }
      '$', '/':
      begin
        if Mini.GetSymbol(Token, self) <> LS_Success then
          { Silently discard }
          Continue;
        Token.Evaluate;
        Val		:= Val + Token.AsString;
      end;
      { Interpolated lists get magic spaces: }
      '@':
      begin
        if Mini.GetSymbol(Token, self) <> LS_Success then Continue;
        Token.Evaluate;
        Token.EachInit;
        E		:= Token.Each;
        while Assigned(E) do begin
          Val		:= Val + E.AsString;
          E		:= Token.Each;
          if Assigned(E) then
            Val		:= Val + ' ';
        end;
      end;

      { Code interpolation. NYI. }
      '`':
      begin
      end;

    end;

    finally
      Inc(I, Mini.BufPos-2);
      FreeAndNil(Mini);
      FreeAndNil(Token);
    end;

  end;

  Value			:= Val;

end;

(*------------------------------------------------------------------------------
Symbols

Symbolics, such as variables, fields, and file handles. The "operation" for
the symbol is to look it up. It may or may not exist (already). Only in the
case of Fields is a warning given when the Field does not exist; the others
may not exist because they're being used as lvalues.

The value of the symbol is not determined here - but rather in the GetAs*
routines, since many values are possible for different symbols in different
contexts.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Symbol;
var
  Bound			: Boolean;
  V			: TLetoScriptVar;
  Val			: TLetoScriptValue;
  S			: String;
  Field			: TGffField;
  FldVal		: TLetoScriptFldVal;
  Handle		: TLetoFile;
  FileVal		: TLetoScriptFileVal;
begin

  Rvalue		:= Env.NilValue;

  { Bound Field }
  // DONE 1: CHANGE: [27r2] Subscripted file syntax change
  // Was: /{%bob => /firstname}
  // Now: %bob =~ /firstname
  // Changed again in R3.
  // Now: /firstname in %bob; 'resource' in %erf
  Bound			:= Assigned(Parent) and (Parent.Op = opBind);

  if Assigned(Block) then begin
    Block.Evaluate;
    FValue		:= Block.AsString;
    Text		:= Text[1] + FValue;
  end;

  if FValue = '' then
    Exception(Err_LS_InvalidName);

  { Accessor }
  if Assigned(Expr) then
    Expr.Evaluate;

  { Lookup }
  case Kind of

    symScalar, symList:
    begin
      V			:= GetVar;
      Lvalue		:= V;
      if not Assigned(V) then
        Exit
      else if Kind = symScalar then
        Rvalue		:= V
      else
        Rvalue		:= V;
      { Abandoned idiom:
      else if Kind = symScalar then
        Capture(V)
      else
        CaptureList(V);
      }
      { The problem with snapshots:
        This code was abandoned and the idiom re-worked.
        Initially, the problem was that OOE was not correct for statements
        such as:
        print $foo, $foo++, $foo
        The result would be 1 0 1, because Args were all getting evaluated
        before print had a chance to output any of them. Specifically, the
        greedy list contained in the comma was being evaluated (recursively).
        The first workaround was to use snapshots. However, snapshots are
        very inefficient, and ultimately problematic. So instead, a new flag
        was introduced, vfEvalEach. With this flag, a comma's sides are not
        evaluated, except during Each, so that they are in effect evaluated
        in a true left-to-right fashion.
        The current implementation is still potentially dangerous. If Each
        isn't used (the comma's sides are inspected naked) or if Each is
        used multiple times, Evaluation won't fire or will fire multiple
        times, respectively. This only applies to greedy args.
      }
    end;

    symField:
    begin
      if Bound then begin
        Handle		:= Parent.Right.AsFile;
        Text		:= '%' + Parent.Right.AsVariant;
      end else begin
        Handle		:= Env.Handle['_'];
        Text		:= '%' + Env.LastHandle;
      end;
      if not Assigned(Handle) then begin
        Complain(Err_LS_NoFile);
        Exit;
      end else if not Handle.IsGff then begin
        Complain(Err_LS_NotGffFile, Text);
        Exit;
      end;
      S			:= AsVariant;
      Text		:= '/' + S;
      { /$FieldNameInVar }
      if S[1] = '$' then begin
        V		:= GetVar(S);
        if not Assigned(V) then begin
          Complain(Err_LS_InvalidField);
          Exit;
        end;
        S		:= V.AsString;
        Text		:= '/' + S;
      end;
      { GffContext }
      Val		:= GetLoopContext;
      if Assigned(Val) and (Val.ValType = vtField) then
        Field		:= Handle.Find(S, Val.AsField)
      else
        Field		:= Handle.Find(S);
      { Silently ignore invalid. }
      if Field.IsValid then begin
        FldVal		:= TLetoScriptFldVal.Create(self, Env, Field, S);
        Rvalue		:= FldVal;
        Lvalue		:= FldVal;
      end;
    end;

    symFile:
    begin
      Handle		:= Env.Handle[FValue];
      if Assigned(Handle) and Handle.IsErf and Assigned(Expr) then begin
        Complain(Err_LS_Deprecated, '%ERF[''resource'']', '''resource'' in %ERF');
        Exit;
      end;
      if not Assigned(Handle) and not VarShouldVivify then begin
        if not(vfDefinedCheck in Flags) then
          Complain(Err_LS_InvalidFile, Text);
        Exit;
      end;
      FileVal		:= TLetoScriptFileVal.Create(self, Env, Handle, FValue);
      Rvalue		:= FileVal;
      Lvalue		:= FileVal;
    end;

  end;

end;

(*------------------------------------------------------------------------------
References

Operations that deal with pointers (references). There are only two: opRef
and opDeref, for constructing a reference and subsequently dereferencing it.
opAttrib, while using the same '.' notation, is technically different.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Reference;
begin

end;

(*------------------------------------------------------------------------------
Attributes

Symbols can be queried for attributes using the period, which looks like
dereferencing, but is only similar in concept (getting something out of
something else).

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Attrib;
var
  Valid			: Boolean;
  S, Ex			: String;
  V			: TLetoScriptValue;
  Offset, Len		: Integer;
  Mask			: TLetoScriptMaskedVar;
  Err			: TLetoScriptError;
  Fatal			: Boolean;
begin
  Left.Evaluate;
  Text			:= Lowercase(Right.AsString);
  Valid			:= True;
  Lvalue		:= Left;

  // DONE 1: ADD: [26] Attrib[expr] (e.g. /LocString.lang[n])
  if Assigned(Right.Expr) then begin
    Right.Expr.Evaluate;
    Ex			:= Right.Expr.AsString;
  end;

  V			:= Left.Rvalue;
  if not Assigned(V) or not V.Defined then
    S			:= ''
  else
  case Left.Kind of

    symScalar:
      if (Text = 'len') or (Text = 'length') then
        S		:= IntToStr(Length(V.AsString))
      // DONE 1: ADD: [26] $var.type, $var.typex
      else if Text = 'type' then
        case TVarData(V.Value).VType of
          varSmallInt, varInteger, varSingle, varDouble,
          varBoolean, varShortInt, varByte, varWord,
          varLongWord, varInt64:
            S		:= 'NUMBER';
          varOleStr, varString:
            S		:= 'STRING';
        else
          S		:= 'UNKNOWN';
        end
      else if Text = 'typex' then
        S		:= IntToStr( TVarData(V.Value).VType )
      // DONE 1: ADD: [26] $var.char[n]
      // DONE 1: ADD: [27] .char works as substr()
      else if (Text = 'c') or (Text = 'char') then begin
        { Figure Offset, Len... }
        Len		:= 1;
        if Assigned(Right.Expr) then begin
          if
            (Right.Expr.Op = opComma) and
            Assigned(Right.Expr.Left) and Assigned(Right.Expr.Right)
          then begin
            Offset	:= Right.Expr.Left.AsInt;
            Len		:= Right.Expr.Right.AsInt;
          end else
            Offset	:= Right.Expr.AsInt;
        end else
          Offset	:= -1;
        { Build and install Mask... }
        Mask :=
          TLetoScriptMaskedVar.Create(
            self, Env, '',
            Left, Offset, Len,
            nil
          );
        Rvalue		:= Mask;
        Lvalue		:= Mask;
        { Diagnose any errors... }
        Mask.Diagnostic(Err, Fatal);
        if Err = LS_Success then
        else if Fatal then
          Exception(Err, '.char[]')
        else
          Complain(Err, '.char[]');
      end
      else
        Valid		:= False;

    symList:
      if Text = 'count' then
        S		:= IntToStr(V.AsInt)
      else if Text = 'first' then
        S		:= V.AsList.First.Key
      else if Text = 'last' then
        S		:= V.AsList.Last.Key
      else if Text = 'type' then begin
        if V.ValType = vtHash then
          S		:= 'HASH'
        else
          S		:= 'ARRAY'
      end
      else
        Valid		:= False;

    symField:
      if (Text = 'langs') and (V.AsField.VarType = gffLocString) then
        // DONE 1: ADD: [26] /LocString.langs
        CaptureList(V.AsField.AsLocString.GetLangs)
      else if (Text = 'names') and (V.AsField.IsStruct) then
        // DONE 1: ADD: [27r3] /Struct.names
        CaptureList(V.AsField.GetNames)
      else
        Valid		:= V.AsField.GetAttrib(Text, Ex, S);

    symFile:
      if Text = 'filename' then
        S		:= V.AsFile.FileName
      else if Text = 'count' then begin
        if V.AsFile.IsErf then
          S		:= IntToStr(V.AsFile.Erf.Count)
        else if V.AsFile.IsGff then
          S		:= IntToStr(V.AsFile.Gff.Root.Count)
        else if V.AsFile.IsFpt then
          S		:= IntToStr(V.AsFile.Fpt.GffCount)
        else
          Valid		:= False
      end
      else if Text = 'sig' then
        S		:= V.AsFile.Signature
      else if Text = 'ver' then
        S		:= V.AsFile.Version
      else if (Text = 'signature') or (Text = 'header') then
        S		:= V.AsFile.Signature + V.AsFile.Version
      //else if Text = 'type' then
        //S		:= IntToStr( Floor(Power(2, Integer(VarType))) )
      //else if Text = 'typestr' then
      //else if Text = 'modified' then
      else
        Valid		:= False;

  end;

  if not Valid then
    Complain(Err_LS_BadAttrib)
  else
    Value		:= S;

end;

(*------------------------------------------------------------------------------
Binary

The binary operations - that is, those that take one or two arguments,
operate upon them in a simple mathematical fashion, and produce a constant
unassignable result.

OOE is made left-to-right here in three ways:

 - Operator precedence
 - Early binding (TLetoScriptValue.Capture)
 - Left.Evaluate before Right.Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Binary;
var
  Co			: TLetoScriptObj;
  V			: TLetoScriptVar;
begin

  if Assigned(Left) then
    Left.Evaluate;
  if Assigned(Right) then
    Right.Evaluate;

  case Op of

    opAuto:
    begin

      if Kind in [acPostInc, acPostDec] then
        Co		:= Left
      else
        Co		:= Right;

      if not (Co.Kind in [symScalar, symList, symField]) then
        Exception(Err_LS_CantModify, Co.Description, Description);

      Co.FFlags		:= Co.FFlags + [vfVivify];
      V			:= Co.GetVar;

      if Kind in [acPostInc, acPostDec] then
        Value		:= V.AsReal;
      case Kind of
        acPostInc, acPreInc:
          V.Value	:= V.AsReal + 1;
        acPostDec, acPreDec:
          V.Value	:= V.AsReal - 1;
      end;
      if Kind in [acPreInc, acPreDec] then
        Value		:= V.AsReal;
    end;

  else
    Value		:= BinaryOp(Op, Left, Right);
  end;

end;

(*------------------------------------------------------------------------------
Boolean

Operations that take one or two arguments and produce a boolean result. The
result is 'assignable' in that its right side is given when asked, but that
token itself may or may not be assignable.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Boolean;
begin

  if Assigned(Left) then
    Left.Evaluate;

  { Short-circuit evaluation. }
  if
    Assigned(Right) and
    (
      not(Op in [opLogAnd, opLogOr, opLitAnd, opLitOr]) or
      ((Op in [opLogAnd, opLitAnd]) and Left.AsBool) or
      ((Op in [opLogOr, opLitOr]) and not Left.AsBool)
    )
  then
    Right.Evaluate;

  Value			:= BooleanOp(Op, Left, Right);
  Lvalue		:= Left;

end;

(*------------------------------------------------------------------------------
Bitwise

Operations that take one or two arguments: the left is a number to operate upon,
and the right is a bitmask of some sort which operates upon the left. The
result is a constant, and is 32 bits wide.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Bitwise;
var
  C			: Cardinal;
begin

  if Assigned(Left) then
    Left.Evaluate;
  if Assigned(Right) then
    Right.Evaluate;

  case Op of

    opBitNot:
      C			:= not Right.AsInt;
    opBitAnd:
      C			:= Left.AsInt and Right.AsInt;
    opBitOr:
      C			:= Left.AsInt or Right.AsInt;
    opBitXor:
      C			:= Left.AsInt xor Right.AsInt;
    opShl:
      C			:= Left.AsInt shl Right.AsInt;
    opShr:
      C			:= Left.AsInt shr Right.AsInt;

  else
    C			:= 0;

  end;

  Value			:= C;

end;

(*------------------------------------------------------------------------------
Binding

Allows for handle context, either for naming a Field in a specific handle,
or naming a resource in a specific ERF.

This is a workaround invented for the interim until object-oriented syntax
makes such naming a function of pointers to file objects.

The syntax is simply:

/field in %handle
'resource' in %erf

The logic is split between here, and Op_Symbol.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Bind;
begin
  { Let Op_Symbol handle the lookups. }
  Right.Evaluate;
  Left.Evaluate;
  
  Rvalue		:= Left;
end;

(*------------------------------------------------------------------------------
Range constructor

Produces a special constant type: a range, composed of a lower and upper
boundary, implying everything between.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Range;
begin

end;

(*------------------------------------------------------------------------------
Conditional (ternary)

A priceless shortcut for if-then, implemented as in C.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Conditional;
begin

  Left.Flags		:= Left.Flags + [vfConditional];
  Left.Evaluate;

  if Left.AsBool then begin
    Value		:= 1;
    Rvalue		:= Expr;
    Lvalue		:= Expr;
    Expr.Evaluate;
  end else begin
    Value		:= 0;
    Rvalue		:= Right;
    Lvalue		:= Right;
    Right.Evaluate;
  end;

end;

(*------------------------------------------------------------------------------
Assignment

Operations that take two arguments: the left is a symbol to be assigned to,
the right is the value that will be assigned.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_Assign;
begin
  // DONE 1: ADD: [27r3] Warning about assignments in conditionals
  if (vfConditional in FFlags) and (Left.Kind <> symFile) then
    Complain(Err_LS_AssInCond);

  FFlags		:= FFlags + [vfVivify];
  Left.Evaluate;

  { List context is arbitrary for a list left. The operation then becomes
    List Assignment. }
  // DONE 1: CHANGE: [27] Distinguish between scalar / list assignment
  if Left.IsList then begin
    Op			:= opLAssign;
    Right.InContext(ocList);
  end;
  Right.Evaluate;

  Assignment(Left, Right);
  Lvalue		:= Left;
  Rvalue		:= Left;

end;

(*------------------------------------------------------------------------------
List constructors

In a list context, these operands separate elements in the list. Otherwise
(any other context; void, boolean, scalar) they separate statements, executed
in order (left to right). The result of the former is, of course, a list.
The result of the latter is the result of the last statement executed (results
from any statements prior are discarded).

TODO:
Force an array:
(, foo)
Force a hash:
(=> , foo) (foo, =>)  # erm, don't do it the 2nd way, very inefficient

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Op_List;
begin

  if Assigned(Parent) and (vfEvalEach in Parent.FFlags) then
    FFlags		:= FFlags + [vfEvalEach];

  if not (vfEvalEach in FFlags) then begin
    if Assigned(Left) then
      Left.Evaluate;
    if Assigned(Right) then
      Right.Evaluate;
  end;

  Rvalue		:= Right;

end;


(*------------------------------------------------------------------------------

	Protected

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
property Defined

Frees Rvalue when a symbol is undefined.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.SetDefined(const ADefined: Boolean);
begin
  inherited;

  if ADefined or (Op <> opSymbol) or not Assigned(Rvalue) then Exit;

  if Lvalue.Undef then begin
    Lvalue		:= nil;
    Rvalue		:= nil;
  end;

end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptObj.Create(
  const AOwner		: TLetoScriptValue;
  const AEnv		: TLetoScriptEnv;
  const AOp		: TLetoScriptOp
);
begin
  inherited Create(AOwner, AEnv);

  ValType		:= vtObj;
  Op			:= AOp;
  Kind			:= okNone;

  FItems		:= TList.Create;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptObj.Destroy;
begin
  Clear;

  if Assigned(FItems) then
    FreeAndNil(FItems);

  if Assigned(FFunc) then
    TLetoScriptFunc(FFunc).Free;

  inherited;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoScriptObj.AsInt: Integer;
var
  V			: TLetoScriptValue;
  S			: String;
  HaveResult		: Boolean;
  procedure _Result_(const I: Integer);
  begin
    Result		:= I;
    HaveResult		:= True;
  end;
begin
  Result		:= 0;
  HaveResult		:= False;

  if not CheckInitialized then
    Complain(Err_LS_Uninit, GetNearestOp.Description)
  else if Kind = idBare then
    Complain(Err_LS_Bareword);

  if not Defined then
    _Result_(0);

  V			:= Rvalue;

  if not Assigned(V) then
    S			:= Value
  else if V.ValType <> vtObj then
    _Result_(V.AsInt)
  else if V.IsList then
    _Result_(V.AsInt)
  else
    S			:= V.AsString;

  if HaveResult then
  else if not TryStrToInt(S, Result) then
    Complain(Err_LS_NotNumeric);

end;

(*------------------------------------------------------------------------------
AsReal

------------------------------------------------------------------------------*)
function TLetoScriptObj.AsReal: Real;
var
  V			: TLetoScriptValue;
  S			: String;
  E			: Extended;
  HaveResult		: Boolean;
  procedure _Result_(const R: Real);
  begin
    Result		:= R;
    HaveResult		:= True;
  end;
begin
  Result		:= 0;
  HaveResult		:= False;

  if not CheckInitialized then
    Complain(Err_LS_Uninit, GetNearestOp.Description)
  else if Kind = idBare then
    Complain(Err_LS_Bareword);

  if not Defined then
    _Result_(0);

  V			:= Rvalue;

  if not Assigned(V) then
    S			:= Value
  else if V.ValType <> vtObj then
    _Result_(V.AsReal)
  else if V.IsList then
    _Result_(V.AsReal)
  else
    S			:= V.AsString;

  if HaveResult then
  else if not
    {$IFDEF MSWINDOWS}
    TryStrToFloat(S, E, Env.FormatSettings)
    {$ENDIF}
    {$IFDEF LINUX}
    TryStrToFloat(S, E)
    {$ENDIF}
  then
    Complain(Err_LS_NotNumeric)
  else
    Result		:= E;

end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoScriptObj.AsBool: Boolean;
var
  V			: TLetoScriptValue;
  S			: String;
begin

  if not CheckInitialized then
    Complain(Err_LS_Uninit, GetNearestOp.Description)
  else if Kind = idBare then
    Complain(Err_LS_Bareword);

//  Result		:= Defined;

  V			:= Rvalue;

  if not Assigned(V) then
    Result		:= Value
  else if V.ValType <> vtObj then
    Result		:= V.AsBool
  else if V.IsList then
    Result		:= V.AsInt > 0
  else begin
    S			:= V.AsString;
    Result		:= (S <> '0') and (S <> '');
  end;

end;

(*------------------------------------------------------------------------------
AsString

------------------------------------------------------------------------------*)
function TLetoScriptObj.AsString: String;
begin
  if not CheckInitialized then
    Complain(Err_LS_Uninit, GetNearestOp.Description)
  else if Kind = idBare then Complain(Err_LS_Bareword);

  Result		:= inherited AsString;

end;

(*------------------------------------------------------------------------------
IsScalar

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsScalar: Boolean;
begin
  Result		:= not IsList;
end;

(*------------------------------------------------------------------------------
IsList

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsList: Boolean;
var
  V			: TLetoScriptValue;
begin
  V			:= Rvalue;

  if Op in [opComma, opKey] then
    Result		:= True
  else if Assigned(V) then
    Result		:= V.IsList
  else
    Result		:= False;

end;

(*------------------------------------------------------------------------------
IsField

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsField: Boolean;
begin
  Result		:= (Kind = symField) or (Op = opBind);
end;

(*------------------------------------------------------------------------------
IsFile

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsFile: Boolean;
begin
  Result		:= Kind = symFile;
end;

(*------------------------------------------------------------------------------
IsScalarVar

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsScalarVar: Boolean;
begin
  Result		:= Kind = symScalar;
end;

(*------------------------------------------------------------------------------
IsListVar

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsListVar: Boolean;
begin
  Result		:= Kind = symList;
end;

(*------------------------------------------------------------------------------
IsBlock

------------------------------------------------------------------------------*)
function TLetoScriptObj.IsBlock: Boolean;
begin
  Result		:= Op = opBlock;
end;

(*------------------------------------------------------------------------------
Description

For error reporting, this function describes a meaningful description of the
object being reported, based on its function, Op, or Kind.

------------------------------------------------------------------------------*)
function TLetoScriptObj.Description(const CT: TCaseType): String;
var
  I			: Integer;
begin

  Result		:= 'Value';

  if Assigned(Func) and (Op <> opNamedOp) then
    Result		:= TLetoScriptFunc(Func).Name

  else if Kind = okNone then
  begin
    for I := Low(OpNames) to High(OpNames) do
      if OpNames[I].Op = Op then begin
        Result		:= OpNames[I].Name;
        Break;
      end;
  end

  else
  begin
    for I := Low(OpKindNames) to High(OpKindNames) do
      if OpKindNames[I].Kind = Kind then begin
        Result		:= OpKindNames[I].Name;
        Break;
      end;
  end;

  case CT of
    ctLowercase:
      Result		:= Lowercase(Result);
    ctUppercase:
      Result		:= Uppercase(Result);
    ctProper: ;
  end;

end;

(*------------------------------------------------------------------------------
Each

This is an intelligent iterator built for multiple types of Token; list
operators (commas, keys), variables (lists), etc. It is recursive to support
hierarchies.

Note that Each is intended only for use with List types. It will remit nil
for any other object type.

------------------------------------------------------------------------------*)
function TLetoScriptObj.Each(
  const EachType	: TLetoScriptEachType
): TLetoScriptValue;
const
  EACH_LEFT_INIT	= 0;
  EACH_LEFT		= 1;
  EACH_RIGHT_INIT	= 2;
  EACH_RIGHT		= 3;
  EACH_DONE		= 4;
var
  Empty			: Boolean;
  FieldVal		: TGffField;
  FileVal		: TLetoFile;
  Val			: TLetoScriptValue;

  procedure _CaptureFlag_(const V: TLetoScriptValue);
  begin
    if vfKey in V.Flags then
      Include(FFlags, vfKey)
    else if vfKeyValue in V.Flags then
      Include(FFlags, vfKeyValue);
  end;

begin

  Result		:= nil;

  if FEach = EACH_DONE then Exit;

  FFlags		:= FFlags - [vfKey, vfKeyValue];

  Empty			:= True;

  case Op of
    opSymbol:
      if
        ( (Kind = symScalar) and Rvalue.Defined ) or
        (Context = ocSymList) or
        (EachType = etCommas)
      then
        Result		:= self
      else
        Empty		:= not Rvalue.Defined;
    opExpr:
      if (EachType = etCommas) then
        Empty		:= not Assigned(Left)
      else
        Empty		:= not Assigned(Rvalue);
    opBind:
      Empty		:= False;
    opSet, opAttrib:
      Empty		:= not Assigned(Rvalue);
    opComma, opKey:
      Empty		:= False;
    opIConst, opFunc, opUnaryOp, opListOp:
    begin
      Empty		:= not Assigned(Rvalue);
      Result		:= self;
    end;
  else
    Result		:= self;
  end;

  if Empty then begin
    FEach		:= EACH_DONE;
    Exit;
  end;

  case Op of

    opSymbol:
    begin

      if FEach = EACH_LEFT_INIT then begin
        Rvalue.EachInit;
        FEach		:= EACH_LEFT;
      end;

      Result		:= Rvalue.Each(EachType);

      _CaptureFlag_(Rvalue);

      { Extra checks and measures... }
      if Assigned(Result) then
      case Result.ValType of
        vtField:
        begin
          FieldVal	:= Result.AsField;
          { Done iterating. }
          if not Assigned(FieldVal) then
            Result	:= nil;
        end;
        vtFile:
        begin
          FileVal	:= Result.AsFile;
          if not Assigned(FileVal) then
            Result	:= nil
          { Error opening this res - no warning, just $! }
          else if FileVal.Error <> Success then
            Env.IoErr	:= FileVal.Error
          { Done iterating. }
          else if not Assigned(FileVal.Gff) then
            Result	:= nil;
        end;
      end;

    end; { opSymbol }

    opBind:
      Result		:= Right.Each(EachType);

    opComma, opKey:
    case FEach of

      EACH_LEFT_INIT:
        if EachType = etCommas then begin
          FEach		:= EACH_RIGHT_INIT;
          Result	:= self;
        end else if not Assigned(Left) then begin
          FEach		:= EACH_RIGHT_INIT;
          Result	:= Each(EachType);
        end else begin
          if vfEvalEach in FFlags then
            Left.Evaluate;
          if Left.IsList then begin
            FEach	:= EACH_LEFT;
            Left.EachInit;
            Result	:= Each(EachType);
          end else begin
            if Op = opKey then
              Include(FFlags, vfKey);
            FEach	:= EACH_RIGHT_INIT;
            if (EachType <> etKeys) or (Op = opKey) then
              Result	:= Left
            else
              Result	:= Each(EachType);
          end;
        end;

      EACH_LEFT:
      begin
        if Op = opKey then
          Include(FFlags, vfKey);
        Result		:= Left.Each(EachType);
        if not Assigned(Result) then begin
          FEach		:= EACH_RIGHT_INIT;
          Result	:= Each(EachType);
        end else
          _CaptureFlag_(Left);
      end;

      EACH_RIGHT_INIT:
        if not Assigned(Right) then
          FEach		:= EACH_DONE
        else begin
          if vfEvalEach in FFlags then
            Right.Evaluate;
          if Right.IsList then begin
            FEach		:= EACH_RIGHT;
            Right.EachInit;
            Result	:= Each(EachType);
          end else begin
            if Op = opKey then
              Include(FFlags, vfKeyValue);
            FEach		:= EACH_DONE;
            if EachType <> etKeys then
              Result	:= Right;
          end;
        end;

      EACH_RIGHT:
      begin
        if Op = opKey then
          Include(FFlags, vfKeyValue);
        Result		:= Right.Each(EachType);
        if Assigned(Result) then
          _CaptureFlag_(Right);
      end;

    end; { opComma, opKey }

    opIConst, opExpr, opSet, opAttrib, opFunc, opUnaryOp, opListOp:
    begin
      if (Op = opExpr) and (EachType = etCommas) then
        Val		:= Left
      else
        Val		:= Rvalue;
      if FEach = EACH_LEFT_INIT then begin
        Val.EachInit;
        FEach		:= EACH_LEFT;
      end;
      Result		:= Val.Each(EachType);
      // DONE 1: BUG: [27r3] Functions returning hash not Each'ing properly
      _CaptureFlag_(Val);
    end;

  end; { case Op }

  if not Assigned(Result) then
    FEach		:= EACH_DONE;

end;

(*------------------------------------------------------------------------------
Complain

Output an error message, but do not immediately terminate. Termination is
either handled by the caller (script still being compiled) or is non-terminal
(a warning, during run-time). Terminal errors during run-time use the Exception
procedure below, which immediately halts program execution after outputting
the error.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Complain(
  const Err		: TLetoScriptError;
  const E1, E2, E3	: String;
  const Fatal		: Boolean
);
var
  S, Isa, Val		: String;
  OutputType		: TOutputType;

  procedure _Err_(const Symptoms: array of const);
  begin
    S			:= S + FormatLSError(Err, Symptoms);
  end;

begin

  if E1 <> '' then
    EStr1		:= E1;
  if E2 <> '' then
    EStr2		:= E2;
  if E3 <> '' then
    EStr3		:= E3;

  Isa			:= Description;
  Val			:= String(FValue);
  S			:= '[';

  {
  if Fatal then
    S			:= S + 'Error'
  else if Err = Err_LS_Exit then
    S			:= S + 'Exit'
  else if Err >= Err_LS_Notice then
    S			:= S + 'Notice'
  else if Err >= Err_LS_Hint then begin
    if not Env.Params.Hints then Exit;
    S			:= S + 'Hint';
  end
  else if Err >= Err_LS_Warn then begin
    if not Env.Params.Warnings then Exit;
    S			:= S + 'Warning';
  end
  else
    S			:= S + 'Error';
  }
  if Fatal then
    OutputType		:= otError
  else if Err = Err_LS_Exit then
    OutputType		:= otExit
  else if Err >= Err_LS_Notice then
    OutputType		:= otNotice
  else if Err >= Err_LS_Hint then begin
    if not Env.Params.Hints then Exit;
    OutputType		:= otHint;
  end
  else if Err >= Err_LS_Warn then begin
    if not Env.Params.Warnings then Exit;
    OutputType		:= otWarn;
  end
  else
    OutputType		:= otError;

  case OutputType of
    otInfo:		S := '[Information] ';
    otOutput: ;
    otExit:		S := '[Exit] ';
    otNotice:		S := '[Notice] ';
    otHint:		S := '[Hint] ';
    otWarn:		S := '[Warning] ';
    otError:		S := '[Error] ';
  end;

  case Err of

    { Errors }
    Err_LS_IO, Err_LS_NYI,
    Err_LS_UnknownOp, Err_LS_Syntax, Err_LS_NoComDocHere,
    Err_LS_InvalidAssoc, Err_LS_InvalidSide, Err_LS_ReadOnlyValue,
    Err_LS_BadOverload:
      _Err_([]);
    Err_LS_BadSub, Err_LS_UndefSub, Err_LS_SubNeedsPredecl,
    Err_LS_SubRedef, Err_LS_SubHasTooManyParms,
    Err_LS_SubBadParam, Err_LS_SubBadDefault, Err_LS_SubBadAttrib,
    Err_LS_BadDeref, Err_LS_Die, Err_LS_Exit:
      _Err_([EStr1]);
    Err_LS_NotInLib, Err_LS_LibDisabled,
    Err_LS_SubIdRedecl, Err_LS_SubBadShort, Err_LS_SubBadOption,
    Err_LS_BadArgType, Err_LS_MissingArg, Err_LS_CantModify,
    Err_LS_InvalidDeclare:
      _Err_([EStr1, EStr2]);
    Err_LS_LiveHandle:
      _Err_([Uppercase(Text)]);
    Err_LS_Unterminated, Err_LS_InvalidName,
    Err_LS_InvalidReal:
      _Err_([Isa]);
    Err_LS_InvalidNum:
      _Err_([Isa, Text[Length(Text)]]);
    Err_LS_BadIndex:
      _Err_([Val]);

    { Warnings }
    Err_LS_Embedded, Err_LS_AssInCond, Err_LS_NotNumeric,
    Err_LS_NoFile, Err_LS_ResRefOverflow, Err_LS_NoTlk, Err_LS_NotFptVarName:
      _Err_([]);
    Err_LS_Warn,
    Err_LS_Useless, Err_LS_Uninit, Err_LS_SubAmbiguous,
    Err_LS_InvalidFile, Err_LS_InvalidFileName,
    Err_LS_NotGffFile, Err_LS_NotErfFile, Err_LS_NotFptFile,
    Err_LS_ImpliedClose, Err_LS_Outside:
      _Err_([EStr1]);
    Err_LS_Deprecated, Err_LS_InvalidArg, Err_LS_SubAmbiguousParam:
      _Err_([EStr1, EStr2]);
    Err_LS_Bareword, Err_LS_InvalidField,
    Err_LS_BadAttrib, Err_LS_NotLocString:
      _Err_([Text]);

  end;

  S := S +
    FormatLSError(
      Err_LS_AtLineNear,
      [Env.Name, Line, LinePos {, Location}]
    );

  if not Env.OutputNL then
    S			:= sLineBreak + S;
  Env.Output(S + sLineBreak, OutputType);

end;

(*------------------------------------------------------------------------------
Exception

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Exception(
  const Err		: TLetoScriptError;
  const E1, E2, E3	: String
);
begin
  Complain(Err, E1, E2, E3, True);
  raise ELetoScriptError.Create('');
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Clear;
var
  I			: Integer;
begin
  if Assigned(FLeft) then
    FLeft.Free;
  if Assigned(FRight) then
    FRight.Free;
  if Assigned(FExpr) then
    FExpr.Free;
  if Assigned(FBlock) then
    FBlock.Free;

  if not Assigned(FItems) then Exit;
  
  for I := 0 to FItems.Count-1 do
    TLetoScriptObj(FItems[I]).Free;
  FItems.Clear;

end;

(*------------------------------------------------------------------------------
WantsBlock

Can this object take a block, and if so, does it not already have one
assigned?

------------------------------------------------------------------------------*)
function TLetoScriptObj.WantsBlock: Boolean;
begin
  Result :=
    not Assigned(Block) and
    (
      (Op in [opNamedOp]) or
      (Kind in [])
    );
end;

(*------------------------------------------------------------------------------
WantsExpr

Can this object take an expr, and if so, does it not already have one
assigned?

------------------------------------------------------------------------------*)
function TLetoScriptObj.WantsExpr: Boolean;
begin
  Result :=
    not Assigned(Expr) and
    (
      (Op in [opUnaryOp, opListOp, opFunc]) or
      (Kind in [idAttrib, noIf, noElsif, noUnless, noFor, noWhile, noSwitch])
    );
end;

(*------------------------------------------------------------------------------
CheckSides

Operators (or named ops) get their sides checked, to enforce basic syntax.

------------------------------------------------------------------------------*)
function TLetoScriptObj.CheckSides: Boolean;
var
  WLeft, WRight		: Boolean;
begin
  case Kind of
    { Polyamorous cases }
    noIf, noUnless:
      Result :=
        (
          Assigned(Expr) and Assigned(Block) and
          not Assigned(Left) and not Assigned(Right)
        ) or (
          Assigned(Left) and Assigned(Right) and
          not Assigned(Expr) and not Assigned(Block)
        );
    noFor, noWhile:
      Result :=
        (
          Assigned(Block) and Assigned(Expr) and
          not Assigned(Left)
        ) or (
          not Assigned(Block) and Assigned(Left) and
          (Assigned(Right) or Assigned(Expr))
        );
    { Unique syntax }
    noElse, noElsif:
      Result :=
        Assigned(Parent) and Assigned(Parent.Parent) and
        Assigned(Parent.Parent.Left) and
        (Parent.Parent.Left.Kind in [noIf, noElsif]) and
        Assigned(Block) and ((Kind = noElse) or Assigned(Expr));
  else
    begin
      // The special case of what appears to be an unidentified subroutine:
      if (Kind = idBare) and Assigned(Right) then
        Exception(Err_LS_SubNeedsPredecl, Text);
      // The special case of binding:
      if (Op = opBind) and (Right.Kind <> symFile) then
        Exception(Err_LS_NotInFile);
      WLeft		:= (Op in OpHasLeft) or (Kind in KindHasLeft);
      WRight		:= (Op in OpHasRight) or (Kind in KindHasRight);
      Result :=
        (
        { Sides don't matter }
          (Op in OpHasLeftOrRight) or
          (Kind in KindHasLeftOrRight)
        ) or ( (
        { Wants left and has left }
          (WLeft and Assigned(Left)) or
        { Does not want left and has no left }
          (not WLeft and not Assigned(Left))
        ) and (
        { Wants right and has right }
          (WRight and Assigned(Right)) or
        { Does not want right and has no right }
          (not WRight and not Assigned(Right))
        ) );
    end;
  end;

end;

(*------------------------------------------------------------------------------
CheckFoldable

------------------------------------------------------------------------------*)
function TLetoScriptObj.CheckFoldable: Boolean;
begin

  Result :=

    { Functions }
    (
      Assigned(Func) and
      TLetoScriptFunc(Func).CheckFoldable
    )

    or

    { Operators }
    (
      (Op in Foldable) and
      (
        not Assigned(Left) or (Left.Kind in NonInterpolative)
      ) and (
        not Assigned(Right) or (Right.Kind in NonInterpolative)
      )
    )

    ;

end;

(*------------------------------------------------------------------------------
CheckArgs

Functions get their arguments checked during preprocessing, so that syntax
errors can be reported even for a function that isn't executed.

Subroutines defined with formal arguments are also subject to this check.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.CheckArgs;
var
  F			: TLetoScriptFunc;
  Args			: TLetoScriptArgs;
  I			: Integer;
  Sub			: TLetoScriptSub;
  Arg			: TLetoScriptArg;
begin
  F			:= nil;
  Sub			:= nil;

  if Assigned(Func) then
    F			:= TLetoScriptFunc(Func)
  else
    Exception(Err_LS_Syntax);

  Args			:= F.Args;

  { Subroutine - extra checks }
  if (F.Parent.Name = 'system') and (F.Fn = Word(fnSub)) then begin
    I			:= Env.Subs.IndexOf(Text);
    if I = -1 then
      Exception(Err_LS_UndefSub);
    Sub			:= TLetoScriptSub(Env.Subs.Objects[I]);
    Args.CopyDefinition(Sub.Args);
  end;

  if Assigned(Sub) then
    EStr2		:= '&' + Text
  else
    EStr2		:= Text;

  { Assign list context }
  if F.Attribs.ListOp then begin
    if Assigned(Expr) then
      Expr.InContext(ocList)
    else if Assigned(Right) then
      Right.InContext(ocList);
  end;


  { Check syntax }

  if Assigned(Expr) then
    BuildArgs(Expr, Args)
  else
    BuildArgs(Right, Args);

  { Check required args }

  for I := 0 to Args.Standards.Count-1 do begin
    Arg			:= Args.GetByIndex(Args.Standards, I);
    if Arg.Specs.NeedsValue and not Arg.ValueAssigned then
      Exception(Err_LS_MissingArg, Arg.FullName);
  end;

  { Check types }

  for I := 0 to F.Args.Standards.Count-1 do
    if not CheckArgType(F.Args.GetByIndex(F.Args.Standards, I)) then
      Exception(Err_LS_BadArgType);

  for I := 0 to F.Args.Options.Count-1 do
    if not CheckArgType(F.Args.GetByIndex(F.Args.Options, I)) then
      Exception(Err_LS_BadArgType);

end;

(*------------------------------------------------------------------------------
BuildArgs

Used by CheckArgs. Iterates over this token's list of arguments presumed to
be delineated by only commas and keys. Symbols and results are always kept
whole. This is a miniature of TLetoScriptObj.Each.

------------------------------------------------------------------------------*)
function TLetoScriptObj.BuildArgs(
  const Obj		: TLetoScriptObj;
  const Args		: TLetoScriptArgs
): TLetoScriptError;
begin

  Result		:= LS_Success;

  if not Assigned(Obj) then Exit;

  // DONE 1: ADD: [27r3] List-separator between greedy list ; options
  if Obj.Op = opStop then begin
    BuildArgs(Obj.Left, Args);
    Args.ListMode	:= False;
    BuildArgs(Obj.Right, Args);
    Exit;
  end;

  if Args.ListMode then begin
    Result		:= Args.AddValue('', Obj);
    if Result <> LS_Success then
      Complain(Result, IntToStr(Args.LastArgNo));
    Exit;
  end;

  case Obj.Op of

    opComma:
    begin
      BuildArgs(Obj.Left, Args);
      BuildArgs(Obj.Right, Args);
      Exit;
    end;

    opKey:
    begin
      // Reserved syntax: FUNCTION => arg
      if not Assigned(Obj.Left) then
        Exit;
      Obj.Left.Evaluate;
      Result		:= Args.AddValue(Obj.Left.AsString, Obj.Right)
    end;

  else { an argument }
    if Obj.Kind in [idBare, idSafe] then begin
      Obj.Op		:= opConst;
      Obj.Kind		:= numInt;
      Obj.Value		:= 1;
      Result		:= Args.AddValue(Obj.Text, Obj);
    end else
      Result		:= Args.AddValue('', Obj);

  end;

  if Result <> LS_Success then
    Complain(Result, IntToStr(Args.LastArgNo));

end;

(*------------------------------------------------------------------------------
CheckArgType

------------------------------------------------------------------------------*)
function TLetoScriptObj.CheckArgType(const Arg: TLetoScriptArg): Boolean;
var
  ANoEval		: Boolean; // or should it be CNoEval?
begin
  { A nil / default value is always the correct type }
  Result		:= not Arg.ValueAssigned;
  if Result then Exit;

  //EStr1			:= IntToStr(Arg.ArgNo);
  EStr1			:= Arg.Name;
  EStr2			:= '';

  ANoEval		:= Arg.Specs.NoEval;
  Arg.Specs.NoEval	:= True;

  case Arg.ArgType of
    atScalar: ;		{ Any value accepted }
    atList: ;		{ Any value accepted }
    atField:
      if not Arg.Value.IsField then
        EStr2		:= 'FIELD';
    atFile:
      if not Arg.Value.IsFile then
        EStr2		:= 'HANDLE';
    atBlock:
      if not Arg.Value.IsBlock then
        EStr2		:= 'BLOCK';
    atScalarVar:
      if not Arg.Value.IsScalarVar then
        EStr2		:= 'SCALAR VAR';
    atListVar:
      if not Arg.Value.IsListVar then
        EStr2		:= 'LIST VAR';
  end;

  Arg.Specs.NoEval	:= ANoEval;

  Result		:= EStr2 = '';

end;

(*------------------------------------------------------------------------------
CheckInitialized

The result is False when this object is "uninitialized", and should warn about
its value being used in this sense. Some uninit values do not yield warnings,
however, such as boolean or assignment. Uninit is also valid in void context.

------------------------------------------------------------------------------*)
function TLetoScriptObj.CheckInitialized: Boolean;
begin

  Result :=
    not Assigned(Parent) or
    Defined or
    ( Parent.Op in [
      opLogNot, opLogAnd, opLogOr,
      opCondIf, opCondThen,
      opAssign, opLAssign,
      opLitNot,
      opLitAnd,
      opLitOr, opLitXor,
      opNamedOp
    ] );

end;

(*------------------------------------------------------------------------------
CheckUseless

Useless use of a %s in void context.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.CheckUseless;
var
  S			: String;
begin

  // Requires the same full logic as foldable code. Anything foldable is
  // useless in a void context, even including functions.

  if S <> '' then
    Complain(Err_LS_Useless, S);

end;

(*------------------------------------------------------------------------------
Preprocess

This function runs immediately after TLetoScript.Parser has completed. The
job of the preprocessor is to:
  * Ensure each token's Left and Right sides are correct.
  * Optimize (code folding).

Because Preprocess is recursive, Parser only needs call it on the Root.
Furthermore, because Preprocess augments the object, it must *not* be called
more than once on any object. FInternal is borrowed for this purpose.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Preprocess;
begin

  // DONE 1: BUG: [27r2] Duplicate Preprocess 
  if Internal then Exit;
  Internal		:= True;

  if Assigned(Left) then
    Left.Preprocess;
  if Assigned(Right) then
    Right.Preprocess;
  if Assigned(Expr) then
    Expr.Preprocess;

  case Op of
    opUnknown:
      Exception(Err_LS_Syntax);
    opKey:
      if Assigned(Left) then begin
        Left.Op		:= opConst;
        if Left.Kind <> quDouble then
          Left.Kind	:= quSingle;
      end;
    opFunc, opUnaryOp, opListOp:
      CheckArgs;
  end;

  if not CheckSides then
    Exception(Err_LS_InvalidSide);

  if CheckFoldable then begin
    Evaluate;
    Op			:= opConst;
    Kind		:= GetValueKind(Value);
    if Assigned(FLeft) then
      FreeAndNil(FLeft);
    if Assigned(FRight) then
      FreeAndNil(FRight);
    if Assigned(Func) then begin
      TLetoScriptFunc(Func).Free;
      Func		:= nil;
    end;
  end;

  if Assigned(FItems) then
    FreeAndNil(FItems);

end;

(*------------------------------------------------------------------------------
InContext

The parent of this token is placing Context upon this part of the statement;
so this token and all of its children take that Context. (Child tokens
may assign a different context to their own children, though.)

Block and Expr do not inherit the context because they are always tied
explicitly to the context of the token owning them.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.InContext(const AContext: TLetoScriptOpCon);
begin
  Context		:= AContext;
  if Assigned(Left) then
    Left.InContext(AContext);
  if Assigned(Right) then
    Right.InContext(AContext);
end;

(*------------------------------------------------------------------------------
AncestorHasFlag

Does some parent of this object have Flag set?

------------------------------------------------------------------------------*)
function TLetoScriptObj.AncestorHasFlag(
  const Flag		: TLetoScriptValFlag
): Boolean;
begin
  Result		:= Flag in Flags;
  if not Result and Assigned(Parent) then
    Result		:= Parent.AncestorHasFlag(Flag);
end;

(*------------------------------------------------------------------------------
Reset

Eliminate any leftover value / Rvalue from a previous call. This sanitizes
the token in a loop, where it may in obscure situations be considered
after an evaluation but before subsequent evaluation.

 - Sanitization is an anti-bug measure. There shouldn't ever *be* a situation
   where a token is inspected before evaluation, and evaluation should be
   absolute in setting value and definedness (nothing leaked from previous
   runs).

 - The call to EachInit is an attempt at optimization (no need for a caller
   to EachInit if the Obj has just been evaluated), but it is also valid to
   Each an Obj several times after evaluation, so EachInit will still live
   in many places in the code.
   -- EachInit is buggy if Evaluate may be called in some conditions,
   such as nested loops (?)

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Reset;
begin

  //EachInit;

  // Lvalue := nil ?
  Rvalue		:= nil;

  if not (Op in [opConst, opIConst, opSymbol]) then begin
    Value		:= '';
    Defined		:= True;
  end;

end;

(*------------------------------------------------------------------------------
Evaluate

Executes this token, and consequently all tokens beneath it (in its tree).

This is generically just a wrapper that passes control onto more specialized
functions for all of the various token types.

Some OOE is defined here (usually left-to-right); but the more relevant aspect
of OOE is not Evaluation, rather it's the use of Rvalue. (Which is generally
aggressive, resulting in what should be a very natural left-to-right tree-down
Order of Execution.)

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.Evaluate(const Con: TLetoScriptOpCon);
begin

//  if Con <> ocScalar then
//    InContext(Con);

  Reset;
  if Con = ocVoid then CheckUseless;

  case Op of
    opNull: ;

    opStop:
    begin
      if Assigned(Left) then begin
        Env.Last	:= Left;
        Left.Evaluate(Con);
      end;
      if Assigned(Right) then begin
        Env.Last	:= Right;
        Right.Evaluate(Con);
      end;
    end;

    opIConst:
      case Kind of
        quDouble, quQQ:
          Op_Quoted;
        quPrintn, quCmd, quQX, quQXU:
        begin
          if (Kind <> quQXU) then
            Op_Quoted;
          TLetoScriptFunc(Func).Evaluate;
        end;
        quQW: ;
      end;

    opBlock, opExpr, opSet:
    begin
      if Assigned(Left) then
        Left.Evaluate(Con);
      Rvalue		:= Left;
      // TODO 4: BUG: Lvalue of Block, Set is not intuitive
      Lvalue		:= Left;
    end;

    { Do not execute the definition. }
    opSub: ;

    opSymbol:
      Op_Symbol;

    {opDeref,} opRef:
      Op_Reference;

    opAttrib:
      Op_Attrib;

    opAuto, opBinExp, opUnaMinus, opUnaPlus,
    opBinMult, opBinDiv, opBinMod, opBinRep,
    opBinPlus, opBinMinus, opLitPlus, opLitMinus, opAddSp: {opRemSp}
      Op_Binary;

    opLogNot, opLogAnd, opLogOr,
    opNumLT, opNumGT, opNumLE, opNumGE, opStrLT, opStrGT, opStrLE, opStrGE,
    opNumEq, opNumNE, opNumCmp, opStrEq, opStrEqI, opStrNE, opStrNEI, opStrCmp,
    opLitNot, opLitAnd, opLitOr, opLitXor:
    begin
      Op_Boolean;
      if Op in [opNumLT..opStrCmp] then
        Lvalue		:= Left;
    end;

    opBitNot, opBitAnd, opBitOr, opBitXor,
    opShl, opShr:
      Op_Bitwise;

    opBind:
      Op_Bind;

    opRange:
      Op_Range;

    opCondIf, opCondThen:
      Op_Conditional;

    opAssign, opLAssign:
      Op_Assign;

    opComma, opKey:
      Op_List;

    opFunc, opUnaryOp, opListOp, opNamedOp:
      TLetoScriptFunc(Func).Evaluate

  else
    // error

  end;

end;

(*------------------------------------------------------------------------------
EvalDefined

Certain arguments to functions are in symbolic format, e.g. %HANDLE,
but act like Delphi var arguments in that they will receive a value, rather
than provide one to the function.

To Evaluate such an argument generates a warning. However, the function
may need to know if the symbol is already in use, which requires evaluation.
The Defined property and ofDefinedCheck can be used to get around this
problem (the specific intent of the defined function, itself), so EvalDefined
combines the setting of that flag, Evaluate, and unsetting the flag, plus
returning the result of Defined, in a single easy function.

------------------------------------------------------------------------------*)
function TLetoScriptObj.EvalDefined: Boolean;
begin
  Include(FFlags, vfDefinedCheck);
  Evaluate;
  Exclude(FFlags, vfDefinedCheck);
  Result		:= Defined;
end;

(*------------------------------------------------------------------------------
GetNearestScope

------------------------------------------------------------------------------*)
function TLetoScriptObj.GetNearestScope: TLetoScriptVarScope;
var
  Token			: TLetoScriptObj;
begin
  Token			:= self;
  Result		:= nil;

  while not Assigned(Result) do begin
    if Assigned(Token) then begin
      Result		:= Token.Scope;
      Token		:= Token.Parent;
    end else
      Result		:= Env.GlobalScope;
  end;

end;

(*------------------------------------------------------------------------------
GetNearestArgScope

Nearest scope that supports @_.

------------------------------------------------------------------------------*)
function TLetoScriptObj.GetNearestArgScope: TLetoScriptVarScope;
var
  Token			: TLetoScriptObj;
begin
  Token			:= self;
  Result		:= nil;

  while not Assigned(Result) do begin
    if Assigned(Token) then begin
      if Assigned(Token.Scope) and Assigned(Token.Scope.ListDefault) then
        Result		:= Token.Scope;
      Token		:= Token.Parent;
    end else
      Result		:= Env.GlobalScope;
  end;

end;

(*------------------------------------------------------------------------------
GetVar

Retrieves the TLetoScriptVar this symbol bears. Intended primarily for
Op_Symbol (to reduce noise, and for redundancy in /$FieldNameInVar), but
also exposed so that functions (like push, unshift) can "summon" a var.

Not the same as AsVar, which is just a lookup.

------------------------------------------------------------------------------*)
function TLetoScriptObj.GetVar(Name: String): TLetoScriptVar;
var
  VT			: TLetoScriptValType;
  Acc			: String;
  IndexIsInt		: Boolean;
  Vivify, My		: Boolean;
  VL			: TLetoScriptVarList;
  Index			: Integer;
begin

  Result		:= nil;

  if Name = '' then begin
    Name		:= FValue;
    if Assigned(Expr) or (Kind = symList) then
      VT		:= vtArray
    else
      VT		:= vtScalar;
  end
  else begin
    if Name[1] = '$' then
      VT		:= vtScalar
    else if Name[1] = '@' then
      VT		:= vtArray
    else
      Exit;
    System.Delete(Name, 1, 1);
  end;

  if Assigned(Expr) then begin
    if not Expr.Defined then begin
      Acc		:= '';
      Complain(Err_LS_Uninit, 'list element');
    end else
      Acc		:= Expr.AsString;
  end;
  IndexIsInt		:= TryStrToInt(Acc, Index);
  Result		:= nil;

  { Lookup / vivification }
  Vivify		:= VarShouldVivify;
  My			:= AncestorHasFlag(vfMyVivify);
  if (Name = '_') and (VT = vtArray) then begin
    Result		:= GetNearestArgScope.Get(Name, VT, False);
    Exit;
  end;
  if My or not Vivify then
    Result		:= GetNearestScope.Get(Name, VT, My);
  if not Assigned(Result) then
    Result		:= Env.GlobalScope.Get(Name, VT, Vivify);

  if not Assigned(Result) or not Assigned(Expr) then Exit;

  { Accessor }
  VL			:= TLetoScriptVarList(Result);
  Vivify		:= Vivify or My;

  if Vivify and IndexIsInt and (Index < 0) then begin
    Value		:= Index;
    Exception(Err_LS_BadIndex);
  end;

  { Use the correct undef key type for a newly vivified array. }
  if (VL.ValType = vtArray) and not Expr.Defined then
    IndexIsInt		:= TryStrToInt('0', Index);

  { Do not promote to hash on a lookup with a non-int value. }
  if (VL.ValType = vtArray) and (IndexIsInt or not Vivify) then
    Result		:= VL.Get(Index, Vivify)
  else begin
    VL.ValType		:= vtHash;
    Result		:= VL.Get(Acc, Vivify);
  end;

end;

(*------------------------------------------------------------------------------
EstablishList

Sets up this Token's Rvalue to be a (copy of a) list. Used in CaptureList
scenarios.

------------------------------------------------------------------------------*)
function TLetoScriptObj.EstablishList: TLetoScriptVarList;
begin
  Result		:= TLetoScriptVarList.Create(self, Env, '', vtArray);
  Rvalue		:= Result;
end;

(*------------------------------------------------------------------------------
CaptureList

A collection of routines for assigning a list of values to this token.
The token's value type becomes a list (Rvalue is a TLetoScriptVarList),
and then the argument is iterated across, each of its values copied
onto this token's growing list.

------------------------------------------------------------------------------*)
procedure TLetoScriptObj.CaptureList(
  const List		: TStringList;
  const FreeList	: Boolean
);
var
  PList			: TLetoScriptVarList;
  I			: Integer;
begin
  PList			:= EstablishList;
  if not Assigned(List) then
    Exit;
  for I := 0 to List.Count-1 do
    PList.Add.Value	:= List[I];
  if FreeList then
    List.Free;
end;

procedure TLetoScriptObj.CaptureList(
  const V		: TLetoScriptValue;
  const EachType	: TLetoScriptEachType
);
begin
  // TODO 4: CHANGE: optimize when copy is a VarList to VarList
  EstablishList.Push(V, EachType);
end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
