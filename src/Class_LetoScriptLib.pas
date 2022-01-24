(*------------------------------------------------------------------------------
Class_LetoScriptLib

The functions supported by LetoScript mostly reside in this unit, for better
maintainability. Some functions have been moved out to separate units for
isolation and readability; this is the recommended approach for adding new
functions (libraries). All functions must be declared in Header_LetoScript.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoScriptLib;

{$I LetoScript.inc}

interface

uses
  SysUtils, Classes, StrUtils, DateUtils, Math, TypInfo,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF}
  Header_Leto, Header_LetoScript,
  Class_LetoFile, Class_ErfFile, Class_ErfStruct, Class_GffFile, Class_GffField,
  Class_TlkFile, Class_XbaseFile, Class_LetoXml, Class_2daFile,
  Class_MultiEvent,
  Class_LetoScriptEnv, Class_LetoScriptObj;

type

  TLetoScriptFunc	= class;

  TLetoScriptLibSys	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptLibMath	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptLibGff	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptLibErf	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptLibBic	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptLibTlk	= class(TLetoScriptLib)

  public

    Tlk			: TTlkFile;
    TlkF		: TTlkFile;
    CustomTlk		: TTlkFile;
    CustomTlkF		: TTlkFile;

    Mode		: TUseTlkMode;

    constructor Create(
      const AName	: String;
      const AEnv	: TLetoScriptEnv
    ); override;
    destructor Destroy; override;
    function CreateFunc(const Syntax: String): Pointer; override;

  end;

  TLetoScriptLib2da	= class(TLetoScriptLib)

  private

    Cache		: T2daHash;

  public

    constructor Create(
      const AName	: String;
      const AEnv	: TLetoScriptEnv
    ); override;
    destructor Destroy; override;
    function CreateFunc(const Syntax: String): Pointer; override;

  end;

  TLetoScriptLibFpt	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptLibFileSys	= class(TLetoScriptLib)
  public
    function CreateFunc(const Syntax: String): Pointer; override;
  end;

  TLetoScriptFunc	= class

  private

  protected

    FName		: String;

    ArgObj		: TLetoScriptObj;
    List		: TLetoScriptVarList; { Used by InitList. }

    function GetName: String; virtual;
    procedure SetName(const AName: String); virtual;

    function GetOp: TLetoScriptOp; virtual;

    function Arg: TLetoScriptArg;
    function ArgVal: TLetoScriptValue;
    function ArgIsObj(const Name: String = ''): Boolean;

    procedure InitList;

    procedure Output(const S: String; const Nl: Boolean = False);

  public

    Parent		: TLetoScriptLib;
    Env			: TLetoScriptEnv;
    Fn			: Word;
    Args		: TLetoScriptArgs;
    Attribs		: TLetoScriptAttribs;

    Token		: TLetoScriptObj;

    property Name: String read GetName write SetName;
    property Op: TLetoScriptOp read GetOp;

    constructor Create(
      Syntax		: String;
      const AParent	: TLetoScriptLib;
      const AEnv	: TLetoScriptEnv
    ); virtual;
    destructor Destroy; override;

    function CheckFoldable: Boolean; virtual;
    function CheckArgFoldable(const A: TLetoScriptArg): Boolean;

    procedure Evaluate; virtual;

    function Describe: String; virtual;

  end;

  TLetoScriptLoopType	= ( ltDo, ltFor, ltWhile, ltResource );
  TLetoScriptLoopParams	= record
    LoopType		: TLetoScriptLoopType;
    List, Loop		: TLetoScriptObj;
    LoopIf, LoopDo	: TLetoScriptObj;
    Default		: TLetoScriptValue;
  end;

  TLetoScriptFuncSys	= class(TLetoScriptFunc)

  private

    procedure Loop(LoopParams: TLetoScriptLoopParams);
    function SystemExec(
      const CmdStr	: String;
      const Wait	: Boolean;
      const StdOut	: TStringList = nil
    ): Cardinal;

    procedure _Do;
    procedure _Else;
    procedure _Elsif;
    procedure _For;
    procedure _If;
    procedure _Unless;
    procedure _While;

    procedure _Caller;
    procedure _Chr;
    procedure _Close;
    procedure _Defined;
    procedure _Delete;
    procedure _Die;
    procedure _Exit;
    procedure _Greedy;
    procedure _IsNum;
    procedure _Keys;
    procedure _Lc;
    procedure _Length;
    procedure _My;
    procedure _Ord;
    procedure _Pop;
    procedure _Print;
    procedure _PrintF;
    procedure _PrintN;
    procedure _Push;
    procedure _Rand;
    procedure _Return;
    procedure _Shift;
    procedure _Sleep;
    procedure _Sub;
    procedure _Subst;
    procedure _Substr;
    procedure _System;
    procedure _SystemQ;
    procedure _Time;
    procedure _Uc;
    procedure _Undef;
    procedure _Unshift;
    procedure _Values;
    procedure _Var;
    procedure _Warn;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncMath	= class(TLetoScriptFunc)

  private

    procedure _Abs;
    procedure _Ceil;
    procedure _DivMod;
    procedure _Exp;
    procedure _Floor;
    procedure _Frac;
    procedure _Frexp;
    procedure _Int;
    procedure _IsZero;
    procedure _Ldexp;
    procedure _Ln;
    procedure _LnXP1;
    procedure _Log2;
    procedure _Log10;
    procedure _LogN;
    procedure _Max;
    procedure _Min;
    procedure _MulDiv;
    procedure _Poly;
    procedure _Round;
    procedure _RoundTo;
    procedure _SameValue;
    procedure _Sign;
    procedure _SimpleRoundTo;
    procedure _Sqr;
    procedure _Sqrt;
    procedure _Trunc;

    procedure _ACos;
    procedure _ACosH;
    procedure _ACot;
    procedure _ACotH;
    procedure _ACsc;
    procedure _ACscH;
    procedure _ASec;
    procedure _ASecH;
    procedure _ASin;
    procedure _ASinH;
    procedure _ATan;
    procedure _ATan2;
    procedure _ATanH;
    procedure _Cosin;
    procedure _CosH;
    procedure _Cotan;
    procedure _CotH;
    procedure _Cosecant;
    procedure _CscH;
    procedure _Hypot;
    procedure _Secant;
    procedure _SecH;
    procedure _Sin;
    procedure _SinCos;
    procedure _SinH;
    procedure _Tan;
    procedure _TanH;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncGff	= class(TLetoScriptFunc)

  private

    Gff			: TGffFile;
    Field		: TGffField;
    Context		: TGffField;

    MatchParams		: TGffMatchParams;

    procedure SetupFind(
      const Root	: TGffField;
      const Name, Value	: String
    );

    procedure DoMatch(
      Sender		: TObject;
      Field		: TGffField;
      var Matches	: Boolean
    );

    procedure _Add;
    procedure _Clear;
    procedure _Delete;
    procedure _Find;
    procedure _Replace;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncErf	= class(TLetoScriptFunc)

  private

    Erf			: TErfFile;

    procedure _Export;
    procedure _Import;
    procedure _List;
    procedure _Remove;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncBic	= class(TLetoScriptFunc)

  private

    procedure _FindBicByTag;
    procedure _FindNewestBic;
    procedure _Vault;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncTlk	= class(TLetoScriptFunc)

  private

    Lib			: TLetoScriptLibTlk;

    procedure _Dialog;
    procedure _AddStrref;
    procedure _FindStrref;
    procedure _GetStrref;
    procedure _GetStrrefFlags;
    procedure _SetStrref;
    procedure _SetStrrefFlags;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFunc2da	= class(TLetoScriptFunc)

  private

    Lib			: TLetoScriptLib2da;

    procedure DoMatch(
      Sender		: TObject;
      Row		: Integer;
      Col		: String;
      ICol		: Integer;
      Value		: String;
      var Matches	: Boolean
    );

    // TODO 4: ADD: $! results from failed 2DA loads
    procedure _Locate;
    procedure _LLocate;
    procedure _Lookup;
    procedure _Meta;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncFpt	= class(TLetoScriptFunc)

  private

    Lib			: TLetoScriptLibFpt;
    Fpt			: TLetoFile;

    procedure _Extract;
    procedure _Inject;

  public

    procedure Evaluate; override;

  end;

  TLetoScriptFuncFileSys = class(TLetoScriptFunc)

  private

    procedure _Copy;
    procedure _Delete;
    procedure _DirExists;
    procedure _FileExists;
    procedure _FileInfo;
    procedure _ForceDirs;
    procedure _Rename;

  public

    procedure Evaluate; override;

  end;


implementation

uses
  Class_LetoScriptSub;

const
  GFF_BFT_INVALID	= $0000;
  GFF_BFT_BYTE		= $0001;
  GFF_BFT_CHAR		= $0002;
  GFF_BFT_WORD		= $0004;
  GFF_BFT_SHORT		= $0008;
  GFF_BFT_DWORD		= $0010;
  GFF_BFT_INT		= $0020;
  GFF_BFT_DWORD64	= $0040;
  GFF_BFT_INT64		= $0080;
  GFF_BFT_FLOAT		= $0100;
  GFF_BFT_DOUBLE	= $0200;
  GFF_BFT_STRING	= $0400;
  GFF_BFT_RESREF	= $0800;
  GFF_BFT_LOCSTRING	= $1000;
  GFF_BFT_VOID		= $2000;
  GFF_BFT_STRUCT	= $4000;
  GFF_BFT_LIST		= $8000;
  GFF_BFT_ANY		= $FFFF;


{ TLetoScriptLibSys }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibSys.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncSys.Create(Syntax, self, Env);
end;


{ TLetoScriptLibMath }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibMath.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncMath.Create(Syntax, self, Env);
end;


{ TLetoScriptLibGff }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibGff.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncGff.Create(Syntax, self, Env);
end;


{ TLetoScriptLibErf }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibErf.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncErf.Create(Syntax, self, Env);
end;


{ TLetoScriptLibBic }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibBic.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncBic.Create(Syntax, self, Env);
end;


{ TLetoScriptLibTlk }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptLibTlk.Create(
  const AName		: String;
  const AEnv		: TLetoScriptEnv
);
begin
  inherited Create(AName, AEnv);
  Mode			:= useTlk;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptLibTlk.Destroy;
begin
  if Assigned(Tlk) then
    FreeAndNil(Tlk);
  if Assigned(TlkF) then
    FreeAndNil(TlkF);
  if Assigned(CustomTlk) then
    FreeAndNil(CustomTlk);
  if Assigned(CustomTlkF) then
    FreeAndNil(CustomTlkF);

  inherited;
end;

(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibTlk.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncTlk.Create(Syntax, self, Env);
end;


{ TLetoScriptLib2da }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptLib2da.Create(
  const AName		: String;
  const AEnv		: TLetoScriptEnv
);
begin
  inherited Create(AName, AEnv);
  Cache			:= T2daHash.Create;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptLib2da.Destroy;
begin
  FreeAndNil(Cache);

  inherited;
end;

(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLib2da.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFunc2da.Create(Syntax, self, Env);
end;


{ TLetoScriptLibFpt }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibFpt.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncFpt.Create(Syntax, self, Env);
end;


{ TLetoScriptLibFileSys }


(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibFileSys.CreateFunc(const Syntax: String): Pointer;
begin
  Result		:= TLetoScriptFuncFileSys.Create(Syntax, self, Env);
end;


{ TLetoScriptFunc }


(*------------------------------------------------------------------------------
property Name

------------------------------------------------------------------------------*)
function TLetoScriptFunc.GetName: String;
begin
  Result		:= FName;
end;

procedure TLetoScriptFunc.SetName(const AName: String);
var
  P			: Integer;
begin
  P			:= Pos('.', AName);
  if P = 0 then
    FName		:= AName
  else
    FName		:= Copy(AName, P+1, Length(AName));
end;

(*------------------------------------------------------------------------------
property Op

------------------------------------------------------------------------------*)
function TLetoScriptFunc.GetOp: TLetoScriptOp;
begin
  if Attribs.UnaryOp then
    Result		:= opUnaryOp
  else if Attribs.ListOp then
    Result		:= opListOp
  else
    Result		:= Args.Op;
end;

(*------------------------------------------------------------------------------
Arg

------------------------------------------------------------------------------*)
function TLetoScriptFunc.Arg: TLetoScriptArg;
begin
  Result		:= Args.First;
end;

(*------------------------------------------------------------------------------
ArgVal

------------------------------------------------------------------------------*)
function TLetoScriptFunc.ArgVal: TLetoScriptValue;
begin
  Result		:= Args.First.Value;
end;

(*------------------------------------------------------------------------------
ArgIsObj

------------------------------------------------------------------------------*)
function TLetoScriptFunc.ArgIsObj(const Name: String): Boolean;
var
  V			: TLetoScriptValue;
begin
  if Name = '' then
    V			:= ArgVal
  else
    V			:= Args[Name];
  Result		:= V.ValType = vtObj;
  if Result then
    ArgObj		:= TLetoScriptObj(V);
end;

(*------------------------------------------------------------------------------
InitList

Function returns a list. The list is kept on the Token's PValue.

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc.InitList;
begin
  List			:= Token.EstablishList;
end;

(*------------------------------------------------------------------------------
Output

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc.Output(const S: String; const Nl: Boolean);
begin
  Token.Env.Output(S, otOutput);
  if Nl then
    Token.Env.Output(sLineBreak, otOutput);
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptFunc.Create(
  Syntax		: String;
  const AParent		: TLetoScriptLib;
  const AEnv		: TLetoScriptEnv
);
begin
  { Note that these may be nil, from helpsys define. }
  Parent		:= AParent;
  Env			:= AEnv;

  Attribs		:= TLetoScriptAttribs.Create(Syntax);
  Args			:= TLetoScriptArgs.Create(Syntax, AEnv, self);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptFunc.Destroy;
begin
  FreeAndNil(Args);

  inherited;
end;

(*------------------------------------------------------------------------------
CheckFoldable

If the function has been flagged as foldable in its syntax (a human decision),
and all of its arguments are foldable, then the function itself is folded.

------------------------------------------------------------------------------*)
function TLetoScriptFunc.CheckFoldable: Boolean;
var
  I			: Integer;
  A			: TLetoScriptArg;
begin
  Result		:= Attribs.Foldable;
  if not Result then Exit;

  for I := 0 to Args.Standards.Count-1 do begin
    A			:= Args.GetByIndex(Args.Standards, I);
    Result		:= Result and CheckArgFoldable(A);
    if not Result then Exit;
  end;

  for I := 0 to Args.Options.Count-1 do begin
    A			:= Args.GetByIndex(Args.Options, I);
    Result		:= Result and CheckArgFoldable(A);
    if not Result then Exit;
  end;

end;

(*------------------------------------------------------------------------------
CheckArgFoldable

------------------------------------------------------------------------------*)
function TLetoScriptFunc.CheckArgFoldable(const A: TLetoScriptArg): Boolean;
var
  Obj			: TLetoScriptObj;
begin
  if A.ValueAssigned then
    Obj			:= TLetoScriptObj(A.Value)
  else
    Obj			:= nil;

  Result :=
    (
      Assigned(Obj) and
      ((Obj.Op = opConst) or Obj.CheckFoldable)
    )
    or
    (
      not Assigned(Obj) and
      (A.Specs.ConstDefault or (A.Value = Env.NilValue))
    );

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc.Evaluate;
begin
  if not Parent.Enabled or (Parent.Disabled.IndexOf(Name) > -1) then
    Token.Exception(Err_LS_LibDisabled, Parent.Name, Name);

  Args.Scope		:= Token.GetNearestScope;
  Args.Reset;
//  Args.Evaluate;

  Token.Value		:= 0;

end;

(*------------------------------------------------------------------------------
Describe

Returns the syntax for this function, as a plain old string.

------------------------------------------------------------------------------*)
function TLetoScriptFunc.Describe: String;
begin
  Result		:= Attribs.Describe + Args.Describe;
end;


{ TLetoScriptFuncSys }


(*------------------------------------------------------------------------------
Loop

Drives the loop functions for, while, etc.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys.Loop(LoopParams: TLetoScriptLoopParams);
var
  LT			: TLetoScriptLoopType;
  List			: TLetoScriptObj;
  V			: TLetoScriptValue;
  OldUse		: String;
  Looping		: Boolean;
  ResWrap, VFile	: TLetoFile;
begin

  LT			:= LoopParams.LoopType;
  List			:= LoopParams.List;
  V			:= nil;

  { Loop initiation. }
  if Assigned(List) then begin
    List.Evaluate;
    List.EachInit;
    if (LT = ltResource) and Assigned(List.Rvalue) then begin
      // Necessary?
      {
      IoErr		:= Erf.Open(List[0]);
      if IoErr = Success then
        Result :=
          TLetoScriptFileVal.Create(
            AOwner, Env,
            TLetoFile.CreateWrapper(Erf.Opened.Gff, Erf, Erf.Opened),
            ''
          );
      Exit;
      }
      List.Left.Evaluate;
      TLetoScriptFileVal(List.Right.Rvalue).ResMask := List.Left.AsString;
    end;
    V			:= List.Each;
    if LT = ltResource then
      Looping		:= Assigned(V) and Assigned(V.AsFile.Gff)
    else
      Looping		:= Assigned(V);
  end else
    Looping		:= LoopParams.LoopIf.AsBool;

  if LT = ltResource then begin
    OldUse		:= Env.UseHandle;
    Env.UseHandle	:= '~';
  end;

  { Loop execution. }
  while Looping do begin

    { Iterative loop }
    if Assigned(List) then
    begin
      if LT = ltResource then begin
        VFile		:= V.AsFile;
        if not Assigned(VFile) or not Assigned(VFile.Gff) then begin
          V		:= List.Each;
          Looping	:= Assigned(V);
          Continue;
        end;
        ResWrap :=
          TLetoFile.CreateWrapper(
            VFile.Gff,
            List.Right.AsFile.Erf,
            List.Right.AsFile.Erf.Opened
          );
        Env.Handle['~'] := ResWrap;
      end;
      // magic referencing temporarily disabled...
      { TODO 4: BUG: W/o magic ref, floats promoted to double
        print /~.path, ": $_\n" for /@;
        XOrientation: (printed as double)
      }
      LoopParams.Default.
        Value		:= V.Value;
      LoopParams.Loop.
        LoopContext	:= V;
      LoopParams.Loop.Evaluate;
      V			:= List.Each;
      Looping		:= Assigned(V);
      if LT = ltResource then
        Env.Handle['~'] := nil;
    end

    { Boolean loop }
    else
    begin
      LoopParams.Loop.Evaluate;
      if
        Assigned(LoopParams.LoopDo) and
        (LoopParams.LoopDo <> LoopParams.LoopIf)
      then
        LoopParams.LoopDo.Evaluate;
      LoopParams.LoopIf.Evaluate;
      Looping		:= LoopParams.LoopIf.AsBool;
    end;

  end;

  if LT = ltResource then
    Env.UseHandle	:= OldUse;

end;

(*------------------------------------------------------------------------------
SystemExec

------------------------------------------------------------------------------*)
function TLetoScriptFuncSys.SystemExec(
  const CmdStr		: String;
  const Wait		: Boolean;
  const StdOut		: TStringList
): Cardinal;
const
  BuffSize		= 1024;
var
{$IFDEF MSWINDOWS}
  SI			: TStartupInfo;
  PI			: TProcessInformation;
  LastError		: Cardinal;
  ErrorState		: Boolean;

  SecAttr		: TSecurityAttributes;
  hStdOutRead,
    hStdOutWrite	: THandle;
  Buffer		: PChar;
  BytesRead		: Cardinal;

{$ENDIF}
{$IFDEF LINUX}
  I			: Integer;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result		:= 0;
  LastError		:= 0;

  { WARNING: HIGHLY experimental code for redirecting STDOUT!
    http://groups.google.com/groups?q=STARTF_USESTDHANDLES+peeknamedpipe+group:borland.public.delphi.*&hl=en&lr=&ie=UTF-8&oe=UTF-8&group=borland.public.delphi.*&selm=3ce13965_1%40dnews&rnum=1
  }

  // Set up SecAttr
  SecAttr.nLength		:= SizeOf(SecAttr);
  SecAttr.lpSecurityDescriptor	:= nil;
  SecAttr.bInheritHandle	:= True;

  // Redirect STDOUT
  if not CreatePipe(hStdOutRead, hStdOutWrite, @SecAttr, 0) then Exit;

  GetStartupInfo(SI);
  SI.cb			:= SizeOf(SI);
  SI.hStdInput		:= STD_INPUT_HANDLE; // Currently not redirected
  SI.hStdOutput		:= hStdOutWrite;
  SI.hStdError		:= hStdOutWrite;
  SI.dwFlags		:= STARTF_USESTDHANDLES;

  ErrorState		:= False;
  GetMem(Buffer, BuffSize + 1);

  try

  Result := Integer(CreateProcess(
    nil,
    PChar(CmdStr),
    nil, nil, True,
    0, nil, nil,
    SI, PI
  ));

  if (Result = 0) or not Wait then Exit;

  repeat

    GetExitCodeProcess(PI.hProcess, Result);
    if Result = 0 then
      LastError		:= GetLastError;

    if Assigned(StdOut) and PeekNamedPipe(
      hStdOutRead,
      Buffer, BuffSize,
      @BytesRead, nil, nil
    ) and (BytesRead > 0) then
      repeat
        if ReadFile(hStdOutRead, Buffer[0], BuffSize, BytesRead, nil) then begin
          Buffer[BytesRead]	:= #0;
          StdOut.Add( String(Buffer) );
        end else
          ErrorState		:= True;
      until (BytesRead < BuffSize) or ErrorState;

  until (Result <> STILL_ACTIVE) or ErrorState;
  if Result = 0 then
    Result		:= LastError;

  finally
    FreeMem(Buffer);
    CloseHandle(PI.hProcess);
    CloseHandle(PI.hThread);
    CloseHandle(hStdOutRead);
    CloseHandle(hStdOutWrite);
  end;

{$ENDIF}

// TODO 4: LINUX: How to / not to fire and forget...?
// TODO 4: LINUX: How to cap stdout?
{$IFDEF LINUX}
  Result		:= Libc.System(PChar(CmdStr));
{$ENDIF}

end;

(*------------------------------------------------------------------------------
do

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Do;
begin
  // TODO 4: ADD: do

  if Assigned(Token.Scope) then
    Token.Scope.ClearAll
  else
    Token.Scope		:= TLetoScriptVarScope.Create(Token, Env);

end;

(*------------------------------------------------------------------------------
else

Assumes correct tree structure. The Parser should guarantee this.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Else;
var
  Test			: TLetoScriptObj;
begin
  { Find the previous test. }
  Test			:= Token.Parent.Parent.Left;
  { Did it fire? }
  // DONE 1: BUG: [27r3] Obj.AsBool inaccurate for actual booleans
  if not Test.AsBool then
    Token.Block.Evaluate;
end;

(*------------------------------------------------------------------------------
elsif

Assumes correct tree structure. The Parser should guarantee this.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Elsif;
var
  Test			: TLetoScriptObj;
  Result		: Boolean;
begin
  { Find the previous test. }
  Test			:= Token.Parent.Parent.Left;
  { Did it fire? }
  if Test.AsBool then
    Result		:= True
  else begin
    Token.Expr.Flags	:= Token.Expr.Flags + [vfConditional];
    Token.Expr.Evaluate;
    Result		:= Token.Expr.AsBool;
    if Result then
      Token.Block.Evaluate;
  end;
  Token.Value		:= Result;
end;

(*------------------------------------------------------------------------------
for

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._For;
var
  LP			: TLetoScriptLoopParams;
begin

  if Assigned(Token.Scope) then
    Token.Scope.ClearAll
  else
    Token.Scope		:= TLetoScriptVarScope.Create(Token, Env);

  FillChar(LP, SizeOf(TLetoScriptLoopParams), 0);
  LP.LoopType		:= ltFor;
  LP.List		:= Token.Expr;
  LP.Default		:= Token.GetNearestScope.ScalarDefault;

  if Assigned(Token.Block) then begin
    if LP.List.Op = opStop then begin
      // for ($i = 0; $i < 10; $i++)
      if Assigned(LP.List.Right) and (LP.List.Right.Op = opStop) then begin
        if Assigned(LP.List.Left) then
          LP.List.Left.Evaluate;
        LP.LoopIf	:= LP.List.Right.Left;
        LP.LoopDo	:= LP.List.Right.Right;
      end
      // for ($i = 0; ; $i++)
      else if Assigned(LP.List.Left) and (LP.List.Left.Op = opStop) then begin
        // for (; ; $i++)  # does nothing
        // for($i=0; ;)    # does nothing
        if
          not Assigned(LP.List.Left.Left) or
          not Assigned(LP.List.Right)
        then Exit;
        LP.List.Left.Left.Evaluate;
        LP.LoopIf	:= LP.List.Right;
        LP.LoopDo	:= LP.List.Right;
      end
      // for ($i < 10; $i++)
      else begin
        LP.LoopIf	:= LP.List.Left;
        LP.LoopDo	:= LP.List.Right;
      end;
      LP.LoopIf.Evaluate;
      LP.List		:= nil;
    end;
    LP.Loop		:= Token.Block;
    // for $each (@list)
    if Assigned(Token.Right) then begin
      // DONE 1: BUG: [27r3] Loop var should be loop-local, destroyed
      Token.Right.Flags	:= Token.Right.Flags + [vfMyVivify];
      Token.Right.Evaluate;
      LP.Default	:= Token.Right.Lvalue;
    end;
  end

  // print for @list
  else begin
    if not Assigned(LP.List) then
      LP.List		:= Token.Right;
    LP.Loop		:= Token.Left;
  end;

  if Assigned(LP.List) and (LP.List.Op = opBind) then
    LP.LoopType		:= ltResource;

  Loop(LP);

end;

(*------------------------------------------------------------------------------
if

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._If;
var
  Result		: Boolean;
begin
  if Assigned(Token.Expr) then begin
    Token.Expr.Flags	:= Token.Expr.Flags + [vfConditional];
    Token.Expr.Evaluate;
    Result		:= Token.Expr.AsBool;
    if Result and Assigned(Token.Block) then
      Token.Block.Evaluate;
  end else begin
    Token.Right.Flags	:= Token.Right.Flags + [vfConditional];
    Token.Right.Evaluate;
    Result		:= Token.Right.AsBool;
    if Result then
      Token.Left.Evaluate;
  end;
  Token.Value		:= Result;
end;

(*------------------------------------------------------------------------------
unless

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Unless;
var
  Result		: Boolean;
begin
  if Assigned(Token.Expr) then begin
    Token.Expr.Flags	:= Token.Expr.Flags + [vfConditional];
    Token.Expr.Evaluate;
    Result		:= not Token.Expr.AsBool;
    if Result and Assigned(Token.Block) then
      Token.Block.Evaluate;
  end else begin
    Token.Right.Flags	:= Token.Right.Flags + [vfConditional];
    Token.Right.Evaluate;
    Result		:= not Token.Right.AsBool;
    if Result then
      Token.Left.Evaluate;
  end;
  Token.Value		:= Result;
end;


(*------------------------------------------------------------------------------
while

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._While;
var
  LP			: TLetoScriptLoopParams;
begin

  if Assigned(Token.Scope) then
    Token.Scope.ClearAll
  else
    Token.Scope		:= TLetoScriptVarScope.Create(Token, Env);

  FillChar(LP, SizeOf(TLetoScriptLoopParams), 0);
  LP.LoopType		:= ltWhile;
  LP.LoopIf		:= Token.Expr;

  (* while (sql.next) { } *)
  if Assigned(Token.Block) then
    LP.Loop		:= Token.Block

  (* print while sql.next *)
  else begin
    if not Assigned(LP.LoopIf) then
      LP.LoopIf		:= Token.Right;
    LP.Loop		:= Token.Left;
  end;

  LP.LoopIf.Flags	:= LP.LoopIf.Flags + [vfConditional];
  LP.LoopIf.Evaluate;

  Loop(LP);

end;

(*------------------------------------------------------------------------------
caller

( Presently being used as a test function. )

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Caller;
begin
  InitList;
  List.Add('foo').Value := 'oof';
  List.Add('bar').Value := 'rab';
  List.Add('baz').Value := 'zab';
end;

(*------------------------------------------------------------------------------
chr $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Chr;
begin
  Token.Value		:= Chr(ArgVal.AsInt);
end;

(*------------------------------------------------------------------------------
close %_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Close;
var
  Name			: String;
  Handles		: TStringList;
  Closing, I		: Integer;
  H, O			: TLetoFile;
  E			: TLetoScriptError;
begin
  Token.Value		:= 0;

  Name			:= ArgVal.AsVariant;
  H			:= ArgVal.AsFile;

  if (Name = '') or (Name = '_') then
    Name		:= Token.Env.LastHandle;

  Handles		:= Token.Env.Handles;
  Closing		:= Handles.IndexOf(Name);

  { Check for and remove duplicates (including open resources). }
  for I := Handles.Count-1 downto 0 do begin
    O			:= TLetoFile(Handles.Objects[I]);
    if
      (I = Closing) or
      (
        (not Assigned(O.ParentErf) and (O <> H)) or
        (Assigned(O.ParentErf) and
          (O.ParentErf <> H.Erf) and (O.ParentStruct <> H.ParentStruct)
        )
      )
    then
      Continue
    else begin
      Token.Complain(Err_LS_ImpliedClose, '%' + Handles[I]);
      Handles.Delete(I);
    end;
  end;

  E			:= Token.Env.CloseHandle(H);
  Token.Env.Errno	:= E;
  Token.Value		:= E = LS_Success;

end;

(*------------------------------------------------------------------------------
defined $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Defined;
begin
  if ArgIsObj then begin
    ArgObj.Flags	:= ArgObj.Flags + [vfDefinedCheck];
    ArgObj.Evaluate;
  end;

  Token.Value		:= Integer( ArgVal.Defined );

end;

(*------------------------------------------------------------------------------
delete $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Delete;
begin

end;

(*------------------------------------------------------------------------------
die @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Die;
var
  E			: TLetoScriptValue;
  S			: String;
begin
  if not Arg.ValueAssigned then
    S			:= 'Died'
  else begin
    if ArgVal.IsList then begin
      ArgVal.EachInit;
      E			:= ArgVal.Each;
      while Assigned(E) do begin
        S		:= S + E.AsString;
        E		:= ArgVal.Each;
      end;
    end else
      S			:= ArgVal.AsString;
  end;

  Token.Complain(Err_LS_Die, S);
  raise ELetoScriptExit.Create('');

end;

(*------------------------------------------------------------------------------
exit $status = 0

A lite version of Perl's exit, does not yet support exit codes. Simply a less
chatty version of die.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Exit;
var
  Status		: Integer;
  S			: String;
begin
  Status		:= ArgVal.AsInt;
  S			:= IntToStr(Status);
  Token.Complain(Err_LS_Exit, S);
  raise ELetoScriptExitStatus.Create(IntToStr(Status));
end;

(*------------------------------------------------------------------------------
greedy @param

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Greedy;
begin
  ArgIsObj;
  ArgObj.Text		:= ArgObj.Text + '?';
  Token.Rvalue		:= ArgObj;
end;

(*------------------------------------------------------------------------------
isnum $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._IsNum;
var
  S			: String;
  I			: Int64;
  E			: Extended;
begin
  // DONE 1: ADD: [26] IsNum
  S			:= ArgVal.AsString;
  Token.Value		:= TryStrToInt64(S, I) or TryStrToFloat(S, E);
end;

(*------------------------------------------------------------------------------
keys @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Keys;
begin
    Token.CaptureList(ArgVal, etKeys);
end;

(*------------------------------------------------------------------------------
lc $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Lc;
begin
  Token.Value		:= Lowercase(ArgVal.AsString);
end;

(*------------------------------------------------------------------------------
length $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Length;
begin
  // DONE 1: ADD: [27] length
  Token.Value		:= Length(ArgVal.AsString);
end;

(*------------------------------------------------------------------------------
my @vars

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._My;
var
  E			: TLetoScriptValue;
  S			: String;
begin

  // DONE 1: ADD: [26] my $x = 1; my($x, $y) = (1, 2)
  ArgIsObj;
  Token.Defined		:= False;
  Token.Lvalue		:= ArgObj;
  Token.Flags		:= Token.Flags + [vfMyVivify];

  ArgObj.Evaluate(ocSymList);

  { Type checking must be done manually. }
  ArgObj.EachInit;
  E			:= ArgObj.Each;
  while Assigned(E) do begin
    if
      (E.ValType <> vtObj) or
      not(TLetoScriptObj(E).Kind in [symScalar, symList]) or
      Assigned(TLetoScriptObj(E).Expr)
    then begin
      if E.ValType = vtObj then
        S		:= TLetoScriptObj(E).Description
      else
        S		:= 'value';
      Token.Exception(Err_LS_InvalidDeclare, S, 'my');
    end;
    E			:= ArgObj.Each;
  end;

end;

(*------------------------------------------------------------------------------
ord $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Ord;
begin
  Token.Value		:= Ord(ArgVal.AsChar);
end;

(*------------------------------------------------------------------------------
pop @array = @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Pop;
var
  List			: TLetoScriptVarList;
begin
  if (ArgIsObj and not ArgObj.Defined) or (ArgVal.AsList.Count = 0) then begin
    Token.Defined	:= False;
    Exit;
  end;

  if ArgIsObj then
    List		:= ArgObj.Lvalue.AsList
  else
    List		:= ArgVal.AsList;

  Token.Capture(List.Last);
  List.Delete(List.Last);

end;

(*------------------------------------------------------------------------------
print @_ = $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Print;
var
  Val, E		: TLetoScriptValue;
begin
  Val			:= ArgVal;

  if Val.IsList then begin
    Val.EachInit;
    E			:= Val.Each;
    while Assigned(E) do begin
      // DONE 1: ADD: [27r2] printlist
      Output(E.AsString, Fn = Word(fnPrintList));
      E			:= Val.Each;
    end;
  end
  else
    Output(Val.AsString, Fn = Word(fnPrintList));

  { Value is only 0 if print is unsuccessful - to a FH for instance. }
  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
printf $format, greedy @items = undef

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._PrintF;
var
  S			: String;
  Items, Item		: TLetoScriptValue;
  VarArr		: array of Variant;
  I			: Integer;
  procedure _Format_(const Id, Str: String);
  begin
    S			:= StringReplace(S, Id, Str, []);
  end;
// TODO 4: CHANGE: Intelligent PrintF
begin

  S			:= Args['format'].AsString;
  Items			:= Args['items'];

  { Insertions }
  Items.EachInit;
  Item			:= Items.Each;
  while Assigned(Item) do begin
    SetLength(VarArr, Length(VarArr)+1);
    VarArr[High(VarArr)] := Item.Value;
    Item		:= Items.Each;
  end;

  { Format }
  // Do this the other way around - hunt for each % and cast it.
  // Refer to perldoc -f sprintf
  for I := 0 to High(VarArr) do
    with TVarData(VarArr[I]) do
      case VType of
        varSmallInt: ;
        varInteger: ;
        varSingle: ;
        varDouble:
          _Format_('%f', FloatToStrF(VDouble, ffGeneral, 15, 15));
        varBoolean: ;
        varShortInt: ;
        varByte: ;
        varWord: ;
        varLongWord: ;
        varInt64:
          _Format_('%d', IntToStr(VInt64));
        varString:
          _Format_('%s', String(VString));
        {
        vtInteger:	_Format_('%d', IntToStr(VInteger));
        vtAnsiString:	_Format_('%s', String(VAnsiString));
        vtChar:	_Format_('%c', VChar);
        vtVariant:	_Format_('%s', String(VAnsiString)); // watch out
        vtBoolean:	S := S + BoolToStr(VBoolean);
        vtExtended:	S := S + FloatToStr(VExtended^);
        vtString:	S := S + VString^;
        vtPChar:	S := S + VPChar;
        vtObject:	S := S + VObject.ClassName;
        vtClass:	S := S + VClass.ClassName;
        vtCurrency:	S := S + CurrToStr(VCurrency^);
        vtVariant:	S := S + String(VVariant^);
        vtInt64:	S := S + IntToStr(VInt64^);
        }
        {
                      varSmallInt: (VSmallInt: SmallInt);
                      varInteger:  (VInteger: Integer);
                      varSingle:   (VSingle: Single);
                      varDouble:   (VDouble: Double);
                      varCurrency: (VCurrency: Currency);
                      varDate:     (VDate: TDateTime);
                      varOleStr:   (VOleStr: PWideChar);
                      varDispatch: (VDispatch: Pointer);
                      varError:    (VError: HRESULT);
                      varBoolean:  (VBoolean: WordBool);
                      varUnknown:  (VUnknown: Pointer);
                      varShortInt: (VShortInt: ShortInt);
                      varByte:     (VByte: Byte);
                      varWord:     (VWord: Word);
                      varLongWord: (VLongWord: LongWord);
                      varInt64:    (VInt64: Int64);
                      varString:   (VString: Pointer);
                      varAny:      (VAny: Pointer);
                      varArray:    (VArray: PVarArray);
                      varByRef:    (VPointer: Pointer);
        }
      end;

  // tacked on...
  S			:= StringReplace(S, '%%', '%', [rfReplaceAll]);

  { Output }
  Output(S);

  { Value is only 0 if printf is unsuccessful - to a FH for instance. }
  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
<>

------------------------------------------------------------------------------*)
// TODO 4: CHANGE: Meaning of <>
procedure TLetoScriptFuncSys._PrintN;
begin
  if Token.AsString <> '' then
    Output(Token.AsString)
  else
    Output( Token.GetNearestScope.Get('$_').AsString );
  Output(sLineBreak);
end;

(*------------------------------------------------------------------------------
push var @array, @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Push;
var
  Arr			: TLetoScriptVarList;
  List			: TLetoScriptValue;
begin

  ArgIsObj('array');
  ArgObj.Flags		:= ArgObj.Flags + [vfVivify];
  Arr			:= ArgObj.GetVar.AsList;
  List			:= Args['_'];

  Token.Value		:= Arr.Push(List);

end;

(*------------------------------------------------------------------------------
rand $x = 2; $seed

0 <= Result < $x

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Rand;
begin
  if Args.Args['seed'].ValueAssigned then
    RandSeed		:= Args['seed'].AsInt;

  Token.Value		:= Random(Args['x'].AsInt);
end;

(*------------------------------------------------------------------------------
return greedy @list = undef

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Return;
var
  Val			: TLetoScriptValue;
begin
  Token.Env.Last	:= Token;

  if Arg.ValueAssigned then begin

    Val			:= ArgVal;

    if Val.IsList then
      Token.CaptureList(Val)
    else
      Token.Value	:= Val.Value;

  end else
    Token.Defined	:= False;

  raise ELetoScriptReturn.Create('');

end;

(*------------------------------------------------------------------------------
shift @array = @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Shift;
var
  List			: TLetoScriptVarList;
begin
  if (ArgIsObj and not ArgObj.Defined) or (ArgVal.AsList.Count = 0) then begin
    Token.Defined	:= False;
    Exit;
  end;

  if ArgIsObj then
    List		:= ArgObj.Lvalue.AsList
  else
    List		:= ArgVal.AsList;

  Token.Capture(List.First);
  List.Delete(List.First);
  List.Reindex(-1);

end;

(*------------------------------------------------------------------------------
sleep $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Sleep;
begin
  Sleep( Cardinal(ArgVal.AsInt) );
end;

(*------------------------------------------------------------------------------
&

This is a virtual function, it actually just redirects to calling the named
sub.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Sub;
var
  Sub			: TLetoScriptSub;
  R			: TLetoScriptObj;
begin
  Sub :=
    TLetoScriptSub(
      Token.Env.Subs.Objects[ Token.Env.Subs.IndexOf(Token.Text) ]
    );

  { This is valid. }
  if not Assigned(Sub) then begin
    Token.Defined	:= False;
    Exit;
  end;

  { Execute. }
  R			:= Sub.Execute(Args);

  { Return value. }
  if not Assigned(R) or not R.Defined then
    Token.Defined	:= False
  else if Token.Context = ocList then
    Token.CaptureList(R)
  else
    Token.Value		:= R.Value;

end;

(*------------------------------------------------------------------------------
subst $expr, $replacement = $_, var $var = $_; $replaceall, $ignorecase

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Subst;
var
  S, R			: String;
  V			: TLetoScriptArg;
  Lval			: TLetoScriptVar;
  Flags			: TReplaceFlags;
begin
  S			:= Args['expr'].AsString;
  R			:= Args['replacement'].AsString;
  V			:= Args.Args['var'];

  if V.ValueAssigned then begin
    ArgIsObj('var');
    Lval		:= ArgObj.Lvalue;
  end else
    Lval		:= Token.GetNearestScope.ScalarDefault;

  Flags			:= [];
  if Args['replaceall'].AsBool then
    Include(Flags, rfReplaceAll);
  if Args['ignorecase'].AsBool then
    Include(Flags, rfIgnoreCase);

  Lval.Value		:= StringReplace(Lval.AsString, S, R, Flags);

  // TODO 4: CHANGE: More efficient subst with match-count return
  //Token.Value :=

end;

(*------------------------------------------------------------------------------
substr $expr, $offset, $length = 0, $replacement = undef

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Substr;
var
  Mask			: TLetoScriptMaskedVar;
  Err			: TLetoScriptError;
  Fatal			: Boolean;
begin
  // DONE 1: CHANGE: [27] Full Perl substr functionality
  Mask :=
    TLetoScriptMaskedVar.Create(
      Token, Env, '',
      Args['expr'], Args['offset'].AsInt, Args['length'].AsInt,
      Args['replacement']
    );
  Token.Rvalue		:= Mask;
  Token.Lvalue		:= Mask;

  Mask.Diagnostic(Err, Fatal);

  if Err = LS_Success then
  else if Fatal then
    Token.Exception(Err, 'substr')
  else
    Token.Complain(Err, 'substr');

end;

(*------------------------------------------------------------------------------
system @list = @_; $nowait = 0; $noquotes = 0

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._System;
var
  Val, E		: TLetoScriptValue;
  NoQuotes		: Boolean;
  CmdStr, S		: String;
begin
  Val			:= ArgVal;
  NoQuotes		:= Args['noquotes'].AsBool;

  if Val.IsList then begin
    Val.EachInit;
    E			:= Val.Each;
    while Assigned(E) do begin
      S			:= E.AsString;
      if
        not NoQuotes and
        (S <> '') and (S[1] <> '''') and (S[1] <> '"') and
        (Pos(' ', S) > 0)
      then
        S		:= '"' + S + '"';
      CmdStr		:= CmdStr + S + ' ';
      E			:= Val.Each;
    end;
    if (CmdStr <> '') and (CmdStr[Length(CmdStr)] = ' ') then
      System.Delete(CmdStr, Length(CmdStr), 1);
  end
  else
    CmdStr		:= Val.AsString;

  Token.Value		:= SystemExec(CmdStr, not Args['nowait'].AsBool);

end;

(*------------------------------------------------------------------------------
``
qx()

This is the system function as implemented by backticks and qx. It functions
the same as system, but its return value is any output in STDOUT, instead of
an ExitCode.

The STDOUT redirection is currently very experimental, and doesn't work at
all on Linux.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._SystemQ;
var
  CmdStr		: String;
  StdOut		: TStringList;
  I			: Integer;
begin
  CmdStr		:= Token.AsString;
  if CmdStr = '' then
    CmdStr		:= Token.GetNearestScope.Get('$_').AsString;

  StdOut		:= TStringList.Create;

  SystemExec(CmdStr, True, StdOut);

  InitList;
  for I := 0 to StdOut.Count - 1 do
    List.Add.Value	:= StdOut[I];

  FreeAndNil(StdOut);

end;

(*------------------------------------------------------------------------------
time

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Time;
begin
  // DONE 1: ADD: [26] time
  Token.Value		:= DateTimeToUnix(Now);
end;

(*------------------------------------------------------------------------------
uc $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Uc;
begin
  Token.Value		:= Uppercase(ArgVal.AsString);
end;

(*------------------------------------------------------------------------------
undef $object!

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Undef;
begin

  Token.Defined		:= False;

  if Arg.ValueAssigned and ArgIsObj then begin
    ArgObj.Evaluate;
    ArgObj.Defined	:= False;
  end;

end;

(*------------------------------------------------------------------------------
unshift var @array, @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Unshift;
var
  Arr			: TLetoScriptVarList;
  List			: TLetoScriptValue;
begin

  ArgIsObj('array');
  ArgObj.Flags		:= ArgObj.Flags + [vfVivify];
  Arr			:= ArgObj.GetVar.AsList;
  List			:= Args['_'];

  Token.Value		:= Arr.Unshift(List);

end;

(*------------------------------------------------------------------------------
values @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Values;
begin
    Token.CaptureList(ArgVal, etValues);
end;

(*------------------------------------------------------------------------------
var $param

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Var;
begin
  ArgIsObj;
  ArgObj.Text		:= ArgObj.Text + '^';
  Token.Rvalue		:= ArgObj;
end;

(*------------------------------------------------------------------------------
warn @_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys._Warn;
var
  E			: TLetoScriptValue;
  S			: String;
begin
  if not Arg.ValueAssigned then begin
    S			:= Token.GetVar('$@').AsString;
    if S = '' then
      S			:= GetLSError(Err_LS_DefaultWarn)
    else
      S			:= S + GetLSError(Err_LS_Caught);
  end else begin
    if ArgVal.IsList then begin
      ArgVal.EachInit;
      E			:= ArgVal.Each;
      while Assigned(E) do begin
        S		:= S + E.AsString;
        E		:= ArgVal.Each;
      end;
    end else
      S			:= ArgVal.AsString;
  end;

  Token.Complain(Err_LS_Warn, S);

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSys.Evaluate;
begin
  inherited;

  if Token.Op = opNamedOp then
    case Token.Kind of
      noContinue: ;
      noDo:		_Do;
      noElse:		_Else;
      noElsif:		_Elsif;
      noFor:		_For;
      noIf:		_If;
      noSwitch: ;
      noUnless:		_Unless;
      noWhile:		_While;
    end

  else
    case TLetoScriptFnSys(Fn) of
      fnCaller:		_Caller;
      fnChomp: ;
      fnChop: ;
      fnChr:		_Chr;
      fnClose:		_Close;
      fnDefined:	_Defined;
      fnDelete:		_Delete;
      fnDie:		_Die;
      fnEach: ;
      fnExists: ;
      fnExit:		_Exit;
      fnGoto: ;
      fnGreedy:		_Greedy;
      fnGrep: ;
      fnHex: ;
      fnIndex: ;
      fnIsNum:		_IsNum;
      fnJoin: ;
      fnKeys:		_Keys;
      fnLast: ;
      fnLc:		_Lc;
      fnLength:		_Length;
      fnLocal: ;
      fnLocaltime: ;
      fnMap: ;
      fnMy:		_My;
      fnNext: ;
      fnNo: ;
      fnOct: ;
      fnOrd:		_Ord;
      fnPack: ;
      fnPop:		_Pop;
      fnPrint,
      fnPrintList:	_Print;
      fnPrintf:		_PrintF;
      fnPrintn:		_PrintN;
      fnPush:		_Push;
      fnRand:		_Rand;
      fnRedo: ;
      fnReset: ;
      fnReturn:		_Return;
      fnReverse: ;
      fnRindex: ;
      fnScalar: ;
      fnShift:		_Shift;
      fnSleep:		_Sleep;
      fnSort: ;
      fnSplice: ;
      fnSplit: ;
      fnSprintf: ;
      fnSub:		_Sub;
      fnS, fnSubst:	_Subst;
      fnSubstr:		_Substr;
      fnSystem:		_System;
      fnSystemQ:	_SystemQ;
      fnTime:		_Time;
      fnUc:		_Uc;
      fnUndef:		_Undef;
      fnUnpack: ;
      fnUnshift:	_Unshift;
      fnUse: ;
      fnValues:		_Values;
      fnVar:		_Var;
      fnWantArray: ;
      fnWarn:		_Warn;

    end;
end;


{ TLetoScriptFuncMath }


(*------------------------------------------------------------------------------
abs

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Abs;
begin
  Token.Value		:= Abs(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
ceil

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Ceil;
begin
  Token.Value		:= Ceil(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
divmod

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._DivMod;
var
  Result, Remainder	: Word;
begin
  InitList;

  DivMod(Args['dividend'].AsInt, Args['divisor'].AsInt, Result, Remainder);

  List.Add.Value	:= Result;
  List.Add.Value	:= Remainder;

end;

(*------------------------------------------------------------------------------
exp

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Exp;
begin
  Token.Value		:= Exp(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
floor

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Floor;
begin
  Token.Value		:= Floor(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
frac

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Frac;
begin
  Token.Value		:= Frac(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
frexp

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Frexp;
var
  Mantissa		: Extended;
  Exponent		: Integer;
begin
  InitList;

  Frexp(ArgVal.AsReal, Mantissa, Exponent);

  List.Add.Value	:= Mantissa;
  List.Add.Value	:= Exponent;

end;

(*------------------------------------------------------------------------------
int

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Int;
begin
  Token.Value		:= Int(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
iszero

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._IsZero;
begin
  Token.Value		:= IsZero(Args['x'].AsReal, Args['epsilon'].AsReal);
end;

(*------------------------------------------------------------------------------
ldexp

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Ldexp;
begin
  Token.Value		:= Ldexp(Args['x'].AsReal, Args['p'].AsInt);
end;

(*------------------------------------------------------------------------------
ln

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Ln;
begin
  Token.Value		:= Ln(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
lnxp1

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._LnXP1;
begin
  Token.Value		:= LnXP1(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
log2

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Log2;
begin
  Token.Value		:= Log2(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
log10

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Log10;
begin
  Token.Value		:= Log10(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
logn

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._LogN;
begin
  Token.Value		:= LogN(Args['base'].AsReal, Args['x'].AsReal);
end;

(*------------------------------------------------------------------------------
max

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Max;
begin
  Token.Value		:= Max(Args['a'].AsReal, Args['b'].AsReal);
end;

(*------------------------------------------------------------------------------
min

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Min;
begin
  Token.Value		:= Min(Args['a'].AsReal, Args['b'].AsReal);
end;

(*------------------------------------------------------------------------------
muldiv

"MulDiv returns the result from multiplying Number by Numerator and then
(integer) dividing by Denominator. If Denominator is 0, MulDiv returns –1."

Solves problems like 4 * 3/6ths in just one function.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._MulDiv;
var
  N, Numerator,
    Denominator		: Integer;
begin
  N			:= Args['n'].AsInt;
  Numerator		:= Args['numerator'].AsInt;
  Denominator		:= Args['denominator'].AsInt;
  if Denominator = 0 then
    Token.Value		:= -1
  else
    Token.Value		:= (Int64(N) * Numerator) div Denominator;
end;

(*------------------------------------------------------------------------------
poly

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Poly;
var
  Val, E		: TLetoScriptValue;
  X			: Real;
  Co			: array of Double;
begin
  Token.Value		:= Poly(0, []);

  Val			:= ArgVal;

  if Val.IsList then begin
    Val.EachInit;
    E			:= Val.Each;
    if not Assigned(E) then Exit;
    X			:= E.AsReal;
    E			:= Val.Each;
    while Assigned(E) do begin
      SetLength(Co, Length(Co)+1);
      Co[High(Co)]	:= E.AsReal;
      E			:= Val.Each;
    end;
    Token.Value		:= Poly(X, Co);
  end
  else
    Token.Value		:= Poly(Val.AsReal, []);

end;

(*------------------------------------------------------------------------------
round

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Round;
begin
  Token.Value		:= Round(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
roundto

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._RoundTo;
begin
  Token.Value		:= RoundTo(Args['x'].AsReal, Args['digit'].AsInt);
end;

(*------------------------------------------------------------------------------
samevalue

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._SameValue;
begin
  Token.Value :=
    SameValue(Args['a'].AsReal, Args['b'].AsReal, Args['epsilon'].AsReal);
end;

(*------------------------------------------------------------------------------
sign

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Sign;
begin
  Token.Value		:= Sign(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
simpleroundto

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._SimpleRoundTo;
begin
  Token.Value :=
    SimpleRoundTo(Args['x'].AsReal, Args['digit'].AsInt);
end;

(*------------------------------------------------------------------------------
sqr

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Sqr;
begin
  Token.Value		:= Sqr(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
sqrt

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Sqrt;
begin
  Token.Value		:= Sqrt(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
trunc

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Trunc;
begin
  Token.Value		:= Trunc(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
acos

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ACos;
begin
  Token.Value		:= ArcCos(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
acosh

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ACosH;
begin
  Token.Value		:= ArcCosH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
acot

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ACot;
begin
  Token.Value		:= ArcCot(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
acoth

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ACotH;
begin
  Token.Value		:= ArcCotH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
acsc

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ACsc;
begin
  Token.Value		:= ArcCsc(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
acsch

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ACscH;
begin
  Token.Value		:= ArcCscH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
asec

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ASec;
begin
  Token.Value		:= ArcSec(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
asech

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ASecH;
begin
  Token.Value		:= ArcSecH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
asin

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ASin;
begin
  Token.Value		:= ArcSin(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
asinh

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ASinH;
begin
  Token.Value		:= ArcSinH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
atan

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ATan;
begin
  Token.Value		:= ArcTan(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
atan2

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ATan2;
begin
  Token.Value		:= ArcTan2(Args['y'].AsReal, Args['x'].AsReal);
end;

(*------------------------------------------------------------------------------
atanh

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._ATanH;
begin
  Token.Value		:= ArcTanH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
cosin

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Cosin;
begin
  Token.Value		:= Cos(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
cosh

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._CosH;
begin
  Token.Value		:= CosH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
cotan

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Cotan;
begin
  Token.Value		:= Cot(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
coth

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._CotH;
begin
  Token.Value		:= CotH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
cosecant

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Cosecant;
begin
  Token.Value		:= Csc(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
csch

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._CscH;
begin
  Token.Value		:= CscH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
hypot

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Hypot;
begin
  Token.Value		:= Hypot(Args['x'].AsReal, Args['y'].AsReal);
end;

(*------------------------------------------------------------------------------
secant

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Secant;
begin
  Token.Value		:= Sec(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
sech

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._SecH;
begin
  Token.Value		:= SecH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
sin

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Sin;
begin
  Token.Value		:= Sin(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
SinCos

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._SinCos;
var
  Sin, Cos		: Extended;
begin
  InitList;

  SinCos(ArgVal.AsReal, Sin, Cos);

  List.Add.Value	:= Sin;
  List.Add.Value	:= Cos;

end;

(*------------------------------------------------------------------------------
sinh

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._SinH;
begin
  Token.Value		:= SinH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
tan

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._Tan;
begin
  Token.Value		:= Tan(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
TanH

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath._TanH;
begin
  Token.Value		:= TanH(ArgVal.AsReal);
end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncMath.Evaluate;
begin
  inherited;

  case TLetoScriptFnMath(Fn) of
    fnMathAbs:		_Abs;
    fnMathCeil:		_Ceil;
    fnMathDivMod:	_DivMod;
    fnMathExp:		_Exp;
    fnMathFloor:	_Floor;
    fnMathFrac:		_Frac;
    fnMathFrexp:	_Frexp;
    fnMathInt:		_Int;
    fnMathIsZero:	_IsZero;
    fnMathLdexp:	_Ldexp;
    fnMathLn:		_Ln;
    fnMathLnXP1:	_LnXP1;
    fnMathLog10:	_Log10;
    fnMathLog2:		_Log2;
    fnMathLogN:		_LogN;
    fnMathMax:		_Max;
    fnMathMin:		_Min;
    fnMathMulDiv:	_MulDiv;
    fnMathPoly:		_Poly;
    fnMathRound:	_Round;
    fnMathRoundTo:	_RoundTo;
    fnMathSameValue:	_SameValue;
    fnMathSign:		_Sign;
    fnMathSimpleRoundTo: _SimpleRoundTo;
    fnMathSqr:		_Sqr;
    fnMathSqrt:		_Sqrt;
    fnMathTrunc:	_Trunc;

    fnMathACos:		_ACos;
    fnMathACosH:	_ACosH;
    fnMathACot:		_ACot;
    fnMathACotH:	_ACotH;
    fnMathACsc:		_ACsc;
    fnMathACscH:	_ACscH;
    fnMathASec:		_ASec;
    fnMathASecH:	_ASecH;
    fnMathASin:		_ASin;
    fnMathASinH:	_ASinH;
    fnMathATan:		_ATan;
    fnMathATan2:	_ATan2;
    fnMathATanH:	_ATanH;
    fnMathCosin:	_Cosin;
    fnMathCosH:		_CosH;
    fnMathCotan:	_Cotan;
    fnMathCotH:		_CotH;
    fnMathCosecant:	_Cosecant;
    fnMathCscH:		_CscH;
    fnMathHypot:	_Hypot;
    fnMathSecant:	_Secant;
    fnMathSecH:		_SecH;
    fnMathSin:		_Sin;
    fnMathSinCos:	_SinCos;
    fnMathSinH:		_SinH;
    fnMathTan:		_Tan;
    fnMathTanH:		_TanH;

  end;

end;

{ TLetoScriptFuncGff }


(*------------------------------------------------------------------------------
SetupFind

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff.SetupFind(
  const Root		: TGffField;
  const Name, Value	: String
);
var
  A			: TLetoScriptValue;
begin

  FillChar(MatchParams, SizeOf(MatchParams), 0);
  MatchParams.Results	:= TStringList.Create;

  MatchParams.ChangesMade := False;

  { Root, Name, Value }
  MatchParams.Root	:= Root;
  MatchParams.Name	:= Name;
  MatchParams.Value	:= Value;

  { MatchCase }
  A			:= Args['matchcase'];
  if not A.AsBool then
    Include(MatchParams.Behavior, mbCIValue);

  { Type }
  A			:= Args['type'];
  if not A.Defined then
    MatchParams.Types	:= ValidGffVars
  else if not TryBFMtoFS(A.AsInt, MatchParams.Types) then
    Token.Exception(Err_LS_BadArgType, 'type', 'FIELDTYPE MASK');

  { Lang }
  A			:= Args['lang'];
  MatchParams.Lang	:= A.AsInt;

  { StrRef }
  A			:= Args['strref'];
  if A.Defined then begin
    Include(MatchParams.Behavior, mbStrRefCheck);
    MatchParams.StrRef	:= Cardinal(A.AsInt);
  end;

  { Depth }
  A			:= Args['depth'];
  MatchParams.Depth	:= A.AsInt;

  { Target }
  A			:= Args['target'];
  MatchParams.Target	:= A.AsInt;

  { HasChildren }
  A			:= Args['haschildren'];
  if not A.Defined then
    Include(MatchParams.Behavior, mbNoChildCheck)
  else if A.AsBool then
    Include(MatchParams.Behavior, mbMustHaveChild)
  else
    Include(MatchParams.Behavior, mbMustHaveNoChild);

  { Match }
  if Args.Args['match'].ValueAssigned then begin
    MatchParams.OnMatch	:= DoMatch;
    Token.LoopContext	:= Args['match'];
  end;

  if Fn <> Word(fnGffReplace) then Exit;

  Include(MatchParams.Behavior, mbReplace);

  { SetType }
  A			:= Args['settype'];
  TryBFTtoFT(A.AsInt, MatchParams.SetType);

  { SetLang }
  A			:= Args['setlang'];
  MatchParams.SetLang	:= A.AsInt;

  { SetStrRef }
  A			:= Args['setstrref'];
  if A.Defined then begin
    Include(MatchParams.Behavior, mbSetStrRef);
    MatchParams.SetStrRef := Cardinal(A.AsInt);
  end;

  { SetValue }
  A			:= Args['setvalue'];
  if A.Defined then begin
    Include(MatchParams.Behavior, mbSetValue);
    MatchParams.SetValue := A.AsString;
  end;

  { Delete }
  A			:= Args['delete'];
  if A.AsBool then
    Include(MatchParams.Behavior, mbDelete);

  { DeleteParent }
  A			:= Args['deleteparent'];
  if A.AsBool then
    Include(MatchParams.Behavior, mbDeleteParent);

end;

(*------------------------------------------------------------------------------
DoMatch

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff.DoMatch(
  Sender		: TObject;
  Field			: TGffField;
  var Matches		: Boolean
);
var
  Blk			: TLetoScriptObj;
  FldVal		: TLetoScriptFldVal;
begin
  Blk			:= TLetoScriptObj(Token.LoopContext);

  FldVal		:= TLetoScriptFldVal.Create(nil, Env, Field, '');
  Blk.LoopContext	:= FldVal;

  Blk.Evaluate;
  Matches		:= Blk.AsBool;

  Blk.LoopContext	:= nil;
  FreeAndNil(FldVal);

end;

(*------------------------------------------------------------------------------
add $name, $value = undef, $type = undef;
  /copyfrom, $lang, $setifexists

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff._Add;
var
  _Name			: TLetoScriptValue;
  Path			: String;
  _Value		: TLetoScriptValue;
  _CopyFrom		: TLetoScriptValue;
  CopyFrom		: TGffField;
  _Type			: TLetoScriptValue;
  FieldType		: TGffVarType;
begin

  _Name			:= Args['name'];
  // DONE 1: CHANGE: [27r2] add supports only simple $name param
  if not ArgIsObj('name') then
    Path		:= _Name.AsString
  else if ArgObj.Kind = symField then
    Path		:= ArgObj.Text
  else begin
    ArgObj.Evaluate;
    Path		:= ArgObj.AsString;
  end;
  if Path = '' then Exit;
  if Path[1] = '/' then
    System.Delete(Path, 1, 1);

  _Value		:= Args['value'];

  _CopyFrom		:= Args['copyfrom'];
  if _CopyFrom.Defined then begin
    CopyFrom		:= _CopyFrom.AsField;
    Field		:= Gff.AddField(gffByte, Path, '', Context);
    Path		:= Field.Name;
    if Field.IsValid then begin
      Token.Value	:= 1;
      // DONE 1: BUG: [27r2] gff.add shouldn't assume same-name or same-index
      Field.CopyFrom(CopyFrom, False);
      { CopyFrom copies Label, which may not always apply. }
      Field.Name	:= Path;
    end else
      Token.Value	:= 0;
    Exit;
  end;

  _Type			:= Args['type'];
  if _Type.Defined then begin
    if not TryBFTtoFT(_Type.AsInt, FieldType) then
      Token.Exception(Err_LS_BadArgType, 'type', 'FIELDTYPE MASK');
  end else if _Value.IsNum then
    FieldType		:= gffInt
  else
    FieldType		:= gffString;

  if Gff.AddField(
    FieldType, Path, _Value.AsString, Context,
    Args['lang'].AsInt, Args['setifexists'].AsBool
  ).IsValid then
    Token.Value		:= 1
  else
    Token.Value		:= 0;

end;

(*------------------------------------------------------------------------------
clear /field = undef

Clears all children of Structs or Lists. On a LocString, clears all strings.
On a Void, clears all data (length set to 0). Without an arg, clears the
entire file (the Root). Returns True unless the Field does not exist.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff._Clear;
var
  _Field		: TLetoScriptValue;
begin

  _Field		:= ArgVal;

  if not _Field.Defined then
    Gff.Root.Clear
  else begin
    Field		:= _Field.AsField;
    if not Assigned(Field) or not Field.IsValid then begin
      Token.Value	:= 0;
      Token.Env.Errno	:= Err_LS_InvalidField;
      Exit;
    end;
    case Field.VarType of
      gffLocString:
        Field.AsLocString.Clear;
      gffVoid:
        SetLength(Field.Data.AsVoid, 0);
    else
      Field.Clear;
    end;
  end;

  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
delete /field, $lang = undef

Delete the Field named. If Lang is given, the Field should be a LocString (or
a warning occurs), and the matching language is removed.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff._Delete;
var
  _Field		: TLetoScriptValue;
  _Lang			: TLetoScriptValue;
  Field			: TGffField;
begin
  _Field		:= Args['field'];
  Field			:= _Field.AsField;
  if not _Field.Defined then begin
    Token.Value		:= 0;
    Token.Env.Errno	:= Err_LS_InvalidField;
    Exit;
  end;

  _Lang			:= Args['lang'];

  if _Lang.Defined then begin
    if Field.VarType <> gffLocString then
      Token.Complain(Err_LS_NotLocString);
    Token.Value		:= Field.AsLocString.DeleteLang(_Lang.AsInt)
  end else begin
    ArgIsObj;
    ArgObj.Undef;
    Token.Value		:= 1;
  end;

end;

(*------------------------------------------------------------------------------
find $name, $value = '*', /root = undef;
  $matchcase = 0, $type, $lang = -1,
  $strref, $depth = -1, $target = -1, $haschildren,
  &match

An extensively powerful routine for finding Fields matching very specific
criteria (even arbitrary), at any point in a GFF.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff._Find;
var
  _Root			: TLetoScriptArg;
begin
  _Root			:= Args.Args['root'];

  if _Root.Value.Defined then
    Field		:= _Root.Value.AsField
  else if _Root.ValueAssigned then
    Exit
  else
    Field		:= nil;

  SetupFind(Field, Args['name'].AsString, Args['value'].AsString);

  Gff.Match(MatchParams, Context);

  Token.CaptureList(MatchParams.Results);

end;

(*------------------------------------------------------------------------------
replace $name, $value = '*', $setvalue = undef, /root = undef;
  $matchcase = 0, $type, $lang = -1, $strref,
  $depth = -1, $target = -1, $haschildren,
  $settype = 0, $setlang = -1, $setstrref, $delete = 0, $deleteparent = 0,
  &match

An extension to find, which operates under the implication that a change
should be made to all matching Fields. This change is indicated by the
additional parameters supported.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff._Replace;
var
  _Root			: TLetoScriptArg;
begin
  _Root			:= Args.Args['root'];

  if _Root.Value.Defined then
    Field		:= _Root.Value.AsField
  else if _Root.ValueAssigned then
    Exit
  else
    Field		:= nil;

  SetupFind(Field, Args['name'].AsString, Args['value'].AsString);

  Gff.Match(MatchParams, Context);

  Token.CaptureList(MatchParams.Results);

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncGff.Evaluate;
var
  S			: String;
  L			: TLetoFile;
  _Context		: TLetoScriptValue;
begin
  inherited;

  Token.Value		:= '';

  Gff			:= nil;
  Field			:= nil;

  S			:= '%' + Token.Env.LastHandle;
  L			:= Token.Env.DefaultHandle;
  if not Assigned(L) then
    Token.Complain(Err_LS_NoFile)
  else if not L.IsGff then
    Token.Complain(Err_LS_NotGffFile, S)
  else
    Gff			:= L.Gff;

  if not Assigned(Gff) then Exit;

  _Context		:= Token.LoopContext;
  if Assigned(_Context) and (_Context.ValType = vtField) then
    Context		:= _Context.AsField
  else
    Context		:= nil;

  case TLetoScriptFnGff(Fn) of
    fnGffAdd:		_Add;
    fnGffClear:		_Clear;
    fnGffDelete:	_Delete;
    fnGffFind:		_Find;
    fnGffReplace:	_Replace;
  end;

end;


{ TLetoScriptFuncErf }


(*------------------------------------------------------------------------------
export @resmask = undef; $saveas

Export a (GFF) resource from an ERF, to file on disk.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncErf._Export;
var
  V			: TLetoScriptListedVar;
  _SaveAs		: TLetoScriptValue;
begin

  _SaveAs		:= Args['saveas'];

  Token.Value		:= 1;

  _List;
  V			:= List.First;
  while Assigned(V) do begin
    if _SaveAs.Defined then begin
      Erf.ExportToFile(V.AsString, _SaveAs.AsString);
      Exit;
    end;
    Erf.ExportToFile(V.AsString);
    V			:= V.Next;
  end;

  Token.Value		:= List.Count;

end;

(*------------------------------------------------------------------------------
import $filemask; $saveas

Import a file on disk into an ERF.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncErf._Import;
var
  Mask, SaveAs		: String;
  Files			: TStringList;
  I			: Integer;
begin

  InitList;

  Mask			:= ConvertPath( Args['filemask'].AsString );
  Files :=
    GetFileList(
      ExtractFileName(Mask),
      ExtractFilePath(Mask),
      False, True
    );

  if Files.Count = 1 then
    SaveAs		:= Args['saveas'].AsString
  else
    SaveAs		:= '';

  for I := 0 to Files.Count - 1 do begin
    List.Add.Value	:= Files[I];
    Erf.Insert(Files[I], SaveAs);
  end;

  FreeAndNil(Files);

end;

(*------------------------------------------------------------------------------
list @resmask = undef

List the resources in an ERF; all by default, or a specific subset, using
@resmask.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncErf._List;
var
  _ResMask		: TLetoScriptValue;
  Names			: array of TStringList;
  E			: TLetoScriptValue;
  Mask			: String;
  N, I			: Integer;
begin

  InitList;

  _ResMask		:= Args['resmask'];

  if not _ResMask.Defined then begin
    SetLength(Names, 1);
    Names[0]		:= Erf.GetListOfNames('*.*');
  end else begin
    _ResMask.EachInit;
    E			:= _ResMask.Each;
    while Assigned(E) do begin
      Mask		:= E.AsString;
      if Mask <> '' then begin
        SetLength(Names, Length(Names)+1);
        Names[High(Names)] := Erf.GetListOfNames(Mask);
      end;
      E			:= _ResMask.Each;
    end;
  end;

  for N := 0 to High(Names) do begin
    for I := 0 to Names[N].Count-1 do
      List.Add.Value	:= Names[N][I];
    Names[N].Free;
  end;

  SetLength(Names, 0);

end;

(*------------------------------------------------------------------------------
remove @resmask = undef

Remove (delete) a resource from an ERF.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncErf._Remove;
var
  V			: TLetoScriptListedVar;
begin

  InitList;
  _List;

  V			:= List.First;
  while Assigned(V) do begin
    Erf.Remove(V.AsString);
    V			:= V.Next;
  end;

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncErf.Evaluate;
var
  S			: String;
  L			: TLetoFile;
begin
  inherited;

  Token.Value		:= '';

  Erf			:= nil;

  S			:= '%' + Token.Env.LastHandle;
  L			:= Token.Env.DefaultHandle;
  if not Assigned(L) then
    Token.Complain(Err_LS_NoFile)
  else if not L.IsErf then
    Token.Complain(Err_LS_NotErfFile, S)
  else
    Erf			:= L.Erf;

  if not Assigned(Erf) then Exit;

  case TLetoScriptFnErf(Fn) of
    fnErfExport:	_Export;
    fnErfImport:	_Import;
    fnErfList:		_List;
    fnErfRemove:	_Remove;
  end;

end;


{ TLetoScriptFuncBic }


(*------------------------------------------------------------------------------
findbicbytag $path, $tag; $filemask = '*.bic'

In a given path, find the first matching GFF (BIC) with a given /Tag.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncBic._FindBicByTag;
var
  F			: TSearchRec;
  Path, Tag, FileMask	: String;
  Gff			: TGffFile;
begin

  Path			:= Args['path'].AsString;
  Tag			:= Args['tag'].AsString;
  FileMask		:= Args['filemask'].AsString;

  if (Path <> '') and (Path[Length(Path)] <> PathDelim) then
    Path		:= Path + PathDelim;

  if FindFirst(Path + FileMask, faAnyFile, F) <> 0 then Exit;

  Gff			:= TGffFile.Create;

  repeat
    if F.Attr and faDirectory = faDirectory then Continue;
    if Gff.LoadFromFile(Path + F.Name, LoadShort) <> Success then Continue;
    if Gff.Find('Tag').AsString = Tag then begin
      Token.Value	:= F.Name;
      Break;
    end;
  until FindNext(F) <> 0;

  SysUtils.FindClose(F);
  FreeAndNil(Gff);

end;

(*------------------------------------------------------------------------------
findnewestbic $path; $filemask = '*.bic'

In a given path, find the newest file (BIC).

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncBic._FindNewestBic;
var
  F			: TSearchRec;
  Path, FileMask	: String;
  Newest		: Integer;
begin

  Path			:= Args['path'].AsString;
  FileMask		:= Args['filemask'].AsString;

  if (Path <> '') and (Path[Length(Path)] <> PathDelim) then
    Path		:= Path + PathDelim;

  if FindFirst(Path + FileMask, faAnyFile, F) <> 0 then Exit;

  Newest		:= 0;

  repeat
    if F.Attr and faDirectory = faDirectory then Continue;
    if (Newest = 0) or (F.Time > Newest) then begin
      Newest		:= F.Time;
      Token.Value	:= F.Name;
    end;
  until FindNext(F) <> 0;

  SysUtils.FindClose(F);

end;

(*------------------------------------------------------------------------------
vault $path = $_, $filemask = '*.bic';
  $readonly = 0, $hidden = 0, $dirsonly = 0, $fullpaths = 1, $recursive = 0

Catalog all the files (BICs) in a given directory. Effectively a dir / ls.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncBic._Vault;
begin
  Token.CaptureList(
    GetFileList(
      Args['filemask'].AsString,
      Args['path'].AsString,
      Args['recursive'].AsBool,
      Args['readonly'].AsBool,
      Args['hidden'].AsBool,
      Args['dirsonly'].AsBool,
      not Args['nofullpaths'].AsBool
    ),
    { FreeList: }
    True
  );
end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncBic.Evaluate;
begin
  inherited;

  case TLetoScriptFnBic(Fn) of
    fnBicFindBicByTag:		_FindBicByTag;
    fnBicFindNewestBic:		_FindNewestBic;
    fnBicVault:			_Vault;
  end;

end;


{ TLetoScriptFuncTlk }


(*------------------------------------------------------------------------------
dialog

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._Dialog;
var
  _Tlk, _TlkF, _Custom, _CustomF,
    _UsingTlk, _UsingTlkF, _UsingCustom,
    _UsingCustomF	: TLetoScriptValue;
  E			: TLetoError;
  ErrNo			: TLetoScriptError;
  S			: String;
begin

  _Tlk			:= Args['tlk'];
  _TlkF			:= Args['tlkf'];
  _Custom		:= Args['custom'];
  _CustomF		:= Args['customf'];
  _UsingTlk		:= Args['usingtlk'];
  _UsingTlkF		:= Args['usingtlkf'];
  _UsingCustom		:= Args['usingcustom'];
  _UsingCustomF		:= Args['usingcustomf'];

  E			:= Success;
  ErrNo			:= LS_Success;
  try

  if _Tlk.Defined then begin
    if not Assigned(Lib.Tlk) then
      Lib.Tlk		:= TTlkFile.Create;
    S			:= _Tlk.AsString;
    if S = '' then begin
      FreeAndNil(Lib.Tlk);
      Exit;
    end
    else if S[1] = '>' then begin
      System.Delete(S, 1, 1);
      E			:= Lib.Tlk.SaveToFile(S);
      if E <> Success then Exit;
    end
    else begin
      E			:= Lib.Tlk.LoadFromFile(S);
      if E <> Success then Exit;
    end;
  end;

  if
    (_TlkF.Defined or _Custom.Defined or _CustomF.Defined) and
    not Assigned(Lib.Tlk)
  then begin
    ErrNo		:= Err_LS_NoTlk;
    Exit;
  end;

  if _TlkF.Defined then begin
    if not Assigned(Lib.TlkF) then
      Lib.TlkF		:= TTlkFile.Create;
    S			:= _TlkF.AsString;
    if S = '' then begin
      Lib.Tlk.Female	:= nil;
      FreeAndNil(Lib.TlkF);
      Lib.Mode		:= useTlk;
    end
    else if S[1] = '>' then begin
      System.Delete(S, 1, 1);
      E			:= Lib.TlkF.SaveToFile(S);
      if E <> Success then Exit;
    end
    else begin
      E			:= Lib.TlkF.LoadFromFile(S);
      if E <> Success then Exit;
      Lib.Tlk.Female	:= Lib.TlkF;
    end;
  end;

  if _Custom.Defined then begin
    if not Assigned(Lib.CustomTlk) then
      Lib.CustomTlk	:= TTlkFile.Create;
    S			:= _Custom.AsString;
    if S = '' then begin
      Lib.Tlk.Alternate	:= nil;
      FreeAndNil(Lib.CustomTlk);
      Lib.Mode		:= useTlk;
    end
    else if S[1] = '>' then begin
      System.Delete(S, 1, 1);
      E			:= Lib.CustomTlk.SaveToFile(S);
      if E <> Success then Exit;
    end
    else begin
      E			:= Lib.CustomTlk.LoadFromFile(S);
      if E <> Success then Exit;
      Lib.Tlk.Alternate	:= Lib.CustomTlk;
    end;
  end;

  if _CustomF.Defined then begin
    if not Assigned(Lib.CustomTlk) then begin
      ErrNo		:= Err_LS_NoTlk;
      Token.Complain(ErrNo);
      Exit;
    end;
    if not Assigned(Lib.CustomTlkF) then
      Lib.CustomTlkF	:= TTlkFile.Create;
    S			:= _CustomF.AsString;
    if S = '' then begin
      Lib.CustomTlk.Female := nil;
      FreeAndNil(Lib.CustomTlkF);
      Lib.Mode		:= useTlk;
    end
    else if S[1] = '>' then begin
      System.Delete(S, 1, 1);
      E			:= Lib.CustomTlkF.SaveToFile(S);
      if E <> Success then Exit;
    end
    else begin
      E			:= Lib.CustomTlkF.LoadFromFile(S);
      if E <> Success then Exit;
      Lib.CustomTlk.Female := Lib.CustomTlkF;
    end;
  end;

  if _UsingTlk.Defined and Assigned(Lib.Tlk) then
    Lib.Mode		:= useTlk
  else if _UsingTlkF.Defined and Assigned(Lib.TlkF) then
    Lib.Mode		:= useTlkF
  else if _UsingCustom.Defined and Assigned(Lib.CustomTlk) then
    Lib.Mode		:= useCustom
  else if _UsingCustomF.Defined and Assigned(Lib.CustomTlkF) then
    Lib.Mode		:= useCustomF;

  finally
    Token.Env.Errno	:= ErrNo;
    Token.Env.IoErr	:= E;
    if (ErrNo = LS_Success) and (E = Success) then
      Token.Value	:= 1;
  end;

end;

(*------------------------------------------------------------------------------
addstrref $value

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._AddStrref;
begin
  case Lib.Mode of
    useTlk:
      Token.Value	:= Lib.Tlk.Add(ArgVal.AsString);
    useTlkF:
      Token.Value	:= Lib.TlkF.Add(ArgVal.AsString);
    useCustom:
      Token.Value	:= Lib.CustomTlk.Add(ArgVal.AsString);
    useCustomF:
      Token.Value	:= Lib.CustomTlkF.Add(ArgVal.AsString);
  end;
end;

(*------------------------------------------------------------------------------
findstrref $stringmask = $_; $ignorecase = 0

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._FindStrref;
var
  S			: String;
  Ignore		: Boolean;
begin
  S			:= Args['stringmask'].AsString;
  Ignore		:= Args['ignorecase'].AsBool;

  if Ignore then
    S			:= Lowercase(S);

  case Lib.Mode of
    useTlk:
      Token.CaptureList(Lib.Tlk.FindString(S, Ignore), True);
    useTlkF:
      Token.CaptureList(Lib.TlkF.FindString(S, Ignore), True);
    useCustom:
      Token.CaptureList(Lib.CustomTlk.FindString(S, Ignore), True);
    useCustomF:
      Token.CaptureList(Lib.CustomTlkF.FindString(S, Ignore), True);
  end;

end;

(*------------------------------------------------------------------------------
getstrref $index = $_

Get the value (string) of a Strref.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._GetStrref;
var
  C			: Cardinal;
begin
  C			:= ArgVal.AsInt;
  if (Lib.Mode = useCustom) or (Lib.Mode = useCustomF) then
    C			:= C or TLK_FLAG_CUSTOM;
  Token.Value :=
    Lib.Tlk.GetString(C, (Lib.Mode = useTlkF) or (Lib.Mode = useCustomF));
end;

(*------------------------------------------------------------------------------
getstrrefflags $index = $_

Get the FLAGS field of a Strref.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._GetStrrefFlags;
var
  C			: Cardinal;
begin
  C			:= ArgVal.AsInt;
  if (Lib.Mode = useCustom) or (Lib.Mode = useCustomF) then
    C			:= C or TLK_FLAG_CUSTOM;
  Token.Value :=
    Lib.Tlk.GetStringFlags(C, (Lib.Mode = useTlkF) or (Lib.Mode = useCustomF));
end;

(*------------------------------------------------------------------------------
setstrref $index, $value = $_

Set the value (string) of a StrRef in the TLK.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._SetStrref;
var
  C			: Cardinal;
  S			: String;
begin
  C			:= Args['index'].AsInt;
  S			:= Args['value'].AsString;
  if (Lib.Mode = useCustom) or (Lib.Mode = useCustomF) then
    C			:= C or TLK_FLAG_CUSTOM;
  Lib.Tlk.SetString(C, S, (Lib.Mode = useTlkF) or (Lib.Mode = useCustomF));
  Token.Value		:= 1;
end;

(*------------------------------------------------------------------------------
setstrrefflags $index, $flags

Set a StrRef's FLAGS field. $flags should be a bitmask appropriate to TLK,
some common bitmasks are provided as consts in the tlk library.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk._SetStrrefFlags;
var
  C, F			: Cardinal;
begin
  C			:= Args['index'].AsInt;
  F			:= Args['flags'].AsInt;
  if (Lib.Mode = useCustom) or (Lib.Mode = useCustomF) then
    C			:= C or TLK_FLAG_CUSTOM;
  Lib.Tlk.SetStringFlags(C, F, (Lib.Mode = useTlkF) or (Lib.Mode = useCustomF));
  Token.Value		:= 1;
end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncTlk.Evaluate;
var
  FnTlk			: TLetoScriptFnTlk;
begin
  inherited;
  Lib			:= TLetoScriptLibTlk(Parent);

  FnTlk			:= TLetoScriptFnTlk(Fn);

  if (FnTlk <> fnTlkDialog) and not Assigned(Lib.Tlk) then begin
    Token.Complain(Err_LS_NoTlk);
    Exit;
  end;

  case FnTlk of
    fnTlkDialog:		_Dialog;
    fnTlkAddStrref:		_AddStrref;
    fnTlkFindStrref:		_FindStrref;
    fnTlkGetStrref:		_GetStrref;
    fnTlkGetStrrefFlags:	_GetStrrefFlags;
    fnTlkSetStrref:		_SetStrref;
    fnTlkSetStrrefFlags:	_SetStrrefFlags;
  end;

end;


{ TLetoScriptFunc2da }


(*------------------------------------------------------------------------------
DoMatch

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc2da.DoMatch(
  Sender		: TObject;
  Row			: Integer;
  Col			: String;
  ICol			: Integer;
  Value			: String;
  var Matches		: Boolean
);
var
  Blk			: TLetoScriptObj;
begin
  Blk			:= TLetoScriptObj(Token.LoopContext);

  // set up vars
  //Blk.Scope.Get('~row', vtScalar, True).Value := Row;

  Blk.Evaluate;
  Matches		:= Blk.AsBool;

  Blk.LoopContext	:= nil;

end;

(*------------------------------------------------------------------------------
locate (SCALAR context)
locate $array; $column = 0, $value = $_, &match

We don't know the row, but we know the column and what value it might have.
Find the first row that matches.

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc2da._Locate;
var
  OnMatch		: T2daMatchEvent;
  A			: TLetoScriptValue;
begin

  OnMatch		:= nil;
  A			:= Args['match'];
  if A.Defined then begin
    OnMatch		:= DoMatch;
    Token.LoopContext	:= A;
  end;

  Token.Value :=
    Lib.Cache.Locate(
      Args['array'].AsString,
      Args['column'].AsString,
      Args['value'].AsString,
      OnMatch
    );

end;

(*------------------------------------------------------------------------------
locate (LIST context)
locate $array; $column = 0, $value = $_, &match

We don't know the row, but we know the column and what value it might have.
Find all rows that match.

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc2da._LLocate;
var
  OnMatch		: T2daMatchEvent;
  A			: TLetoScriptValue;
begin

  OnMatch		:= nil;
  A			:= Args['match'];
  if A.Defined then begin
    OnMatch		:= DoMatch;
    Token.LoopContext	:= A;
  end;

  Token.CaptureList(
    Lib.Cache.LocateAll(
      Args['array'].AsString,
      Args['column'].AsString,
      Args['value'].AsString,
      OnMatch
    ), True
  );

end;

(*------------------------------------------------------------------------------
lookup $array, $row = $_, $column = 0

We know the row and which column. Get the value at that specific location.

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc2da._Lookup;
var
  S			: String;
begin

  if
    Lib.Cache.Lookup(
      Args['array'].AsString,
      Args['row'].AsInt,
      Args['column'].AsString,
      S
    )
  then
    Token.Value		:= S
  else
    Token.Defined	:= False;

end;

(*------------------------------------------------------------------------------
meta

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc2da._Meta;
var
  _Dir, _Caching,
    _BuildCache		: TLetoScriptValue;
begin

  _Dir			:= Args['dir'];
  _Caching		:= Args['caching'];
  _BuildCache		:= Args['buildcache'];

  if _Dir.Defined then
    Lib.Cache.Directory	:= _Dir.AsString;

  if _Caching.Defined then
    Lib.Cache.Caching	:= _Caching.AsBool;

  if _BuildCache.Defined then begin
    Token.Complain(Err_LS_NYI);
    //Lib.Cache.BuildCache;
  end;

  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFunc2da.Evaluate;
begin
  inherited;
  Lib			:= TLetoScriptLib2da(Parent);

  case TLetoScriptFn2da(Fn) of
    fn2daLocate:
      if Token.Context = ocList then
        _LLocate
      else
        _Locate;
    fn2daLookup:	_Lookup;
    fn2daMeta:		_Meta;
  end;

end;


{ TLetoScriptFuncFpt }


(*------------------------------------------------------------------------------
extract $filename, $varname, %handle

Extract a GFF from an FPT.

Open the FPT set described by $filename, look up $varname in the memo, and from
that data build a GFF, spawning %handle.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFpt._Extract;
var
  F			: TLetoFile;
  VarName, H		: String;
begin

  ArgIsObj('handle');
  if ArgObj.EvalDefined then
    ArgObj.Exception(Err_LS_LiveHandle);

  VarName		:= Args['varname'].AsString;
  if Fpt.Fpt.Dbf.IndexOf(VarName) = -1 then begin
    Token.Env.Errno	:= Err_LS_NotFptVarName;
    Exit;
  end;

  F			:= TLetoFile.Create(ftGff, '');
  Token.Env.IoErr	:= F.Gff.LoadFromStream(Fpt.Fpt.ExtractStream(VarName));
  if Token.Env.IoErr <> Success then Exit;
  H			:= ArgObj.Text;
  H			:= StringReplace(H, '%', '', []);
  Token.Env.Handle[H]	:= F;

  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
inject $filename, $varname, %handle = %_

Inject GFF data into an FPT. $filename and $varname must already exist.

fpt.inject actually works like the old Phoenix <fpt:replace>. There is no
equivalent of the Phoenix <fpt:inject>, the Unicorn fpt library is meant
to be "lite", in favor of SQL or SCORCO.

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFpt._Inject;
var
  F			: TLetoFile;
  VarName		: String;
  Stream		: TMemoryStream;
begin

  ArgIsObj('handle');
  F			:= ArgObj.AsFile;
  if not Assigned(F) then
    ArgObj.Exception(Err_LS_InvalidFile)
  else if not F.IsGff then
    ArgObj.Exception(Err_LS_NotGffFile);

  VarName		:= Args['varname'].AsString;
  if Fpt.Fpt.Dbf.IndexOf(VarName) = -1 then begin
    Token.Env.Errno	:= Err_LS_NotFptVarName;
    Exit;
  end;

  Stream		:= TMemoryStream.Create;
  F.Gff.SaveToStream(Stream);

  Token.Value		:= Fpt.Fpt.ReplaceNamed(Stream, VarName);
  Fpt.Save;

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFpt.Evaluate;
var
  Filename		: String;
begin
  inherited;
  Lib			:= TLetoScriptLibFpt(Parent);

  Fpt			:= TLetoFile.Create;
  try

  Filename		:= Args['filename'].AsString;
  Token.Env.IoErr	:= Fpt.Open(Filename);
  if Token.Env.IoErr <> Success then Exit;
  if not Fpt.IsFpt then begin
    Token.Text		:= Filename;
    Token.Complain(Err_LS_NotFptFile, Filename);
    Exit;
  end;

  case TLetoScriptFnFpt(Fn) of
    fnFptExtract:	_Extract;
    fnFptInject:	_Inject;
  end;

  finally
    FreeAndNil(Fpt);
  end;

end;


{ TLetoScriptFuncFileSys }


(*------------------------------------------------------------------------------
$filecopy $from, $to; $overwrite = 0

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._Copy;
var
  SrcName, DestName	: String;
  Overwrite		: Boolean;
  E			: TLetoError;
  Source, Dest		: TFileStream;
begin
  // DONE 1: ADD: [25] API-specific FileCopy, FileMove

  SrcName		:= Args['from'].AsString;
  DestName		:= Args['to'].AsString;
  Overwrite		:= Args['overwrite'].AsBool;

  E			:= Success;
  try

  if not FileExists(SrcName) then
    E		:= Err_Missing_File
  else if FileExists(DestName) and not Overwrite then
    E			:= Err_No_Overwrite
  else if FileExists(DestName) and not SysUtils.DeleteFile(DestName) then
    E			:= Err_Locked_File;
  if E <> Success then Exit;

  try
    Source :=
      TFileStream.Create(SrcName, fmOpenRead or fmShareDenyWrite);
  except
    E			:= Err_Locked_File;
    Exit;
  end;

  try
    Dest :=
      TFileStream.Create(DestName, fmCreate or fmShareExclusive);
  except
    E			:= Err_Locked_File;
    Exit;
  end;

  Dest.CopyFrom(Source, 0);

  finally
    if Assigned(Source) then
      FreeAndNil(Source);
    if Assigned(Dest) then
      FreeAndNil(Dest);
    Token.Env.IoErr	:= E;
    if E = Success then
      Token.Value	:= 1;
  end;

end;

(*------------------------------------------------------------------------------
filedelete $filename = $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._Delete;
begin
  Token.Value		:= Integer( SysUtils.DeleteFile(ArgVal.AsString) );
end;

(*------------------------------------------------------------------------------
$directoryexists $dirname = $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._DirExists;
begin
  Token.Value		:= Integer( DirectoryExists(ArgVal.AsString) );
end;

(*------------------------------------------------------------------------------
fileexists $filename = $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._FileExists;
begin
  Token.Value		:= Integer( FileExists(ArgVal.AsString) );
end;

(*------------------------------------------------------------------------------
fileinfo $filename = $_

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._FileInfo;
var
  F			: TSearchRec;
  {$IFDEF MSWINDOWS}
  SysTime		: _SYSTEMTIME;
  function _FileTime_(const WinTime: _FILETIME): String;
  begin
    if not FileTimeToSystemTime(WinTime, SysTime) then
      Result		:= ''
    else
      Result :=
        IntToStr(SysTime.wYear) + ' ' +
        IntToStr(SysTime.wMonth) + ' ' +
        IntToStr(SysTime.wDayOfWeek) + ' ' +
        IntToStr(SysTime.wDay) + ' ' +
        IntToStr(SysTime.wHour) + ' ' +
        IntToStr(SysTime.wMinute) + ' ' +
        IntToStr(SysTime.wSecond) + ' ' +
        IntToStr(SysTime.wMilliseconds);
  end;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  // DONE 1: ADD: [27r3] FileSys.FileInfo
  // TODO 4: ADD: Wildcard support for fileinfo (requires hash-in-array)
  if FindFirst(Args['filename'].AsString, faAnyFile, F) = 0 then begin
    InitList;
    List.Add('time').Value		:= F.Time;
    List.Add('size').Value		:= F.Size;
    List.Add('attr').Value		:= F.Attr;
    List.Add('name').Value		:= F.Name;
    List.Add('excludeattr').Value	:= F.ExcludeAttr;
    List.Add('findhandle').Value	:= F.FindHandle;
    List.Add('fileattributes').Value	:= F.FindData.dwFileAttributes;
    List.Add('creationtime').Value	:= _FileTime_(F.FindData.ftCreationTime);
    List.Add('lastaccesstime').Value	:= _FileTime_(F.FindData.ftLastAccessTime);
    List.Add('lastwritetime').Value	:= _FileTime_(F.FindData.ftLastWriteTime);
    List.Add('filesizehigh').Value	:= F.FindData.nFileSizeHigh;
    List.Add('filesizelow').Value	:= F.FindData.nFileSizeLow;
    List.Add('reserved0').Value		:= F.FindData.dwReserved0;
    List.Add('reserved1').Value		:= F.FindData.dwReserved1;
    SysUtils.FindClose(F);
  end;
  {$ENDIF}

  // TODO 4: LINUX: FileInfo
  {$IFDEF LINUX}
  {$ENDIF}
end;

(*------------------------------------------------------------------------------
forcedirectories $path

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._ForceDirs;
begin
  Token.Value		:= Integer( ForceDirectories(ArgVal.AsString) );
end;

(*------------------------------------------------------------------------------
filerename $from, $to; $overwrite = 0

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys._Rename;
var
  OldName, NewName	: String;
  Overwrite		: Boolean;
begin

  OldName		:= Args['from'].AsString;
  NewName		:= Args['to'].AsString;
  Overwrite		:= Args['overwrite'].AsBool;

  case Overwrite of

    True:
    begin
      {$IFDEF MSWINDOWS}
      { Windows will normally fail when attempting this. }
      if FileExists(NewName) then
        Overwrite	:= SysUtils.DeleteFile(NewName);
      {$ENDIF}
      if Overwrite then
        Token.Value	:= Integer( SysUtils.RenameFile(OldName, NewName) )
      else
        Token.Value	:= 0;
    end;

    False:
      { Linux will normally want to overwrite. }
      if FileExists(NewName) then
        Token.Value	:= 0
      else
        Token.Value	:= Integer( SysUtils.RenameFile(OldName, NewName) );

  end;

end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncFileSys.Evaluate;
begin
  inherited;

  case TLetoScriptFnFileSys(Fn) of
    fnFSFileCopy:	_Copy;
    fnFSFileDelete:	_Delete;
    fnFSDirExists:	_DirExists;
    fnFSFileExists:	_FileExists;
    fnFSFileInfo:	_FileInfo;
    fnFSFileMove,
    fnFSFileRename:	_Rename;
    fnFSForceDirs:	_ForceDirs;
  end;

end;


(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
