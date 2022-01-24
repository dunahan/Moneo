(*------------------------------------------------------------------------------
Class_LetoScriptSub

Handles LetoScript SUBROUTINEs, attending to all their unique properties
and syntax.

Notes:

sub

Defines a sub-procedure. The syntax goes well beyond what Perl defines, for
something more like C / Delphi.

The simplest syntax is the anonymous code block constructor:

sub { CODE };

In this form, CODE is not executed, and the result is merely a reference. The
stop is required. This syntax is meant to be assigned or used as an lvalue:

$coderef = sub { CODE };  # assigns
&$coderef;                # executes

The most common syntax is the definition of an actual procedure:

sub NAME { CODE }

NAME is what distinguishes this syntax. NAME can be anything, and follows the
same rules as for a symbol. A stop is not required after the block.

To define arguments that a sub can receive, place them in parantheses after
the sub's NAME:

sub NAME ( ARGS ) { CODE }

ARGS is intuitively like C / Delphi, and each arg's type is specified using
a symbolic:

sub foo($a, @b, $c) { print $a, @b, $c )

Notice how an array can be passed in as a whole argument, and is not subject
to auto-expansion as in Perl. You can pass an anonymous array using Delphi
syntax:

foo('Hello', ['Bob', 'Jim'], 'foo');

The array of 'Bob' and 'Jim' is received in @b, and you can use @b.count as
well as pop / shift to work on it.

The use of ; in the ARGS list separates requires arguments from optional
arguments. A runtime error is generated if your script does not comply with
your own requirements:

sub foo($a, @b; $c, @d) { ... }

As an added feature, calls to subs may argue-by-name, just as you can to
LetoScript's own functions. Given the definition of foo just above, you could
call it like so:

foo('Hello', ['Bob', 'Jim'], d => ['today', 'tomorrow'])

If ARGS is not included in the sub's definition, it by default will store
any received arguments in @_. The syntax looks like this:

sub NAME { CODE }

Finally, subs can have attributes, much like in C / Delphi. (For instance,
cdecl.) These are specified in a [] block, after ARGS:

sub NAME ( ARGS ) [ ATTRIBS ] { CODE }
sub NAME [ ATTRIBS ] { CODE }


This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoScriptSub;

{$I LetoScript.inc}

interface

uses
  Classes, SysUtils,
  Header_Leto, Header_LetoScript,
  Class_LetoScriptEnv,
  Class_LetoScript, Class_LetoScriptObj, Class_LetoScriptLib;

type

  TLetoScriptSub	= class(TLetoScriptFunc)

  private

    FIsCodeRef		: Boolean;

    FParam		: TLetoScriptObj;
    FAttrib		: TLetoScriptObj;
    FCode		: TLetoScriptObj;

    procedure SetAnonName;

    procedure SetParam(const Value: TLetoScriptObj);
    procedure SetAttrib(const Value: TLetoScriptObj);
    procedure SetCode(const Value: TLetoScriptObj);

    function BuildParams(
      const Param	: TLetoScriptObj;
      const List	: TStringList
    ): TLetoScriptArg;

    procedure BuildAttribs(const Attrib: TLetoScriptObj);

    procedure ArgsToVars(const Vars: TStringList);

  protected

    function GetName: String; override;
    procedure SetName(const AName: String); override;

    function GetOp: TLetoScriptOp; override;

  public

    Token		: TLetoScriptObj;

    EmptyParam		: Boolean;
    EmptyAttrib		: Boolean;

    property IsCodeRef: Boolean read FIsCodeRef;

    property Param: TLetoScriptObj read FParam write SetParam;
    property Attrib: TLetoScriptObj read FAttrib write SetAttrib;
    property Code: TLetoScriptObj read FCode write SetCode;

    constructor Create(
      Syntax		: String;
      const AParent	: TLetoScriptLib;
      const AEnv	: TLetoScriptEnv
    ); override;
    destructor Destroy; override;

    function CheckFoldable: Boolean; override;

    function Define: TLetoScriptError;

    function Execute(const Vars: TLetoScriptArgs): TLetoScriptObj;

  end;

implementation


{ TLetoScriptSub }


(*------------------------------------------------------------------------------
SetAnonName

Give an anonymous sub a name. This happens automatically when the sub is
assigned its Arg, Attrib, or Code, and Name is not already set.

Doing so implies an anonymous subroutine, which is automatically a code ref.

------------------------------------------------------------------------------*)
procedure TLetoScriptSub.SetAnonName;
begin
  FIsCodeRef		:= True;
  Name			:= 'CODE(0x' + IntToHex( Integer(@self), 1 ) + ')';
end;

(*------------------------------------------------------------------------------
property Param

------------------------------------------------------------------------------*)
procedure TLetoScriptSub.SetParam(const Value: TLetoScriptObj);
begin
  if Name = '' then SetAnonName;
  FParam		:= Value;
  EmptyParam		:= False;

  if Assigned(Value) then
    BuildParams(Value, Args.Standards);

end;

(*------------------------------------------------------------------------------
property Attrib

------------------------------------------------------------------------------*)
procedure TLetoScriptSub.SetAttrib(const Value: TLetoScriptObj);
begin
  if Name = '' then SetAnonName;
  FAttrib		:= Value;
  EmptyAttrib		:= False;

  if Assigned(Value) then
    BuildAttribs(Value);

end;

(*------------------------------------------------------------------------------
property Code

------------------------------------------------------------------------------*)
procedure TLetoScriptSub.SetCode(const Value: TLetoScriptObj);
begin
  if Name = '' then SetAnonName;
  FCode			:= Value;
end;

(*------------------------------------------------------------------------------
property Name

------------------------------------------------------------------------------*)
function TLetoScriptSub.GetName: String;
begin
  Result		:= FName;
end;

procedure TLetoScriptSub.SetName(const AName: String);
var
  I			: Integer;
begin
  FName			:= AName;
  Token.Text		:= AName;

  if TryGetLSFunction('*', FName, I) then
    Token.Complain(Err_LS_SubAmbiguous, LS_Functions[I].Name);

end;

(*------------------------------------------------------------------------------
property Op

------------------------------------------------------------------------------*)
function TLetoScriptSub.GetOp: TLetoScriptOp;
begin
  if EmptyParam then
    Result		:= opFunc

  else if Attribs.ListOp then
    Result		:= opListOp
  else if Attribs.UnaryOp then
    Result		:= opUnaryOp

  else if
    not Assigned(Param) or
    (Arg.ArgType = atList) or
//    Arg.Specs.Greedy or
    (Param.Op in [opComma, opStop])
  then
    Result		:= opListOp
  else
    Result		:= opUnaryOp;

end;

(*------------------------------------------------------------------------------
BuildParams

Serialize ( ARGS ), check syntax.

------------------------------------------------------------------------------*)
function TLetoScriptSub.BuildParams(
  const Param		: TLetoScriptObj;
  const List		: TStringList
): TLetoScriptArg;
var
  S			: String;
  Func			: TLetoScriptFunc;
  I			: Integer;
begin

  Result		:= nil;

  case Param.Op of

    opUnaryOp:
    begin
      Func		:= TLetoScriptFunc(Param.Func);
      if not Func.Attribs.ParamOp then
        Param.Exception(Err_LS_SubBadParam, Name);
      Func.Evaluate;
      Result		:= BuildParams(TLetoScriptObj(Param.Rvalue), List);
    end;

    opSymbol:
    begin
      S			:= Param.Text;
      if
        (Args.Standards.IndexOf(S) > -1) or
        (Args.Options.IndexOf(S) > -1)
      then
        Token.Exception(Err_LS_BadSub, Name);
      Result		:= Args.AddArg(S, List);
      if TryGetLSFunction('*', Result.Name, I) then
        Token.Complain(Err_LS_SubAmbiguousParam, LS_Functions[I].Name, Name);
    end;

    opAssign:
    begin
      Result		:= BuildParams(Param.Left, List);
      Result.Specs.NeedsValue := False;
      // SALT: sub foo($a = undef)
      if
        Assigned(Param.Right.Func) and
        ( TLetoScriptFunc(Param.Right.Func).Parent.Name = 'system' ) and
        ( TLetoScriptFunc(Param.Right.Func).Fn = Word(fnUndef) ) and
        ( TLetoScriptFunc(Param.Right.Func).Args.LastArgNo = 0 )
      then
        Result.Specs.NilValue := True
      // Is CheckFoldable redundant after PreProcess?
      else if (Param.Right.Op = opConst) or Param.Right.CheckFoldable then
        Result.MakeConstDefault(Param.Right.Value)
      // SALT: sub foo($a = $_)
      else if
        (Param.Right.Kind in [symScalar, symList]) and
        (Param.Right.AsVariant = '_')
      then
        Result.DefValSpec := Param.Right.Text
      else
        Token.Exception(Err_LS_SubBadDefault, Name);
      // TODO 4: ADD: Interpolative argument defaults
    end;

    opComma:
    begin
      if not Assigned(Param.Left) or not Assigned(Param.Right) then
        Token.Exception(Err_LS_BadSub, Name);
      BuildParams(Param.Left, List);
      BuildParams(Param.Right, List);
    end;

    opStop:
    begin
      BuildParams(Param.Left, List);
      BuildParams(Param.Right, Args.Options);
    end;

  else
    Param.Exception(Err_LS_SubBadParam, Name);

  end;

end;

(*------------------------------------------------------------------------------
BuildAttribs

Serialize [ ATTRIBS ].

------------------------------------------------------------------------------*)
procedure TLetoScriptSub.BuildAttribs(const Attrib: TLetoScriptObj);
begin

  case Attrib.Op of

    opConst:
      if
        not (Attrib.Kind in [quSingle, idBare]) or
        not Attribs.AddAttrib( Lowercase(Attrib.AsVariant) )
      then
        Param.Exception(Err_LS_SubBadAttrib, Name);

    opKey:
    begin
      Token.Exception(Err_LS_SubBadAttrib, Name);
    end;

    opComma:
    begin
      if not Assigned(Attrib.Left) or not Assigned(Attrib.Right) then
        Token.Exception(Err_LS_BadSub, Name);
      BuildAttribs(Attrib.Left);
      BuildAttribs(Attrib.Right);
    end;

  else
    Param.Exception(Err_LS_SubBadAttrib, Name);

  end;

end;

(*------------------------------------------------------------------------------
ArgsToVars

------------------------------------------------------------------------------*)
procedure TLetoScriptSub.ArgsToVars(const Vars: TStringList);
var
  I			: Integer;
  A			: TLetoScriptArg;
begin

  for I := 0 to Vars.Count-1 do begin

    A			:= TLetoScriptArg(Vars.Objects[I]);
    A.Scope		:= Code.Scope;

    case A.ArgType of

      atScalar, atScalarVar:
        if A.Value <> Env.NilValue then
          Code.Scope.Get(A.Name, vtScalar, True).CopyFrom(A.Value);

      atList, atListVar:
        Code.Scope.Get(A.Name, vtList, True).
          AsList.Unshift(A.Value);

      atField: ;

      atFile: ;

      atBlock: ;

    end;

  end;

end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptSub.Create(
  Syntax	: String;
  const AParent	: TLetoScriptLib;
  const AEnv	: TLetoScriptEnv
);
begin
  inherited Create(Syntax, AParent, AEnv);

  EmptyParam		:= False;
  EmptyAttrib		:= False;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptSub.Destroy;
begin

  { The sub is the owner for all of these: }
  FreeAndNil(FParam);
  FreeAndNil(FAttrib);
  FreeAndNil(FCode);

  { Token is being freed by the Env. }

  inherited;
end;

(*------------------------------------------------------------------------------
CheckFoldable

------------------------------------------------------------------------------*)
function TLetoScriptSub.CheckFoldable: Boolean;
begin
  // TODO 4: ADD: Foldable subroutines (macros)
  Result		:= False;
end;

(*------------------------------------------------------------------------------
Define

Used by TLetoScript at the end of a sub's definition. The steps necessary at
this point are,

 - Check syntax of params and attribs

 - Any salt to be applied (e.g., the @_ shortcut)

 - Add the sub to the environment's list in Subs

------------------------------------------------------------------------------*)
function TLetoScriptSub.Define: TLetoScriptError;
var
  List			: TStringList;
  I			: Integer;
  A			: TLetoScriptArg;
begin

  Result		:= LS_Success;

  { Syntax checks }
  Token.EStr2		:= Name;
  List			:= TStringList.Create;
  List.Sorted		:= True;
  List.Duplicates	:= dupError;
  try
    for I := 0 to Args.Standards.Count-1 do begin
      Token.EStr1	:= Args.Standards[I];
      A			:= Args.GetByIndex(Args.Standards, I);
      if Token.EStr1 = '_' then begin
        Token.EStr1	:= A.FullName;
        if
          ( (A.ArgType in [atScalar, atScalarVar]) and (I > 0) )
          or
          ( (A.ArgType in [atList, atListVar]) and (I < Args.Standards.Count-1) )
        then begin
          Result	:= Err_LS_SubBadShort;
          Break;
        end;
      end else
        List.Add(Token.EStr1);
    end;
    for I := 0 to Args.Options.Count-1 do begin
      Token.EStr1	:= Args.Options[I];
      if Token.EStr1 = '_' then begin
        Token.EStr1	:= Args.GetByIndex(Args.Options, I).FullName;
        Result		:= Err_LS_SubBadOption;
        Break;
      end;
      List.Add(Token.EStr1);
    end;
  except
    on E:EStringListError do
      Result		:= Err_LS_SubIdRedecl;
  end;
  FreeAndNil(List);

  if Result <> LS_Success then Exit;

  // SALT: sub foo { ... } == sub foo (@_) { ... }
  if not Assigned(Param) and not EmptyParam then
    Args.AddArg('@_', Args.Standards);

  { Register }
  if Env.Subs.IndexOf(Name) = -1 then
    Env.Subs.AddObject(Name, self)
  else
    Result		:= Err_LS_SubRedef;

end;

(*------------------------------------------------------------------------------
Execute

Establishes scope and pulls in Args, then executes Code, and ensures a
return value.

------------------------------------------------------------------------------*)
function TLetoScriptSub.Execute(const Vars: TLetoScriptArgs): TLetoScriptObj;
var
  N			: String;
begin

  Result		:= nil;

  if not Assigned(Code) then
    Exit;

  try
  N			:= Env.Name;

  { Establish a discrete scope for this sub. }
  if not Assigned(Code.Scope) then
    Code.Scope		:= TLetoScriptVarScope.Create(Code, Env);
  Code.Scope.PrepArgScope;

  { Propagate vars. }
  ArgsToVars(Vars.Standards);
  ArgsToVars(Vars.Options);

  { Run Code, and catch any return. }
  try
    Token.Env.Name	:= Token.Env.Name + ' &' + Name;  // This causes verbose backtracing
    Code.Evaluate;
    if Code.Op = opStop then
      Result		:= TLetoScriptObj(Token.Env.Last)
    else
      Result		:= Code;
  except
    on ELetoScriptReturn do
      Result		:= TLetoScriptObj(Token.Env.Last);
  end;

  finally
    Token.Env.Name	:= N;
  end;

end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
