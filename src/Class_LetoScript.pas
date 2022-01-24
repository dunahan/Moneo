(*------------------------------------------------------------------------------
Class_LetoScript

LetoScript is a powerful (VHL) scripting language for interacting with GFF
and ERF data. It offers enough flexibility in terms of extraction and
manipulation that whole utilities can be built using it, although in general
it is meant for "batch jobs", especially where MODs are concerned.

Class_LetoScript is a wrapper for Class_LetoScriptObj, and provides generic
access to script compilation, evaluation, and rendering. Its primary
responsibility is the multi-pass process (lexer, parser, etc.) of converting
text into tokens. Class_LetoScriptObj &c conain the actual logic for
evaluating/rendering them.

Version history:

01: Minotaur. Build 4.0.03 - 4.0.04 (Nov 2003)
  First version, string-based and primarily concerned with named expressions
  (Fields), for-loops, and simple boolean statements.
02: Doppleganger. Builds 4.0.05 - 4.0.9 (Nov 2003 - Dec 2003)
  The lexer was expanded to use objects (TLetoScriptObj) instead of relying
  purely on the character stream. Tokenization relies on XML-like <token>
  syntax. File handle support, numerous expressions. Class_LetoParser
  becomes Class_LetoScript, and the language is given its name.
03: Phoenix. Builds 4.0.10 - 4.0.18 (Jan 2004 - Apr 2004)
  Lists, hashes (and anon constructors), and formalization of the XML-like
  syntax. Greatly expanded, but quickly outgrew the limitations of the
  piecemeal Lexer and XML-like syntax.
04: Unicorn. Builds 4.0.19 ... (May 2004 - present)
  Syntax rewrite, based on Perl. Lexer is all-in-one, and the Parser is
  introduced: so that evaluation now uses parse trees.
05: Dragon. ...?
  This version is planned to be introduced possibly around build 30. It
  will introduce objects; object-oriented LetoScript.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoScript;

{$I LetoScript.inc}

interface

uses
  Classes, SysUtils, Math, StrUtils,
  Header_Leto, Header_LetoScript,
  Class_LetoFile, Class_ErfFile, Class_GffFile,
  Class_LetoScriptEnv, Class_LetoScriptObj;

type

  TLetoScript		= class

  private

    FEnv		: TLetoScriptEnv;
    FOwnEnv		: Boolean;

    procedure Compiler;

    procedure SetEnv(const Value: TLetoScriptEnv);
    function GetLib(Name: String): TLetoScriptLib;

    function AddLib(const ALib: TLetoScriptLib): TLetoScriptLib;

  public

    RootContext		: TLetoScriptOpCon;

    Stream		: TStream;
    Buf			: String;
    Chr			: Char;
    BufPos, BufLine	: Cardinal;
    BufSize		: Cardinal;

    Text		: String;

    PeekPos		: Cardinal;
    PeekChr		: Char;

    Sought		: String;

    Libs		: array of TLetoScriptLib;
    SysLib		: TLetoScriptLib;
    ErrStr		: String;

    property Env: TLetoScriptEnv read FEnv write SetEnv;
    property Lib[Name: String]: TLetoScriptLib read GetLib;

    constructor Create(
      const Con		: TLetoScriptOpCon;
      const AEnv	: TLetoScriptEnv = nil
    );
    destructor Destroy; override;

    procedure BuildFuncACL(const ACL: String);
    function EnableDisable(
      const Name	: String;
      const Enable	: Boolean
    ): TLetoScriptError;

    procedure Compile(const AStream: TStream); overload;
    procedure Compile(const S: String); overload;

    function Lexer(
      const Root	: TLetoScriptObj;
      const Stop	: TLetoScriptOp
    ): TLetoScriptError;

    function Parser(
      const List	: TList;
      var Root		: TLetoScriptObj
    ): TLetoScriptError;

    { Used by Lexer - exposed for cross-use }

    procedure GetLine;
    function GetChr(const Count: Cardinal = 1): Char;

    function Peek: Char;
    procedure Peeked;

    function SeekStr(const Substr: String): Boolean;
    function SeekLine(S: String): Boolean;

    function EscapedChr: String;

    function GetToken(const Token, Prev: TLetoScriptObj): TLetoScriptError;

    function GetOp(const Token, Prev: TLetoScriptObj): TLetoScriptError;

    function GetNamed(const Token, Prev: TLetoScriptObj): TLetoScriptError;

    function GetFunc(const Token, Prev: TLetoScriptObj): TLetoScriptError;

    function GetConst(const Token, Prev: TLetoScriptObj): TLetoScriptError;

    function GetSymbol(const Token, Prev: TLetoScriptObj): TLetoScriptError;

    function GetQuoted(const Token, Prev: TLetoScriptObj): TLetoScriptError;

  end;


implementation

uses
  Class_LetoScriptLib, Class_LetoScriptSub, Class_LetoScriptSql;

type

  CharSet		= set of Char;

  TLexCompound		= record
    Icon		: String;
    Op			: TLetoScriptOp;
    Kind		: TLetoScriptOpKind;
  end;

const

  Whitespace		= [' ', #9];
  Alpha			= ['a'..'z', 'A'..'Z'];
  Numer			= ['0'..'9'];

  SymbolicIndex		= '!#%^&*()-=+[]{}\|;:,<.>/?~';
  Symbolics		: array[1..Length(SymbolicIndex)] of TLetoScriptOp = (
    opLogNot, opComment, opBinMod, opBitXor, opBitAnd, opBinMult,
    opLParen, opRParen, opBinMinus, opAssign, opBinPlus,
    opLBrack, opRBrack, opLBrace, opRBrace, opRef, opBitOr,
    opStop, opCondThen, opComma, opNumLT, opDeref, opNumGT,
    opBinDiv, opCondIf, opBitNot
  );

  HasCompound		= '!%^&*(-=+|<.>/';
  Compounds		: array[1..31] of TLexCompound = (
    ( Icon: '!=';	Op: opNumNE;	Kind: okNone ),
    ( Icon: '%=';	Op: opAssign;	Kind: eqMod ),
    ( Icon: '^=';	Op: opAssign;	Kind: eqBitXor ),
    ( Icon: '&&';	Op: opLogAnd;	Kind: okNone ),
    ( Icon: '&=';	Op: opAssign;	Kind: eqBitAnd ),
    ( Icon: '&&=';	Op: opAssign;	Kind: eqLogAnd ),
    ( Icon: '**';	Op: opBinExp;	Kind: okNone ),
    ( Icon: '*=';	Op: opAssign;	Kind: eqMult ),
    ( Icon: '**=';	Op: opAssign;	Kind: eqExp ),
    ( Icon: '***';	Op: opComment;	Kind: comDoc ),
    ( Icon: '(*';	Op: opComment;	Kind: comPascal ),
    ( Icon: '--';	Op: opAuto;	Kind: acPostDec ),
    ( Icon: '-=';	Op: opAssign;	Kind: eqMinus ),
//    ( Icon: '->';	Op: opDeref;	Kind: okNone ),
//    ( Icon: '-^';	Op: opRemSp;	Kind: okNone ),
    ( Icon: '==';	Op: opNumEq;	Kind: okNone ),
    ( Icon: '=>';	Op: opKey;	Kind: okNone ),
//    ( Icon: '=~';	Op: opBind;	Kind: okNone ),
    ( Icon: '++';	Op: opAuto;	Kind: acPostInc ),
    ( Icon: '+=';	Op: opAssign;	Kind: eqPlus ),
//    ( Icon: '+^';	Op: opAddSp;	Kind: okNone ),
    ( Icon: '||';	Op: opLogOr;	Kind: okNone ),
    ( Icon: '|=';	Op: opAssign;	Kind: eqBitOr ),
    ( Icon: '||=';	Op: opAssign;	Kind: eqLogOr ),
    ( Icon: '<<';	Op: opShl;	Kind: okNone ),
    ( Icon: '<<=';	Op: opAssign;	Kind: eqShl ),
    ( Icon: '<=';	Op: opNumLE;	Kind: okNone ),
    ( Icon: '<=>';	Op: opNumCmp;	Kind: okNone ),
    ( Icon: '..';	Op: opRange;	Kind: okNone ),
    ( Icon: '>>';	Op: opShr;	Kind: okNone ),
    ( Icon: '>>=';	Op: opAssign;	Kind: eqShr ),
    ( Icon: '>=';	Op: opNumGE;	Kind: okNone ),
    ( Icon: '/=';	Op: opAssign;	Kind: eqDiv ),
    ( Icon: '/*';	Op: opComment;	Kind: comCPP ),
    ( Icon: '//';	Op: opComment;	Kind: okNone )
  );

  NamedOps		: array[1..35] of TLexCompound = (
    ( Icon: 'and';	Op: opLitAnd;	Kind: okNone ),
    ( Icon: 'cmp';	Op: opStrCmp;	Kind: okNone ),
    ( Icon: 'continue';	Op: opNamedOp;	Kind: noContinue ),
    ( Icon: 'do';	Op: opNamedOp;	Kind: noDo ),
    ( Icon: 'else';	Op: opNamedOp;	Kind: noElse ),
    ( Icon: 'elsif';	Op: opNamedOp;	Kind: noElsif ),
    ( Icon: 'et';	Op: opLitPlus;	Kind: okNone ), // DONE 1: ADD: [27r3] Synonyms et, pluss, y
    ( Icon: 'eq';	Op: opStrEq;	Kind: okNone ),
    ( Icon: 'eqi';	Op: opStrEqI;	Kind: okNone ),
    ( Icon: 'for';	Op: opNamedOp;	Kind: noFor ),
    ( Icon: 'ge';	Op: opStrGE;	Kind: okNone ),
    ( Icon: 'gt';	Op: opStrGT;	Kind: okNone ),
    ( Icon: 'if';	Op: opNamedOp;	Kind: noIf ),
    ( Icon: 'in';	Op: opBind;	Kind: okNone ), // DONE 1: CHANGE: [27r3] =~ to in
    ( Icon: 'le';	Op: opStrLE;	Kind: okNone ),
    ( Icon: 'lt';	Op: opStrLT;	Kind: okNone ),
    ( Icon: 'minus';	Op: opLitMinus;	Kind: okNone ),
    ( Icon: 'ne';	Op: opStrNE;	Kind: okNone ),
    ( Icon: 'nei';	Op: opStrNEI;	Kind: okNone ),
    ( Icon: 'not';	Op: opLitNot;	Kind: okNone ),
    ( Icon: 'or';	Op: opLitOr;	Kind: okNone ),
    ( Icon: 'plus';	Op: opLitPlus;	Kind: okNone ),
    ( Icon: 'plussp';	Op: opAddSp;	Kind: okNone ),
    ( Icon: 'q';	Op: opConst;	Kind: quQ ),
    ( Icon: 'qq';	Op: opConst;	Kind: quQQ ),
    ( Icon: 'qw';	Op: opConst;	Kind: quQW ),
    ( Icon: 'qx';	Op: opConst;	Kind: quQX ),
    ( Icon: 'sub';	Op: opSub;	Kind: okNone ),
    ( Icon: 'switch';	Op: opNamedOp;	Kind: noSwitch ),
    ( Icon: 'unless';	Op: opNamedOp;	Kind: noUnless ),
    ( Icon: 'until';	Op: opNamedOp;	Kind: noUntil ),
    ( Icon: 'while';	Op: opNamedOp;	Kind: noWhile ),
    ( Icon: 'x';	Op: opBinRep;	Kind: okNone ),
    ( Icon: 'xor';	Op: opLitXor;	Kind: okNone ),
    ( Icon: 'y';	Op: opAddSp;	Kind: okNone )
  );

function GetCompoundIndex(const S: String): Cardinal;
var
  C			: Cardinal;
begin
  for C := Low(Compounds) to High(Compounds) do
    if Compounds[C].Icon = S then begin
      Result		:= C;
      Exit;
    end;
  Result		:= 0;
end;


{ TLetoScript }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
Compiler

------------------------------------------------------------------------------*)
procedure TLetoScript.Compiler;
var
  Root, Token		: TLetoScriptObj;
begin

  Chr			:= #10;
  BufPos		:= 0;
  BufLine		:= 0;
  PeekPos		:= 0;

  Root			:= TLetoScriptObj.Create(nil, Env, opExpr);
  Root.Items.Capacity	:= 512;
  Token			:= nil;

  try

  try

  { 1st pass: Lexical analysis
    Starting with a raw stream of bytes, convert string data into logical
    tokens (lexemes) and check for basic syntax errors and early warnings.
  }
  if Lexer(Root, opUnknown) <> LS_Success then Exit;

  { 2nd pass: Build parse tree
    A logical tree is constructed from the array of tokens, statement
    structure is checked, and any optimizations (such as folding) take place.
  }
  if Parser(Root.Items, Token) <> LS_Success then Exit;

  { 3rd pass: Evaluation
    The tree is executed, from the bottom up.
    Preprocessing of subs and tokens is done here.
  }
  if Assigned(Token) then
    Token.Evaluate(RootContext);

  except
    on ELetoScriptError do Chr := #0;
    on ELetoScriptExit do  Chr := #0;
    on E:ELetoScriptExitStatus do
      raise ELetoScriptExitStatus.Create(E.Message);
//      Halt( StrToIntDef(E.Message, 0) );
    on E:Exception do
      Env.Output('[EXCEPTION] ' + E.Message, otError);
  end;

  finally
    FreeAndNil(Root);
    if Assigned(Token) then
      FreeAndNil(Token);
  end;

end;

(*------------------------------------------------------------------------------
property Env

------------------------------------------------------------------------------*)
procedure TLetoScript.SetEnv(const Value: TLetoScriptEnv);
begin
  if FOwnEnv then begin
    FOwnEnv		:= False;
    FreeAndNil(FEnv);
  end;
  FEnv			:= Value;
end;

(*------------------------------------------------------------------------------
property Lib

------------------------------------------------------------------------------*)
function TLetoScript.GetLib(Name: String): TLetoScriptLib;
var
  I			: Integer;
begin
  Name			:= Lowercase(Name);
  Result		:= nil;
  for I := Low(Libs) to High(Libs) do
    if Libs[I].Name = Name then begin
      Result		:= Libs[I];
      Break;
    end;
end;

(*------------------------------------------------------------------------------
AddLib

------------------------------------------------------------------------------*)
function TLetoScript.AddLib(const ALib: TLetoScriptLib): TLetoScriptLib;
begin
  SetLength(Libs, Length(Libs)+1);
  Libs[High(Libs)]	:= ALib;
  Result		:= ALib;
end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScript.Create(
  const Con		: TLetoScriptOpCon;
  const AEnv		: TLetoScriptEnv
);
begin
  RootContext		:= Con;

  FOwnEnv		:= not Assigned(AEnv);
  if FOwnEnv then
    FEnv		:= TLetoScriptEnv.Create
  else
    FEnv		:= AEnv;

  { Static, builtin libraries }
  SysLib :=
    AddLib(TLetoScriptLibSys.Create(	'system', FEnv));

  AddLib(TLetoScriptLibMath.Create(	'math', FEnv));
  AddLib(TLetoScriptLibGff.Create(	'gff',  FEnv));
  AddLib(TLetoScriptLibErf.Create(	'erf',  FEnv));
  AddLib(TLetoScriptLibBic.Create(	'bic',  FEnv));
  AddLib(TLetoScriptLibTlk.Create(	'tlk',  FEnv));
  AddLib(TLetoScriptLib2da.Create(	'meta', FEnv));
  AddLib(TLetoScriptLibFpt.Create(	'fpt',  FEnv));
  AddLib(TLetoScriptLibFileSys.Create(	'filesys', FEnv));

  {$IFDEF LETOSCRIPT_SQL}
  AddLib(TLetoScriptLibSql.Create(	'sql', FEnv));
  {$ENDIF}

  BuildFuncArray;
  BuildFuncACL(DefaultACL);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScript.Destroy;
var
  I			: Word;
begin
  if Assigned(Stream) then
    FreeAndNil(Stream);

  if FOwnEnv then
    FreeAndNil(FEnv);

  for I := 0 to High(Libs) do
    Libs[I].Free;
  SetLength(Libs, 0);

  inherited;
end;

(*------------------------------------------------------------------------------
BuildFuncACL

Builds the entire function ACL. ACL should be a space-delimited string
containing only the names of principals to be DENIED, everything else is
(re-)ENABLED. Effectively, every principal in ACL is given to EnableDisable
with Enable set to False, and errors ignored.

Create will pass DefaultACL to this function.

Passing a blank string will enable all modules and all methods.

------------------------------------------------------------------------------*)
procedure TLetoScript.BuildFuncACL(const ACL: String);
var
  List			: TStringList;
  I			: Integer;
begin
  for I := Low(Libs) to High(Libs) do begin
    Libs[I].Enabled	:= True;
    Libs[I].Disabled.Clear;
  end;

  List			:= TStringList.Create;
  List.CommaText	:= ACL;
  for I := 0 to List.Count - 1 do
    EnableDisable(List[I], False);

  FreeAndNil(List);

end;

(*------------------------------------------------------------------------------
EnableDisable

Enables or disables a modules or single method, specified by Name. (Methods
must be qualified, eg system.system).

If the principal cannot be found, the result is an error, whose %s is in
ErrStr.

------------------------------------------------------------------------------*)
function TLetoScript.EnableDisable(
  const Name		: String;
  const Enable		: Boolean
): TLetoScriptError;
var
  P			: Integer;
  Module, Method	: String;
  ALib			: TLetoScriptLib;
begin
  Result		:= LS_Success;

  P			:= Pos('.', Name);

  if P > 0 then begin
    Module		:= Copy(Name, 1, P-1);
    Method		:= Copy(Name, P+1, Length(Name));
    ErrStr		:= Module;
    ALib		:= Lib[Module];
    if not Assigned(ALib) then
      Result		:= Err_LS_NoLib
    else if Enable then begin
      ErrStr		:= Method;
      P			:= ALib.Disabled.IndexOf(Method);
      if P = -1 then
        Result		:= Err_LS_NotInLib
      else
        ALib.Disabled.Delete(P);
    end
    else
      ALib.Disabled.Add(Method);
  end

  else begin
    ErrStr		:= Name;
    ALib		:= Lib[Name];
    if Assigned(ALib) then
      ALib.Enabled	:= Enable
    else
      Result		:= Err_LS_NoLib;
  end;

end;

(*------------------------------------------------------------------------------
Compile

------------------------------------------------------------------------------*)
procedure TLetoScript.Compile(const AStream: TStream);
begin
  Stream		:= AStream;
  Stream.Seek(0, 0);

  Compiler;

  Stream		:= nil;

end;

procedure TLetoScript.Compile(const S: String);
begin
  Stream		:= TStringStream.Create(S);

  Compiler;

  FreeAndNil(Stream);

end;

(*------------------------------------------------------------------------------
Lexer

The Lexer's job is to convert a stream of characters into "lexemes" (tokens).
Tokens represent the "words" in the language, and the Lexer is the "ear" that
is turning sounds into words. It is then the Parser's job to make (grammatical,
structural) sense out of the words.

------------------------------------------------------------------------------*)
function TLetoScript.Lexer(
  const Root		: TLetoScriptObj;
  const Stop		: TLetoScriptOp
): TLetoScriptError;
var
  Token, Prev, Child	: TLetoScriptObj;
  Pretext		: String;
  Func			: TLetoScriptFunc;
  Sub			: TLetoScriptSub;

  procedure _Complain_(const Err: TLetoScriptError; const O: TLetoScriptObj = nil);
  begin
    if Err = LS_Success then Exit;
    Result		:= Err;
    if Assigned(O) then
      O.Complain(Result, O.Text)
    else
      Token.Complain(Result, Token.Text);
  end;

begin

  Result		:= LS_Success;

  if Root.Items.Capacity < 32 then
    Root.Items.Capacity	:= 32;

  Prev			:= TLetoScriptObj.Create(nil, Env, opNull);
  Root.Items.Add(Prev);
  Token			:= nil;

  { Catch up if the caller is Peeking. }
  Peeked;

  while Result = LS_Success do begin

    // DONE 1: BUG: [25] All-whitespace lines (tabs) cause Syntax Error
    while(Chr in Whitespace + [#10]) do begin
      while Chr in Whitespace do GetChr;
      if Chr = #10 then begin
        GetLine;
        if GetChr = #10 then Break; { EOF }
      end;
    end;
    if Chr = #10 then Break; { EOF }

    {
    Pretext		:= '';
    if BufPos > 3 then
      Pretext		:= Copy(Buf, BufPos-3, 2)
    else if BufPos > 2 then
      Pretext		:= Buf[BufPos-2];
    }

    Token		:= TLetoScriptObj.Create(nil, Env, opUnknown);
    Token.Line		:= BufLine;
    Token.LinePos	:= BufPos-1;

    Result		:= GetToken(Token, Prev);

    //Token.Location	:= Pretext + String(Token.Value) + Copy(Buf, BufPos-1, 2);

    if Result <> LS_Success then begin
      Token.Text	:= Text;
      Token.Complain(Result);
      Break;
    end;

    { Handle some individual cases. }

    { >> Handle the special syntax of SUB }
    if Prev.Op = opSub then begin

      Sub		:= TLetoScriptSub(Prev.Func);

      case Token.Op of

        // NAME
        opConst:
        begin
          if (Sub.Name <> '') or (Token.Kind <> idLabel) then begin
            _Complain_(Err_LS_BadSub, Prev);
            Break;
          end;
          Sub.Name	:= Text;
          FreeAndNil(Token);
        end;

        // ( ARGS )
        opLParen:
        begin
          if Assigned(Sub.Param) or Sub.EmptyParam then
            _Complain_(Err_LS_BadSub, Prev)
          else begin
            Result	:= Lexer(Token, opRParen);
          end;
          if Result <> LS_Success then Break;
          Result	:= Parser(Token.Items, Child);
          if Result <> LS_Success then Break;
          FreeAndNil(Token);
          if Assigned(Child) then
            Sub.Param	:= Child
          else
            Sub.EmptyParam := True;
          Child		:= nil;
        end;

        // [ ATTRIBS ]
        opLBrack:
        begin
          if Assigned(Sub.Attrib) or Sub.EmptyAttrib then
            _Complain_(Err_LS_BadSub, Prev)
          else begin
            Result	:= Lexer(Token, opRBrack);
          end;
          if Result <> LS_Success then Break;
          Result	:= Parser(Token.Items, Child);
          if Result <> LS_Success then Break;
          FreeAndNil(Token);
          if Assigned(Child) then
            Sub.Attrib	:= Child
          else
            Sub.EmptyAttrib := True;
          Child		:= nil;
        end;

        // { CODE }
        opLBrace:
        begin
          Result	:= Lexer(Token, opRBrace);
          if Result <> LS_Success then Break;
          { Ensure Code is not a Func, which screws up Scope. }
          Token.Items.Add( TLetoScriptObj.Create(nil, Env, opStop) );
          Result	:= Parser(Token.Items, Child);
          if Result <> LS_Success then Break;
          FreeAndNil(Token);
          Sub.Code	:= Child;
          Child		:= nil;
          Result	:= Sub.Define;
          if Result <> LS_Success then begin
            Prev.Complain(Result);
            Break;
          end;
          Root.Items.Delete(Root.Items.Count-1); { The sub is in Env.Subs }
          Token		:= TLetoScriptObj.Create(nil, Env, opStop);
          Root.Items.Add(Token);
          Prev		:= Token;
        end;

        opStop:
        begin
          // TODO 4: CHANGE: Predeclaration must match definition, to enforce code maintenance.
          if Sub.Name = '' then begin
            _Complain_(Err_LS_BadSub, Prev);
            Break;
          end;
          if Env.Subs.IndexOf(' ' + Sub.Name) = -1 then
            Env.Subs.AddObject(' ' + Sub.Name, Sub);
          Root.Items.Delete(Root.Items.Count-1);
          Root.Items.Add(Token);
          Prev		:= Token;
        end;

      else
        _Complain_(Err_LS_BadSub, Prev);

      end;

      Continue;

    end; { subs }

    { >> Other individual cases. }
    case Token.Op of

      opInvalid:
        _Complain_(Err_LS_UnknownOp);

      opComment:
      begin
        case Token.Kind of
          { Perl-style # and C-style // }
          okNone:
            Chr		:= #10;
          { C++ style /* */ }
          comCPP:
            if not SeekStr('*/') then
              _Complain_(Err_LS_Unterminated);
          { Pascal-style (* *) }
          comPascal:
            if not SeekStr('*)') then
              _Complain_(Err_LS_Unterminated);
          { *** Document comment }
          comDoc:
            if BufPos <> 5 then
              _Complain_(Err_LS_NoComDocHere)
            else if not SeekLine(Buf) then
              _Complain_(Err_LS_Unterminated);
        end;
        if Result <> LS_Success then Break;
        //else if Chr <> #10 then GetChr;
        FreeAndNil(Token);
        Continue;
      end;

      opLParen:
      begin
        Token.Op	:= opExpr;
        Result		:= Lexer(Token, opRParen);
        if Result <> LS_Success then Break;
        if Prev.WantsExpr then begin
          if Prev.Op in [opUnaryOp, opListOp] then
            Prev.Op	:= opFunc;
          Result	:= Parser(Token.Items, Child);
          FreeAndNil(Token);
          Prev.Expr	:= Child;
          Child		:= nil;
          Continue;
        end;
      end;

      opLBrace:
      begin
        Token.Op	:= opBlock;
        Result		:= Lexer(Token, opRBrace);
        if Result = LS_Success then
          Result	:= Parser(Token.Items, Child);
        if Result <> LS_Success then Break;
        // DONE 1: BUG: [27r3] Empty block is valid
        if not Assigned(Child) then begin
          Child		:= TLetoScriptObj.Create(nil, Env, opStop);
          Child.Items.Add( TLetoScriptObj.Create(nil, Env, opStop) );
        end;
        if Prev.WantsBlock then begin
          FreeAndNil(Token);
          Prev.Block	:= Child;
          Child		:= nil;
          { SALT: LOOP BLOCK is a whole statement without stop }
          if Prev.Op = opNamedOp then begin
            Token	:= TLetoScriptObj.Create(nil, Env, opStop);
            Root.Items.Add(Token);
            Prev	:= Token;
          end;
          Continue;
        end else
          Token.Left	:= Child;
        Child		:= nil;
      end;

      opLBrack:
      begin
        if
          (Prev.Op   in [opSymbol, opExpr]) or
          (Prev.Kind in [idAttrib])
        then
          Token.Op	:= opExpr
        else
          Token.Op	:= opSet;
        Result		:= Lexer(Token, opRBrack);
        if Result <> LS_Success then Break;
        if Token.Op = opExpr then begin
          Result	:= Parser(Token.Items, Child);
          if (Result = LS_Success) and not Assigned(Child) then
            _Complain_(Err_LS_Syntax);
          FreeAndNil(Token);
          Prev.Expr	:= Child;
          Child		:= nil;
          Continue;
        end;
      end;

      opSub:
      begin
        Sub		:= TLetoScriptSub.Create('', SysLib, Env);
        Sub.Token	:= Token;
        Token.Func	:= Sub;
      end;

      opSymbol:
        { SALT: for $value (@list) { ... }
        if (Token.Kind = symScalar) and (Prev.Kind in [noFor]) then begin
          Root.Items.Add(Token);
          Continue;
        end;

      opDeref:
      begin
        Token.Text	:= Prev.Text;
        if Prev.Kind = idBare then begin
          Prev.Op	:= opDeref;
          Prev.Kind	:= okNone;
          Token.Free;
          Continue;
        end else if Prev.Op = opSymbol then
          Token.Op	:= opAttrib
        else
          _Complain_(Err_LS_BadDeref);
      end;

      opAuto:
        if not(Prev.Op in Terms) then
          case Token.Kind of
            acPostInc: Token.Kind := acPreInc;
            acPostDec: Token.Kind := acPreDec;
          end;

      { SALT: Braid ? ? : : }
      opCondIf:
      begin
        Result		:= Lexer(Token, opCondThen);
        if Result <> LS_Success then Break;
        Result		:= Parser(Token.Items, Child);
        Token.Expr	:= Child;
        Child		:= nil;
      end;

      opNamedOp:
      begin
        Func		:= SysLib.CreateFunc('');
        Func.Token	:= Token;
        Func.Fn		:= Word(fnNamed);
        Token.Func	:= Func;
      end;

    else
      if Token.Op = Stop then
        Break;

    end;

    { SALT: Safewords such as -Delete }
    if (Token.Kind = idBare) and (Prev.Op = opUnaMinus) then begin
      Prev.Op		:= opConst;
      Prev.Kind		:= idSafe;
      Prev.Text		:= Token.Text;
      Prev.Value	:= Token.Text;
      Prev.Line		:= Token.Line;
      Prev.LinePos	:= Token.LinePos;
      Token.Free;
      Continue;
    end;

    if Token.Kind in [quQ, quQQ, quQW, quQX] then begin
      PeekPos		:= 0;
      while PeekChr in Whitespace do Peek;
      case PeekChr of
        '(':
          Text		:= ')';
        '{':
          Text		:= '}';
        '[':
          Text		:= ']';
        '<':
          Text		:= '>';
      else
        Text		:= PeekChr;
      end;
      Peeked;
      if Token.Kind = quQW then begin
        // TODO 4: ADD: QW
        Result		:= Err_LS_NYI;
      end else
        Result		:= GetQuoted(Token, Prev);
      if Result <> LS_Success then
        Token.Complain(Result);
    end;

    // DONE 1: BUG: [27r3] print if (false) || (true)
    { SALT: print if (false) || (true) }
    if (Token.Kind in [noIf, noUnless]) and not(Prev.Op in [opNull, opStop]) then begin
      Prev		:= Token;
      Token		:= TLetoScriptObj.Create(nil, Env, opUnknown);
      Result		:= Lexer(Token, opStop);
      if Result <> LS_Success then Break;
      Result		:= Parser(Token.Items, Child);
      FreeAndNil(Token);
      Prev.Right	:= Child;
      Child		:= nil;
      Root.Items.Add(Prev);
      Token		:= TLetoScriptObj.Create(nil, Env, opStop);
      Root.Items.Add(Token);
      Prev		:= Token;
      Continue;
    end;

    // temp hack: disguise derefs for correct Parser'ing
    if Prev.Op = opDeref then begin
      Prev.Op		:= Token.Op;
      Prev.Kind		:= Token.Kind;
      Prev.Value	:= Token.Value;
      Prev.Func		:= Token.Func;
      //Prev.Fn		:= Token.Fn;
      if Assigned(Token.Func) then
        TLetoScriptFunc(Token.Func).Token := Prev;
      Token.Func	:= nil;
      Token.Free;
      Continue;
    end;

    if Result <> LS_Success then Break;

    Root.Items.Add(Token);
    Prev		:= Token;

  end; { while Success }

  PeekPos		:= 0;

  if (Result <> LS_Success) and Assigned(Token) then
    FreeAndNil(Token);
  if (Result <> LS_Success) and Assigned(Child) then
    FreeAndNil(Child);

  if (Result <> LS_Success) or (Stop in [opUnknown, opStop]) then
  else if Assigned(Token) and (Token.Op = Stop) then
    FreeAndNil(Token)
  else begin
    Result		:= Err_LS_Unterminated;
    Root.Complain(Result);
  end;

  { Free the very first 'Prev' holder }
  TLetoScriptObj(Root.Items[0]).Free;
  Root.Items.Delete(0);

end;

(*------------------------------------------------------------------------------
Parser

The Parser's job is to turn a list (array) of Tokens into a tree. Specifically,
an "Ordered Associative Tree" (OAT). A custom algorithm is used for this,
with a few resemblances to a heap sort.

Root is assigned as the root of the tree. All parsed Tokens from the List
are removed (but not Free'd, because they're in Root), but a semi-parsed
List may result if there is a syntax error. In any case, no Token will exist
in both List and Root.

Exercises:

-2**-4 (-0.0625)
-2**~4294967294 (2)
1 ? 1 ? 'foo' : 'bar' : 'baz'
print 1, 2, print 'foo', 'bar'

------------------------------------------------------------------------------*)
function TLetoScript.Parser(
  const List		: TList;
  var Root		: TLetoScriptObj
): TLetoScriptError;
var
  Token, Node, Child	: TLetoScriptObj;
  Token_Prec		: Byte;

  procedure _ADOPT_;
  begin
    Node.Right		:= Token;
    Node		:= Token;
  end;

  procedure _DEMOTE_;
  begin
    if Assigned(Node.Parent) then
      Node.Parent.Right	:= Token;
    Token.Left		:= Node;
    Node		:= Token;
  end;

  procedure _DISPLACE_;
  begin
    Token.Left		:= Node.Right;
    Node.Right		:= Token;
    Node		:= Token;
  end;

begin
  Result		:= LS_Success;
  Root			:= nil;

  if not Assigned(List) or (List.Count = 0) then Exit;

  while List.Count > 0 do begin

    Token		:= List[0];
    List.Delete(0);

    if not Assigned(Root) then begin
      Root		:= Token;
      Node		:= Token;
    end;

    { Certain operators have dependent precedence.
      From perldoc perlop:
      << In the absence of parentheses, the precedence of list operators such
      as "print", "sort", or "chmod" is either very high or very low depending
      on whether you are looking at the left side or the right side of the
      operator. >>
      Unary ops appear to behave the same (because it should be treated as
      a TERM for the purpose of parsing).
      Token.Prec represents "from the left", and Node.Prec "from the right".
      Token_Prec is used to arbitrate this discrepancy.
    }
    if Token.Op in OpPrecLeftRight then
      Token_Prec	:= High(Precedence)
    else
      Token_Prec	:= Token.Prec;

    { Sort. }

    while True do begin
      if Token = Root then { Root }
        Break
      else if Token_Prec > Node.Prec then begin
        _ADOPT_;
        Break;
      end
      else while Token_Prec < Node.Prec do
        { Who gets demoted? }
        if Assigned(Node.Parent) and (Token_Prec < Node.Parent.Prec) then
          Node		:= Node.Parent
        else
          Break;
      if Token_Prec < Node.Prec then
        _DEMOTE_
      else if Token_Prec > Node.Prec then
        _DISPLACE_
      else if Token.Op in AssocLeft then
        _DEMOTE_
      else if Token.Op in AssocRight then
        _DISPLACE_
      else if not Assigned(Node.Right) then
        _ADOPT_
      else
        Token.Exception(Err_LS_InvalidAssoc);
      Break;
    end;

    { Sub-parse Blocks, Exprs, etc. }

    case Token.Op of
      // dangling block?
      opBlock: ;
      opExpr, opSet:
      begin
        Result		:= Parser(Token.Items, Child);
        Token.Left	:= Child;
      end;
    end;

    if Assigned(Token.Block) and Assigned(Token.Block.Items) then begin
      Result		:= Parser(Token.Block.Items, Child);
      Token.Block.Free;
      Token.Block	:= Child;
    end;

    if Assigned(Token.Expr) and Assigned(Token.Expr.Items) then begin
      Result		:= Parser(Token.Expr.Items, Child);
      Token.Expr.Free;
      Token.Expr	:= Child;
    end;

    if Result <> LS_Success then Break;

  end; { while List.Count }

  { Retrace Root }
  while Assigned(Root.Parent) do
    Root		:= Root.Parent;

  if Result = LS_Success then
    Root.Preprocess;

end;

(*------------------------------------------------------------------------------
GetLine

------------------------------------------------------------------------------*)
procedure TLetoScript.GetLine;
begin

  { Read until next newline or EOF. Skip blank lines.
    Newline is not system-dependent, because the script may have
    come from a different OS. Newline is considered #10, and #13s
    are always discarded (anywhere).
  }
  Buf			:= '';
  while Stream.Position < Stream.Size do begin
    Stream.Read(Chr, 1);
    if Chr = #13 then
      Continue
    else if Chr in [#10, #4, #26] then
      Break
    else
      Buf		:= Buf + Chr;
  end;

  { Manual EOF. ^D ^Z }
  if Chr in [#4, #26] then
    Stream.Position	:= Stream.Size
  { Skip blank lines. }
  else if (Buf = '') and (Stream.Position < Stream.Size) then
    GetLine;

  Inc(BufLine);
  BufPos		:= 1;
  BufSize		:= Length(Buf);

end;

(*------------------------------------------------------------------------------
GetChr

Get the next char in Buf (advances position).

------------------------------------------------------------------------------*)
function TLetoScript.GetChr(const Count: Cardinal): Char;
var
  I			: Integer;
begin
  Result		:= Chr;

  for I := 1 to Count do begin
    if BufPos > BufSize then
      Result		:= #10
    else
      Result		:= Buf[BufPos];
    Inc(BufPos);
  end;

  Chr			:= Result;
  PeekPos		:= 0;

end;

(*------------------------------------------------------------------------------
Peek

Look ahead at the next char, without advancing the current pointer. This
can be called successively to simulate a Seek, PeekPos keeps track of how
far head to look. PeekPos is reset after GetChr. It is not possible to Peek
past the end of the line, and #10 will be returned in this case.

------------------------------------------------------------------------------*)
function TLetoScript.Peek: Char;
begin
  if BufPos + PeekPos > BufSize then
    Result		:= #10
  else
    Result		:= Buf[BufPos + PeekPos];

  Inc(PeekPos);
  PeekChr		:= Result;

end;

(*------------------------------------------------------------------------------
Peeked

Peek has been used to terminate the token. Now BufPos needs to "catch up",
so forward it to where Peek is currently looking.

------------------------------------------------------------------------------*)
procedure TLetoScript.Peeked;
begin
  GetChr(PeekPos);
end;

(*------------------------------------------------------------------------------
SeekStr

------------------------------------------------------------------------------*)
function TLetoScript.SeekStr(const Substr: String): Boolean;
var
  P			: Cardinal;
  S			: String;
begin
  PeekPos		:= 0;
  Sought		:= '';

  repeat
    P			:= PosEx(Substr, Buf, BufPos);
    if P = 0 then begin
      Sought		:= Sought + Copy(Buf, BufPos, BufSize);
      GetLine;
    end else begin
      S			:= Copy(Buf, BufPos, P-BufPos);
      Sought		:= Sought + S;
      GetChr( Length(S) + Length(Substr) + 1 );
    end;
  until (P > 0) or (Buf = '');

  Result		:= Buf <> '';

end;

(*------------------------------------------------------------------------------
SeekLine

------------------------------------------------------------------------------*)
function TLetoScript.SeekLine(S: String): Boolean;
begin
  { Very weird bug:
    SeekLine wasn't working when defined with const S. Apparently, when
    Buf was passed in as S, and then Buf consequently changed in the
    loop below, *so did S*. So the const was taken off, and now it
    works. This is a known symptom of optimization / refcounting in
    Delphi. Google "const interface bug".
  }
  repeat
    GetLine
  until (Buf = S) or (Buf = '');

  Result		:= Buf <> '';
end;

(*------------------------------------------------------------------------------
EscapedChr

Read ahead to produce an escaped character. Previously used by GetQuoted, now
exposed generically. Upon invalid syntax, the Result will be blank.

From perlop:
    \t		tab             (HT, TAB)
    \n		newline         (NL)
    \r		return          (CR)
    \f		form feed       (FF)
    \b		backspace       (BS)
    \a		alarm (bell)    (BEL)
    \e		escape          (ESC)
    \033	octal char	(ESC)
    \x1b	hex char	(ESC)
    \x{263a}	wide hex char	(SMILEY)
    \c[		control char    (ESC)
    \N{name}	named Unicode character

------------------------------------------------------------------------------*)
function TLetoScript.EscapedChr: String;
var
  S			: String;
  I			: Integer;
  O			: Cardinal;
begin

  Result		:= GetChr;

  if Result = #10 then Exit;

  // TODO 4: ADD: Expanded escaped char support

  { Single-character escape sequences }
  case Chr of

    { Metacharacters }
    'a': Result		:= #7;
    'b': Result		:= #8;
    'e': Result		:= #27;
    'f': Result		:= #12;
    'n': Result		:= sLineBreak;
    'r': Result		:= #13;
    't': Result		:= #9;
    'v': Result		:= #11;

    { Hex }
    'x':
    begin
      S			:= 'x' + GetChr + GetChr;
      if (Chr <> #10) and TryStrToInt(S, I) and (I > -1) and (I < 256) then
        Result		:= Char(I)
      else
        Result		:= '';
    end;

    { Octal }
    '0'..'7':
    begin
      S			:= Chr;
      for I := 1 to 2 do
        if GetChr in ['0'..'7'] then
          S		:= S + Chr
        else
          S		:= S + '0';
      if TryStrToOct(S, O) and (O < 256) then
        Result		:= Char(O)
      else
        Result		:= '';
    end;

    // x{

    //'c': ;

    //'N': ;

  end;

  { If none of the above cases are met, a "convenience remittal" should occur
    by default. }

end;

(*------------------------------------------------------------------------------
GetToken

------------------------------------------------------------------------------*)
function TLetoScript.GetToken(const Token, Prev: TLetoScriptObj): TLetoScriptError;
var
  ExpectOp		: Boolean;
  TokenPos		: Cardinal;
begin

  //Result		:= Success;

  PeekPos		:= 0;

  // TODO 4: CHANGE: Warn for ambiguous unary operator use; e.g. rand - rand (see Perl)
  ExpectOp :=
    (Prev.Op in Terms) or
    (Prev.Kind in [acPostInc, acPostDec]);

  TokenPos		:= BufPos;

  case Chr of

    'a'..'z', 'A'..'Z', '_':
      if ExpectOp then
        Result		:= GetOp(Token, Prev)
      else
        Result		:= GetNamed(Token, Prev);

    '0':
    begin
      Token.Op		:= opConst;
      case Peek of
        'b', 'B':
          Token.Kind	:= numBin;
        'x', 'X':
          Token.Kind	:= numHex;
        '0'..'9':
          Token.Kind	:= numOct;
      else
        Token.Kind	:= numInt;
      end;
      Text		:= '0';
      if Token.Kind <> numInt then
        Text		:= Text + GetChr;
      Result		:= GetConst(Token, Prev);
    end;

    '1'..'9':
    begin
      Token.Kind	:= numInt;
      Text		:= Chr;
      Result		:= GetConst(Token, Prev);
    end;

    '.':
      if Peek in ['0'..'9'] then begin
        Token.Kind	:= numReal;
        Text		:= Chr;
        Result		:= GetConst(Token, Prev);
      end else
        Result		:= GetOp(Token, Prev);

    '+':
    begin
      Result		:= GetOp(Token, Prev);
      if (Token.Op = opBinPlus) and not ExpectOp then
        Token.Op	:= opUnaPlus;
    end;
    '-':
    begin
      Result		:= GetOp(Token, Prev);
      if (Token.Op = opBinMinus) and not ExpectOp then
        Token.Op	:= opUnaMinus;
    end;

    '/':
      if ExpectOp or (Peek in ['/', '*']) then
        Result		:= GetOp(Token, Prev)
      else
        Result		:= GetSymbol(Token, Prev);

    '$', '@':
      Result		:= GetSymbol(Token, Prev);

    '%': // reserved: *typeglob &func
      if ExpectOp then
        Result		:= GetOp(Token, Prev)
      else
        Result		:= GetSymbol(Token, Prev);

    '''', '"', '`', '<':
      if (Chr = '<') and ExpectOp then
        Result		:= GetOp(Token, Prev)
      else begin
        case Chr of
          '''':
            Token.Kind	:= quSingle;
          '"':
            Token.Kind	:= quDouble;
          '`':
            Token.Kind	:= quCmd;
          '<':
            Token.Kind	:= quPrintn;
        end;
        Text		:= Chr;
        Result		:= GetQuoted(Token, Prev);
      end;

  else
    Result		:= GetOp(Token, Prev);

  end;

  { Catch up now if not caught up already. }
  if BufPos = TokenPos then
    GetChr( Length(Text)+1 );

end;

(*------------------------------------------------------------------------------
GetOp

------------------------------------------------------------------------------*)
function TLetoScript.GetOp(const Token, Prev: TLetoScriptObj): TLetoScriptError;
type
  _OpType_		= (otChr, otCmp, otNam);
var
  SI, CI, I		: Integer;
  S			: String;

  // yuck!
  procedure _Set_(const Index: Cardinal; const OT: _OpType_);
  begin
    if Index = 0 then begin
      Result		:= Err_LS_Syntax;
      Exit;
    end;
    case OT of
      otChr:
      begin
        Token.Op	:= Symbolics[Index];
        //Token.Text	:= Chr;
      end;
      otCmp:
      begin
        Token.Op	:= Compounds[Index].Op;
        Token.Kind	:= Compounds[Index].Kind;
      end;
      otNam:
      begin
        Token.Op	:= NamedOps[Index].Op;
        Token.Kind	:= NamedOps[Index].Kind;
      end;
    end;
  end;

begin

  Result		:= LS_Success;

  PeekPos		:= 0;

  Text			:= Chr;

  SI			:= 0;
  CI			:= 0;

  if Chr in Alpha then begin
    while Peek in Alpha do
      Text		:= Text + PeekChr;
    S			:= Lowercase(Text);
    for I := Low(NamedOps) to High(NamedOps) do
      if NamedOps[I].Icon = S then begin
        CI		:= -1;
        SI		:= I;
        Break;
      end;
  end else begin
    SI			:= Pos(Lowercase(Chr), SymbolicIndex);
    CI			:= Pos(Lowercase(Chr), HasCompound);
  end;

  if CI = -1 then
    _Set_(SI, otNam)
  else if CI > 0 then begin
    I			:= 0;
    repeat
      CI		:= GetCompoundIndex(Lowercase(Text + Peek));
      if CI > 0 then begin
        Text		:= Text + PeekChr;
        I		:= CI;
      end;
    until CI = 0;
    if I = 0 then
      _Set_(SI, otChr)
    else
      _Set_(I, otCmp);
  end else
    _Set_(SI, otChr);

  { Catch up. }
  GetChr(Length(Text));

end;

(*------------------------------------------------------------------------------
GetNamed

The token uses an alphanumeric name. It could be a label, an attribute, a
named operator, or a function.

------------------------------------------------------------------------------*)
function TLetoScript.GetNamed(const Token, Prev: TLetoScriptObj): TLetoScriptError;
var
  P			: Integer;
  N			: String;
  LegalChars		: CharSet;
begin
  Result		:= LS_Success;

  Text			:= Chr;

  PeekPos		:= 0;

  if Prev.Op = opUnaMinus then
    LegalChars		:= ['a'..'z', 'A'..'Z', '0'..'9', '_']
  else
    LegalChars		:= ['a'..'z', 'A'..'Z', '0'..'9', '_', ':'];

  { Terminate }
  while Peek in LegalChars do
    Text		:= Text + PeekChr;

  { Catch up }
  Peeked;

  { Qualify }

  Token.Value		:= Text;
  Token.Text		:= Text;
  N			:= Lowercase(Text);

  // DONE 1: ADD: [27] Unary minus options (Safewords): -Delete
  { Safeword }
  if Prev.Op = opUnaMinus then begin
    Token.Op		:= opConst;
    Token.Kind		:= idBare;
    Exit;
  end;

  { Labels }
  P			:= Pos(':', Text);
  if P > 0 then begin
    Token.Op		:= opConst;
    Token.Kind		:= idLabel;
    if (P < Length(Text)) then
      Result		:= Err_LS_Syntax
    else
      SetLength(Text, Length(Text)-1);
    Exit;
  end;

  { Attribs }
  if Prev.Op = opAttrib then begin
    Token.Op		:= opConst;
    Token.Kind		:= idAttrib;
    Exit;
  end;

  { sub NAME }
  if Prev.Op = opSub then begin
    Token.Op		:= opConst;
    Token.Kind		:= idLabel;
    Exit;
  end;

  { Try as a named operator }
  for P := Low(NamedOps) to High(NamedOps) do
    if N = NamedOps[P].Icon then begin
      Token.Op		:= NamedOps[P].Op;
      Token.Kind	:= NamedOps[P].Kind;
      Exit;
    end;

  { Finally, try as a function of some sort }
  Result		:= GetFunc(Token, Prev);

end;

(*------------------------------------------------------------------------------
GetFunc

The only other thing this token could be is a function... or a bareword.
Here's how we determine which:

"If it looks like a function, it is a function." If the next character is
left-paren or comma, e.g.:

  foo (
  foo,

Then this word is treated as a function. If its name is qualified (system.foo)
then we bypass subroutine checking, otherwise the subs list is checked first,
as subs get higher precedence than internal functions.

In the case of "foo (", no match at this point produces an error.

In the case of "foo," no match at this point indicates a bareword.

In all other cases, there may be no arg list or the arg list may be presented
without parens. For a sub to work in this fashion it must be predeclared, so
check for a match on any sub predeclared until now. If there's no match, try
to match it to a func.

If there's still no match, it's a bareword.

  - Dereferencing mungs the whole setup, so at present it is accomodate using
    workarounds.

------------------------------------------------------------------------------*)
function TLetoScript.GetFunc(const Token, Prev: TLetoScriptObj): TLetoScriptError;
var
  P, Found		: Integer;
  FC			: TFuncCode;
  C			: Int64;
  E			: Extended;
  L, N, S		: String;
  Sub			: TLetoScriptSub;
  Func			: TLetoScriptFunc;
begin
  Result		:= LS_Success;

  N			:= Lowercase(Text);

  while PeekChr in Whitespace do Peek;
  if PeekChr in ['(', ','] then
    Token.Op		:= opFunc;

  { Try as a sub }
  if Prev.Op <> opDeref then begin
    P			:= Env.Subs.IndexOf(N);
    if (P = -1) and (Token.Op = opUnknown) then
      P			:= Env.Subs.IndexOf(' ' + N);
    if P > -1 then begin
      Func		:= SysLib.CreateFunc('');
      Func.Fn		:= Word(fnSub);
      Func.Token	:= Token;
      Token.Func	:= Func;
      Sub		:= TLetoScriptSub( Env.Subs.Objects[P] );
      if Token.Op <> opFunc then
        Token.Op	:= Sub.Op;
      Exit;
    end;
  end;

  if Prev.Op = opDeref then begin
    // TODO 3: BUG: Deref kludge, should actually check against references
    Text		:= N;
    L			:= Lowercase(Prev.Text);
  end else
    L			:= '*';

  { Try as a function }
  if not TryGetLSFunction(L, N, Found) then begin
    if L <> '*' then begin
      Token.EStr1	:= Text;
      Token.EStr2	:= L;
      Result		:= Err_LS_NotInLib;
    end else if PeekChr = '(' then
      Result		:= Err_LS_UndefSub;
    { ... otherwise, it's just a bareword: }
    Token.Op		:= opConst;
    Token.Kind		:= idBare;
    Exit;
  end;

  FC			:= LS_Functions[Found];
  S			:= FC.Code;
  { Constant }
  if FC.Fn = -1 then begin
    Token.Op		:= opConst;
    if TryStrToInt64(S, C) then begin
      Token.Kind	:= numInt;
      Token.Value	:= C;
    end else if TryStrToFloat(S, E) then begin
      Token.Kind	:= numReal;
      Token.Value	:= E;
    end else begin
      Token.Kind	:= quSingle;
      Token.Value	:= S;
    end;
    Exit;
  end;

  { Function }
  L			:= FC.Name;
  L			:= Copy(L, 1, Pos('.', L)-1);
  Func			:= nil;
  for P := 0 to High(Libs) do
    if Libs[P].Name = L then begin
      Func		:= Libs[P].CreateFunc(S);
      Break;
    end;

  if not Assigned(Func) then begin
    Result		:= Err_LS_BadDeref;
    Token.EStr1		:= Token.Text;
    Exit;
  end;

  Func.Name		:= N;
  Func.Token		:= Token;
  Func.Fn		:= FC.Fn;
  Token.Func		:= Func;
  if Token.Op <> opFunc then
    Token.Op		:= Func.Op;

end;

(*------------------------------------------------------------------------------
GetConst

12345
12345.67
.23E-10             # a very small number
3.14_15_92          # a very important number
4_294_967_296       # underscore for legibility
0xff                # hex
0xdead_beef         # more hex
0377                # octal
0b011011            # binary

------------------------------------------------------------------------------*)
function TLetoScript.GetConst(const Token, Prev: TLetoScriptObj): TLetoScriptError;
var
  S			: String;
begin
  Result		:= LS_Success;

  Token.Op		:= opConst;
  S			:= Text;

  PeekPos		:= 0;

  while
    (Result = LS_Success) and
    (Peek in ['_', '0'..'9', '.', 'a'..'f', 'A'..'F'])
  do begin
    case PeekChr of

      '0'..'9', 'a'..'d', 'A'..'D':
        if
          (
            ( (Token.Kind = numBin) and not(PeekChr in ['0'..'1']) ) or
            ( (Token.Kind = numOct) and not(PeekChr in ['0'..'7']) ) or
            ( (Token.Kind <> numHex) and (PeekChr in ['a'..'d', 'A'..'D']) )
          )
        then
          Result	:= Err_LS_InvalidNum;

      'e', 'E':
        if Token.Kind in [numInt, numReal] then begin
          Token.Kind	:= numSci;
          if
            (BufPos + PeekPos <= BufSize) and
            (Buf[BufPos + PeekPos] in ['+', '-'])
          then begin
            S		:= S + PeekChr;
            Peek;
          end;
        end else if Token.Kind <> numHex then
          Result	:= Err_LS_InvalidNum;

      'f', 'F':
        if Token.Kind <> numHex then
          Result	:= Err_LS_InvalidNum;

      '.':
        if Token.Kind <> numInt then
          Result	:= Err_LS_InvalidNum
        else
          Token.Kind	:= numReal;

    end;

    S			:= S + PeekChr;

  end;

  if Result <> LS_Success then Exit;

  Peeked;

  S			:= StringReplace(S, '_', '', [rfReplaceAll]);

  while (Length(S) > 0) and (S[1] = '0') do
    System.Delete(S, 1, 1);

  if S = '' then begin
    Token.Value		:= 0;
    Token.Text		:= '0';
  end else begin
    Token.Text		:= S;
    // TODO 2: ADD: Big numbers
    try
      case Token.Kind of
        numInt:
          Token.Value	:= StrToInt64(S);
        numReal, numSci:
          {$IFDEF MSWINDOWS}
          Token.Value	:= StrToFloat(S, Env.FormatSettings);
          {$ENDIF}
          {$IFDEF LINUX}
          Token.Value	:= StrToFloat(S);
          {$ENDIF}
        numBin:
          Token.Value	:= StrToBin(StringReplace(S, 'b', '', [rfIgnoreCase]));
        numHex:
          Token.Value	:= StrToInt64('0' + S);
        numOct:
          Token.Value	:= StrToOct(S);
      end;
    except
      Token.Value	:= Infinity;
      //Result		:= Err_LS_BigNumber;
    end;
  end;

end;

(*------------------------------------------------------------------------------
GetSymbol

------------------------------------------------------------------------------*)
function TLetoScript.GetSymbol(const Token, Prev: TLetoScriptObj): TLetoScriptError;
const
  Alphanum		= Alpha + Numer;
  SYM_VAR		= 0;
  SYM_FIELD		= 1;
  SYM_FILE		= 2;
  LegalFirsts		: array[SYM_VAR..SYM_FILE] of CharSet = (
    (Alphanum + ['_', '/', '$', '~', '!', '#']),
    (Alpha + ['_', '[', ']', '~', '^', '@', '$']),
    (Alpha + ['_', '~'])
  );
  LegalNames		: array[SYM_VAR..SYM_FILE] of CharSet = (
    (Alphanum + ['_']),
    (Alphanum + ['_', '[', ']', '/', '?']), // no more /Field#
    (Alphanum + ['_'])
  );
var
  Block, Child		: TLetoScriptObj;
  LegalFirst, LegalName	: CharSet;
  Name			: String;

  procedure _Define_(const Kind: TLetoScriptOpKind; const Index: Integer);
  begin
    Token.Kind		:= Kind;
    LegalFirst		:= LegalFirsts[Index];
    LegalName		:= LegalNames[Index];
  end;

begin
  Result		:= LS_Success;

  Token.Op		:= opSymbol;

  case Chr of
    '$': _Define_(symScalar, SYM_VAR);
    '@': _Define_(symList,   SYM_VAR);
    '/': _Define_(symField,  SYM_FIELD);
    '%': _Define_(symFile,   SYM_FILE);
  end;

  PeekPos		:= 0;

  while Peek in Whitespace do ;

  if PeekChr = '{' then begin
    Token.Value		:= '{BLOCK}';
    Token.Text		:= Chr + '{BLOCK}';
    Peeked;
    Block		:= TLetoScriptObj.Create(nil, Env, opBlock);
    Block.Line		:= BufLine;
    Block.LinePos	:= BufPos-1;
    GetChr;
    Result		:= Lexer(Block, opRBrace);
    if Result = LS_Success then begin
      Result		:= Parser(Block.Items, Child);
      Block.Free;
      Token.Block	:= Child;
    end;
    Exit;
  end

  else if PeekChr in LegalFirst then begin
    Name		:= PeekChr;
    if PeekChr in Numer then
      while Peek in Numer do
        Name		:= Name + PeekChr
    else
      while Peek in LegalName do
        Name		:= Name + PeekChr;
  end

  else
    Result		:= Err_LS_InvalidName;

  Token.Value		:= Name;
  Token.Text		:= Chr + Name;

  Peeked;

end;

(*------------------------------------------------------------------------------
GetFile

Filehandle names must begin with an alpha or underscore, and may contain any
alphanumeric or underscore. As with variables, filehandle names are not
case-sensitive, although it is conventional to use all-uppercase names.
Leading whitespace is ignored.

The only reserved name is @~ for the current (context) file. This is usually
the most recent filehandle to be created, but @~ can also be assigned to
in order to change the context, e.g. @~ = @BOB

It is possible to subscript filehandle names, so that arbitrary names (or
interpolated names) can be used. E.g. @{'An unlikely name'}

// Assign a string or scalar to open file, @BOB = 'bob.bic'
// Assign undef to close, @BOB = undef; undef @ERF
// How to save? re-assign? @BOB = 'bob2.bic'; @ERF = '>'
// Assign a filehandle to open from it, @IFO = @{'ERF/Module.IFO'}
// How to force or check file-type? @BOB == ft 'Gff' ? <good> : <bad!>
// Use for @1 @2 ?

------------------------------------------------------------------------------*)

(*------------------------------------------------------------------------------
GetQuoted

------------------------------------------------------------------------------*)
function TLetoScript.GetQuoted(const Token, Prev: TLetoScriptObj): TLetoScriptError;
var
  S			: String;
  Constant		: Boolean;
  Nest			: Cardinal;
  Matching		: Char;
  Func			: TLetoScriptFunc;
begin
  Token.Op		:= opConst;
  Result		:= LS_Success;
  if (Token.Kind = quQX) and (Text = '''') then
    Token.Kind		:= quQXU;

  Constant		:= True;
  Nest			:= 0;
  if Text = ')' then
    Matching		:= '('
  else if Text = '}' then
    Matching		:= '{'
  else if Text = ']' then
    Matching		:= '['
  else if Text = '>' then
    Matching		:= '<'
  { GetOp directs fnPrintn here with '<' }
  else if Text = '<' then begin
    Text		:= '>';
    Matching		:= '<';
  end
  else
    Matching		:= #0;

  PeekPos		:= 0;

  GetChr;
  while Chr <> Text do begin

    if Chr = #10 then begin
      S			:= S + sLineBreak;
      GetLine;
      if GetChr = #10 then begin
        Result		:= Err_LS_Unterminated;
        Exit;
      end;
    end;

    if (Chr <> #0) and (Chr = Matching) then begin
      S			:= S + Chr;
      Inc(Nest);
    end

    { Full escape support (delayed). }
    else if
      (Chr <> '\') or
      (Token.Kind in [quDouble, quQQ, quPrintn, quCmd, quQX])
    then begin
      S			:= S + Chr;
      if Chr in ['$', '@', '/', '\', '`'] then
        Constant	:= False;
      { Always protect at least one }
      if Chr = '\' then
        S		:= S + GetChr;
    end

    { Partial support. }
    else if Token.Kind in [quSingle, quQ, quQXU] then begin
      if Peek in ['\', Text[1], Matching] then
        S		:= S + GetChr
      else
        S		:= S + Chr;
    end;

    while (GetChr = Text) and (Nest > 0) do begin
      S			:= S + Chr;
      Dec(Nest);
    end;

  end;

  Token.Value		:= S;
  Token.Text		:= S;

  if (Token.Kind = quQ) or ((Token.Kind in [quDouble, quQQ]) and Constant) then
    Token.Kind		:= quSingle
  { Interpolative constant. }
  { qx'' is still called an IConst so that Each works on it. }
  else if Token.Kind in [quDouble, quQQ, quPrintn, quCmd, quQX, quQXU] then
    Token.Op		:= opIConst;

  if Token.Kind = quPrintn then begin
    Func		:= SysLib.CreateFunc('@_');
    Func.Token		:= Token;
    Func.Fn		:= Word(fnPrintn);
    Token.Func		:= Func;
  end;

  if Token.Kind in [quCmd, quQX, quQXU] then begin
    Func		:= SysLib.CreateFunc('@_');
    Func.Token		:= Token;
    Func.Fn		:= Word(fnSystemQ);
    Func.Name		:= 'system';
    Token.Func		:= Func;
  end;

  GetChr;

end;


(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
