(*------------------------------------------------------------------------------
Class_Moneo

The command-line interface for Moneo.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_Moneo;

{$I LetoScript.inc}

interface

uses
  SysUtils, Classes, TypInfo, IniFiles, StrUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Header_Leto, Header_LetoScript,
  Class_LetoXml,
  Class_LetoScript, Class_LetoScriptLib,
  Class_LetoScriptSub, Class_LetoScriptEnv;

type

  THelpTopic		= class

  private

  public

    Name		: String;
    Desc		: String;
    Node		: String;

    Visible		: Boolean;

    constructor Create(const AName, ADesc, ANode: String; AVisible: Boolean = true);
    destructor Destroy; override;

  end;

  // DONE 1: ADD: [27r3] Moneo settings, including color
  TMoneoMods		= record
    ColorsOn		: Boolean;
    ColorBold		: Byte;
    ColorCode		: Byte;
    ColorError		: Byte;
    ColorInput		: Byte;
    ColorLink		: Byte;
    ColorPrompt		: Byte;
    ColorText		: Byte;
    ColorWarning	: Byte;
    HelpBanners		: Boolean;
    HelpBannerChar	: Char;
    HelpFile		: String;
    Prompt		: String;
    TextHeight		: Byte;
    TextWidth		: Byte;
  end;

  TMoneo		= class

  private

    Env			: TLetoScriptEnv;
    LetoScript		: TLetoScript;
    HelpLoaded		: Boolean;
    HelpXml		: TLetoXml;
    HelpVer		: TLetoXmlElem;
    PageBuff		: TStringList;
    PageLine		: Integer;
    CurrentColor	: Integer;
    LogFile		: String;
    LogStream		: TFileStream;
    SettingsFile	: String;
    SettingsLoaded	: Boolean;
    SettingsXml		: TLetoXml;

    Mods		: TMoneoMods;

    procedure Syntax;
    procedure Version;

    function Choice(
      const Question	: array of String
    ): String;

    function YNChoice(
      const Question	: array of String
    ): Boolean;

    procedure Parse(const Stream: TStream); overload;
    procedure Parse(const S: String); overload;

    procedure Output(S: String; const TextType: TOutputType = otOutput);
    procedure Color(const C: Integer);
    procedure WordWrap(
      S			: String;
      const NewLine	: Boolean = True;
      const LineLen	: Byte = 0
    );
    procedure Page(Text: String);
    procedure Wrap(Text: String);
    procedure Depage(Text: String);
    function HelpText(const Script: String): String;
    procedure HelpAction(const Script: String);
    procedure Paginate;

    procedure Shell(const Stream: TStream);

    procedure ShellCmd(const CmdStr: String);
    procedure Define(const Method: String);
    procedure Diagnostics;
    procedure ListACL;
    procedure EnableDisable(
      const Name	: String;
      const Enable	: Boolean;
      const ShowErrors	: Boolean = True
    );
    procedure ListMembers(const I: Integer; MemberType: String);
    procedure LoadHelp;
    procedure LoadSettings(const ShowErrors: Boolean);
    procedure SaveSettings;
    procedure SetCmd(const Prop, Val: String);
    procedure SetShow(const Prop, Val: String);
    procedure SetBool(var ABool: Boolean; const Prop, Val: String);
    procedure SetByte(var AByte: Byte; const Prop, Val: String);
    procedure SetInt(var AInt: Integer; const Prop, Val: String);
    procedure SetChar(var AChar: Char; const Prop, Val: String);
    procedure SetString(var AStr: String; const Prop, Val: String);
    function GetHelpTopics(const Root: TLetoXmlElem): TStringList;
    procedure ListHelpTopics(
      const Root	: TLetoXmlElem;
      const List	: TStringList;
      const ErrMsg	: String
    );
    procedure HelpTopic(
      const Node	: TLetoXmlElem;
      const Topics	: TStringList;
      const Words	: TStringList
    );

  public

    constructor Create;
    destructor Destroy; override;

    procedure Run;

  end;

implementation

type
  TShellMode		= (smSingle, smScript, smCmd);

const
  NL			= sLineBreak;
  Prompts		= '>:#';
  MODE_CMD		= 29; { Ctrl-] }
  MODE_CMD_EX		= 24; { Ctrl-X }
  MODE_SCR		= 27; { Ctrl-[ }
  MODE_SCR_EX		= 12; { Ctrl-L }

  HelpSys_Ver		= '1.0';

  SHELLCOLOR_BLACK	= 0;
  SHELLCOLOR_BLUE	= 1;
  SHELLCOLOR_GREEN	= 2;
  SHELLCOLOR_CYAN	= 3;
  SHELLCOLOR_RED	= 4;
  SHELLCOLOR_PURPLE	= 5;
  SHELLCOLOR_YELLOW	= 6;
  SHELLCOLOR_WHITE	= 7;
  SHELLCOLOR_GREY	= 8;
  SHELLCOLOR_HI_BLUE	= 9;
  SHELLCOLOR_HI_GREEN	= 10;
  SHELLCOLOR_HI_CYAN	= 11;
  SHELLCOLOR_HI_RED	= 12;
  SHELLCOLOR_HI_PURPLE	= 13;
  SHELLCOLOR_HI_YELLOW	= 14;
  SHELLCOLOR_HI_WHITE	= 15;

  ModeNotes		: array[smSingle..smCmd] of String = (
    '',
    'Scriptlet started. Use Ctrl+[ again to execute.\n',
    'Command mode invoked. Type ''help'' for the help system. Type ''exit'' to return\n' +
      'to the shell.\n'
  );

  ShellBanner =
    '\nMoneo v' + LetoVersion + MyVersion + ' interactive shell.\n' +
    'Enter lines as statements. Type ''exit'' to end.\n\n' +
    'For help with Moneo and LetoScript, hit Ctrl+] (control key and right square\n' +
    'bracket), Enter, and type ''help''.\n';

  HelpSys_NoHelp =
    'Helpfile is not loaded. Only minimal help is available.\n';
  HelpSys_BadHelp =
    'Helpfile has bad format or missing nodes. Only minimal help is available.\n';
  HelpSys_MinHelp =
    'The following builtin commands can be used:\n' +
    '  define\n' +
    '  diagnostics\n' +
    '  disable\n' +
    '  enable\n' +
    '  list\n' +
    '  loadhelp\n' +
    '  loadsettings\n' +
    '  savesettings\n' +
    '  set\n' +
    'For help with these commands, use <command> ?\n' +
    'E.g.: define ?\n';

  HelpSys_Page =
    ' --- [n]ext page - [p]revious page - [q]uit ---';
  HelpSys_Page_Basic =
    ' --- [Enter] to continue ---';

  HelpSys_NotFound =
    'Topic not found. Use ? for a list of help topics, or ''commands'' for a\n' +
    'list of command mode utilities. (If you need to leave command mode to write\n' +
    'code, type ''exit''.)\n';
  HelpSys_CmdNotFound =
    'Topic not found. Use ''info command'' for help with command-mode utilities.\n' +
    'Use ? for a list of general help topics.\n';
  HelpSys_BadTopic =
    'This topic appears to be formatted incorrectly. Its list of topics cannot\n' +
    'be displayed. Check ''about'' and contact the author.\n';

  HelpSys_Define =
    'define <method>\n' +
    'Supply a method (qualified by module if necessary) for its definition. For\n' +
    'example, try: define print\n';
  HelpSys_Enable =
    'disable|enable [target]\n' +
    'Administratively disable or re-enable a module or method. If [target] is\n' +
    'not given, the current ACL is shown. Otherwise [target] is:\n' +
    '  <module>          Affects all of <module>''s methods: disable filesys\n' +
    '    .<method>       Affects a single method: disable filesys.filedelete\n' +
    '  all               Affects all methods in all modules: enable all\n';
  HelpSys_List =
    'list <subject>\n' +
    'Subjects known:\n' +
    '  modules           Lists currently loaded modules.\n' +
    '  methods           Lists all methods in all loaded modules.\n' +
    '    in <module>     Lists all methods in module named <module>.\n' +
    '  constants         Lists all currently defined constants.\n' +
    '    in <module>     Lists all constants defined in <module>.\n';
  HelpSys_LoadHelp =
    'loadhelp [helpfile]\n' +
    'Load or reload the helpfile. The default helpfile is LetoScriptHelp.xml.\n' +
    'Specify a different filename to use with [helpfile]. This can also be set as\n' +
    'a preference:\n' +
    '  set helpfile=MyHelpFile.xml\n' +
    '  loadhelp\n' +
    '  savesettings\n' +
    'To start Moneo with a different helpfile, use -H:\n' +
    '  moneo -H MyHelpFile.xml\n' +
    'You can make changes to a helpfile with an XML editor and reload it without\n' +
    'exiting Moneo, or command mode.\n';
  HelpSys_LoadSettings =
    'loadsettings [settingsfile]\n' +
    'Load or reload your personal preferences for Moneo. The default settingsfile\n' +
    'is MoneoSettings.xml. Specify a different filename with [settingsfile].\n' +
    'To start Moneo with a different settingsfile, use -Z:\n' +
    '  moneo -Z BobSettings.xml\n';
  HelpSys_SaveSettings =
    'savesettings [settingsfile]\n' +
    'Save your current settings. The default file to use is MoneoSettings.xml.\n' +
    'Specify a different filename to use with [settingsfile]. To load custom\n' +
    'settings when starting Moneo, use -Z:\n' +
    '  moneo -Z BobSettings.xml\n';
  HelpSys_Set =
    'set [<setting> [value]]\n' +
    'Use set by itself to display all current settings. Include [setting] to see\n' +
    'the current vault of that setting. [value] changes the setting:\n' +
    '  set color true\n' +
    '  set prompt LetoScript\n' +
    '  set textwidth 100\n';


{ THelpTopic }

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor THelpTopic.Create(
  const AName, ADesc,
    ANode		: String;
  AVisible		: Boolean
);
begin
  Name			:= AName;
  Desc			:= ADesc;
  Node			:= ANode;
  Visible		:= AVisible;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor THelpTopic.Destroy;
begin

  inherited;
end;


{ TMoneo }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
Syntax

------------------------------------------------------------------------------*)
procedure TMoneo.Syntax;
begin
  Write(
    'LetoScript shell and compiler (build ' + LetoVersion + MyVersion + ')' + NL +
    'Copyright 2004-2006, David Frauzel (dragon@weathersong.net)' + NL +
    'Open source under the BSD license. See License.txt for details.' + NL +
    NL +
    'Usage: moneo [switches] [--] [programfile] [arguments]' + NL +
    NL +
    '  -$                 Forces shell mode.' + NL +
    '  -e program         One line of program (several allowed).' + NL +
    '  -H xmlfile         Override default LetoScriptHelp.xml.' + NL +
    '  -L[flags] logfile  Output to logfile in addition to STDOUT.' + NL +
    '  -v                 Displays detailed version information.' + NL +
    '  -X                 Disables warnings.' + NL +
    '  -Z xmlfile         Override default MoneoSettings.xml.' + NL +
    NL +
    'Shell mode will execute if no programfile is given.' + NL +
    NL +
    'For help learning LetoScript, run Moneo (no arguments), and follow' + NL +
    'the prompts.' + NL
  );
end;

(*------------------------------------------------------------------------------
Version

------------------------------------------------------------------------------*)
procedure TMoneo.Version;
begin
  Write(
    'This is Moneo build ' + MyVersion + '.' + NL +
    'It was compiled on the Leto engine version ' + LetoVersion + '.' + NL +
    'Version date is ' + VersionDate + '.' + NL +
    'This build is codenamed ' + LS_Version + '.' + NL +
    'To see a list of included modules, use "list modules" in command mode.' + NL +
    'Support forums: http://weathersong.infopop.cc' + NL
    (*
    'Compiled-in modules:' + NL +
    '  (Standard)' + NL +
    '    system, math, gff, erf, bic, tlk, meta, filesys' + NL +
    '  (Optional)' + NL +
    {$IFDEF LETOSCRIPT_SQL}
    '    sql (' +
      {$IFDEF LETOSCRIPT_SQL_ZEOS}'ZeosLib' +{$ENDIF}
      {$IFDEF LETOSCRIPT_SQL_LETO}'Leto' +{$ENDIF}
    ')' + NL +
    {$ENDIF}
    'Online documentation: http://www.aros.net/~sueko/leto/docs/lsnut2/' + NL +
    *)
  );
end;

(*------------------------------------------------------------------------------
Choice

------------------------------------------------------------------------------*)
function TMoneo.Choice(
  const Question	: array of String
): String;
var
  I			: Integer;
  S			: String;
begin
  for I := Low(Question) to High(Question)-1 do
    WriteLn(Question[I]);
  Write(Question[High(Question)] + ' ');
  ReadLn(S);

  Result		:= Lowercase(Trim(S));
end;

(*------------------------------------------------------------------------------
YNChoice

------------------------------------------------------------------------------*)
function TMoneo.YNChoice(const Question: array of String): Boolean;
var
  S			: String;
begin
  S			:= Choice(Question);
  Result		:= (S = 'y') or (S = 'yes');
end;

(*------------------------------------------------------------------------------
Parse

Pass a script along to the compiler.

------------------------------------------------------------------------------*)
procedure TMoneo.Parse(const Stream: TStream);
begin
  Env.OutputNL		:= True;
  if Assigned(Stream) then
    LetoScript.Compile(Stream);
  if not Env.OutputNL then
    Output(NL);
end;

procedure TMoneo.Parse(const S: String);
begin
  Env.OutputNL		:= True;
  LetoScript.Compile(S);
  if not Env.OutputNL then
    Output(NL);
end;

(*------------------------------------------------------------------------------
Output

For the OnOutput event of LetoScript.

------------------------------------------------------------------------------*)
procedure TMoneo.Output(S: String; const TextType: TOutputType);
begin
  if TextType = otWarn then
    Color(Mods.ColorWarning)
  else if TextType = otError then
    Color(Mods.ColorError);
  Write(S);
  Color(Mods.ColorText);
  if Assigned(LogStream) then
    LogStream.Write(PChar(S)^, Length(S));
end;

(*------------------------------------------------------------------------------
Color

------------------------------------------------------------------------------*)
procedure TMoneo.Color(const C: Integer);
begin
  if not Mods.ColorsOn then
    Exit;

  CurrentColor		:= C;

  {$IFDEF MSWINDOWS}
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE), C);
  {$ENDIF}
end;

(*------------------------------------------------------------------------------
WordWrap

Outputs S immediately, with line wrapping. This should be phased out and
replaced with Page.

------------------------------------------------------------------------------*)
procedure TMoneo.WordWrap(
  S			: String;
  const NewLine		: Boolean;
  const LineLen		: Byte
);
var
  Width, P		: Integer;
  WidthReset		: Boolean;
  Len, SoftBr, HardBr	: Integer;
  procedure _Shift_(const Part, New: String; const WS: Boolean = True);
  begin
    if WS then
      WriteStrings(Part)
    else
      Write(Part);
    S			:= New;
  end;
begin
  Width			:= Mods.TextWidth - LineLen;
  WidthReset		:= False;

  while Length(S) > 0 do begin
    if WidthReset then
      Width		:= Mods.TextWidth
    else
      WidthReset	:= True;

    Len			:= Length(S);
    SoftBr		:= Pos('\n', S);
    if SoftBr = 0 then
      SoftBr		:= Len+1;
    HardBr		:= Pos(sLineBreak, S);
    if HardBr = 0 then
      HardBr		:= Len+1;

    if (Width < Len) and (Width < SoftBr) and (Width < HardBr) then begin
      P			:= Width;
      while not(S[P] in [' ', #9]) and (P > 0) do
        Dec(P);
      if P = 0 then
        P			:= Width - 1;
      WriteStrings(Copy(S, 1, P));
      S			:= Copy(S, P+1, Length(S));
      Continue;
    end

    else if (Len < SoftBr) and (Len < HardBr) then
      _Shift_(S, '', Pos('\n', S) > 0)

    else if (SoftBr < HardBr) then
      _Shift_( Copy(S, 1, SoftBr-1), Copy(S, SoftBr+2, Length(S)) )

    else
      _Shift_( Copy(S, 1, HardBr), Copy(S, HardBr+Length(sLineBreak), Length(S)) );

  end;

  if NewLine then
    WriteStrings('');

end;

(*------------------------------------------------------------------------------
Page

Add text to the pagination buffer.

------------------------------------------------------------------------------*)
procedure TMoneo.Page(Text: String);
var
  Lines			: TStringList;
  Br			: Integer;
  procedure _Macro_(const M, S: String);
  begin
    Text := StringReplace(Text, M, S, [rfReplaceAll]);
  end;
begin
  Lines			:= TStringList.Create;

  { Macros }
  // TODO 3: CHANGE: More efficient Page (macros)
  _Macro_(sLineBreak, '\n');
  _Macro_('{{', '[[code]]');
  _Macro_('}}', '[[/code]]');
  _Macro_('<[', '[[link]]');
  _Macro_(']>', '[[/link]]');

  { First pass - linebreaks }
  while Length(Text) > 0 do begin
    Br			:= Pos('\n', Text);
    if Br = 0 then begin
      Lines.Add(Text);
      Break;
    end;
    Lines.Add( Copy(Text, 1, Br-1) );
    Text		:= Copy(Text, Br+2, Length(Text));
  end;

  { Second pass - lines }
  while Lines.Count > 0 do begin
    Wrap(Lines[0]);
    Lines.Delete(0);
  end;

  FreeAndNil(Lines);

end;

(*------------------------------------------------------------------------------
Wrap

Used by Page.

------------------------------------------------------------------------------*)
procedure TMoneo.Wrap(Text: String);
var
  I, ParseMode		: Integer;
  LineLen		: Integer;
  Ch			: Char;
  Line			: String;
  Script		: String;
  Word, S		: String;
const
  PARSE_TEXT		= 0;
  PARSE_LBRACKET	= 1;
  PARSE_SCRIPT		= 2;
  PARSE_RBRACKET	= 3;
  procedure _PageLine_;
  begin
    PageBuff.Add(Line);
    Line		:= '';
    LineLen		:= 0;
    Inc(PageLine);
    if PageLine = Mods.TextHeight then
      PageLine		:= 0;
  end;
begin
  I			:= 1;
  ParseMode		:= PARSE_TEXT;
  LineLen		:= 0;
  Line			:= '';

  if Text = '' then begin
    _PageLine_;
    Exit;
  end;

  while I <= Length(Text) do begin
    Ch			:= Text[I];
    if (ParseMode = PARSE_LBRACKET) and (Ch <> '[') then begin
      Word		:= Word + '[';
      ParseMode		:= PARSE_TEXT;
    end;
    case Ch of
      '[':
        if ParseMode = PARSE_TEXT then begin
          ParseMode	:= PARSE_LBRACKET;
          Script	:= '';
        end
        else if ParseMode = PARSE_LBRACKET then begin
          ParseMode	:= PARSE_SCRIPT;
          // Small bug here, wraps wrong with: [[code]]\\[[/code]])
          if LineLen + Length(Word) >= Mods.TextWidth then
            _PageLine_;
          Line		:= Line + Word;
          Inc(LineLen, Length(Word));
          Word		:= '';
        end;
      ']':
        if ParseMode = PARSE_SCRIPT then
          ParseMode	:= PARSE_RBRACKET
        else if ParseMode = PARSE_RBRACKET then begin
          ParseMode	:= PARSE_TEXT;
          if Lowercase(Script) = 'page' then
            while PageLine <> 0 do
              _PageLine_
          else begin
            S		:= HelpText(Script);
            if S = '' then
              Line	:= Line + '[[' + Script + ']]'
            else
              Word	:= Word + S;
          end;
        end else
          Word		:= Word + ']';
      ' ', '-':
        if ParseMode = PARSE_SCRIPT then
          Script	:= Script + Ch
        else if Word = '' then begin
          Line		:= Line + Ch;
          Inc(LineLen);
        end else if LineLen + Length(Word) + 1 > Mods.TextWidth then begin
          _PageLine_;
          Line		:= Word + Ch;
          LineLen	:= Length(Word)+1;
          Word		:= '';
        end else begin
          Line		:= Line + Word + Ch;
          Inc(LineLen, Length(Word)+1);
          Word		:= '';
        end;
    else
      case ParseMode of
        PARSE_SCRIPT:
          Script	:= Script + Ch;
        PARSE_TEXT:
          Word		:= Word + Ch;
      end; { case ParseMode }
    end; { case Ch }
    Inc(I);
  end; { while Length(Text) }

  if Word <> '' then begin
    if LineLen + Length(Word) > Mods.TextWidth then
      _PageLine_;
    Line		:= Line + Word;
    Inc(LineLen, Length(Word));
    Word		:= '';
  end;
  if Line <> '' then
    _PageLine_;

end;

(*------------------------------------------------------------------------------
Depage

Outputs one line of text, which may contain [[actions]].

------------------------------------------------------------------------------*)
procedure TMoneo.Depage(Text: String);
var
  P			: Integer;
begin
  P			:= Pos('[[', Text);
  while P <> 0 do begin
    Write( Copy(Text, 1, P-1) );
    Text		:= Copy(Text, P+2, Length(Text));
    P			:= Pos(']]', Text);
    HelpAction( Copy(Text, 1, P-1) );
    Text		:= Copy(Text, P+2, Length(Text));
    P			:= Pos('[[', Text);
  end;

  WriteLn(Text);

end;

(*------------------------------------------------------------------------------
HelpText

Expands a [[help script]] code into text.

------------------------------------------------------------------------------*)
function TMoneo.HelpText(const Script: String): String;
var
  S			: String;
begin
  { Maintain the original, use this for examination: }
  S			:= Lowercase(Script);

  if S = 'author' then
    Result		:= HelpVer.Props['author'].Value
  else if S = 'email' then
    Result		:= HelpVer.Props['email'].Value
  else if S = 'contrib' then
    Result		:= HelpVer.Props['contrib'].Value
  else if S = 'date' then
    Result		:= HelpVer.Props['date'].Value
  else if S = 'moneo' then
    Result		:= HelpVer.Props['moneo'].Value
  else if S = '/n' then
    Result		:= '\n'
  else if not Mods.ColorsOn and ((S = 'link') or (S = '/link')) then
    Result		:= ''''

  else if
    (S = '&') or (S = '&&')
  then
    Result		:= S

  else
    Result		:= '';

end;

(*------------------------------------------------------------------------------
HelpAction

Performs [[actions]] embedded in the helpsys file.

------------------------------------------------------------------------------*)
procedure TMoneo.HelpAction(const Script: String);
var
  S			: String;
  I			: Integer;
begin
  { Maintain the original, use this for examination: }
  S			:= Lowercase(Script);

  if S = 'b' then
    Color(Mods.ColorBold)
  else if S = 'code' then
    Color(Mods.ColorCode)
  else if S = 'link' then
    Color(Mods.ColorLink)
  else if (Copy(S, 1, 6) = 'color ') and TryStrToInt(Copy(S, 7, Length(S)), I) then
    Color(I)
  else if
    (S = '/b') or
    (S = '/code') or
    (S = '/link') or
    (S = '/color')
  then
    Color(Mods.ColorText);

end;

(*------------------------------------------------------------------------------
Paginate

Display buffered pages of text one page at a time.

------------------------------------------------------------------------------*)
procedure TMoneo.Paginate;
var
  Line			: Integer;
  I, C			: Integer;
  {$IFDEF MSWINDOWS}
  hIn			: Cardinal;
  SaveMode, NumRead	: Cardinal;
  Ch			: Char;
  {$ENDIF}
begin
  Line			:= 0;

  {$IFDEF MSWINDOWS}
  hIn			:= GetStdHandle(STD_INPUT_HANDLE);
  GetConsoleMode(hIn, SaveMode);
  SetConsoleMode(hIn, 0);

  while True do begin
    if Line < 0 then
      Line		:= 0;
    for I := Line to Line + Mods.TextHeight - 1 do
      if I < PageBuff.Count then
        Depage(PageBuff[I])
      else
        Break;
    if Line + Mods.TextHeight >= PageBuff.Count then
      Break;
    C			:= CurrentColor;
    Color(Mods.ColorPrompt);
    Write(HelpSys_Page);
    NumRead		:= 0;
    Ch			:= #0;
    while NumRead < 1 do
      ReadConsole(hIn, @Ch, 1, NumRead, nil);
    Color(C);
    WriteLn;
    case Ch of
      'n', 'N':
        Line		:= Line + Mods.TextHeight;
      'p', 'P':
        Line		:= Line - Mods.TextHeight;
      'q', 'Q':
      begin
        WriteStrings('');
        Break;
      end;
    end;
  end;
  SetConsoleMode(hIn, SaveMode);
  {$ENDIF}

  {$IFDEF LINUX}
  while True do begin
    if Line < 0 then
      Line		:= 0;
    for I := Line to Line + Mods.TextHeight - 1 do
      if I < PageBuff.Count then
        Depage(PageBuff[I])
      else
        Break;
    if Line + Mods.TextHeight >= PageBuff.Count then
      Break;
    C			:= CurrentColor;
    Color(Mods.ColorPrompt);
    Write(HelpSys_Page_Basic);
    ReadLn;
    Color(C);
    Line		:= Line + Mods.TextHeight;
  end;
  {$ENDIF}

  PageBuff.Clear;
  PageLine		:= 0;

  {
  http://groups.google.com/group/fido7.ru.delphi/browse_thread/thread/c770a5bfeac3725c/ce786394a71cf915?lnk=st&q=delphi+read+char+console&rnum=14#ce786394a71cf915
  hIn := GetStdHandle(STD_INPUT_HANDLE);
 var
  Ch: Char;
  NumRead: DWORD;
  SaveMode: DWORD;
begin
  GetConsoleMode(hIn, SaveMode);
  SetConsoleMode(hIn, 0);
  NumRead := 0;
  while NumRead < 1 do
    ReadConsole(hIn, @Ch, 1, NumRead, nil);

  SetConsoleMode(hIn, SaveMode);
  Result := Ch;
end;
  }

end;

(*------------------------------------------------------------------------------
Shell

------------------------------------------------------------------------------*)
procedure TMoneo.Shell(const Stream: TStream);
var
  Scripting		: Boolean;
  Mode, LastMode	: TShellMode;
  S, Slc		: String;
  Scriptlet		: TStringStream;

  procedure _Mode_(const NewMode: TShellMode);
  begin
    if Mode = NewMode then
      Mode		:= LastMode
    else begin
      LastMode		:= Mode;
      Mode		:= NewMode;
      WriteStrings(ModeNotes[Mode]);
    end;
  end;

begin

  WriteStrings(ShellBanner);

  if Assigned(Stream) then
    Parse(Stream);

  Env.Name		:= '-';

  Scripting		:= True;
  Mode			:= smSingle;

  Scriptlet		:= TStringStream.Create('');

  while Scripting do begin

    Color(Mods.ColorPrompt);
    Write(Mods.Prompt + Prompts[Ord(Mode)+1] + ' ');
    Color(Mods.ColorInput);
    ReadLn(S);
    Color(Mods.ColorText);

    Slc			:= Lowercase(S);

    if S = '' then begin
      if Mode = smSingle then
        WriteLn('');
      Continue;
    end;

    if Ord(S[1]) in [MODE_CMD, MODE_CMD_EX] then
      _Mode_(smCmd)

    else if (Mode = smScript) and (Ord(S[1]) in [MODE_SCR, MODE_SCR_EX]) then
    begin
      _Mode_(smScript);
      Parse(Scriptlet);
      Scriptlet.Size	:= 0;
    end

    else if (Mode = smSingle) and (Ord(S[1]) in [MODE_SCR, MODE_SCR_EX]) then
      _Mode_(smScript)

    else case Mode of

      smSingle:
        if Slc = 'exit' then
          Scripting	:= False
        else
          Parse(S);

      smScript:
        Scriptlet.WriteString(S + #10);

      smCmd:
        if (Slc = 'exit') or (Slc = 'quit') then
          _Mode_(smCmd)
        else
          ShellCmd(S);

    end; { case Mode }

  end; { while Scripting }

end;

(*------------------------------------------------------------------------------
ShellCmd

------------------------------------------------------------------------------*)
// DONE 1: ADD: [27r3] command mode, help system
procedure TMoneo.ShellCmd(const CmdStr: String);
var
  I, P			: Integer;
  Str, S, Verb		: String;
  Words, Modules,
    Topics, List	: TStringList;
  XmlRoot		: TLetoXmlElem;
begin
  Str			:= Lowercase(CmdStr);
  P			:= Pos(' ', Str);
  if P > 0 then begin
    Verb		:= Copy(Str, 1, P-1);
    S			:= Copy(Str, P+1, Length(Str));
  end else
    Verb		:= Str;

  Words			:= TStringList.Create;
  Words.CommaText	:= S;

  Modules		:= TStringList.Create;
  for I := Low(LetoScript.Libs) to High(LetoScript.Libs) do
    Modules.Add(LetoScript.Libs[I].Name);

  XmlRoot		:= HelpXml.Root;
  Topics		:= GetHelpTopics(XmlRoot);

  List			:= TStringList.Create;

  try

  { Builtin commands: }

  if (Verb = 'define') or (Verb = 'syntax') then begin
    if (Words.Count = 0) or (Words[0] = '?') then
      WordWrap(HelpSys_Define)
    else
      Define(Words[0]);
  end

  else if (Verb = 'diagnostics') or (Verb = 'diag') then
    Diagnostics

  else if
    (Verb = 'disable') or (Verb = 'enable') or
    (Verb = 'deny') or (Verb = 'allow')
  then begin
    if Words.Count < 1 then
      ListACL
    else if (Words.Count > 1) or (Words[0] = '?') then
      WordWrap(HelpSys_Enable)
    else if Words[0] = 'all' then
      for I := 0 to Modules.Count - 1 do begin
        LetoScript.Libs[I].Enabled := (Verb = 'enable') or (Verb = 'allow');
        LetoScript.Libs[I].Disabled.Clear;
      end
    else
      EnableDisable(Words[0], (Verb = 'enable') or (Verb = 'allow'));
  end

  else if (Verb = 'list') or (Verb = 'show') then begin
    if Words.Count > 0 then
      S			:= Words[0]
    else
      S			:= '';
    if (S = '') or (S = '?') then
      WordWrap(HelpSys_List)
    else if (S = 'modules') or (S = 'libs') then begin
      WriteStrings('Active modules:');
      for I := Low(LetoScript.Libs) to High(LetoScript.Libs) do begin
        Color(Mods.ColorLink);
        Write('  ' + LetoScript.Libs[I].Name);
        if not LetoScript.Libs[I].Enabled then begin
          Color(Mods.ColorWarning);
          Write(' (disabled)');
        end;
        Color(Mods.ColorText);
        WriteStrings('');
      end;
      WriteStrings('');
    end
    else if
      (S = 'all') or (S = '*') or
      (S = 'methods') or (S = 'functions') or (S = 'constants')
    then begin
      Words.Delete(0);
      if (Words.Count > 0) and (Words[0] = 'in') then
        Words.Delete(0);
      if Words.Count > 0 then
        List.AddStrings(Words)
      else
        List.AddStrings(Modules);
      for I := 0 to List.Count - 1 do begin
        P := Modules.IndexOf(List[I]);
        if P = -1 then
          WriteStrings('Module ' + List[I] + ' is not defined.\n')
        else
          ListMembers(P, S);
      end;
    end
  end

  else if Verb = 'loadhelp' then begin
    if S = '?' then
      WordWrap(HelpSys_LoadHelp)
    else begin
      if Words.Count > 0 then
        Mods.HelpFile	:= Copy(CmdStr, P+1, Length(CmdStr));
      LoadHelp;
    end;
  end

  else if Verb = 'loadsettings' then begin
    if S = '?' then
      WordWrap(HelpSys_LoadSettings)
    else begin
      if Words.Count > 0 then
        SettingsFile	:= Copy(CmdStr, P+1, Length(CmdStr));
      LoadSettings(True);
    end;
  end

  else if Verb = 'savesettings' then begin
    if S = '?' then
      WordWrap(HelpSys_SaveSettings)
    else begin
      if Words.Count > 0 then
        SettingsFile	:= Copy(CmdStr, P+1, Length(CmdStr));
      SaveSettings;
    end;
  end

  else if Verb = 'set' then begin
    if S = '?' then
      WordWrap(HelpSys_Set)
    else begin
      Words.CommaText	:= CmdStr + ',?,?';
      Words.Delete(0);
      SetCmd(Words[0], Words[1]);
    end;
  end

  { Now try the helpfile: }
  
  else if not HelpLoaded then
    WordWrap(HelpSys_NoHelp + HelpSys_MinHelp)
  else if (Verb = '?') or (Verb = 'help') then begin
    if Words.Count = 0 then    
      ListHelpTopics(XmlRoot, Topics, HelpSys_BadHelp + HelpSys_MinHelp)
    else begin
      Words.Insert(0, 'info');
      Words.Insert(1, 'command');
      HelpTopic(XmlRoot, Topics, Words);
    end;
  end else begin
    Words.Insert(0, Verb);
    HelpTopic(XmlRoot, Topics, Words);
  end;

  finally
    FreeAndNil(Words);
    FreeAndNil(Modules);
    for I := 0 to Topics.Count - 1 do
      THelpTopic(Topics.Objects[I]).Free;
    FreeAndNil(Topics);
    FreeAndNil(List);
  end;

end;

(*------------------------------------------------------------------------------
Define

------------------------------------------------------------------------------*)
procedure TMoneo.Define(const Method: String);
var
  Defined		: Boolean;
  P			: Integer;
  Lib, Name		: String;
  List			: TStringList;
  FC			: TFuncCode;
  S, Def		: String;
  C			: Int64;
  E			: Extended;
  Func			: TLetoScriptFunc;
begin
  Defined		:= False;

  P			:= Pos('.', Method);
  if P > 0 then begin
    Lib			:= Copy(Method, 1, P-1);
    Name		:= Copy(Method, P+1, Length(Method));
  end else begin
    Lib			:= '*';
    Name		:= Method;
  end;

  { Try as a sub }
  if (Lib = '*') or (Lib = 'local') then begin
    P			:= Env.Subs.IndexOf(Name);
    Defined		:= P > -1;
    if Defined then
      WordWrap(
        'sub ' + Name + ' ' +
        TLetoScriptSub(Env.Subs.Objects[P]).Describe + '\n'
      );
  end;

  { Try as an internal }
  List			:= GetLSMatchingMethods(Lib, Name);
  Defined		:= Defined or (List.Count > 0);
  for P := 0 to List.Count - 1 do begin
    FC			:= LS_Functions[ StrToInt(List[P]) ];
    { Constant }
    S			:= FC.Code;
    if FC.Fn = -1 then begin
      Def		:= FC.Name + ' is a constant, value = ';
      if TryStrToInt64(S, C) then begin
        if S[1] = '$' then
          Def		:= Def + S + ' (' + IntToStr(C) + ')'
        else
          Def		:= Def + S;
      end else if TryStrToFloat(S, E) then
        Def		:= Def + FloatToStrF(E, ffGeneral, 15, 15)
      else
        Def		:= Def + '''' + S + '''';
      WordWrap(Def + '\n');
    end
    { Function }
    else begin
      Func		:= TLetoScriptFunc.Create(S, nil, nil);
      S			:= FC.Name + ' is a ';
      if Func.Op = opListOp then
        S		:= S + 'list operator '
      else
        S		:= S + 'scalar operator ';
      S			:= S + Func.Describe;
      WordWrap(S + '\n');
      FreeAndNil(Func);
    end;
  end;

  if not Defined then
    WriteStrings(Method + ' is not defined.');

  FreeAndNil(List);

end;

(*------------------------------------------------------------------------------
Diagnostics

------------------------------------------------------------------------------*)
procedure TMoneo.Diagnostics;
var
  I			: Integer;
  S			: String;
  VerNode		: TLetoXmlElem;
  procedure _Item_(const S: String; const V: String = '');
  begin
    Write(
      '  ' + S +
      StringOfChar('.',  18 - Length(S)) +
      ': '
    );
    if V <> '' then
      WriteStrings(V);
  end;
begin
  WriteStrings('Moneo diagnostics:');

  { Engine }
  _Item_('Engine', LetoVersion);

  { Moneo }
  _Item_('Moneo', MyVersion);
  _Item_('  Date', VersionDate);
  _Item_('  Codename', LS_Version);

  { Settings }
  _Item_('SettingsFile');
  if not SettingsLoaded then
    S			:= 'NOT LOADED'
  else
    S			:= 'LOADED';
  WriteStrings('"' + SettingsFile + '", ' + S);

  { HelpSys }
  VerNode		:= nil;
  _Item_('HelpSys', HelpSys_Ver);
  _Item_('HelpFile');
  if not HelpLoaded then
    S			:= 'NOT LOADED'
  else begin
    S			:= 'LOADED, ';
    VerNode		:= HelpXml.Root.Items.ItemNamed['version'];
    if Assigned(VerNode) then
      S			:= S + 'OK'
    else
      S			:= S + 'ERROR';
  end;
  WriteStrings('"' + Mods.HelpFile + '", ' + S);
  if Assigned(VerNode) then begin
    _Item_('  moneo', VerNode.Props['moneo'].Value);
    _Item_('  helpsys', VerNode.Props['helpsys'].Value);
    _Item_('  date', VerNode.Props['date'].Value);
    _Item_('  author', VerNode.Props['author'].Value);
    _Item_('  email', VerNode.Props['email'].Value);
  end;

  { Color test }
  if Mods.ColorsOn then begin
    _Item_('Color test');
    for I := 0 to 15 do begin
      Color(I);
      Write('X');
    end;
    Color(Mods.ColorText);
    WriteStrings('');
  end;

  WriteStrings('');

end;

(*------------------------------------------------------------------------------
ListACL

Easier than hunting through list methods.

------------------------------------------------------------------------------*)
procedure TMoneo.ListACL;
var
  DefaultList		: TStringList;
  S			: String;
  I, P			: Integer;
  Lib			: TLetoScriptLib;
begin
  Color(Mods.ColorLink);

  { ALLOW }
  DefaultList		:= TStringList.Create;
  DefaultList.CommaText	:= DefaultACL;
  for I := 0 to DefaultList.Count - 1 do begin
    S			:= DefaultList[I];
    if S = '' then
      Continue;
    P			:= Pos('.', S);
    if P = 0 then begin
      Lib		:= LetoScript.Lib[S];
      if Assigned(Lib) and Lib.Enabled then
        WriteStrings('  ALLOW ' + S);
    end else begin
      Lib		:= LetoScript.Lib[Copy(S, 1, P-1)];
      if not Assigned(Lib) then
        Continue;
      S			:= Copy(S, P+1, Length(S));
      if Lib.Disabled.IndexOf(S) = -1 then
        WriteStrings('  ALLOW ' + DefaultList[I]);
    end;
  end;
  FreeAndNil(DefaultList);

  { DENY }
  for I := Low(LetoScript.Libs) to High(LetoScript.Libs) - 1 do begin
    Lib			:= LetoScript.Libs[I];
    if not Lib.Enabled then
      WriteStrings('  DENY ' + Lib.Name);
    for P := 0 to Lib.Disabled.Count - 1 do
      WriteStrings('  DENY ' + Lib.Name + '.' + Lib.Disabled[P]);
  end;

  WriteStrings('');
  Color(Mods.ColorText);

end;

(*------------------------------------------------------------------------------
EnableDisable

Enable or disable a module, or a method.

------------------------------------------------------------------------------*)
procedure TMoneo.EnableDisable(
  const Name		: String;
  const Enable		: Boolean;
  const ShowErrors	: Boolean
);
var
  Err			: TLetoScriptError;
begin
  Err			:= LetoScript.EnableDisable(Name, Enable);
  if (Err <> LS_Success) and ShowErrors then
    WriteStrings(
      StringReplace(GetLSError(Err), '%s', LetoScript.ErrStr, []) + '\n'
    );
end;

(*------------------------------------------------------------------------------
ListMembers

List the members (constants or methods) of a module.

------------------------------------------------------------------------------*)
procedure TMoneo.ListMembers(const I: Integer; MemberType: String);
var
  Lib			: TLetoScriptLib;
  S			: String;
  List			: TStringList;
  L			: Integer;
begin
  if MemberType = 'functions' then
    MemberType		:= 'methods';

  Lib			:= LetoScript.Libs[I];

  Write('In module ' + Lib.Name);
  if not Lib.Enabled then begin
    Color(Mods.ColorWarning);
    Write(' (disabled)');
    Color(Mods.ColorText);
  end;
  WriteStrings(':');

  // TODO 3: CHANGE: More efficient GetMethods
  List			:= GetLSLibMembers(Lib.Name, MemberType);

  for L := 0 to List.Count - 1 do begin
    S			:= List[L];
    Color(Mods.ColorLink);
    Write('  ' + S);
    if Lib.Disabled.IndexOf(S) > -1 then begin
      Color(Mods.ColorWarning);
      Write(' (disabled)');
    end;
    Color(Mods.ColorText);
    WriteStrings('');
  end;

  WriteStrings('');

  FreeAndNil(List);

end;

(*------------------------------------------------------------------------------
LoadHelp

Loads or reloads the helpfile.

------------------------------------------------------------------------------*)
procedure TMoneo.LoadHelp;
var
  Stream		: TStream;
begin
  HelpLoaded		:= False;
  if (Mods.HelpFile <> '') and FileExists(Mods.HelpFile) then begin
    try
      Stream		:= TFileStream.Create(Mods.HelpFile, fmOpenRead);
    except
      WordWrap(
        GetLSError(Err_Moneo_BadHelp) +
        ' "' + Mods.HelpFile + '": ' +
        GetLSError(Err_Moneo_Locked)
      );
    end;
    HelpLoaded		:= HelpXml.LoadFromStream(Stream, False);
    FreeAndNil(Stream);
  end;

  HelpVer		:= HelpXml.Root.Items.ItemNamed['version'];
  if not Assigned(HelpVer) then
    HelpVer		:= HelpXml.Root.Items.Add('version');

end;

(*------------------------------------------------------------------------------
LoadSettings

Loads or reloads the settingsfile.

------------------------------------------------------------------------------*)
procedure TMoneo.LoadSettings(const ShowErrors: Boolean);
var
  Stream		: TStream;
  Node, ACE		: TLetoXmlElem;
  I			: Integer;
  Principal, Action	: String;
begin
  SettingsLoaded	:= False;
  if (SettingsFile <> '') and FileExists(SettingsFile) then begin
    try
      Stream		:= TFileStream.Create(SettingsFile, fmOpenRead);
    except
      WordWrap(
        GetLSError(Err_Moneo_BadSettings) +
        ' "' + SettingsFile + '": ' +
        GetLSError(Err_Moneo_Locked)
      );
    end;
    SettingsLoaded	:= SettingsXml.LoadFromStream(Stream, False);
    FreeAndNil(Stream);
  end;

  { Moneo environment. }
  Node			:= SettingsXml.Root.Items.ItemNamed['MoneoEnv'];
  if Assigned(Node) then
    with Mods do begin
      ColorsOn		:= Node.Props['Color'].AsBool;
      ColorBold		:= Node.Props['Color-Bold'].AsInt;
      ColorCode		:= Node.Props['Color-Code'].AsInt;
      ColorError	:= Node.Props['Color-Error'].AsInt;
      ColorInput	:= Node.Props['Color-Input'].AsInt;
      ColorLink		:= Node.Props['Color-Link'].AsInt;
      ColorPrompt	:= Node.Props['Color-Prompt'].AsInt;
      ColorText		:= Node.Props['Color-Text'].AsInt;
      ColorWarning	:= Node.Props['Color-Warning'].AsInt;
      HelpBanners	:= Node.Props['HelpBanners'].AsBool;
      HelpBannerChar	:= Node.Props['HelpBannerChar'].AsChar;
      Prompt		:= Node.Props['Prompt'].Value;
      TextHeight	:= Node.Props['Text-Height'].AsInt;
      TextWidth		:= Node.Props['Text-Width'].AsInt;
    end;

  { LetoScript module Access Control List. }
  LetoScript.BuildFuncACL(DefaultACL);
  Node			:= SettingsXml.Root.Items.ItemNamed['ModuleACL'];
  if Assigned(Node) then
    for I := 0 to Node.Items.Count - 1 do begin
      ACE		:= Node.Items[I];
      if Lowercase(ACE.Name) <> 'ace' then
        Continue;
      Principal		:= Lowercase(ACE.Props['principal'].Value);
      Action		:= Lowercase(ACE.Props['action'].Value);
      if (Principal <> '') and ((Action = 'allow') or (Action = 'deny')) then
        EnableDisable(Principal, Action = 'allow', ShowErrors);
    end;

end;

(*------------------------------------------------------------------------------
SaveSettings

Saves Mods to settingsxml.

------------------------------------------------------------------------------*)
procedure TMoneo.SaveSettings;
var
  Root, Node, ACE	: TLetoXmlElem;
  I, D, P		: Integer;
  Lib			: TLetoScriptLib;
  S			: String;
begin
  Root			:= SettingsXml.Root;
  Root.Name		:= 'MoneoSettings';

  { Moneo environment. }
  Node			:= Root.Establish('MoneoEnv');
  with Mods do begin
    Node.Props.Establish('Color', BoolToStr(ColorsOn, True));
    Node.Props.Establish('Color-Bold', IntToStr(ColorBold));
    Node.Props.Establish('Color-Code', IntToStr(ColorCode));
    Node.Props.Establish('Color-Error', IntToStr(ColorError));
    Node.Props.Establish('Color-Input', IntToStr(ColorInput));
    Node.Props.Establish('Color-Link', IntToStr(ColorLink));
    Node.Props.Establish('Color-Prompt', IntToStr(ColorPrompt));
    Node.Props.Establish('Color-Text', IntToStr(ColorText));
    Node.Props.Establish('Color-Warning', IntToStr(ColorWarning));
    Node.Props.Establish('HelpBanners', BoolToStr(HelpBanners, True));
    Node.Props.Establish('HelpBannerChar', HelpBannerChar);
    Node.Props.Establish('Prompt', Prompt);
    Node.Props.Establish('Text-Height', IntToStr(TextHeight));
    Node.Props.Establish('Text-Width', IntToStr(TextWidth));
  end;

  { LetoScript module Access Control List. }
  Node			:= Root.Establish('ModuleACL');
  Node.Clear;
  for I := Low(LetoScript.Libs) to High(LetoScript.Libs) do begin
    Lib			:= LetoScript.Libs[I];
    P			:= Pos(',' + Lib.Name + ',', DefaultACL);
    { Modules: }
    if
      ( (P = 0) and not Lib.Enabled ) or
      ( (P > 0) and Lib.Enabled )
    then begin
      ACE		:= Node.Items.Add('ACE');
      ACE.Props.Add('principal', Lib.Name);
      if Lib.Enabled then
        ACE.Props.Add('action', 'allow')
      else
        ACE.Props.Add('action', 'deny');
    end;
    { Denied methods: }
    for D := 0 to Lib.Disabled.Count - 1 do begin
      S			:= Lib.Name + '.' + Lib.Disabled[D];
      if Pos(S + ',', DefaultACL) > 0 then
        Continue;
      ACE		:= Node.Items.Add('ACE');
      ACE.Props.Add('principal', S);
      ACE.Props.Add('action', 'deny');
    end;
    { An allowed method that overrides DefaultACL: }
    P			:= Pos(Lib.Name + '.', DefaultACL);
    if Lib.Enabled and (P > 0) then begin
      S			:= Copy(DefaultACL, P, Length(DefaultACL));
      S			:= Copy(S, 1, Pos(',', S)-1);
      if Lib.Disabled.IndexOf( Copy(S, Pos('.', S)+1, Length(S)) ) = -1 then begin
        ACE		:= Node.Items.Add('ACE');
        ACE.Props.Add('principal', S);
        ACE.Props.Add('action', 'allow');
      end;
    end;
  end;

  SettingsXml.SaveToFile(SettingsFile);
end;

(*------------------------------------------------------------------------------
SetCmd

------------------------------------------------------------------------------*)
procedure TMoneo.SetCmd(const Prop, Val: String);
begin

  if (Prop = '') or (Prop = '?') then begin
    SetBool(Mods.ColorsOn, 'color', '?');
    SetByte(Mods.ColorBold, 'color-bold', '?');
    SetByte(Mods.ColorCode, 'color-code', '?');
    SetByte(Mods.ColorError, 'color-error', '?');
    SetByte(Mods.ColorInput, 'color-input', '?');
    SetByte(Mods.ColorLink, 'color-link', '?');
    SetByte(Mods.ColorPrompt, 'color-prompt', '?');
    SetByte(Mods.ColorText, 'color-text', '?');
    SetByte(Mods.ColorWarning, 'color-warning', '?');
    SetBool(Mods.HelpBanners, 'helpbanners', '?');
    SetChar(Mods.HelpBannerChar, 'helpbannerchar', '?');
    SetString(Mods.HelpFile, 'helpfile', '?');
    SetString(Mods.Prompt, 'prompt', '?');
    SetByte(Mods.TextHeight, 'text-height', '?');
    SetByte(Mods.TextWidth, 'text-width', '?');
  end

  else if Prop = 'color' then
    SetBool(Mods.ColorsOn, Prop, Val)
  else if Prop = 'color-bold' then
    SetByte(Mods.ColorBold, Prop, Val)
  else if Prop = 'color-code' then
    SetByte(Mods.ColorCode, Prop, Val)
  else if Prop = 'color-error' then
    SetByte(Mods.ColorError, Prop, Val)
  else if Prop = 'color-input' then
    SetByte(Mods.ColorInput, Prop, Val)
  else if Prop = 'color-link' then
    SetByte(Mods.ColorLink, Prop, Val)
  else if Prop = 'color-prompt' then
    SetByte(Mods.ColorPrompt, Prop, Val)
  else if Prop = 'color-text' then
    SetByte(Mods.ColorText, Prop, Val)
  else if Prop = 'color-warning' then
    SetByte(Mods.ColorWarning, Prop, Val)
  else if Prop = 'helpbanners' then
    SetBool(Mods.HelpBanners, Prop, Val)
  else if Prop = 'helpbannerchar' then
    SetChar(Mods.HelpBannerChar, Prop, Val)
  else if (Prop = 'helpfile') then
    SetString(Mods.HelpFile, Prop, Val)
  else if Prop = 'prompt' then
    SetString(Mods.Prompt, Prop, Val)
  else if Prop = 'text-height' then
    SetByte(Mods.TextHeight, Prop, Val)
  else if Prop = 'text-width' then
    SetByte(Mods.TextWidth, Prop, Val)

  else
    WriteStrings('Unrecognized property. Use ''set'' by itself for a list.');

end;

(*------------------------------------------------------------------------------
SetShow

------------------------------------------------------------------------------*)
procedure TMoneo.SetShow(const Prop, Val: String);
var
  S			: String;
begin
  S := '  ' + Prop + StringRepeat(' ', 18 - Length(Prop)) + '= ';
  if Copy(Prop, 1, 6) = 'color-' then
    S			:= S + '[[color ' + Val + ']]' + Val + '[[/color]]'
  else
    S			:= S + Val;
  Depage(S);
end;

(*------------------------------------------------------------------------------
SetBool

------------------------------------------------------------------------------*)
procedure TMoneo.SetBool(var ABool: Boolean; const Prop, Val: String);
begin
  if (Val = 'on') or (Val = 'true') then
    ABool		:= True
  else if (Val = 'off') or (Val = 'false') then
    ABool		:= False;
  SetShow(Prop, BoolToStr(ABool, True));
end;

(*------------------------------------------------------------------------------
SetByte

------------------------------------------------------------------------------*)
procedure TMoneo.SetByte(var AByte: Byte; const Prop, Val: String);
var
  TryByte		: Integer;
begin
  if Val = '?' then
  else if not TryStrToInt(Val, TryByte) or (TryByte < 0) or (TryByte > 255) then
    WriteStrings('Value for ' + Prop + ' must be 0 to 255.')
  else
    AByte		:= TryByte;
  SetShow(Prop, IntToStr(AByte));
end;

(*------------------------------------------------------------------------------
SetInt

------------------------------------------------------------------------------*)
procedure TMoneo.SetInt(var AInt: Integer; const Prop, Val: String);
var
  TryByte		: Integer;
begin
  if Val = '?' then
  else if not TryStrToInt(Val, TryByte) then
    WriteStrings('Value for ' + Prop + ' must be an integer.')
  else
    AInt		:= TryByte;
  SetShow(Prop, IntToStr(AInt));
end;

(*------------------------------------------------------------------------------
SetChar

------------------------------------------------------------------------------*)
procedure TMoneo.SetChar(var AChar: Char; const Prop, Val: String);
begin
  if Val = '?' then
  else if Length(Val) <> 1 then
    WriteStrings('Value for ' + Prop + ' must be a single character.')
  else
    AChar		:= Val[1];
  SetShow(Prop, AChar);
end;

(*------------------------------------------------------------------------------
SetString

------------------------------------------------------------------------------*)
procedure TMoneo.SetString(var AStr: String; const Prop, Val: String);
begin
  if Val <> '?' then
    AStr		:= Val;
  SetShow(Prop, '"' + AStr + '"');
end;

(*------------------------------------------------------------------------------
GetHelpTopics

------------------------------------------------------------------------------*)
function TMoneo.GetHelpTopics(const Root: TLetoXmlElem): TStringList;
var
  Index			: TLetoXmlElem;
  I, A			: Integer;
  Link			: TLetoXmlElem;
  Name			: String;
  Vis			: Boolean;
  Aliases		: TStringList;
  procedure _AddTopic_;
  begin
    if Name <> '' then
      Result.AddObject(
        Name,
        THelpTopic.Create(
          Name,
          Link.Props['desc'].Value, Link.Props['node'].Value,
          Vis
        )
      );
  end;
begin
  Result		:= TStringList.Create;
  Index			:= Root.Items.ItemNamed['index'];
  if not Assigned(Index) then
    Exit;

    for I := 0 to Index.Items.Count - 1 do begin
      Link		:= Index.Items[I];
      Name		:= Link.Props['name'].Value;
      if Link.HasProp('visible') then
        Vis		:= Link.Props['visible'].AsBool
      else
        Vis		:= True;
      _AddTopic_;
      if Link.HasProp('aliases') then begin
        Vis		:= False;
        Aliases		:= TStringList.Create;
        Aliases.CommaText := Link.Props['aliases'].Value;
        for A := 0 to Aliases.Count - 1 do begin
          Name		:= Aliases[A];
          _AddTopic_;
        end;
        FreeAndNil(Aliases);
      end;
    end;

end;

(*------------------------------------------------------------------------------
ListHelpTopics

------------------------------------------------------------------------------*)
procedure TMoneo.ListHelpTopics(
  const Root		: TLetoXmlElem;
  const List		: TStringList;
  const ErrMsg		: String
);
var
  Index			: TLetoXmlElem;
  Banner		: String;
  I			: Integer;
  Link			: THelpTopic;
begin
  Index		:= Root.Items.ItemNamed['index'];
  if not Assigned(Index) or (List.Count = 0) then
    WordWrap(ErrMsg)
  else begin
    if Mods.HelpBanners then begin
      if Root.HasProp('title') then
        Banner		:= Root.Props['title'].Value
      else
        Banner		:= 'INDEX';
      WriteStrings(
        StringRepeat(Mods.HelpBannerChar, Mods.TextWidth-1) + '\n' +
        Banner + '\n' +
        StringRepeat(Mods.HelpBannerChar, Mods.TextWidth-1)
      );
    end;
    WordWrap(Index.Props['header'].Value);
    for I := 0 to List.Count - 1 do begin
      Link		:= THelpTopic(List.Objects[I]);
      if Link.Visible then begin
        Write('  ');
        Color(Mods.ColorLink);
        Write(Link.Name);
        Color(Mods.ColorText);
        WordWrap(
          StringRepeat(' ', 18 - Length(Link.Name)) +
          Link.Desc,
          True, 2 + Length(Link.Name)
        );
      end;
    end;
    WordWrap(Index.Props['footer'].Value);
  end;

end;

(*------------------------------------------------------------------------------
HelpTopic

------------------------------------------------------------------------------*)
procedure TMoneo.HelpTopic(
  const Node		: TLetoXmlElem;
  const Topics		: TStringList;
  const Words		: TStringList
);
var
  NodeType		: String;
  Banner		: String;
  Request		: String;
  I			: Integer;
  Link			: THelpTopic;
  Section		: TLetoXmlElem;
  Index			: TStringList;
begin

  Index			:= nil;

  if Node = HelpXml.Root then
    NodeType		:= 'index'
  else
    NodeType		:= Node.Name;

  { For an article, ignore Words. Print the article, exit. }
  if NodeType = 'article' then begin
    if Node.HasProp('title') then
      Banner		:= Node.Props['title'].Value
    else
      Banner		:= '(Untitled)';
    if Mods.HelpBanners then
      Page(
        StringRepeat(Mods.HelpBannerChar, Mods.TextWidth-1) + '\n' +
        Banner + '\n' +
        StringRepeat(Mods.HelpBannerChar, Mods.TextWidth-1)
      );
    Page(Node.Value + '\n');
    Paginate;
    Exit;
  end;

  if Words.Count = 0 then
    Request		:= '?'
  else
    Request		:= Words[0];
  I			:= Topics.IndexOf(Request);
  Section		:= nil;
  if I > -1 then begin
    Link		:= THelpTopic(Topics.Objects[I]);
    Section		:= Node.Find('*', 'name', Link.Node);
  end;

  { Index style }

  if Request = '?' then
    ListHelpTopics(Node, Topics, HelpSys_BadTopic)

  else if (I = -1) or not Assigned(Section) then begin
    if Lowercase(Node.Props['name'].Value) = 'command' then
      Page(HelpSys_CmdNotFound)
    else
      Page(HelpSys_NotFound);
    Paginate;
  end

  else begin
    Index		:= GetHelpTopics(Section);
    Words.Delete(0);
    HelpTopic(Section, Index, Words);
  end;

  if Assigned(Index) then begin
    for I := 0 to Index.Count - 1 do
      THelpTopic(Index.Objects[I]).Free;
    FreeAndNil(Index);
  end;

end;

(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TMoneo.Create;
var
  S			: String;
begin

  {$IFDEF MSWINDOWS}
  S			:= ExtractFilePath( ParamStr(0) );
  {$ENDIF}
  // TODO 4: LINUX: Extract app's file path (necessary?)
  {$IFDEF LINUX}
  S			:= '';
  {$ENDIF}

  Env			:= TLetoScriptEnv.Create;
  Env.OnOutput		:= Output;
  Env.Params.Embedded	:= False;
  // DONE 1: CHANGE: [27r3] Warnings now enabled by default
  Env.Params.Warnings   := True;

  HelpXml		:= TLetoXml.Create('', False);
  SettingsXml		:= TLetoXml.Create('', False);
  PageBuff		:= TStringList.Create;

  LetoScript		:= TLetoScript.Create(ocVoid, Env);

  with Mods do begin
    ColorsOn		:= False;
    ColorBold		:= SHELLCOLOR_HI_WHITE;
    ColorCode		:= SHELLCOLOR_HI_PURPLE;
    ColorError		:= SHELLCOLOR_HI_RED;
    ColorInput		:= SHELLCOLOR_HI_CYAN;
    ColorLink		:= SHELLCOLOR_HI_CYAN;
    ColorPrompt		:= SHELLCOLOR_HI_WHITE;
    ColorText		:= SHELLCOLOR_WHITE;
    ColorWarning	:= SHELLCOLOR_HI_RED;
    HelpBanners		:= True;
    HelpBannerChar	:= '-';
    Prompt		:= 'Moneo';
    TextHeight		:= 24;
    TextWidth		:= 79;
  end;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TMoneo.Destroy;
begin

  FreeAndNil(HelpXml);
  FreeAndNil(SettingsXml);
  FreeAndNil(PageBuff);

  if Assigned(LogStream) then
    FreeAndNil(LogStream);

  FreeAndNil(Env);

  FreeAndNil(LetoScript);

end;

(*------------------------------------------------------------------------------
Run

The Run procedure now includes a command-line switch processor, and no longer
uses the generic-syntax GetParams / VerifyParams provided in Header_Leto.

------------------------------------------------------------------------------*)
procedure TMoneo.Run;
type
  TMoneoMode = (
    mmSwitches, mmSyntax, mmEval, mmHelpFile, mmSettingsFile,
    mmLog, mmAppendLog,
    mmShell, mmVer
  );
  TMoneoModeSet		= set of TMoneoMode;
var
  I, L			: Integer;
  S, LogFlags		: String;
  C			: Char;
  Modes			: TMoneoModeSet;
  Args			: TStringList;
  Eval			: TStringStream;
  Stream		: TFileStream;
begin

  Modes			:= [mmSwitches];
  Mods.HelpFile		:= 'LetoScriptHelp.xml';
  SettingsFile		:= 'MoneoSettings.xml';

  Args			:= TStringList.Create;
  Eval			:= TStringStream.Create('');
  Stream		:= nil;

  try

  for I := 1 to ParamCount do begin
    S			:= ParamStr(I);

    { Switches that take a param after a space: }
    if mmEval in Modes then begin
      Exclude(Modes, mmEval);
      Eval.WriteString(S + NL);
      Continue;
    end
    else if mmLog in Modes then begin
      Exclude(Modes, mmLog);
      LogFile		:= S;
      Continue;
    end
    else if mmHelpFile in Modes then begin
      Exclude(Modes, mmHelpFile);
      Mods.HelpFile	:= S;
      Continue;
    end
    else if mmSettingsFile in Modes then begin
      Exclude(Modes, mmSettingsFile);
      SettingsFile	:= S;
      Continue;
    end;


    if (mmSwitches in Modes) and (S[1] <> '-') then
      Exclude(Modes, mmSwitches);

    if not (mmSwitches in Modes) then begin
      Args.Add(S);
      Continue;
    end;

    { Explicitly terminates switch processing. }
    if S = '--' then begin
      Exclude(Modes, mmSwitches);
      Continue;
    end;

    { Strip off the hyphen. }
    System.Delete(S, 1, 1);

    for L := 1 to Length(S) do begin
      C			:= S[L];

      case C of

        '$':
          Include(Modes, mmShell);

        'e':
        begin
          if L < Length(S) then
            Eval.WriteString(Copy(S, L+1, Length(S)) + NL)
          else
            Include(Modes, mmEval);
          Break;
        end;

        'H':
        begin
          if L < Length(S) then
            Mods.HelpFile := Copy(S, L+1, Length(S))
          else
            Include(Modes, mmHelpFile);
          Break;
        end;

        'h', '?':
          Include(Modes, mmSyntax);

        'L':
          if LogFile <> '' then
            Include(Modes, mmSyntax)
          else begin
            Include(Modes, mmLog);
            LogFlags	:= Copy(S, L+1, Length(S));
            Break;
          end;

        'v':
          Include(Modes, mmVer);

        'X':
          Env.Params.Warnings := False;

        'Z':
        begin
          if L < Length(S) then
            SettingsFile := Copy(S, L+1, Length(S))
          else
            Include(Modes, mmSettingsFile);
          Break;
        end;

      else
        Include(Modes, mmSyntax);

      end; { case C }

      if mmSyntax in Modes then Break;

    end; { for Length(S) }

  end; { for ParamCount }

  { -v }
  if mmVer in Modes then begin
    Version;
    Exit;
  end;

  { -L[flags] }
  if LogFlags <> '' then begin
    if LogFile = '' then
      Include(Modes, mmSyntax)
    else
    for L := 1 to Length(LogFlags) do begin
      C			:= LogFlags[L];
      case C of
        '+':
          Include(Modes, mmAppendLog);
      else
        Include(Modes, mmSyntax);
      end;
    end;
  end;

  { Syntax error! }
  if mmSyntax in Modes then begin
    Syntax;
    Exit;
  end;

  { -H helpfile }
  LoadHelp;

  { -Z settingsfile }
  LoadSettings(False);

  { Program file: }
  if Args.Count > 0 then begin
    S			:= Args[0];
    Env.Name		:= S;
    try
      if FileExists(S) then
        Stream		:= TFileStream.Create(S, fmOpenRead)
      else
        WriteLn(
          GetLSError(Err_Moneo_BadScript) +
          ' "' + S + '": ' +
          GetLSError(Err_Moneo_NoSuchFile)
        );
    except
      WriteLn(
        GetLSError(Err_Moneo_BadScript) +
        ' "' + S + '": ' +
        GetLSError(Err_Moneo_Locked)
      );
    end;
    Args.Delete(0);
  end;

  { LogStream: }
  if LogFile <> '' then begin
    try
      // DONE 1: BUG: [27r3] -L broken
      if (mmAppendLog in Modes) and FileExists(LogFile) then
        LogStream	:= TFileStream.Create(LogFile, fmOpenWrite)
      else
        LogStream	:= TFileStream.Create(LogFile, fmCreate);
    except
      WriteLn(
        GetLSError(Err_Moneo_BadLog) +
        ' "' + LogFile + '": ' +
        GetLSError(Err_Moneo_Locked)
      );
    end;
    LogStream.Seek(0, soFromEnd);
  end;

  { Eval: }
  if Eval.Size > 0 then begin
    S			:= Env.Name;
    Env.Name		:= '-e';
    Parse(Eval);
    Env.Name		:= S;
  end;

  // TODO 4: ADD: Receive command-line user args (in Args)

  { Operate: }
  if (mmShell in Modes) or (not Assigned(Stream) and (Eval.Size = 0)) then
    Shell(Stream)
  else
    Parse(Stream);

  finally
    if Assigned(Stream) then
      FreeAndNil(Stream);
    FreeAndNil(Args);
    FreeAndNil(Eval);
  end;

end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
