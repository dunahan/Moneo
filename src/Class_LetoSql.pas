(*------------------------------------------------------------------------------
Class_LetoSSql

A core class unit which defines support for SQLite, natively. This has no
external dependencies, except those libraries the SQL implementations
themselves require (sqlite3.dll, for instance). It is currently only
intended to support SQLite. This makes it a very lightweight alternative
to ZeosLib, especially designed for scripters who want an "easy" database
setup - SQLite is the very definition. Note that only SQLite 3.x is supported.

Support for other protocols *may* be forthcoming, as a general alternative
to ZeosLib - however, the intention is to avoid bloat, to avoid extraneous
DLLs (only those needed by the protocol itself), and above all, remain
cross-platform.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoSql;

{$I LetoScript.inc}

// Nothing in this entire unit applies without this switch:
{$IFNDEF LETOSCRIPT_SQL_LETO}
interface
implementation
{$ELSE}

interface

uses
  SysUtils, Classes, StrUtils,
  API_SQLite3;

type

  TLetoSQL		= class

  private

    FProtocol		: String;
    FDatabase		: String;
    FConnected		: Boolean;
    FBof		: Boolean;
    FEof		: Boolean;

    FBlobTable		: String;
    FBlobCol		: String;
    FBlobWhere		: String;
    FBlobCriteria	: String;

    function EscapeQuotes(const S: String): String;

    function GetBof: Boolean; virtual; abstract;
    function GetEof: Boolean; virtual; abstract;

  protected

    procedure SetDatabase(const dbName: String); virtual; abstract;

  public

    property Protocol: String read FProtocol;
    property Database: String read FDatabase write SetDatabase;

    property Connected: Boolean read FConnected;

    property Bof: Boolean read GetBof;
    property Eof: Boolean read GetEof;

    constructor Create; virtual;
    destructor Destroy; override;

    function Connect: Boolean; virtual; abstract;
    function Disconnect: Boolean; virtual; abstract;

    function Exec(const S: String): Boolean; virtual; abstract;
    function Query(
      const Q		: String;
      const FirstStep	: Boolean = True
    ): Boolean; virtual; abstract;

    procedure First; virtual; abstract;
    procedure Next; virtual; abstract;
    procedure Prior; virtual; abstract;
    procedure Last; virtual; abstract;

    function RecordCount: Integer; virtual; abstract;
    function RowsAffected: Integer; virtual; abstract;
    function InsertId: Integer; virtual; abstract;

    function FieldByCol(const Col: Integer): Variant; virtual; abstract;
    function FieldByName(const Name: String): Variant; virtual; abstract;

    procedure FieldNames(const List: TStringList); virtual; abstract;

    procedure BindBlob(
      const Table, Col, Where,
        Criteria	: String
    ); virtual;

    function GetBlobStream(const Stream: TStream): Boolean; virtual; abstract;

    function SaveBlobStream(const Stream: TMemoryStream): Boolean; virtual; abstract;

  end;


  TLetoSQLite		= class(TLetoSQL)

  private

    Db			: Pointer;
    Statement		: Pointer;

    Columns		: TStringList;

    function GetBof: Boolean; override;
    function GetEof: Boolean; override;

  protected

    procedure SetDatabase(const dbName: String); override;

    procedure Open(const dbName: String);
    procedure Close;

  public

    constructor Create; override;
    destructor Destroy; override;

    function Connect: Boolean; override;
    function Disconnect: Boolean; override;

    function Exec(const S: String): Boolean; override;
    function Query(
      const Q		: String;
      const FirstStep	: Boolean = True
    ): Boolean; override;

    procedure First; override;
    procedure Next; override;
    procedure Prior; override;
    procedure Last; override;

    function RecordCount: Integer; override;
    function RowsAffected: Integer; override;
    function InsertId: Integer; override;

    function FieldByCol(const Col: Integer): Variant; override;
    function FieldByName(const Name: String): Variant; override;

    procedure FieldNames(const List: TStringList); override;

    function GetBlobStream(const Stream: TStream): Boolean; override;

    function SaveBlobStream(const Stream: TMemoryStream): Boolean; override;

  end;


implementation


{ TLetoSQL }


(*------------------------------------------------------------------------------
EscapeQuotes

Changes all ' into '', as a limited safety on user-input SQL syntax.

------------------------------------------------------------------------------*)
function TLetoSQL.EscapeQuotes(const S: String): String;
begin
  Result		:= StringReplace(S, '''', '''''', [rfReplaceAll]);
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoSQL.Create;
begin

  FConnected		:= False;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoSQL.Destroy;
begin

  inherited;
end;

(*------------------------------------------------------------------------------
BindBlob

The BindBlob procedure helps to abstract the differences in implementation
for streaming in and out of a BLOB with different SQL libraries. (E.g.,
it's the bind_blob method in SQLite, but an entirely different paradigm
using ZeosLib.)

Specifying the table, col, row and criteria are sufficient for the underlying
code to build its own SQL statement to satisfy any library's implementation.
These vars correspond to,

UPDATE Table SET Col WHERE Where = Criteria
 - or -
SELECT Col FROM Table WHERE Where = Criteria

------------------------------------------------------------------------------*)
procedure TLetoSQL.BindBlob(const Table, Col, Where, Criteria: String);
begin
  FBlobTable		:= Table;
  FBlobCol		:= Col;
  FBlobWhere		:= Where;
  FBlobCriteria		:= Criteria;
end;


{ TLetoSQLite }


(*------------------------------------------------------------------------------
property Bof

------------------------------------------------------------------------------*)
function TLetoSQLite.GetBof: Boolean;
begin
  Result		:= FBof;
end;

(*------------------------------------------------------------------------------
property Eof

------------------------------------------------------------------------------*)
function TLetoSQLite.GetEof: Boolean;
begin
  Result		:= FEof;
end;

(*------------------------------------------------------------------------------
property Database

------------------------------------------------------------------------------*)
procedure TLetoSQLite.SetDatabase(const dbName: String);
begin

  FConnected		:= False;

  if dbName <> '' then
    Open(dbName)
  else
    Close;

end;

(*------------------------------------------------------------------------------
Open

------------------------------------------------------------------------------*)
procedure TLetoSQLite.Open(const dbName: String);
var
  Msg			: String;
begin
  if FDatabase <> '' then Close;

  if (sqlite3_open(PAnsiChar(dbName), Db) <> SQLITE_OK) or not Assigned(Db) then begin
    Msg			:= sqlite3_errmsg(Db);
    sqlite3_close(Db);
    Db			:= nil;
    raise Exception.Create(Msg);
  end;

  FConnected		:= True;
  FDatabase		:= dbName;
  FBof			:= True;
  FEof			:= True;

end;

(*------------------------------------------------------------------------------
Close

------------------------------------------------------------------------------*)
procedure TLetoSQLite.Close;
begin
  FDatabase		:= '';
  FConnected		:= False;
  FBof			:= True;
  FEof			:= True;

  if Assigned(Db) then
    sqlite3_close(Db);

end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoSQLite.Create;
var
  LibName		: String;
begin

  Db			:= nil;
  Statement		:= nil;

  Columns		:= TStringList.Create;

  if not LoadLibSqlite3(LibName) then
    raise Exception.Create(
      'Failed to load SQLite 3 library'
    );

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoSQLite.Destroy;
begin
  Disconnect;

  FreeAndNil(Columns);

  inherited;
end;

(*------------------------------------------------------------------------------
Connect

Do not use Connect. Set the Database property to the filename, instead.

(However, Connect may be used as a shortcut for Connected.)

------------------------------------------------------------------------------*)
function TLetoSQLite.Connect: Boolean;
begin
  Result		:= Connected;
end;

(*------------------------------------------------------------------------------
Disconnect

This is a shortcut for setting Database to ''.

------------------------------------------------------------------------------*)
function TLetoSQLite.Disconnect: Boolean;
begin
  if Assigned(Statement) then
    sqlite3_finalize(Statement);
  Database		:= '';
  Result		:= not Connected;
end;

(*------------------------------------------------------------------------------
Exec

Beware, the statement is not prepared with escaped quotes. SQL injection is
possible here - the calling function must do the sanitizing, or abstract
Exec into appropriate SQL functions.

------------------------------------------------------------------------------*)
function TLetoSQLite.Exec(const S: String): Boolean;
var
  ErrMsg		: PAnsiChar;
begin
  Result		:= Assigned(Db);
  if not Result then Exit;

  Result := sqlite3_exec(Db, PAnsiChar(S), nil, nil, ErrMsg) = SQLITE_OK;

  if not Result then
    raise Exception.Create(ErrMsg);

end;

(*------------------------------------------------------------------------------
Query

------------------------------------------------------------------------------*)
function TLetoSQLite.Query(const Q: String; const FirstStep: Boolean): Boolean;
var
  P			: Pointer;
  I			: Integer;
begin
  Columns.Clear;

  Result		:= Assigned(Db);
  if not Result then Exit;

  if Assigned(Statement) then
    sqlite3_finalize(Statement);

  Result := sqlite3_prepare(Db, PAnsiChar(Q), -1, Statement, P) = SQLITE_OK;

  if not Result then
    raise Exception.Create( sqlite3_errmsg(Db) );

  { This makes internal tracking and bounds protection possible. }
  for I := 0 to sqlite3_column_count(Statement)-1 do
    Columns.Add( sqlite3_column_name(Statement, I) );

  { Immediately seek to the First record. }
  if FirstStep then
    First;

end;

(*------------------------------------------------------------------------------
First

------------------------------------------------------------------------------*)
procedure TLetoSQLite.First;
begin
  if not Assigned(Db) or not Assigned(Statement) then Exit;

  if not sqlite3_reset(Statement) = SQLITE_OK then
    raise Exception.Create( sqlite3_errmsg(Db) );

  FBof			:= True;
  FEof			:= False;

  Next;

end;

(*------------------------------------------------------------------------------
Next

------------------------------------------------------------------------------*)
procedure TLetoSQLite.Next;
begin
  if not Assigned(Db) or not Assigned(Statement) then Exit;

  case sqlite3_step(Statement) of
    SQLITE_BUSY:
      raise Exception.Create( 'Database busy' );
    SQLITE_DONE:
      FEof		:= True;
    SQLITE_ROW: ;
  else { SQLITE_ERROR, SQLITE_MISUSE, and undefined results }
    raise Exception.Create( sqlite3_errmsg(Db) );
  end;

end;

(*------------------------------------------------------------------------------
Prior

sqlite3 does not implement Prior, or Last. Only Next is explicitly implemented,
and First can be coaxed using sqlite3_reset.

------------------------------------------------------------------------------*)
procedure TLetoSQLite.Prior;
begin
  raise Exception.Create('PRIOR is not implemented by sqlite3');
end;

(*------------------------------------------------------------------------------
Last

sqlite3 does not implement Prior, or Last. Only Next is explicitly implemented,
and First can be coaxed using sqlite3_reset.

------------------------------------------------------------------------------*)
procedure TLetoSQLite.Last;
begin
  raise Exception.Create('LAST is not implemented by sqlite3');
end;

(*------------------------------------------------------------------------------
RecordCount

sqlite3 has no explicit RecordCount function, so the result is always 1.
Either the caller must implement this implicitly, or the user must rely on
while Next.

------------------------------------------------------------------------------*)
function TLetoSQLite.RecordCount: Integer;
begin
  Result		:= 1;
end;

(*------------------------------------------------------------------------------
RowsAffected

Only works after INSERT, UPDATE, DELETE. See the sqlite3 API reference
for additional notes.

------------------------------------------------------------------------------*)
function TLetoSQLite.RowsAffected: Integer;
begin
  if Assigned(Db) then
    Result		:= sqlite3_changes(Db)
  else
    Result		:= -1;
end;

(*------------------------------------------------------------------------------
InsertId

------------------------------------------------------------------------------*)
function TLetoSQLite.InsertId: Integer;
begin
  if Assigned(Db) then
    Result		:= sqlite3_last_insert_rowid(Db)
  else
    Result		:= -1;
end;

(*------------------------------------------------------------------------------
FieldByCol

------------------------------------------------------------------------------*)
function TLetoSQLite.FieldByCol(const Col: Integer): Variant;
begin
  Result :=
    Assigned(Db) and
    Assigned(Statement) and
    (Col > -1) and (Col < Columns.Count);

  if Result then
    Result := '' + sqlite3_column_text(Statement, Col);

end;

(*------------------------------------------------------------------------------
FieldByName

------------------------------------------------------------------------------*)
function TLetoSQLite.FieldByName(const Name: String): Variant;
begin
  Result		:= FieldByCol( Columns.IndexOf(Name) );
end;

(*------------------------------------------------------------------------------
FieldNames

------------------------------------------------------------------------------*)
procedure TLetoSQLite.FieldNames(const List: TStringList);
begin
  if Assigned(Db) and Assigned(Statement) and Assigned(List) then
    List.AddStrings( Columns );
end;

(*------------------------------------------------------------------------------
GetBlobStream

------------------------------------------------------------------------------*)
function TLetoSQLite.GetBlobStream(const Stream: TStream): Boolean;
var
  Col			: Integer;
  C			: Integer;
  P			: Pointer;
begin
  Result := Assigned(Db) and Assigned(Stream) and
    Query(
      'SELECT ' + FBlobCol + ' ' +
      'FROM ' + FBlobTable + ' ' +
      'WHERE ' + FBlobWhere + ' ' +
      '= ' + FBlobCriteria
    );
  if not Result then Exit;

  Col			:= Columns.IndexOf(FBlobCol);
  Result		:= Col > -1;
  if not Result then Exit;

  C			:= sqlite3_column_bytes(Statement, Col);
  P			:= sqlite3_column_blob(Statement, Col);

  Stream.Seek(0, 0);
  Stream.Write(P^, C);

  Result		:= Stream.Size = C;

end;

(*------------------------------------------------------------------------------
SaveBlobStream

------------------------------------------------------------------------------*)
function TLetoSQLite.SaveBlobStream(const Stream: TMemoryStream): Boolean;
begin
  Result := Assigned(Db) and Assigned(Stream) and
    Query(
      'UPDATE ' + FBlobTable + ' ' +
      'SET ' + FBlobCol + ' = ? ' +
      'WHERE ' + FBlobWhere + ' ' +
      '= ' + FBlobCriteria,
      False
    );
  if not Result then Exit;

  sqlite3_bind_blob(Statement, 1, Stream.Memory, Stream.Size, nil);

  sqlite3_step(Statement);
  sqlite3_finalize(Statement);

end;


{$ENDIF} // LETOSCRIPT_SQL_LETO (unit-wide)

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
