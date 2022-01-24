(*------------------------------------------------------------------------------
API_sqlite3

This is the API connection unit for sqlite3.dll. It was created with help
from both the SQLite home page, and an example translation in the LibSQL
project:

https://sourceforge.net/projects/libsql/

It is not complete, and is intended specifically for the use of LetoScript
(although it is a generic translation).

This code may be updated into a derivative work, or completely rewritten,
at the discretion of the LetoScript project. As shown, it is merely acting
as an API translation. It is cross-platform, and apparently compatible
with FreePascal.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit API_SQLite3;

{$IFDEF FPC}
{$MODE Delphi}
{$H+}
{$ELSE}
  {$IFNDEF LINUX}
  {$DEFINE WIN32}
  {$ENDIF}
{$ENDIF}

interface

{ For FreePascal (?) }
{$IFDEF FPC}
{$DEFINE WTYPES}
{$ENDIF}

{ For Kylix }
{$IFDEF LINUX}
{$DEFINE WTYPES}
{$ENDIF}

uses
  {$IFNDEF LINUX}
    {$IFDEF FPC}
      DynLibs
    {$ELSE}
      Windows
    {$ENDIF}
  {$ELSE}
    SysUtils
  {$ENDIF};

{$IFDEF WTYPES}
type
  PWChar		= PWideChar;
{$ENDIF}

function LoadLibSQLite3(var LibName: String): Boolean;

const SQLITE_OK		=  0;   // Successful result
const SQLITE_ERROR	=  1;   // SQL error or missing database
const SQLITE_INTERNAL	=  2;   // An internal logic error in SQLite
const SQLITE_PERM	=  3;   // Access permission denied
const SQLITE_ABORT	=  4;   // Callback routine requested an abort
const SQLITE_BUSY	=  5;   // The database file is locked
const SQLITE_LOCKED	=  6;   // A table in the database is locked
const SQLITE_NOMEM	=  7;   // A malloc() failed
const SQLITE_READONLY	=  8;   // Attempt to write a readonly database
const SQLITE_INTERRUPT	=  9;   // Operation terminated by sqlite_interrupt()
const SQLITE_IOERR	= 10;   // Some kind of disk I/O error occurred
const SQLITE_CORRUPT	= 11;   // The database disk image is malformed
const SQLITE_NOTFOUND	= 12;   // (Internal Only) Table or record not found
const SQLITE_FULL	= 13;   // Insertion failed because database is full
const SQLITE_CANTOPEN	= 14;   // Unable to open the database file
const SQLITE_PROTOCOL	= 15;   // Database lock protocol error
const SQLITE_EMPTY	= 16;   // (Internal Only) Database table is empty
const SQLITE_SCHEMA	= 17;   // The database schema changed
const SQLITE_TOOBIG	= 18;   // Too much data for one row of a table
const SQLITE_CONSTRAINT	= 19;   // Abort due to contraint violation
const SQLITE_MISMATCH	= 20;   // Data type mismatch
const SQLITE_MISUSE	= 21;   // Library used incorrectly
const SQLITE_NOLFS	= 22;   // Uses OS features not supported on host
const SQLITE_AUTH	= 23;   // Authorization denied
const SQLITE_ROW	= 100;  // sqlite_step() has another row ready
const SQLITE_DONE	= 101;  // sqlite_step() has finished executing

const SQLITE_UTF8	= 1;
const SQLITE_UTF16LE	= 2;
const SQLITE_UTF16BE	= 3;
const SQLITE_UTF16	= 4;    // Use native byte order
const SQLITE_ANY	= 5;

{ From sqlite3_column_type }
const SQLITE_INTEGER	= 1;
const SQLITE_FLOAT	= 2;
const SQLITE_TEXT	= 3;
const SQLITE_BLOB	= 4;
const SQLITE_NULL	= 5;

const SQLITEDLL : PChar =
  {$IFDEF LINUX}
    'libsqlite3.so'
  {$ELSE}
    'sqlite3.dll'
  {$ENDIF};

type

  PSQLite		= Pointer;

  TSqlite_Func		= record
    P			: Pointer;
  end;
  PSQLite_Func		= ^TSQLite_Func;

  (*
  procProgressCallback = procedure (UserData:Integer); cdecl;
  Tsqlite_create_function = function(
    db: Pointer;
    {const}zName:PChar;
    nArg: Integer;
    xFunc: PSqlite_func{*,int,const char**};
    UserData: Integer
  ) : Integer; cdecl;
  *)

var

  sqlite3_bind_blob:
    function(
      Statement		: Pointer;
      Param		: Integer;
      Value		: Pointer;
      Size		: Integer;
      DestructorPtr	: Pointer
    ): Integer; cdecl;

  sqlite3_bind_parameter_count:
    function(Statement: Pointer): Integer; cdecl;

  sqlite3_bind_parameter_name:
    function(Statement: Pointer; ParamNo: Integer): PChar; cdecl;

  sqlite3_busy_handler:
    procedure(db: Pointer; CallbackPtr: Pointer; Sender: TObject); cdecl;

  sqlite3_busy_timeout:
    procedure(db: Pointer; TimeOut: Integer); cdecl;

  sqlite3_changes	: function(db: Pointer): Integer; cdecl;

  sqlite3_close		: function(db: Pointer): Integer; cdecl;

  { CollationFunction is defined as
    function ColFunc(
      pCollNeededArg: Pointer;
      db: Pointer;
      eTextRep: Integer;
      CollSeqName: PChar
    ): Pointer;
  }
  sqlite3_collation_needed:
    function(db: Pointer; CollNeededArg: Pointer; CollationFunctionPtr: Pointer): Integer; cdecl;

  sqlite3_collation_needed16:
    function(db: Pointer; CollNeededArg: Pointer; CollationFunctionPtr: Pointer): Integer; cdecl;

  sqlite3_column_blob:
    function(Statement: Pointer; Col: Integer): Pointer; cdecl;

  sqlite3_column_bytes:
    function(Statement: Pointer; Col: Integer): Integer; cdecl;

  sqlite3_column_bytes16:
    function(Statement: Pointer; Col: Integer): Integer; cdecl;

  sqlite3_column_count:
    function(Statement: Pointer): Integer; cdecl;

  sqlite3_column_decltype:
    function(Statement: Pointer; Col: Integer): PChar; cdecl;

  sqlite3_column_decltype16:
    function (Statement: Pointer; Col: Integer): PWChar; cdecl;

  sqlite3_column_double:
    function(Statement: Pointer; Col: Integer): Double; cdecl;

  sqlite3_column_int:
    function(Statement: Pointer; Col: Integer): Integer; cdecl;

  sqlite3_column_int64:
    function(Statement: Pointer; Col: Integer): Int64; cdecl;

  sqlite3_column_name:
    function(Statement: Pointer; Col: Integer): PChar; cdecl;

  sqlite3_column_name16:
    function (Statement: Pointer; Col: Integer): PWChar; cdecl;

  sqlite3_column_text:
    function(Statement: Pointer; Col: Integer): PChar; cdecl;

  sqlite3_column_text16:
    function(Statement: Pointer; Col: Integer): PWChar; cdecl;

  sqlite3_column_type:
    function(Statement: Pointer; Col: Integer): Integer; cdecl;

  sqlite3_complete	: function(const sql: PChar): Integer; cdecl;

  sqlite3_complete16	: function(const sql: PWChar): Integer; cdecl;

  { CompareFunction is defined as
    function ComFunc(
      pCtx: Pointer;
      str1Length: Integer;
      str1: PWChar;
      str2Length: Integer;
      str2: PWChar
    ): Pointer;
  }
  sqlite3_create_collation:
    function(db: Pointer; CollName: PChar; TextRep: Integer; Ctx: Pointer; CompareFuncPtr: Pointer): Integer; cdecl;

  sqlite3_create_collation16:
    function(db: Pointer; CollName: PWChar; TextRep: Integer; Ctx: Pointer; CompareFuncPtr: Pointer): Integer; cdecl;

  sqlite3_data_count	: function(Statement: Pointer): Integer; cdecl;

  sqlite3_errcode	: function(db: Pointer): Integer; cdecl;

  sqlite3_errmsg	: function(db : Pointer): PChar; cdecl;

  sqlite3_errmsg16	: function(db : Pointer): PWChar; cdecl;

  sqlite3_exec:
    function(db: Pointer; SQLStatement: PChar; CallbackPtr: Pointer; Sender: TObject; var ErrMsg: PChar): Integer; cdecl;

  sqlite3_finalize	: function(Statement: Pointer): Integer; cdecl;

  sqlite3_free		: procedure(P: PChar); cdecl;

  sqlite3_free_table	: procedure(Table: PChar); cdecl;

  sqlite3_get_table:
    function(db: Pointer; SQLStatement: PChar; var ResultPtr: Pointer; var RowCount: Integer; var ColCount: Integer; var ErrMsg: PChar): Integer; cdecl;

  sqlite3_interrupt	: procedure(db : Pointer); cdecl;

  sqlite3_last_insert_rowid:
    function(db: Pointer): Int64; cdecl;

  sqlite3_libversion	: function(): PChar; cdecl;

  sqlite3_open:
    function(dbName: PChar; var db: Pointer): Integer; cdecl;

  sqlite3_open16:
    function(dbName: PWChar; var db: Pointer): Integer; cdecl;

  sqlite3_prepare:
    function(db: Pointer; SQLStatement: PChar; SQLLength: Integer; var Statement: Pointer; var Tail: Pointer): Integer; cdecl;

  sqlite3_prepare16:
    function(db: Pointer; SQLStatement: PWChar; SQLLength: Integer; var Statement: Pointer; var Tail: pointer): Integer; cdecl;

  sqlite3_reset		: function(Statement: Pointer): Integer; cdecl;

  sqlite3_step		: function(Statement: Pointer): Integer; cdecl;

  sqlite3_total_changes	: function(db: Pointer): Integer; cdecl;

  Libs3Loaded		: Boolean = False;
  DLLHandle		: THandle;
  MsgNoError		: String;


implementation


{$IFDEF FPC}
{ This helps FPC typecast. (?) }
function GetProcAddress(Module: HMODULE; ProcName: PChar): Pointer;
begin
  Result		:= GetProcedureAddress(Module, ProcName);
end;
{$ENDIF}

function LoadLibSQLite3(var LibName: String): Boolean;
  function _Get_(const N: String): Pointer;
  begin
    Result		:= GetProcAddress(DLLHandle, PAnsiChar(N));
  end;
begin
  Result		:= Libs3Loaded;
  if Result then Exit;

  if LibName = '' then
    LibName		:= SQLITEDLL;

  DLLHandle		:= LoadLibrary(PChar(LibName));

  Result		:= DLLHandle <> 0;
  Libs3Loaded		:= Result;
  if not Result then Exit;

  @sqlite3_bind_blob		:= _Get_('sqlite3_bind_blob');
  @sqlite3_bind_parameter_count	:= _Get_('sqlite3_bind_parameter_count');
  @sqlite3_bind_parameter_name	:= _Get_('sqlite3_bind_parameter_name');
  @sqlite3_busy_handler		:= _Get_('sqlite3_busy_handler');
  @sqlite3_busy_timeout		:= _Get_('sqlite3_busy_timeout');
  @sqlite3_changes		:= _Get_('sqlite3_changes');
  @sqlite3_close		:= _Get_('sqlite3_close');
  @sqlite3_collation_needed	:= _Get_('sqlite3_collation_needed');
  @sqlite3_collation_needed16	:= _Get_('sqlite3_collation_needed16');
  @sqlite3_column_blob		:= _Get_('sqlite3_column_blob');
  @sqlite3_column_bytes		:= _Get_('sqlite3_column_bytes');
  @sqlite3_column_bytes16	:= _Get_('sqlite3_column_bytes16');
  @sqlite3_column_count		:= _Get_('sqlite3_column_count');
  @sqlite3_column_decltype	:= _Get_('sqlite3_column_decltype');
  @sqlite3_column_decltype16	:= _Get_('sqlite3_column_decltype16');
  @sqlite3_column_double	:= _Get_('sqlite3_column_double');
  @sqlite3_column_int		:= _Get_('sqlite3_column_int');
  @sqlite3_column_int64		:= _Get_('sqlite3_column_int64');
  @sqlite3_column_name		:= _Get_('sqlite3_column_name');
  @sqlite3_column_name16	:= _Get_('sqlite3_column_name16');
  @sqlite3_column_text		:= _Get_('sqlite3_column_text');
  @sqlite3_column_text16	:= _Get_('sqlite3_column_text16');
  @sqlite3_column_type		:= _Get_('sqlite3_column_type');
  @sqlite3_complete		:= _Get_('sqlite3_complete');
  @sqlite3_complete16		:= _Get_('sqlite3_complete16');
  @sqlite3_create_collation	:= _Get_('sqlite3_create_collation');
  @sqlite3_create_collation16	:= _Get_('sqlite3_create_collation16');
  @sqlite3_data_count		:= _Get_('sqlite3_data_count');
  @sqlite3_errCode		:= _Get_('sqlite3_errcode');
  @sqlite3_errmsg		:= _Get_('sqlite3_errmsg');
  @sqlite3_errmsg16		:= _Get_('sqlite3_errmsg16');
  @sqlite3_exec			:= _Get_('sqlite3_exec');
  @sqlite3_finalize		:= _Get_('sqlite3_finalize');
  @sqlite3_free			:= _Get_('sqlite3_free');
  @sqlite3_free_table		:= _Get_('sqlite3_free_table');
  @sqlite3_get_table		:= _Get_('sqlite3_get_table');
  @sqlite3_interrupt		:= _Get_('sqlite3_interrupt');
  @sqlite3_last_insert_rowid	:= _Get_('sqlite3_last_insert_rowid');
  @sqlite3_libversion		:= _Get_('sqlite3_libversion');
  @sqlite3_open			:= _Get_('sqlite3_open');
  @sqlite3_open16		:= _Get_('sqlite3_open16');
  @sqlite3_prepare		:= _Get_('sqlite3_prepare');
  @sqlite3_prepare16		:= _Get_('sqlite3_prepare16');
  @sqlite3_reset		:= _Get_('sqlite3_reset');
  @sqlite3_step			:= _Get_('sqlite3_step');
  @sqlite3_total_changes	:= _Get_('sqlite3_total_changes');

  Result :=
    Assigned(sqlite3_bind_blob) and
    Assigned(sqlite3_bind_parameter_count) and
    Assigned(sqlite3_bind_parameter_name) and
    Assigned(sqlite3_busy_handler) and
    Assigned(sqlite3_busy_timeout) and
    Assigned(sqlite3_changes) and
    Assigned(sqlite3_close) and
    Assigned(sqlite3_collation_needed) and
    Assigned(sqlite3_collation_needed16) and
    Assigned(sqlite3_column_blob) and
    Assigned(sqlite3_column_bytes) and
    Assigned(sqlite3_column_bytes16) and
    Assigned(sqlite3_column_count) and
    Assigned(sqlite3_column_decltype) and
    Assigned(sqlite3_column_decltype16) and
    Assigned(sqlite3_column_double) and
    Assigned(sqlite3_column_int) and
    Assigned(sqlite3_column_int64) and
    Assigned(sqlite3_column_name) and
    Assigned(sqlite3_column_name16) and
    Assigned(sqlite3_column_text) and
    Assigned(sqlite3_column_text16) and
    Assigned(sqlite3_column_type) and
    Assigned(sqlite3_complete) and
    Assigned(sqlite3_complete16) and
    Assigned(sqlite3_create_collation) and
    Assigned(sqlite3_create_collation16) and
    Assigned(sqlite3_data_count) and
    Assigned(sqlite3_errcode) and
    Assigned(sqlite3_errmsg) and
    Assigned(sqlite3_errmsg16) and
    Assigned(sqlite3_exec) and
    Assigned(sqlite3_finalize) and
    Assigned(sqlite3_free) and
    Assigned(sqlite3_free_table) and
    Assigned(sqlite3_get_table) and
    Assigned(sqlite3_interrupt) and
    Assigned(sqlite3_last_insert_rowid) and
    Assigned(sqlite3_libversion) and
    Assigned(sqlite3_open) and
    Assigned(sqlite3_open16) and
    Assigned(sqlite3_prepare) and
    Assigned(sqlite3_prepare16) and
    Assigned(sqlite3_reset) and
    Assigned(sqlite3_step) and
    Assigned(sqlite3_total_changes);

  Libs3Loaded		:= Result;

  if not (Result) then begin
    {$IFDEF FPC}
      UnloadLibrary(DLLHandle);
    {$ELSE}
      FreeLibrary(DLLHandle);
    {$ENDIF}
    DllHandle := 0;
  end;

end;


initialization

finalization
  if DLLHandle <> 0 then
    {$IFDEF FPC}
      UnloadLibrary(DLLHandle);
    {$ELSE}
      FreeLibrary(DLLHandle);
    {$ENDIF}

end.
