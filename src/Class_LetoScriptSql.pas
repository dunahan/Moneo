(*------------------------------------------------------------------------------
Class_LetoScriptSql

A descendant of Class_LetoScript with functionality specific to the SQL
library, implemented potentially for several possible solutions across many
platforms and compilers, and SQL servers (MySQL, SQLite, ODBC, MSSQL, etc.)

This file demonstrates, incidentally, how Class_LetoScriptLib could eventually
be broken out into separate units for each library, or groups of libs,
considering that unit is rapidly becoming monolithic.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoScriptSql;

{$I LetoScript.inc}

// Nothing in this entire unit applies without this switch:
{$IFNDEF LETOSCRIPT_SQL}
interface
implementation
{$ELSE}

interface

uses
  SysUtils, Classes, StrUtils, TypInfo,
  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  DB, ZConnection, ZDataSet,
  {$ENDIF}
  {$IFDEF LETOSCRIPT_SQL_LETO}
  Class_LetoSql,
  {$ENDIF}
  Header_Leto, Header_LetoScript,
  Class_LetoFile,
  Class_LetoScriptEnv, Class_LetoScriptObj, Class_LetoScriptLib;

type

  TLetoScriptLibSql	= class(TLetoScriptLib)

  private

    {$IFDEF LETOSCRIPT_SQL_ZEOS}
    Sql			: TZQuery;
    {$ENDIF}
    {$IFDEF LETOSCRIPT_SQL_LETO}
    Sql			: TLetoSQL;
    {$ENDIF}

  public

    constructor Create; override;
    destructor Destroy; override;

    function Contains(
      const Name	: String;
      var Item		: TLibItem
    ): Boolean; override;

    function CreateFunc: Pointer; override;

  end;

  TLetoScriptFuncSql = class(TLetoScriptFunc)

  private

    Lib			: TLetoScriptLibSql;
    {$IFDEF LETOSCRIPT_SQL_ZEOS}
    Query		: TZQuery;
    {$ENDIF}
    {$IFDEF LETOSCRIPT_SQL_LETO}
    Query		: TLetoSQL;
    {$ENDIF}

    function Ready: Boolean;

    procedure _BindBlob;
    procedure _Bof;
    procedure _Connect;
    procedure _Disconnect;
    procedure _Eof;
    procedure _Exec;
    procedure _Field;
    procedure _FieldNames;
    procedure _Fields;
    procedure _First;
    procedure _InsertId;
    procedure _Last;
    procedure _Next;
    procedure _Params;
    procedure _Prior;
    procedure _Query;
    procedure _RecordCount;
    procedure _Retrieve;
    procedure _RowsAffected;
    procedure _Store;

  public

    procedure Evaluate; override;

  end;


implementation


{ TLetoScriptLibSql }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoScriptLibSql.Create;
begin
  inherited;
  Name			:= 'sql';

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoScriptLibSql.Destroy;
begin
  if Assigned(Sql) then begin

    {$IFDEF LETOSCRIPT_SQL_ZEOS}
    Sql.Connection.Free;
    {$ENDIF}

    {$IFDEF LETOSCRIPT_SQL_LETO}
    {$ENDIF}

    FreeAndNil(Sql);

  end;

  inherited;
end;

(*------------------------------------------------------------------------------
Contains

------------------------------------------------------------------------------*)
function TLetoScriptLibSql.Contains(
  const Name		: String;
  var Item		: TLibItem
): Boolean;
var
  I			: Integer;
begin
  inherited Contains(Name, Item);

  Result		:= IsConst(Name, SqlConsts, Item);
  if Result then Exit;

  I := GetEnumValue(TypeInfo(TLetoScriptFnSql), 'fnSql' + Name);
  Result		:= I > -1;
  if not Result then Exit;

  Item.Found		:= True;
  if TLetoScriptFnSql(I) in SqlListOps then
    Item.ItemType	:= itListOp
  else
    Item.ItemType	:= itUnaryOp;
  Item.AsFn		:= I;

end;

(*------------------------------------------------------------------------------
CreateFunc

------------------------------------------------------------------------------*)
function TLetoScriptLibSql.CreateFunc: Pointer;
begin
  Result		:= TLetoScriptFuncSql.Create(self);
end;


{ TLetoScriptFuncSql }


(*------------------------------------------------------------------------------
Ready

Abstracts SQL readiness.

------------------------------------------------------------------------------*)
function TLetoScriptFuncSql.Ready: Boolean;
begin

  Result :=

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
    Query.Connection.Connected and (Query.RecordCount > 0);
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
    Query.Connected and (Query.RecordCount > 0);
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
bindblob Table, Col, Where, Criteria

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._BindBlob;
var
  Col			: String;
  Q			: String;
begin

  GetArgs(['$table', '$col', '$where', '$criteria'], 4, []);

  Col			:= Args('col').AsString;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if Query.Connection.Database = '' then
    Exit;

  Q :=
    'SELECT ' + Col + ' ' +
    'FROM ' + Args('table').AsString + ' ' +
    'WHERE ' + Args('where').AsString + ' ' +
    '= ' + Args('criteria').AsString;

  Query.Close;
  Query.SQL.Text	:= Q;
  Query.RequestLive	:= True;
  Query.Open;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.BindBlob(
    Args('table').AsString,
    Col,
    Args('where').AsString,
    Args('criteria').AsString
  );
  Q := '';
  {$ENDIF}

  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
bof

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Bof;
begin
  if not Ready then Exit;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Token.Value		:= Query.Bof;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Token.Value		:= Query.Bof;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
connect

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Connect;
var
  User, Password	: String;
  Host			: String;
  Port			: Integer;
  Database, Proto	: String;
  DSN, Source		: String;
  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  C			: TZConnection;
  {$ENDIF}
begin
  GetArgs(
    ['$user', '$password', '$host', '$port'], 0,
    ['$database', '$protocol', '$dsn', '$datasource']
  );

  // These may cause warnings, depending on your compiler switch.
  User			:= Args('user', '').AsString;
  Password		:= Args('password', '').AsString;
  Host			:= Args('host', 'localhost', True).AsString;
  Port			:= Args('port', 3306).AsInt;
  Database		:= Args('database', '').AsString;
  DSN			:= Args('dsn', '').AsString;
  Source		:= Args('datasource', '').AsString;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Proto			:= Lowercase(Args('protocol', 'mysql').AsString);

  C			:= Query.Connection;
  C.Protocol		:= Proto;
  C.HostName		:= Host;
  C.User		:= User;
  C.Password		:= Password;
  C.Port		:= Port;
  C.Database		:= Database;
  C.Connect;
  Token.Value		:= C.Connected;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Proto			:= Lowercase(Args('protocol', 'sqlite3').AsString);

  if Proto = 'sqlite3' then
  begin
    if Assigned(Lib.Sql) then
      Lib.Sql.Free;
    Lib.Sql		:= TLetoSQLite.Create;
    Query		:= Lib.Sql;
    Query.Database	:= Database;
    Token.Value		:= Query.Connected;
  end

  else
    raise Exception.Create('Unsupported SQL protocol "' + Proto + '"');
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
disconnect

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Disconnect;
begin

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Query.Close;
  Query.Connection.Disconnect;
  Token.Value		:= not Query.Connection.Connected;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.Disconnect;
  Token.Value		:= not Query.Connected;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
eof

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Eof;
begin
  if not Ready then Exit;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Token.Value		:= Query.Eof;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Token.Value		:= Query.Eof;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
exec

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Exec;
begin

  // TODO 3: ADD: Integrated SQL formatting

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Query.SQL.Text	:= Operate.AsString;
  Query.ExecSQL;
  Token.Value		:= 1;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Token.Value		:= Query.Exec(Operate.AsString);
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
field
field EXPR

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Field;
var
  Val			: TLetoScriptValue;
begin
  if not Ready then Exit;

  Val			:= Operate;
  if Val.AsString = '' then
    Val.Value		:= 0;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if Val.IsNum then
    Token.Value		:= Query.Fields[Val.AsInt].AsVariant
  else
    Token.Value		:= Query.FieldValues[Val.AsString];
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if Val.IsNum then
    Token.Value		:= Query.FieldByCol(Val.AsInt)
  else
    Token.Value		:= Query.FieldByName(Val.AsString);
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
fieldnames

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._FieldNames;
var
  Names			: TStringList;
  I			: Integer;
begin
  InitList;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if not Query.Connection.Connected then Exit;

  Names			:= TStringList.Create;
  Query.GetFieldNames(Names);
  for I := 0 to Names.Count-1 do
    List.Add.Value	:= Names[I];
  FreeAndNil(Names);
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if not Query.Connected then Exit;

  Names			:= TStringList.Create;
  Query.FieldNames(Names);
  for I := 0 to Names.Count-1 do
    List.Add.Value	:= Names[I];
  FreeAndNil(Names);
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
fields
fields LIST

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Fields;
var
  Val, E		: TLetoScriptValue;
begin
  InitList;

  if not Ready then Exit;

  Val			:= Operate(ocList);

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if Val.IsList then begin
    Val.EachInit;
    E			:= Val.Each;
    while Assigned(E) do begin
      if E.IsNum then
        List.Add.Value	:= Query.Fields[E.AsInt].AsVariant
      else
        List.Add.Value	:= Query.FieldValues[E.AsString];
      E			:= Val.Each;
    end;
  end
  else
  if Val.IsNum then
    List.Add.Value	:= Query.Fields[Val.AsInt].AsVariant
  else
    List.Add.Value	:= Query.FieldValues[Val.AsString];
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if Val.IsList then begin
    Val.EachInit;
    E			:= Val.Each;
    while Assigned(E) do begin
      if E.IsNum then
        List.Add.Value	:= Query.FieldByCol(E.AsInt)
      else
        List.Add.Value	:= Query.FieldByName(E.AsString);
      E			:= Val.Each;
    end;
  end
  else
  if Val.IsNum then
    List.Add.Value	:= Query.FieldByCol(Val.AsInt)
  else
    List.Add.Value	:= Query.FieldByName(Val.AsString);
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
first

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._First;
begin
  if not Ready then Exit;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Query.First;
  Token.Value		:= 1;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.First;
  Token.Value		:= 1;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
insertid

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._InsertId;
begin

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  { Zeos doesn't implement (expose) this, currently. The result is always 0. }
  // TODO 4: BUG: No InsertId from Zeos.
  Token.Value		:= 0;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if not Query.Connected then Exit;
  Token.Value		:= Query.InsertId;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
last

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Last;
begin
  if not Ready then Exit;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Query.Last;
  Token.Value		:= 1;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.Last;
  Token.Value		:= 1;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
next

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Next;
begin
  if not Ready then Exit;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Query.Next;
  if not Query.Eof then
    Token.Value		:= 1;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.Next;
  if not Query.Eof then
    Token.Value		:= 1;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
params

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Params;
var
  Database		: String;
  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  C			: TZConnection;
  {$ENDIF}
begin
  GetArgs(
    [], 0,
    ['$database']
  );

  Database		:= Args('database', '').AsString;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  C			:= Query.Connection;
  C.Database		:= Database;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.Database	:= Database;
  {$ENDIF}

  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
prior

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Prior;
begin
  if not Ready then Exit;

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Query.Prior;
  if not Query.Bof then
    Token.Value		:= 1;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Query.Prior;
  if not Query.Bof then
    Token.Value		:= 1;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
query

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Query;
begin

  // TODO 3: ADD: Syntax for pulling query into array of hash
  {
    @col_names = query 'select * from foo', @table;
    $table[0 $col_name[0]] ...
  }

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if Query.Connection.Database = '' then
    Exit;

  Query.Close;
  Query.SQL.Text	:= Operate.AsString;
  Query.RequestLive	:= True;
  Query.Open;

  Token.Value		:= Query.RecordCount;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if Query.Query(Operate.AsString) then
    Token.Value		:= Query.RecordCount;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
recordcount

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._RecordCount;
begin

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if not Query.Connection.Connected then Exit;
  Token.Value		:= Query.RecordCount;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if not Query.Connected then Exit;
  Token.Value		:= Query.RecordCount;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
retrieve %HANDLE

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Retrieve;
var
  _Handle		: TLetoScriptValue;
  F			: TLetoFile;
  Blob			: TStream;
  H			: String;
begin
  if not Ready then Exit;

  GetArgs(['%handle!'], 1, []);

  _Handle		:= Args('handle');

  if TLetoScriptObj(_Handle).EvalDefined then
    TLetoScriptObj(_Handle).Exception(Err_LS_LiveHandle);

  try

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Blob := Query.CreateBlobStream( Query.Fields[0], bmRead );
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Blob			:= TMemoryStream.Create;
  Token.Value		:= Query.GetBlobStream(Blob);
  if not Token.Value then Exit;
  {$ENDIF}

  F			:= TLetoFile.Create(ftGff, '');
  Token.Env.IoErr	:= F.Gff.LoadFromStream(Blob);
  if Token.Env.IoErr <> Success then Exit;
  H			:= TLetoScriptObj(_Handle).Text;
  H			:= StringReplace(H, '%', '', []);
  Token.Env.Handle[H]	:= F;

  finally
    FreeAndNil(Blob);
  end;

  Token.Value		:= 1;

end;

(*------------------------------------------------------------------------------
rowsaffected

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._RowsAffected;
begin

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  if not Query.Connection.Connected then Exit;
  Token.Value		:= Query.RowsAffected;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  if not Query.Connected then Exit;
  Token.Value		:= Query.RowsAffected;
  {$ENDIF}

end;

(*------------------------------------------------------------------------------
store %HANDLE

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql._Store;
var
  _Handle		: TLetoScriptValue;
  F			: TLetoFile;
  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Blob, Res		: TStream;
  {$ENDIF}
  {$IFDEF LETOSCRIPT_SQL_LETO}
  Blob			: TMemoryStream;
  {$ENDIF}
begin
  if not Ready then Exit;

  GetArgs(['%handle'], 0, []);
  _Handle		:= Args('handle');
  F			:= _Handle.AsFile;
  if not Assigned(F) then
    TLetoScriptObj(_Handle).Exception(Err_LS_InvalidFile, Text { <-- add this } );

  {$IFDEF LETOSCRIPT_SQL_ZEOS}
  Blob := Query.CreateBlobStream( Query.Fields[0], bmWrite );
  try
    Res			:= TMemoryStream.Create;
    try
      F.Gff.SaveToStream(Res);
      Blob.CopyFrom(Res, 0);
    finally
      FreeAndNil(Res);
    end;
  finally
    FreeAndNil(Blob);
  end;

  if Query.State <> dsEdit then
    Query.Edit;

  Query.Post;
  Query.ApplyUpdates;
  Query.CommitUpdates;
  {$ENDIF}

  {$IFDEF LETOSCRIPT_SQL_LETO}
  Blob			:= TMemoryStream.Create;
  try
    F.Gff.SaveToStream(Blob);
    Query.SaveBlobStream(Blob);
  finally
    FreeAndNil(Blob);
  end;
  {$ENDIF}

  Token.Value		:= 1;

  { %ERF['foo.nss'] }


end;

(*------------------------------------------------------------------------------
Evaluate

------------------------------------------------------------------------------*)
procedure TLetoScriptFuncSql.Evaluate;
begin
  inherited;
  Lib			:= TLetoScriptLibSql(Parent);

  if not Assigned(Lib.Sql) then begin

    {$IFDEF LETOSCRIPT_SQL_ZEOS}
    Lib.Sql		:= TZQuery.Create(nil);
    Query		:= Lib.Sql;
    Query.Connection	:= TZConnection.Create(nil);
    {$ENDIF}

    {$IFDEF LETOSCRIPT_SQL_LETO}
      // Query is constructed based on protocol choice, in Connect.
    {$ENDIF}

  end else
    Query		:= Lib.Sql;

  try
    case TLetoScriptFnSql(Fn) of
      fnSqlBindBlob:	_BindBlob;
      fnSqlBof:		_Bof;
      fnSqlConnect:	_Connect;
      fnSqlDisconnect:	_Disconnect;
      fnSqlEof:		_Eof;
      fnSqlExec:	_Exec;
      fnSqlField:	_Field;
      fnSqlFieldNames:	_FieldNames;
      fnSqlFields:	_Fields;
      fnSqlFirst:	_First;
      fnSqlInsertId:	_InsertId;
      fnSqlLast:	_Last;
      fnSqlNext:	_Next;
      fnSqlParams:	_Params;
      fnSqlPrior:	_Prior;
      fnSqlQuery:	_Query;
      fnSqlRecordCount:	_RecordCount;
      fnSqlRetrieve:	_Retrieve;
      fnSqlRowsAffected: _RowsAffected;
      fnSqlStore:	_Store;
    end;
  except
    on E: Exception do begin
      Token.Env.Vars['$!'].Value := E.Message;
      Token.Defined	:= False;
    end;
  end;

end;


{$ENDIF} // LETOSCRIPT_SQL (unit-wide)


(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
