import std/[asyncdispatch, net, macros]
from std/sugar import dump, `=>`
from std/endians import bigEndian64, bigEndian32, bigEndian16
from std/strformat import fmt
from std/md5 import getMd5
from std/asyncnet import AsyncSocket, newAsyncSocket, connect, close, recv,
     send, isClosed 
when defined(ssl):
  from std/asyncnet import wrapConnectedSocket
  from std/net import newContext, SslCVerifyMode, SslHandshakeType
from std/strutils import parseUint, parseInt, parseFloat, parseBool, join
from std/sequtils import map, repeat
from std/tables import TableRef, newTable, `[]=`, `[]`, `$`, pairs
import std/[typeinfo, times]

from nimSHA2 import SHA256Digest
from scram/client import newScramClient, prepareFirstMessage, prepareFinalMessage

import multisock

const psqlProt = 196608'i32

template writeBE*[T](s: pointer, data: T) =
  when cpuEndian == bigEndian:
    let d = data
    copyMem(s, unsafeAddr d, sizeof data)
  else:
    var be: T
    var old = data
    when sizeof(data) == 8:
      let toBe = bigEndian64
    elif sizeof(data) == 4:
      let toBe = bigEndian32
    elif sizeof(data) == 2:
      let toBe = bigEndian16
    toBe(addr be, addr old)
    copyMem(s, addr be, sizeof data)

template readBE*(s: pointer, which: typedesc): untyped =
  when cpuEndian == bigEndian:
    var r: which
    copyMem(addr r, s, sizeof r)
    r
  else:
    var r, v: which
    when sizeof(v) == 8:
      let biggy = bigEndian64
    elif sizeof(v) == 4:
      let biggy = bigEndian32
    elif sizeof(v) == 2:
      let biggy = bigEndian16
    copyMem(addr v, s, sizeof v)
    biggy(addr r, addr v)
    r

type
  BackendCommand* = enum
    NoCommandBe = 0.byte
    ParseComplete = byte '1'
    BindComplete = '2'
    CloseComplete = '3'
    NotificationResponse = 'A'
    CommandComplete = 'C'
    DataRow = 'D'
    ErrorResponse = 'E'
    CopyInResponse = 'G'
    CopyOutResponse = 'H'
    EmptyQueryResponse = 'I'
    KeyData = 'K'
    NoticeResponse = 'N'
    AuthCmd = 'R'
    ParameterStatus = 'S'
    RowDescription = 'T'
    FunctionCallResponse = 'V'
    CopyBothResponse = 'W'
    ReadyForQuery = 'Z'
    CopyDoneBe = 'c'
    CopyDataBe = 'd'
    NoData = 'n'
    PortalSuspended = 's'
    ParameterDescription = 't'
    NegotiateProtocolVersion = 'v'

  FrontendCommand* = enum
    NoCommandFe = 0.byte
    Bind = byte 'B'
    # CancelRequest?
    Close = 'C'
    Describe = 'D'
    Execute = 'E'
    FunctionCall = 'F'
    Flush = 'H'
    Parse = 'P'
    Query = 'Q'
    Sync = 'S'
    Terminate = 'X'
    CopyDoneFe = 'c'
    CopyDataFe = 'd'
    CopyFail = 'f'
    PasswordMessage = 'p' # also used in GSSAPI, SSPI and SASL response
    # SslRequest?
    # GssEncRequest?
    # StartupMessage?

  AuthKind* {.size: sizeof(int32).} = enum
    authOk = (0, "AuthenticationOk")
    authKerberosV5 = (2, "AuthenticationKerberosV5")
    authClearText = (3, "AuthentiationClearTextPassword")
    authMd5 = (5, "AuthenticationMD5Password")
    authScmCred = (6, "AuthenticationSCMCredential")
    authGss = (7, "AuthenticationGSS")
    authGssCont = (8, "AuthenticationGSSContinue")
    authSspi = (9, "AuthenticationSSPI")
    authSasl = (10, "AuthenticationSASL")
    authSaslCont = (11, "AuthenticationSASLContinue")
    authSaslFinal = (12, "AuthenticationSASLFinal")

  Byten* = distinct string
  CmdTag* = distinct string
  ErrorKind* = distinct char
  NoticeKind* = distinct char
  Indicator* = distinct char

  CopyFormat* = enum
    cfText cfBinary
  FormatCode* = CopyFormat
  PgTypeLen = distinct int16
  PgAttypeMod = distinct int32

  FieldInfo* = object
    name*: string
    tableId*: int32
    attrNum*: int16
    id*: int32
    size*: PgTypeLen
    typeModifier*: PgAttypeMod
    format: FormatCode

  MessageBackend* = object
    case kind*: BackendCommand
    of NoCommandBe: discard
    of ParseComplete: discard
    of BindComplete: discard
    of CloseComplete: discard
    of NotificationResponse:
      notifId*: int32
      channelName*: string
      payload*: string
    of CommandComplete:
      tag*: CmdTag
      oid*: int
      rows*: int64
    of DataRow:
      totalColumn*: int16
      values*: seq[Byten]
    of ErrorResponse:
      errors*: seq[(ErrorKind, string)]
      #errmsg*: string
    of CopyInResponse:
      inFormat*: CopyFormat
      inFormats*: seq[CopyFormat]
    of CopyOutResponse:
      outFormat*: CopyFormat
      outFormats*: seq[CopyFormat]
    of EmptyQueryResponse: discard
    of KeyData:
      processId*: int32
      secret*: int32
    of NoticeResponse:
      notices*: seq[(NoticeKind, string)]
    of AuthCmd:
      case authKind*: AuthKind
      of authOk: discard
      of authKerberosV5: discard
      of authClearText: discard
      of authMd5:
        salt*: Byten
      of authScmCred: discard
      of authGss: discard
      of authGssCont:
        gssapi*: Byten
      of authSspi: discard
      of authSasl:
        saslMech*: string
      of authSaslCont:
        saslData*: Byten
      of authSaslFinal:
        saslFinal*: Byten
    of ParameterStatus:
      runtimeParam*: string
      statusValue*: string
    of RowDescription:
      fields*: seq[FieldInfo]
    of FunctionCallResponse:
      functionResult*: Byten
    of CopyBothResponse:
      bothFormat*: CopyFormat
      bothFormats*: seq[CopyFormat]
    of ReadyForQuery:
      indicator*: Indicator
    of CopyDoneBe:
      copyDone*: Byten
    of CopyDataBE:
      copyData*: Byten
    of NoData: discard
    of PortalSuspended: discard
    of ParameterDescription:
      parameters*: seq[int32]
    of NegotiateProtocolVersion: discard

  MessageFrontend* = object
    case kind*: FrontendCommand
    of NoCommandFe: discard
    of Bind: discard
    of Close: discard
    of Describe: discard
    of Execute: discard
    of FunctionCall: discard
    of Flush: discard
    of Parse: discard
    of Query: discard
    of Sync: discard
    of Terminate: discard
    of CopyDoneFE: discard
    of CopyDataFE:
      copyData*: Byten
    of CopyFail: discard
    of PasswordMessage: discard

  LoginResult* = object
    msg*: MessageBackend
    processId*: int32
    secret*: int32
    success*: bool
    reason*: string
    options*: TableRef[string, string]

proc `$`*(b: Byten): string {.borrow.}
proc `$`*(c: CmdTag): string {.borrow.}
proc `$`*(e: ErrorKind): string {.borrow.}
proc `$`*(n: NoticeKind): string {.borrow.}
proc `$`*(i: Indicator): string {.borrow.}
proc `$`*(p: PgTypeLen): string {.borrow.}
proc `$`*(p: PgAttypeMod): string {.borrow.}
proc len*(b: Byten): int {.borrow.}

template getMsgLen(sock: AsyncSocket|Socket): untyped =
  when sock is AsyncSocket:
    let lenmsg = await sock.recv(4)
  else:
    let lenmsg = sock.recv(4)
  let reslen = (unsafeAddr lenmsg[0]).readBe(int32)
  when sock is AsyncSocket:
    let restmsg = await sock.recv(reslen - 4)
  else:
    let restmsg = sock.recv(reslen - 4)
  (reslen, restmsg)

proc parseAuth(sock: AsyncSocket): Future[MessageBackend]{.multisock.} =
  let
    (_, restmsg) = sock.getMsgLen
    authkind = (unsafeAddr restmsg[0]).readBe(int32).AuthKind

  result = MessageBackend(
    kind: AuthCmd,
    authKind: authkind,
  )

  case authkind
  of authMd5:
    result.salt = Byten restmsg[4 .. ^1]
  of authGssCont:
    result.gssapi = Byten restmsg[4 .. ^1]
  of authSasl:
    result.saslMech = restmsg[4 .. ^2]
  of authSaslCont:
    result.saslData = Byten restmsg[4 .. ^1]
  of authSaslFinal:
    result.saslFinal = Byten restmsg[4 .. ^1]
  of authOk, authKerberosV5, authClearText, authScmCred,
     authGss, authSSpi:
    discard

proc parseKeyData(sock: AsyncSocket): Future[MessageBackend]{.multisock.} =
  let (_, restmsg) = sock.getMsgLen
  result = MessageBackend(
    kind: KeyData,
    processId: (unsafeAddr restmsg[0]).readBe(int32),
    secret: (unsafeAddr restmsg[4]).readBe(int32),
  )

proc parseCommandComplete(sock: AsyncSocket): Future[MessageBackend]{.multisock.} =
  var (_, restmsg) = sock.getMsgLen

  #result = MessageBackend(kind: CommandComplete)
  template fetchRows(taglen: int, add = 0) : untyped =
    let payload = restmsg[taglen .. ^2]
    var r: int
    try:
      r = parseInt payload
    except CatchableError:
      echo "got exception:", getCurrentExceptionMsg()
      r = 0
    r
  var readto = 1
  var tag: CmdTag
  case restmsg[0]
  of 'I':
    tag = CmdTag "INSERT"
    readto = 3
  of 'D': tag = CmdTag "DELETE"
  of 'U': tag = CmdTag "UPDATE"
  of 'S': tag = CmdTag "SELECT"
  of 'M': tag = CmdTag "MOVE"
  of 'F': tag = CmdTag "FETCH"
  of 'C': tag = CmdTag "COPY"
  else: discard
  let rows = fetchRows(tag.string.len + readto)
  result = MessageBackend(
    kind: CommandComplete,
    tag: tag,
    rows: rows)

proc parseCopyData[T: MessageBackend|MessageFrontend](sock: AsyncSocket):
  Future[T] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen

  when T is MessageBackend:
    result = MessageBackend(kind: CopyDataBe, copyData: Byten restmsg)
  else:
    result = MessageBackend(kind: CopyDataFe, copyData: Byten restmsg)


template copybodyResponse(restmsg: string, field1, field2: untyped): untyped =
  `field1` = restmsg[0].CopyFormat
  let colen = (addr restmsg[1]).readBe(int16)
  `field2` = newseq[CopyFormat](colen)
  for i, f in `field2`.mpairs:
    f = (addr restmsg[2 + i*2]).readBe(int16).CopyFormat

proc parseCopyInResponse(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  result = MessageBackend(kind: CopyInResponse)
  copybodyResponse(restmsg, result.inFormat, result.inFormats)

proc parseCopyOutResponse(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  result = MessageBackend(kind: CopyOutResponse)
  copybodyResponse(restmsg, result.outFormat, result.outFormats)

proc parseCopyBothResponse(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  result = MessageBackend(kind: CopyBothResponse)
  copybodyResponse(restmsg, result.bothFormat, result.bothFormats)


proc parseDataRow(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  #[
  result = MessageBackend( kind: DataRow )
  result.totalColumn = (addr restmsg[0]).readBe(int16)
  result.values = newseq[Byten](result.totalColumn)
  ]#
  let totalColumn = (addr restmsg[0]).readBe(int16)
  var values = newseq[Byten](totalColumn)
  var currpos = 2
  for i in 0 .. values.high:
    let bytelen = (addr restmsg[currpos]).readBe(int32); currpos += 4
    if bytelen == -1:
      values[i] = Byten "NULL"
      continue
    values[i] = Byten restmsg[currpos ..< currpos + bytelen]
    currpos += bytelen

  result = MessageBackend(
    kind: DataRow,
    totalColumn: totalColumn,
    values: values,
  )

template errNoticeResp(restmsg: string, which: typedesc, field: untyped): untyped =
  `field` = @[]
  var currpos = 0
  while currpos < restmsg.len and restmsg[currpos] != '\0':
    let kind = which restmsg[currpos]
    inc currpos
    var msg = ""
    while restmsg[currpos] != '\0':
      msg &= restmsg[currpos]
      inc currpos
    inc currpos # discard last null
    `field`.add (kind, msg)

proc parseErrorResponse(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  result = MessageBackend(kind: ErrorResponse)
  var (_, restmsg) = sock.getMsgLen
  errNoticeResp(restmsg, ErrorKind, result.errors)

proc parseNoticeResponse(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  result = MessageBackend(kind: NoticeResponse)
  var (_, restmsg) = sock.getMsgLen
  errNoticeResp(restmsg, NoticeKind, result.notices)

proc parseFunctionCallResponse(sock: AsyncSocket): Future[MessageBackend]{.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  let reslen = (addr restmsg[0]).readBe(int32)
  result = MessageBackend( kind: FunctionCallResponse )
  if reslen < 0:
    result.functionResult = Byten "NULL"
    return
  result.functionResult = Byten restmsg[sizeof(int32) ..< sizeof(int32) + reslen]

proc retrieveString(s: string, start: int): (string, int) =
  var cur = start
  var str = ""
  while cur < s.len and s[cur] != '\0':
    str &= s[cur]
    inc cur
  inc cur
  (str, cur)

proc parseNotificationResponse(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  #[
  result = MessageBackend(
    kind: NotificationResponse,
    notifId: (addr restmsg[0]).readBe(int32)
  )
  var curr = 4
  (result.channelName, curr) = restmsg.retrieveString curr
  (result.payload, curr) = restmsg.retrieveString curr
  ]#
  let notifId = (addr restmsg[0]).readBe(int32)
  var
    curr = 4
    channame = ""
    payload = ""
  (chanName, curr) = restmsg.retrieveString curr
  (payload, curr) = restmsg.retrieveString curr
  result = MessageBackend(
    kind: NotificationResponse,
    notifId: notifId,
    channelName: chanName,
    payload: payload,
  )


proc parseParameterDesc(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  result = MessageBackend(kind: ParameterDescription)
  let paramlen = (addr restmsg[0]).readBe(int16)
  result.parameters = newseq[int32](paramlen)
  for i in 0 .. result.parameters.high:
    result.parameters[i] = (addr restmsg[4+i*4]).readBe(int32)

proc parseParameterStat(sock: AsyncSocket): Future[MessageBackend]{.multisock.} =
  var (_, restmsg) = sock.getMsgLen
  result = MessageBackend(kind: ParameterStatus)
  var curr = 0
  (result.runtimeParam, curr) = restmsg.retrieveString curr
  (result.statusValue, curr) = restmsg.retrieveString curr

proc parseRowDesc(sock: AsyncSocket): Future[MessageBackend] {.multisock.} =
  var (_, msg) = sock.getMsgLen
  result = MessageBackend(kind: RowDescription)
  var elem = (addr msg[0]).readBe(int16)
  result.fields = newseq[FieldInfo](elem)
  var cur = 2
  for i in 0 ..< elem:
    var field = FieldInfo()
    (field.name, cur) = msg.retrieveString(cur)
    field.tableId = (addr msg[cur]).readBe(int32); cur += 4
    field.attrNum = (addr msg[cur]).readBe(int16); cur += 2
    field.id = (addr msg[cur]).readBe(int32); cur += 4
    field.size = PgTypeLen (addr msg[cur]).readBe(int16); cur += 2
    field.typeModifier = PgAttypeMod (addr msg[cur]).readBe(int32); cur += 4
    field.format = if (addr msg[cur]).readBe(int16) == 0: cfText else: cfBinary
    cur += 2
    result.fields[i] = field

proc parseReply(sock: AsyncSocket): Future[MessageBackend]{.multisock.} =
  let cmd = await sock.recv(1)
  if cmd.len < 1:
    result = MessageBackend( kind: NoCommandBe )
    return
  let cmdkind = cmd[0].BackendCommand
  result = MessageBackend(kind: cmdkind)
  case cmdkind
  of AuthCmd:
    result = await parseAuth(sock)
  of KeyData:
    result = await parseKeyData(sock)
  of BindComplete, CloseComplete, CopyDoneBe, EmptyQueryResponse,
     NoData, ParseComplete, PortalSuspended:
    discard await sock.recv(4)
  of CommandComplete:
    result = await parseCommandComplete(sock)
  of CopyDataBe:
    result = await sock.parseCopyData[:MessageBackend]
  of CopyInResponse:
    result = await sock.parseCopyInResponse()
  of CopyOutResponse:
    result = await sock.parseCopyOutResponse()
  of CopyBothResponse:
    result = await sock.parseCopyBothResponse()
  of DataRow:
    result = await sock.parseDataRow()
  of ErrorResponse:
    result = await sock.parseErrorResponse()
  of FunctionCallResponse:
    result = await sock.parseFunctionCallResponse()
  of NoticeResponse:
    result = await sock.parseNoticeResponse()
  of NotificationResponse:
    result = await sock.parseNotificationResponse()
  of ParameterDescription:
    result = await sock.parseParameterDesc()
  of ParameterStatus:
    result = await sock.parseParameterStat()
  of ReadyForQuery:
    var (_, restmsg) = sock.getMsgLen
    result.indicator = Indicator restmsg[0]
  of RowDescription:
    result = await sock.parseRowDesc()
  else:
    result = MessageBackend(
      kind: ErrorResponse,
      errors: @[(ErrorKind '0', "missing command kind available")],
    )

proc writeString*(s: var string, str: string): int =
  s &= str & '\0'
  result = str.len + 1

proc genConnOpt*(s: var string, user: string, database = user, options = "",
                    replication = ""): int =
  const
    ufield = "user"
    dbfield = "database"
    optfield = "options"
    replfield = "replication"
  for sk in [ufield, user, dbfield, database]:
    result += s.writeString sk
  if options != "":
    result += s.writeString optfield
    result += s.writeString options
  if replication != "":
    result += s.writeString replfield
    result += s.writeString replication
  s &= '\0'; result += 1

proc genPasswordMessage*(body: string): (string, int) =
  var s = ""
  s &= char PasswordMessage
  var length = 0
  s.setLen(5)
  #copyMem(addr s[1], addr length, 4); length += 4
  (addr s[1]).writeBe(length.int32); length += 4
  length += s.writeString body
  (addr s[1]).writeBE length.int32
  result = (s, length)

proc genStartupMsg*(user: string, database = user, options = "",
                    replication = ""): (string, int) =
  var s = ""
  var length = 0
  s.setLen(8); length += 4
  (addr s[4]).writeBE psqlProt; length += 4
  length += s.genConnOpt(user, database, options, replication)
  (addr s[0]).writeBE length.int32
  result = (s, length)

proc genMd5Body(username, password, salt: string): string =
  "md5" & getMd5(getMd5(password & username) & salt)

proc genQuery*(query: string): string =
  result &= Query.char
  let length = 4 + query.len + 1
  result.setlen(length)
  (addr result[1]).writeBe(length.int32)
  result[5 .. ^2] = query
  result[^1] = '\0'

proc genFlush: string =
  result.setLen 5
  result[0] = char Flush
  (addr result[1]).writeBe(4'i32)

func fetchError(r: MessageBackend): string =
  r.errors.map((it) => fmt"Error tag {it[0]}: {it[1]}").join("\n")

proc saslInitResp(mech, resp: string): string =
  result &= PasswordMessage.char
  # SASLInitialResponse is:
  # Byte('p'): not counted
  # Int32 = 4 byte = length total
  # String = mech.len + null terminator
  # Int32 = 4 byte = resp length
  # Byten = string resp.len
  var length = 4 + mech.len + 1 + 4 + resp.len
  result.setLen(1 + length)
  (addr result[1]).writeBe(length.int32)
  #discard result.writeString mech
  var curr = 1 + 4
  copyMem(addr result[curr], unsafeAddr mech[0], mech.len)
  curr += mech.len
  result[curr] = '\0'
  inc curr
  (addr result[curr]).writeBe(resp.len.int32)
  curr += 4
  copyMem(addr result[curr], unsafeAddr resp[0], resp.len)

proc saslResp(msg: string): string =
  var length = 4 + msg.len
  result = newstring(1 + 4 + msg.len)
  result[0] = PasswordMessage.char
  (addr result[1]).writeBe(length.int32)
  copyMem(addr result[5], unsafeAddr msg[0], msg.len)

proc loginAuth*(sock: AsyncSocket, startupMsg, username, password: string):
  Future[LoginResult] {.multisock.} =
  asyncCheck sock.send startupMsg
  when defined(gresqlinVerbose): echo "startupMsg sent"
  result.options = newTable[string,string]()
  var reply = await sock.parseReply
  var scram = newScramClient[Sha256Digest]()
  if reply.kind != AuthCmd:
    result.reason = fmt"not auth cmd, got {reply.kind}"
    return
  while reply.kind != ReadyForQuery:
    if reply.kind == AuthCmd:
      case reply.authKind
      of authMd5:
        let
          bodypass = genMd5Body(username, password, string reply.salt)
          (passmsg, _) = genPasswordMessage(bodypass)
        asyncCheck sock.send passmsg
      of authSasl:
        let msg = scram.prepareFirstMessage(username)
        asyncCheck sock.send saslInitResp(reply.saslMech, msg)
      of authSaslCont:
        let fin = scram.prepareFinalMessage(password, reply.saslData.string)
        asyncCheck sock.send saslResp(fin)
      of authSaslFinal:
        when defined(gresqlinVerbose): dump reply.saslFinal
      of authClearText:
        let (body, _) = genPasswordMessage password
        asyncCheck sock.send body
      of authOk: discard
      else:
        when defined(gresqlinVerbose):
          echo fmt"reply auth-kind: {reply.authKind}"
        result.reason = "only supported salted-md5, scram-sha-256 and clear-text"
        return
    elif reply.kind == ErrorResponse:
      result.reason = reply.fetchError
      return
    elif reply.kind == ParameterStatus:
      when defined(gresqlinVerbose):
        echo fmt"parameter status runtime '{reply.runtimeParam}' " &
          fmt"with status '{reply.statusValue}'"
      result.options[reply.runtimeParam] = reply.statusValue
    elif reply.kind == KeyData:
      result.processId = reply.processId
      result.secret = reply.secret
    else:
      result.reason = "got unrecognized command {reply.kind}"

    reply = await sock.parseReply

  result.msg = reply
  result.success = true

proc convert(fields: seq[FieldInfo], values: seq[Byten],
             args: varargs[Any, toAny]): string =
  if fields.len != args.len:
    result = fmt"expected {fields.len} length but provided {args.len} length"
    return

  for i, field in fields:
    var x = args[i]
    #dump field
    if field.format == cfText:
      case x.kind
      of akInt .. akInt64:
        var v = try: parseInt(values[i].string) except: 0
        x.setBiggestInt v
      of akUint .. akUint64:
        var v = try: parseUint(values[i].string) except: 0
        x.setBiggestUint v
      of akFloat .. akFloat128:
        var v = try: parseFloat(values[i].string) except: 0
        x.setBiggestFloat v
      of akString: x.setString values[i].string
      of akBool:
        var v = try: parseBool(values[i].string) except: false
        x.assign v.toAny
      of akChar:
        var v = values[i].string[0]
        x.assign v.toAny
      else: discard

proc fetchQuery*(sock: AsyncSocket, query: string):
  Future[(seq[seq[string]], string)] {.multisock.} =
  asyncCheck sock.send query
  var
    fields: seq[FieldInfo]
    reply = await sock.parseReply
  while reply.kind != ReadyForQuery:
    if reply.kind == ErrorResponse:
      result[1] = reply.fetchError
      return
    elif reply.kind == NoCommandBe:
      result[1] = "invalid: no command tag"
      return
      #dump reply
      #discard
    elif reply.kind == RowDescription: fields = reply.fields
    elif reply.kind == DataRow:
      #var v: (int32, string, float32, string)
      #var errmsg = fields.convert(reply.values, v[0], v[1], v[2], v[3])
      #if errmsg != "": echo errmsg
      #result[0].add v
      result[0].add reply.values.map((it) => string(it))
    reply = await sock.parseReply

proc execQuery*(sock: AsyncSocket, query: string): Future[MessageBackend] {.multisock.} =
  asyncCheck sock.send query
  result = await sock.parseReply

proc sslRequest*(): string =
  result = newstring 8
  (addr result[0]).writeBe(8'i32)
  (addr result[4]).writeBe(80877103'i32)

when defined(ssl):
  proc enquireSsl*(sock: AsyncSocket, host: string) {.multisock.} =
    asyncCheck sock.send sslRequest()
    let abyte = waitfor sock.recv(1)
    when defined(gresqlinVerbose): dump abyte
    if abyte.len > 0 and abyte[0] != 'S':
      when defined(gresqlinVerbose): echo "no ssl"
      return
    when defined(gresqlinVerbose): echo "ssl ok"
    let ctx = newContext(verifyMode = CVerifyNone)
    ctx.wrapConnectedSocket(sock, handshakeAsClient, host)

when isMainModule and not defined(dryrun):
  var
    #username = "postgres"
    username = "pquser"

  proc main(socket: AsyncSocket): Future[void] {.multisock.} =
    var
      #socket = newAsyncSocket()
      host = "127.0.0.1"
    asyncCheck socket.connect(host, Port 5432)
    when defined(ssl):
      asyncCheck socket.enquireSsl(host)
    let (startupMsg, length) = genStartupMsg(username, database = "pqdb")
    dump startupMsg
    dump length
    #var (reply, idprocess, secret, success, errors, servercfg) =
    var loginres =
      await socket.loginAuth(startupMsg, username, "psqlpass")
    if not loginres.success: quit loginres.reason
    echo fmt"ready for query indicator: '{char loginres.msg.indicator}'"
    #let querynow = genQuery "select now();"
    #let querynow = genQuery "select 1.5 \"one-half\";"
    #(reply, errors) = await socket.execQuery genQuery(
    #  "alter table local.gresqlin add column if not exists created_at timestamp with time zone default now()")
    #dump errors
    var reply = loginres.msg
    reply = await socket.execQuery genQuery(
      fmt"insert into local.gresqlin(msg, boring_float, created_at) values('hehe insert: {now()}', 44.22, '{$now()}')")
    dump reply
    let querynow = genQuery "select * from local.gresqlin;"
    dump querynow
    let (rows, reason) = await socket.fetchQuery querynow
    if reason != "":
      echo reason
    else:
      for i, row in rows:
        echo fmt"row {i:02}: {row}"
    close socket

  waitfor main(newAsyncSocket())
  main(newSocket())
