" This file contains the code necessary to talk to the scion server
" -> haskellcomplete#EvalScion )
"
" This implementation requires has('python') support
"
" You can look up some use cases in the ftplugin file.
"
" This code is based on the initial implementation found in shim by Benedikt Schmidt
" The server side code can be found in src-scion-server/Scion/Server/ProtocolVim.hs

if exists('g:tovl')
  fun! s:Log(level, msg)
    call tovl#log#Log("haskellcomplete",a:level, a:msg)
  endf
else
  fun! s:Log(level, msg)
    echoe a:msg
  endf
endif

" require python or exit
if !has('python') | call s:Log(0, "Error: scion requires vim compiled with +python") | finish | endif

let g:vim_scion_protocol_version = "0"

" use this to connect to a socket
" py scionConnectionSetting = "/tmp/scion-io"

" returns string part before and after cursor
function! haskellcomplete#BcAc()
  let pos = col('.') -1
  let line = getline('.')
  return [strpart(line,0,pos), strpart(line, pos, len(line)-pos)]
endfunction

" completion functions
if !exists('g:haskellcompleteAll')
  let g:haskellcompleteAll='' " '' or '-all'  '-all' means complete from the set of all function exported by all modules found in all used packages
endif

fun! haskellcomplete#CompletModule(findstart, base)
  if a:findstart
    let [bc,ac] = haskellcomplete#BcAc()
    return len(bc)-len(matchstr(bc,'\S*$'))
  else
    let [bc,ac] = haskellcomplete#BcAc()
    let addImport = bc !~ 'import\s\+\S*$'
    let matches = haskellcomplete#EvalScion(
      \ { 'request' : 'cmdModuleCompletion'
      \ , 'camelCase' : 'True'
      \ , 'short' : a:base
      \ })
    if addImport
      call map(matches, string('import ').'.v:val')
    endif
    return matches
  endif
endf

" example: echo haskellcomplete#EvalScion({'request' : 'cmdConnectionInfo', 'file' : 'test.hs'})
function! haskellcomplete#EvalScion(request)
  " the first string converts the vim object into a string, the second
  " converts this string into a python string
  let g:scion_arg = string(a:request)
  py evalscionAssign(vim.eval('g:scion_arg'))
  " warnings
  for w in get(g:scion_result, 'warnings', [])
    call s:Log(1, w) | echo w
  endfor
  " errors
  if has_key(g:scion_result,'error')
    call s:Log(0, g:scion_result['error'])
    throw "There was a scion server error :".g:scion_result['error']
  else
    return g:scion_result['result']
  endif
endfunction

function! s:DefPython()
python << PYTHONEOF
import sys, tokenize, cStringIO, types, socket, string, vim, popen2
from subprocess import Popen, PIPE


class ScionServerConnection:
  """base of a server connection. They all provide two methods: send and receive bothe sending or receiving a single line separated by \\n"""
  def send(self, line):
    self.scion_i.write("%s\n"%line)
    self.scion_i.flush()
  def receive(self):
    return self.scion_o.readline()[:-1]

class ScionServerConnectionStdinOut(ScionServerConnection):
  """this connection launches the server and connects to its stdin and stdout streams"""
  def __init__(self, scion_executable):
    #self.scion_o,self.scion_i,e = popen2.popen3('%s -i -f /tmp/scion-log'%(scion_executable))
    p = Popen([scion_executable,"-i","-f", "/tmp/scion-log"], shell = False, bufsize = 1, stdin = PIPE, stdout = PIPE, stderr = PIPE)
    self.scion_o = p.stdout
    self.scion_i = p.stdin
  def receive(self):
    s = ScionServerConnection.receive(self)
    if s[:6] == "scion:":
      # ghc doesn't always use the ghc API to print statements.. so ignore all
      # lines not marked by "scion:" at the beginning
      # see README.markdown
      return s[6:]
    else:
      # throw away non "scion:" line and try again
      return self.receive()

class ScionServerConnectionSocket(ScionServerConnection):
  """connects to the scion server by either TCP/IP or socketfile"""
  def __init__(self, connection):
    # connection either (host, port) or (socketfile)
    if type(connection) == type((0,0)):
      # tuple -> host, port
      su = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    else: # must be path -> file socket
      su = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    su.settimeout(10)
    su.connect(connection)
    # making file to use readline()
    self.scion_o = su.makefile('rw')
    self.scion_i = self.scion_o

server_connection = None
told_user_about_missing_configuration = 0
lastScionResult = "";
def connectscion():
    # check that connection method has been defined
    global server_connection
    global told_user_about_missing_configuration
    if 0 == told_user_about_missing_configuration:
      try:
        print "connecting to scion %s"%scionConnectionSetting.__str__()
      except NameError:
        vim.command("sp")
        b = vim.current.buffer
        b.append( "you haven't defined scionConnectionSetting")
        b.append( "Do so by adding one of the following lines to your .vimrc:")
        b.append( "TCP/IP, socket, stdio")
        b.append( "py scionConnectionSetting = ('socket', \"socket file location\") # socket connection")
        b.append( "py scionConnectionSetting = ('socket', (127.0.0.1', 4005)) # host, port TCIP/IP connection")
        b.append( "py scionConnectionSetting = ('scion', \"scion_server location\") # stdio connection ")
        told_user_about_missing_configuration = 1

    if scionConnectionSetting[0] == "socket":
      server_connection = ScionServerConnectionSocket(scionConnectionSetting[1])
    else:
        server_connection = ScionServerConnectionStdinOut(scionConnectionSetting[1])

    # handshake
    server_connection.send("select scion-server protocol:vim %s" % vim.eval('g:vim_scion_protocol_version'))
    res = server_connection.receive()
    if res != "ok":
      raise Exception("failed connecting to scion Reason: `%s'" % res)

# sends a command and returns the returned line
def evalscion(str):
    global server_connection
    try:
      server_connection.send(str)
    except:
      vim.command('echom "%s"'% ("(re)connecting to scion"))
      connectscion()
      server_connection.send(str)
    return server_connection.receive()

# str see EvalScion
def evalscionAssign(str):
  global lastScionResult
  """assigns scion result to g:scion_result, result should either be 
    { "result" : ..., "error" : [String] }"""
  vim.command("silent! unlet g:scion_result")
  lastScionResult = evalscion(str)
  vim.command("silent! let g:scion_result = %s" % lastScionResult)
  vim.command("if !exists('g:scion_result') | let g:scion_result = {'error' : \"couldn't parse scion result.\n%s\nTry :py print lastScionResult to see full server response\" } | endif " % str[:80])

# sys.path.extend(['.','..'])
PYTHONEOF
endfunction

call s:DefPython()
" vim: set et ts=4:
