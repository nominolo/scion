" haskellcomplete.vim - Omni Completion for haskell
"
" This file talks to the scion server. You need python support
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

" TODO: implement stdin/ out
py scionConnectionSetting = ('127.0.0.1', 4005)
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
"function! haskellcomplete#CompleteIdentifier(findstart, base)
"  return haskellcomplete#CompleteWhat(a:findstart, a:base, 'identifier'.g:haskellcompleteAll)
"endfunction
"function! haskellcomplete#CompleteModule(findstart, base)
"  return haskellcomplete#CompleteWhat(a:findstart, a:base, 'module')
"endfunction

" example: echo haskellcomplete#EvalScion({'request' : 'file-info', 'file' : 'test.hs'})
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
import sys, tokenize, cStringIO, types, socket, string, vim

scionsocketFile = None
lastScionResult = "";
def connectscion():
    # TODO add stdin out support
    if type(scionConnectionSetting) == type((0,0)):
      # tuple -> host, port
      print "connecting to adress", scionConnectionSetting
      su = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    else: # must be path -> file socket
      print "connecting to file socket", scionConnectionSetting
      su = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    su.settimeout(10)
    su.connect(scionConnectionSetting)
    # handshake
    su.send("select scion-server protocol:vim %s\n" % vim.eval('g:vim_scion_protocol_version'));
    # using file interface to be able to use readline..
    file = su.makefile('rw')
    res = scionsocketFile.readline() 
    if res != 'ok':
      raise Exception("failed connecting to scion %. Reason: " % res)
    else:
      return file

# sends a command and returns the returned line
def evalscion(str):
    global scionsocketFile
    if (scionsocketFile == None):
      scionsocketFile = connectscion()
    try:
      scionsocketFile.write(str + "\n")
    except:
      vim.command('echoe "%s"' % "lost connection ? trying reconnect")
      scionsocketFile = connectscion()
      scionsocketFile.write(str + "\n")
    scionsocketFile.flush()
    return scionsocketFile.readline();

# str see EvalScion
def evalscionAssign(str):
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
