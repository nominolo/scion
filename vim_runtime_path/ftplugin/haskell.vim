finish
" old shim stuff :
setlocal omnifunc=haskellcomplete#CompleteIdentifier
setlocal completefunc=haskellcomplete#CompleteModule
command! -nargs=1 GrepScope call haskellcomplete#GrepScope(<f-args>)
command! -nargs=1 FindModulesExporting echo haskellcomplete#FindModulesExporting(<f-args>)

let g:haskellModuleImportBehaviour = 'interactive' " one of interactive, automatic

if !exists('g:modulePreferenceCacheFile')
  let g:modulePreferenceCacheFile = expand('$HOME').'/.vim/modulePreferenceCacheFile'
endif

if filereadable(g:modulePreferenceCacheFile)
  let g:modulePreferences = eval(readfile(g:modulePreferences, 'b'))
else
  let g:modulePreferences = {
  \   'Data.Map' : { 'q' : 'M' }
  \ , 'Control.Monad' : { 'fitness' : 10 }

  \ }
endif

" returns 0 (= not importet)
"   or   { 'q' : <opitonal qualifier name>
"        , 'functions' : [ list of explicit imported functions ] }
function! ImportInfo(module)
  let pos = 
endfunction


" needs g:modulePreferences, g:haskellModuleImportBehaviour
function! HaskellImportIdentifier(id)
  let modules = haskellcomplete#FindModulesExporting(a:id)
endfunction
