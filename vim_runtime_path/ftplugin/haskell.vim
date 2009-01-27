if exists('g:dont_load_haskell_scion_interface_simple')
  finish
endif

" r = scion result with error locations
" func : either setqflist or setloclist
fun! ScionResultToErrorList(action, func, r)
  let compilationResult = has_key(a:r, 'compilationResult') ? a:r['compilationResult'] : a:r
  let g:foo = compilationResult
  let qflist = compilationResult['compilationErrors'] + compilationResult['compilationWarnings']

  " for debugging
  let g:scion_qf_list = qflist
  if has_key(a:r, 'inProject')
    let inProj = "inProject : ". a:r['inProject']
  else
    let inProj = ""
  endif
  if (has_key(a:r,'compilationSucceeded') && a:r['compilationSucceeded']) 
        \ || (!has_key(a:r, 'compilationSucceeded') && len(qflist) == 0)
    return printf(a:action." success. ".inProj." compilationTime: %s", compilationResult['compilationTime'])
  else
    call call(a:func, [qflist])
    return printf(a:action." there are errors, ".inProj." compilationTime: %s", compilationResult['compilationTime'])
  endif
endfun

" very simple user interface to expose scion functionality
" I'll implement a better interface in tovl.
" (http://github.com/MarcWeber/theonevimlib)

fun! s:BackgroundTypecheckFile(...)
  " no file given defaults to current buffer
  let file = a:0 > 0 ? a:1 : expand('%:p')
  let r =  haskellcomplete#EvalScion({'request' : 'cmdBackgroundTypecheckFile', 'file' : file})
  echo ScionResultToErrorList('file check', 'setqflist', r)
endf

fun! s:OpenCabalProject(...)
  let builddir = a:0 > 0 ? a:1 : "dist"
  echo haskellcomplete#EvalScion(
    \ {'request' : 'cmdOpenCabalProject', 'root_dir' : getcwd(),
    \ 'dist_dir' : builddir, 'extra_args' : a:000[1:] }
    \)
endf

" ===== you don't need any project for these:  =============
command! -buffer ConnectionInfo
  \ echo haskellcomplete#EvalScion({'request' : 'cmdConnectionInfo'})

" list supported languages
command! -buffer ListSupportedLanguages
  \ echo haskellcomplete#EvalScion({'request' : 'cmdListSupportedLanguages'})

" list supported pragmas
command! -buffer ListSupportedPragmas
  \ echo haskellcomplete#EvalScion({'request' : 'cmdListSupportedPragmas'})

" list supported flags
command! -buffer ListSupportedFlags
  \ echo haskellcomplete#EvalScion({'request' : 'cmdListSupportedFlags'})

" ===== loading a cabal project: ============================

" assuming pwd is current cabal directory containing the .cabal file 
" optional argument specifies the cabal build (dist) directory
command! -buffer -nargs=* -complete=file OpenCabalProject
  \ call s:OpenCabalProject(<f-args>)

" arg either "library" or "executable:name"
command! -buffer -nargs=1 LoadComponent
  \ echo ScionResultToErrorList('load component finished: ','setqflist',haskellcomplete#EvalScion({'request' : 'cmdLoadComponent', 'component' : <q-args>}))

" list exposed 
command! -buffer ListExposedModules
  \ echo haskellcomplete#EvalScion({'request' : 'cmdListExposedModules'})
command! -buffer -nargs=* -complete=file BackgroundTypecheckFile
  \ call s:BackgroundTypecheckFile(<f-args>)
command! -buffer ThingAtPoint
  \ echo haskellcomplete#EvalScion({'request' : 'cmdThingAtPoint', 'file' : expand('%:p'), 'line' : line('.').'', 'col' : col('.').''})
command! -buffer ThingAtPointExportedByHack
  \ echo filter(
      \ split(haskellcomplete#EvalScion({'request' : 'cmdThingAtPointMoreInfo'
        \, 'file' : expand('%:p')
        \, 'line' : line('.').'', 'col' : col('.').''})['Just'],"\n")
      \ , 'v:val =~ '.string(expand('<cword>').' ='))[0]

command! -buffer ListRdrNamesInScope
  \ echo haskellcomplete#EvalScion({'request' : 'cmdListRdrNamesInScope'})
