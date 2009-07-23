" vim encodes strings using ''. JSON requires ".

" dummy type which is used to encode "null"
" same could be done for true / false. But we don't use those yet
fun! json#NULL()
  return function("json#NULL")
endf

fun! json#Encode(thing)
  if type(a:thing) == type("")
    return '"'.escape(a:thing,'"').'"'
  elseif type(a:thing) == type({})
    let pairs = []
    for [key, value] in items(a:thing)
      call add(pairs, json#Encode(key).':'.json#Encode(value))
      unlet key | unlet value
    endfor
    return "{".join(pairs, ",")."}"
  elseif type(a:thing) == type(0)
    return a:thing
  elseif type(a:thing) == type([])
    return '['.join(map(a:thing, "json#Encode(v:val)"),",").']'
    return 
  elseif string(a:thing) == string(json#NULL())
    return "null"
  else
    throw "unexpected new thing: ".string(a:thing)
  endif
endf

" usage example: echo json#Encode({'method': 'connection-info', 'id': 0, 'params': [3]})
