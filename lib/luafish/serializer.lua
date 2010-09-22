local M = {}

local funcs = {}

-- Convert AST into a string.
local function convert(ast)
  --print(ast.tag)
  return funcs[ast.tag](ast)
end

function funcs.Block(ast)
  local ts = {}
  for i=1,#ast do
    ts[#ts+1] = convert(ast[i])
  end
  return table.concat(ts, '\n')
end

function funcs.Set(ast)
  local cnames = {}
  local cvalues = {}
  local names, values = ast[1], ast[2]
  for i=1,#names do
    cnames[#cnames+1] = convert(names[i])
  end
  for i=1,#values do
    cvalues[#cvalues+1] = convert(values[i])
  end

  return table.concat(cnames, ',') .. '=' .. table.concat(cvalues, ',')  
end

function funcs.Do(ast)
  return 'do ' .. convert(ast[1]) .. ' end'
end

function funcs.While(ast)
  return 'while ' .. convert(ast[1]) .. ' do ' .. convert(ast[2]) .. ' end'
end

function funcs.Repeat(ast)
  return 'repeat ' .. convert(ast[1]) .. ' until ' .. convert(ast[2])
end

function funcs.If(ast)
  local ts = {}
  ts[#ts+1] = 'if '
  ts[#ts+1] = convert(ast[1])
  ts[#ts+1] = ' then '
  ts[#ts+1] = convert(ast[2])
  local i=3; while i <= #ast do
    if i < #ast then
      ts[#ts+1] = ' elseif '
      ts[#ts+1] = convert(ast[i])
      ts[#ts+1] = ' then '
      ts[#ts+1] = convert(ast[i+1])
      i = i + 2
    else
      ts[#ts+1] = ' else '
      ts[#ts+1] = convert(ast[i])
      i = i + 1
    end
  end
  ts[#ts+1] = ' end'
  return table.concat(ts, '')
end

function funcs.Fornum(ast)
  return 'for ' .. convert(ast[1]) .. ' = ' ..
         convert(ast[2]) .. ',' .. convert(ast[3]) ..
         (#ast == 5 and convert(ast[4]) or '') ..
         ' do ' .. convert(ast[#ast]) .. ' end'
end

function funcs.Forin(ast)
  return 'for ' .. convert(ast[1]) .. ' in ' .. convert(ast[2]) ..
         ' do ' .. convert(ast[3]) .. ' end'
end

function funcs.FunctionDef(ast)
  return 'function ' .. convert(ast[1]) .. '(' .. convert(ast[2]) .. ') ' ..
         convert(ast[3]) .. ' end'
end

function funcs.LocalFunctionDef(ast)
  return 'local ' .. funcs.FunctionDef(ast)
end

function funcs.Local(ast)
  local cnames = {}
  local cvalues = {}

  local names, values = ast[1], ast[2]
  for i=1,#names do
    cnames[#cnames+1] = convert(names[i])
  end
  if values then
    for i=1,#values do
      cvalues[#cvalues+1] = convert(values[i])
    end
  end

  return 'local ' ..  table.concat(cnames, ',') ..
         (values and '=' .. table.concat(cvalues, ',') or '')
end

function funcs.Id(ast)
  return ast[1]
end

function funcs.Nil(ast)
  return 'nil'
end

function funcs.False(ast)
  return 'false'
end

function funcs.True(ast)
  return 'true'
end

function funcs.Dots(ast)
  return '...'
end

funcs.VARARG = funcs.Dots --fix: make same in parser?

function funcs.Number(ast)
  -- TODO? NaN and +-INF?
  return tostring(ast[1])
end

function funcs.String(ast)
  return string.format("%q", ast[1])
end



function funcs.Return(ast)
  return 'return' .. (ast[1] and ' ' .. convert(ast[1]) or '') --FIX? if no list
end

function funcs.Break(ast)
  return 'break'
end

function funcs.ExpList(ast)
  local ts = {}
  for i=1,#ast do
    ts[#ts+1] = convert(ast[i])
  end
  return table.concat(ts, ',')
end

funcs.VarList = funcs.ExpList
funcs.NameList = funcs.ExpList

local tocode  = {}
tocode.Or     = 'or'
tocode.And    = 'and'
tocode['..']  = '..'
tocode['^']   = '^'
tocode['<=']  = '<='
tocode['<']   = '<'
tocode['>=']  = '>='
tocode['>']   = '>'
tocode['~=']  = '~='
tocode['==']  = '=='
tocode['+']   = '+'
tocode['-']   = '-'
tocode['*']   = '*'
tocode['/']   = '/'
tocode['%']   = '%'
tocode.Not    = 'not'
tocode['#']   = '#'
tocode['Neg'] = '-'

function funcs.Op(ast)
  local id = ast[1]
  local code = tocode[id]
  if #ast == 3 then
    return convert(ast[2]) .. ' ' .. code .. ' ' .. convert(ast[3])
  elseif #ast == 2 then
    return code .. ' ' .. convert(ast[2])
  else assert(false) end
end

function funcs.Paren(ast)
  return '(' .. convert(ast[1]) .. ')'
end

function funcs.Call(ast)
  return convert(ast[1]) .. '(' .. convert(ast[2]) .. ')'
end

function funcs.Invoke(ast)
  return convert(ast[1]) .. ':' .. ast[2][1] ..
    '(' .. convert(ast[3]) .. ')'
end

function funcs.Index(ast)
  return convert(ast[1]) .. '[' .. convert(ast[2]) .. ']'
end

function funcs.IndexShort(ast)
  return convert(ast[1]) .. '.' .. convert(ast[2])
end

function funcs.Function(ast)
  return 'function(' .. convert(ast[1]) .. ') ' .. convert(ast[2]) .. ' end'
end

function funcs.FuncName(ast)
  local ts = {}
  local has_colon = false
  for i=1,#ast do
    local part_ast = ast[i]
    if part_ast == ':' then -- colon part follows
      has_colon = true
      break
    end
    ts[#ts+1] = convert(part_ast)
  end
  return table.concat(ts, '.') ..
      (has_colon and ':' .. convert(ast[#ast]) or '')
end

function funcs.Table(ast)
  -- improve output style?
  local ts = {}
  for i=1,#ast do
    local ele_ast = ast[i]
    local key_ast, value_ast = ele_ast[1], ele_ast[2]
    local key, value = convert(key_ast), convert(value_ast)
    ts[#ts+1] = '[' .. key .. ']=' .. value
  end
  return '{' .. table.concat(ts,',') .. '}'
end

function M.ast_to_code(ast)
  return convert(ast)
end

return M

--[=[

=NAME

luafish.serializer - serializes an AST to string

=SYNOPSIS

  local S = require "luafish.serializer"
  
  -- execute an AST
  local code = S.ast_to_code(ast)
  local f = assert(loadstring(code))
  return f()

=DESCRIPTION

This serializes an AST to a string.  This is useful if you
want to execute the AST in Lua via loadstring.

=STATUS

WARNING: This is not well tested.  Results may be wrong.

=INTERFACE

See source code.

=AUTHOR/CREDITS

David Manura. Licensed under the same terms as Lua itself.

--]=]
