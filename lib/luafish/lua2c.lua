local M = {}

local f = {}

local c_from_opid = {}
c_from_opid['*'] = '*'
c_from_opid['/'] = '/'
c_from_opid['+'] = '+'
c_from_opid['-'] = '-'
--FIX: define more ops.

local function to_c(ast)
  local handler = assert(f[ast.tag], ast.tag)
  return handler(ast)
end

function f.Op(ast)
  local id,a,b = ast[1],ast[2],ast[3]
  return to_c(a) .. ' ' .. c_from_opid[id] .. ' ' .. to_c(b)
end

function f.Number(ast)
  return tostring(ast[1])
end

function f.ExpList(ast)
  assert(#ast == 1)
  return to_c(ast[1])
end

function f.Assign(ast)
  local s = ''
  local names_ast,vals_ast = ast[1],ast[2]
  for i=1,#names_ast do
    s = s .. to_c(names_ast[i]) .. ' = ' .. to_c(vals_ast[i])
  end
  return s  
end

function f.Id(ast)
  return ast[1]
end

function f.Local(ast)
  local names_ast,vals_ast = ast[1],ast[2]
  local ts = {}
  for i=1,#names_ast do
    ts[#ts+1] = 'double ' .. to_c(names_ast[i]) .. ' = ' .. to_c(vals_ast[i])
  end
  local s = table.concat(ts, ';\n')
  return s
end

function f.LocalFunctionDef(ast)
  local id_ast,args_ast,block_ast = ast[1],ast[2],ast[3]

  local ts = {}
  for i=1,#args_ast do
    ts[#ts+1] = 'double ' .. to_c(args_ast[i])
  end
  local args_c = table.concat(ts, ', ')

  local block_cobj = to_c(block_ast)
  local block_c = block_cobj[1]
  assert(not block_cobj.toplevel, 'not impl')
  

  local s = 'double ' .. to_c(id_ast) .. '(' .. args_c .. ') {\n' .. 
            block_c .. '}\n'
  return {'', toplevel=s}
end

function f.Return(ast)
  local s = 'return ' .. to_c(ast[1])
  return s
end

function f.Call(ast)
  local id_ast,args_ast = ast[1],ast[2]

  if id_ast[1] == 'print' then
    assert(#args_ast == 1)
    local s = 'printf("%f\\n", ' .. to_c(args_ast[1]) .. ')'
    return s
  end

  local ts = {}
  for i=1,#args_ast do
    ts[#ts+1] = to_c(args_ast[i])
  end
  local args_c = table.concat(ts, ', ')

  local s = to_c(id_ast) .. '(' .. args_c .. ')'
  return s
end

function f.Block(ast)
  local stop

  local s = ''
  for i=1,#ast do
    local c = to_c(ast[i])
    if type(c) == 'table' and c.toplevel then
      stop = stop or ''
      stop = stop .. c.toplevel
    else
      s = s .. c .. ';\n'
    end
  end
  return {s, toplevel = stop}
end

function M.c_from_ast(ast)
  assert(ast.tag == 'Block')
  local cobj = to_c(ast)

  return [[
#include <stdio.h>
]] .. (cobj.toplevel and cobj.toplevel or '') .. [[
int main() {
]] .. cobj[1] ..
[[
return 0;
}
]]
end

function M.c_from_lua(input)
  local Parser = require 'luafish.parser'
  local p = Parser()
  local ast = p:parse(input)
  return M.c_from_ast(ast)
end

if ... ~= 'luafish.parser' then
  local input = assert( (...) )
  
  local result = M.c_from_lua({input})

  print(result)
end

return M

-- Lua-To-C compiler.
-- WARNING: this is very-very preliminary.  It's more of a prototype.
-- It makes many assumptions.
--
-- example usage:
--   lua lib/luafish/lua2c.lua examples/1.lua | gcc -xc -
--
--FIX:MORE DOCS

