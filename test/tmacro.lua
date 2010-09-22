local mloadstring = require "luafish.macro" . loadstring

local function mdostring(code)
  local f = assert(mloadstring(code, 'tmacro.lua', true))
  return f()
end

mdostring [[
  ONCOMPILE(function()
    function MACRO.SETTYPE(obj_ast, type_ast)
      obj_ast.stype = type_ast[1]
    end
    function MACRO.TYPE(obj_ast)
      return 'value', obj_ast.stype
    end
    function MACRO.TOAST(ast)
      return 'value', ast
    end
    function MACRO.ISNUMBER(ast)
      return 'value', M.TNumber.isa(ast.stype)
    end
  end)

  assert(1 + 1 == 2)

  local x = 2
  SETTYPE(x, 'integer')

  local function test(y)
    SETTYPE(y, 'integer')
    for y=1,10 do
      assert(TYPE(y) == nil)

      -- NOTE: the below will cause the above assert to fail.
      -- that might not be what we want.  Should types be mutable?
      -- SETTYPE(y, 'number')
    end
    assert(TYPE(y) == 'integer')
  end

  assert(TYPE(x) == 'integer')

  test(x)

  local x = 1
  assert(ISNUMBER(1+2+x*x))

  local ast = TOAST(1 + 2*x)
  assert(ast.tag == 'Op' and ast[1] == '+')
  assert(ast[3].tag == 'Op' and ast[3][1] == '*')
]]

print 'done'

