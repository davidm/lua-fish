-- type_usage_m.lua
--
-- Test of static type checking via the LuaFish
-- LPeg-based macro processor.
--

-- Code executed at compile-time.
ONCOMPILE(function()
  -- SETTYPE and TYPE are macros evaluated
  -- at compile time.  These respectively bind a static type
  -- to a lexical and retrieve that type.

  function MACRO.SETTYPE(obj_ast, type_ast)
    obj_ast.stype = type_ast[1]
  end
  function MACRO.TYPE(obj_ast)
    return 'value', obj_ast.stype
  end
  function MACRO.ISNUMBER(obj_ast)
    return 'value', M.TNumber.isa(obj_ast.stype)
  end
  function MACRO.ISFUNCTION(obj_ast)
    return 'value', M.TFunction.isa(obj_ast.stype)
  end
end)

local x, c = 3, 'ok'
SETTYPE(x, 'integer')
SETTYPE(c, 'string')

local function test(y)
  SETTYPE(y, 'integer')
  assert(TYPE(y) == 'integer')
  print(TYPE(y))
end

-- demonstration of the TYPE macro.
assert(TYPE(x) == 'integer')
assert(ISFUNCTION(test))

test(x) -- ok, types match

-- compile error: "argument y type is string but expecting integer"
-- test('ok')

-- type of arithmetic expression is deduced at compile time
local x = 1
SETTYPE(x, 'number')
assert(ISNUMBER(1+2+x*x))
assert(not ISNUMBER("test" .. "test"))

print 'done'

