-- type_usage2.lua
-- LuaFish static type checking example.
-- Using math library.
-- Requires LuaFish 0.4.
--
-- Note: CAPS identifiers are typically macros.

-- Compile-time import of static typing macros NUMBER, STRING, and TYPED
REQUIRE 'luafish.type'

-- disable global variable usage
NOGLOBALS()

-- Compile-time import of static type definitions for standard modules.
local math = REQUIRE 'math'
local _G = REQUIRE '_G'

local print = _G.print

-- False conditional demonstrates that static type checking is done
-- at compile-time.
if false then
  print(math.sqrt) -- ok
  --print(math.asdf) -- compile error: asdf not in math

  --print(math.sqrt('one')) -- compile error: arg must be number
  -- print(math.sqrt(2,3)) -- compile error: num args
  -- print(math.sqrt(-1)) -- compile error: arg must be non-negative
  print(math.sqrt(2)) -- ok

  local x = 2  -- weak, implicit type Number
  --x() -- compiler error: not callable
  x = print() -- implicit type not unknown after calling unknown function
  x() -- ok now

  -- Note: compare this to the above.
  local x = TYPED(-3) -- bind strong, implicit type to lexical
  --local x = NUMBER(-3)  -- alternate form with explicit type
  --x() -- compile error: not callable
  x = print() -- does not modify strong type
  --x() -- compile error: not callable

  local x = -3
  --print(math.sqrt(x)) -- compile error: arg must be non-negative
  x = x + 2
  --print(math.sqrt(x)) -- compile error: arg must be non-negative
  x = x + 1
  print(math.sqrt(x)) -- ok

  --math.sqrt(math.sin(-math.pi/2)) -- compile error: arg must be non-negative

  local x = STRING(print()) -- bind string type, unknown value f()
  x = 5 -- doesn't affect strong type
        -- TODO: we could guard against such assignment.
  --print(math.sqrt(x)) -- compile error: arg must be number

  local sqrt = math.sqrt
  -- print(sqrt(-2)) -- compile error: arg must be non-negative

  local sqrt = TYPED(math.sqrt)
  -- print(sqrt(-2)) -- compile error: arg must be non-negative
end

print 'type_usage2.lua : done'
