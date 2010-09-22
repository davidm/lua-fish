-- module_usage2.lua
-- LuaFish example that tests square2.lua module.
-- It uses both the static type and runtime definition
-- in square2.lua.

print 'DEBUG:main:begin compiletime' -- trace

-- TSquare is the static type of square2.
local TSquare = require "square2"

-- This compiles and executes the given code string.
-- During compilation, the SQUARE macro is evaluated.
-- The SQUARE macro is defined as TSquare.bind, which
-- binds the given lexical to the TSquare static type
-- and returns an empty code block that replaces the macro
-- in the AST.  The code is then statically checked
-- against the bound static types.  Finally, the code
-- is executed.
require "luafish.staticmodule" {
  SQUARE = TSquare.bind, ISSQUARE = TSquare.isa
} [[
  print 'DEBUG:main:end compiletime'
  print 'DEBUG:main:begin runtime'

  -- Load run-time behavior of square2.
  local Square = require "square2" . class

  -- Create instance.  Assign to lexical.
  -- Bind static-type to lexical.
  local m = Square.create(5); SQUARE(m)

  -- This demonstrates that even though the following code is
  -- not executed at run-time, it is still compile-time checked.
  if false then
    m:setcolor('blue') -- ok
    local a = m.hello           -- compile error (field name)
    local b = m.setcolor(m,'blue') -- ok
    local b = m.setcolor(m,5)   -- compile error (arg type)
    local b = m.setcolor(m,5,6) -- compile error (num args)
  
    local b = (m * 2):area(1)   -- compile error (num args)

    -- local a = false + false  -- compile error (op not defined) 
    local a = false and true    -- ok
    local a = 5 + 3^3           -- ok
  end

  print 'DEBUG:main:end runtime'
]]

--[[OUTPUT:
DEBUG:main:begin compiletime
DEBUG:square2:begin compiletime
DEBUG:square2:end compiletime
DEBUG:square2:begin runtime
DEBUG:square2:end runtime
static __index  [TSquare Class] setcolor
static call     {"Id","m"}      {"String","blue"}
static __index  [TSquare Class] hello
ERROR:  hello not in [TSquare Class]
static __index  [TSquare Class] setcolor
static call     {"Id","m"}      {"String","blue"}
static __index  [TSquare Class] setcolor
static call     {"Id","m"}      {"Number",5}
ERROR:  second param must be string
static __index  [TSquare Class] setcolor
static call     {"Id","m"}      {"Number",5}    {"Number",6}
ERROR:  second param must be string
ERROR:  expected two arguments
static __mul    [TSquare Class] table: 0127EE68
ERROR:  first op must be TSquare
static __index  [TSquare Class] area
static call     {"Paren",{"*",{"Id","m"},{"Number",2}}}        {"Number",1}
ERROR:  expected zero arguments
DEBUG:main:end compiletime
DEBUG:main:begin runtime
DEBUG:main:end runtime
--]]
