-- module_usage.lua
-- LuaFish example, for modules code containing macros.

-- This code is called at compile-time.
ONCOMPILE(function()
  -- load square module since we need static types.
  local Square = mrequire "square"

  -- Define a compile-time macro that associates a lexical with a
  -- static type of TSquare. [HACK: TSquare is currently global]
  function MACRO.SQUARE(obj)
    print('SETSQUARE', obj)
    obj.stype = TSquare
  end
end)

local Square = require "square"

print 'begin runtime'

-- Create instance.  Assign to lexical.
-- Bind static-type to lexical.
local m = Square.create(5); SQUARE(m)

local x = m.area -- ok
local y = m.area -- ok

for n=1,10 do
  local a = m:area() -- ok
  local b = m:perimeter() -- ok
end

m:setcolor('blue') -- ok

-- Note: even though this code is not executed at run-time,
-- it is still compile-time checked.
if false then
  local a = m.hello           -- compile error: hello not in class
  local b = m.setcolor(m,'blue') -- ok
  local b = m.setcolor(m,5)   -- compile error: arg 2 not string
  local b = m.setcolor(m,'blue',6) -- compile error: expected two args

  local b = (m * 2):area() -- ok
end

print 'end runtime'

--[[OUTPUT:
squarem:begin compiletime
squarem:end compiletime
squarem:begin runtime
squarem:end runtime
SETSQUARE       {"Id","m"}
static __index  [TSquare Class] area
static __index  [TSquare Class] area
static __index  [TSquare Class] area
static call     {"Id","m"}
static __index  [TSquare Class] perimeter
static call     {"Id","m"}
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
static call     {"Id","m"}      {"String","blue"}       {"Number",6}
ERROR:  expected two arguments
begin runtime
end runtime
--]]
