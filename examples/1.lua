-- example for Lua->C compiler.
local x,y = 4,5

x = x + 1

local function f(x)
  return x * x
end

x = f(x)

print(x)
