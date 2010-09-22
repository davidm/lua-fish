-- luafish.lua
-- This is a front-end to the LuaFish macro processor.
--
-- Example usage:
--    export LUA_PATH='lib/?.lua;examples/?.lua;?.lua'
--    lua bin/luafish.lua examples/type_usage2.lua

local macro = require 'luafish.macro'

macro.addloader()

local is_show_ast = false
local start = 1
while true do
  local v = select(start, ...)
  if not(v and v:find '-' == 1 and #v > 1) then
    break
  end
  if v == '-a' then
    is_show_ast = true
  else
    error('unrecognized option ' .. v)
  end
  start = start + 1
end
local filename = select(start, ...)

if is_show_ast then
  local Parser = require 'luafish.parser'
  local p = Parser()
  print(p:parse{filename})
else
  local f = assert(macro.loadfile(filename))
  return f(select(2, ...)) -- possibly improve arg handling
end

