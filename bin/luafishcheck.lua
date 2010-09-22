-- luafishcheck.lua

local function display_help()
  io.stderr:write("usage: luafishcheck <filename.lua>\n\n" ..
                  "Does code analysis on Lua file.")
end

local filename = arg[1]
if not filename then
  display_help()
  os.exit(1)
end

local fh = io.open(filename)
local text = fh:read'*a'
fh:close()

local Macro = require "luafish.macro"
local mloadstring = Macro.loadstring

local status, message = mloadstring(text)
if status then
  print "ok"
else
  print("fail", message)
end


