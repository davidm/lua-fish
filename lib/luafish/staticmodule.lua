-- luafish/staticmodule.lua

local LuaFishParser = require "luafish.parser"
local LuaFishMacro = require "luafish.macro"
local LuaFishSerializer = require "luafish.serializer"

local function runtime(vars)
  return function(code)
    local ast = LuaFishParser():parse(code)
    LuaFishMacro.resolve_lexical_scope(ast)
    LuaFishMacro.process_macros(ast, {MACRO = vars})
    local f = assert(loadstring(LuaFishSerializer.ast_to_code(ast)))
    return f()
  end
end

return runtime

--[=[
=NAME

luafish.staticmodule - Supports implementing modules that
  define both compile-time static types and run-time behavior.

=DESCRIPTION

See examples and code.

=AUTHOR/CREDITS

David Manura. Licensed under the same terms as Lua itsel

--]=]