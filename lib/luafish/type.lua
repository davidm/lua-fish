local Parser = require "luafish.parser"
local ASTNode = assert(Parser.ASTNode)
local Macro = require "luafish.macro"

local TNumber = Macro.TNumber
local TString = Macro.TString

local M = {}

function M.init(env)
  function env.MACRO.TYPE(type_ast)
    assert(type_ast.last)
    local mtype = type_ast[1]
    type_ast.last.stype = mtype
  end

  function env.MACRO.TYPED(obj_ast)
    obj_ast.typed = true
    return obj_ast
  end

  env.MACRO.NUMBER = TNumber.bind
  env.MACRO.STRING = TString.bind
end

return M
