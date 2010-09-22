-- luafish/macro.lua

local LuaFishParser = require "luafish.parser"
local LuaFishSerializer = require "luafish.serializer"
local ASTNode = LuaFishParser.ASTNode

local M = {}

-- number type
local TNumber = {}
M.TNumber = TNumber
local TNumberMT = {}
setmetatable(TNumber, TNumberMT)
function TNumberMT:__tostring()
  return 'Number[' .. tostring(self.value) .. ']'
end
function TNumber.create(ast)
  local self = setmetatable({}, {__index = TNumber})
  if ast == nil then -- do nothing
  elseif type(ast) == 'number' then
    self.value = ast
  elseif TNumber.isa(ast.stype) then
    return ast.stype
  elseif ast.tag == 'Number' then
    local value = ast[1]
    self.value = value
  elseif ast.tag == 'Id' then
    -- self.value not set
  else
    -- nothing
  end
  return self
end
function TNumber.bind(ast)
  local mtype = TNumber.create(ast)
  ast.stype = mtype
  ast.typed = true
  return ast
end
function TNumber.isa(obj)
  local mt = getmetatable(obj)
  return (mt and mt.__index == TNumber) and true or false
end
function TNumber:__neg()
  --print 'neg'
  local value = self.value
  if value then value = - value end
  --print('nv', value)
  return TNumber.create(value)
end
function TNumber:__add(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 + value2 end
  return TNumber.create(value)
end
function TNumber:__sub(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 - value2 end
  return TNumber.create(value)
end
function TNumber:__mul(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 * value2 end
  return TNumber.create(value)
end
function TNumber:__div(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 / value2 end
  return TNumber.create(value)
end
function TNumber:__mod(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 % value2 end
  return TNumber.create(value)
end
function TNumber:__pow(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 ^ value2 end
  return TNumber.create(value)
end

-- string type
local TString = {}
M.TString = TString
local TStringMT = {}
setmetatable(TString, TStringMT)
function TStringMT:__tostring()
  return 'String[' .. tostring(self.value) .. ']'
end
function TString.create(ast)
  local self = setmetatable({}, {__index = TString})
  if ast == nil then -- do nothing
  elseif type(ast) == 'string' then
    self.value = ast
  elseif TString.isa(ast.stype) then
    return ast.stype
  elseif ast.tag == 'String' then
    local value = ast[1]
    self.value = value
  elseif ast.tag == 'Id' then
    -- self.value not set
  else
    -- nothing
  end
  return self
end
function TString.bind(ast)
  local mtype = TString.create(ast)
  ast.stype = mtype
  ast.typed = true
  return ast
end
function TString.isa(obj)
  local mt = getmetatable(obj)
  return (mt and mt.__index == TString) and true or false
end
function TString:__concat(other)
  local value1, value2 = self and self.value, other and other.value
  local value; if value1 and value2 then value = value1 .. value2 end
  return TString.create(value)  
end
--FIX-more


-- function type
local TFunction = {}
M.TFunction = TFunction
local TFunctionMT = {}
setmetatable(TFunction, TFunctionMT)
function TFunctionMT:__tostring()
  return 'Function[' .. tostring(self.value) .. ']'
end
function TFunction.create(ast)
  local self = setmetatable({}, {__index = TFunction})
  if ast == nil then -- do nothing
  elseif type(ast) == 'string' then
    self.value = ast
  elseif TFunction.isa(ast.stype) then
    return ast.stype
  elseif ast.tag == 'String' then
    local value = ast[1]
    self.value = value
  elseif ast.tag == 'Id' then
    -- self.value not set
  else
    -- nothing
  end
  return self
end
function TFunction.bind(ast)
  local mtype = TFunction.create(ast)
  ast.stype = mtype
  ast.typed = true
  return ast
end
function TFunction.isa(obj)
  local mt = getmetatable(obj)
  return (mt and mt.__index == TFunction) and true or false
end
function TFunction.__call(obj)
  --improve
end
--FIX-more


-- Searches for module C<name> in C<packagepath>,
-- where C<packagepath> is of the format in C<package.path>.
-- Based on search in Lua Compat.
local function locate(packagepath, name)
  for pattern in packagepath:gfind("[^;]+") do
    local filename = pattern:gsub("%?", name)
    local f = io.open(filename)
    if f then
      f:close()
      return filename
    end
  end
  return nil
end


-- Converts a Lua value C<o> to an AST C<ast>.
--
--   ast = object_to_ast(o)
--
-- Note: this is only partly implemented and will assert
-- if the type of C<o> is not supported.
function M.object_to_ast(o)
  if type(o) == 'string' then
    return ASTNode {'String', o}
  elseif type(o) == 'number' then
    return ASTNode {'Number', o}
  elseif o == nil then
    return ASTNode {'Nil'}
  elseif o == false then
    return ASTNode {'False'}
  elseif o == true then
    return ASTNode {'True'}
  elseif type(o) == 'table' then
    --LIMITATION: only does numeric keys + tag since non-numeric keys can
    -- be recursive
    local elements = {}
    elements[#elements+1] = 'Table'
    if o.tag ~= nil then
      elements[#elements+1] = ASTNode {'Field', M.object_to_ast 'tag', M.object_to_ast(o.tag)}
    end
    for k,v in ipairs(o) do
      elements[#elements+1] = ASTNode {'Field',
         M.object_to_ast(k), M.object_to_ast(v)}
    end
    return ASTNode(elements)
  else
    assert(false, "NOT IMPLEMENTED for " .. tostring(o))
  end
end

--TODO-doc
function M.ast_to_object(ast)
  if ast.tag == 'String' or ast.tag == 'Number' then
    return ast[1]
  elseif ast.tag == 'Nil' then return nil
  elseif ast.tag == 'False' then return false
  elseif ast.tag == 'True' then return true
  else
    local code = LuaFishSerializer.ast_to_code(ast)
    local func = assert(loadstring('return ' .. code))
    return func()
  end
end

local is_numeric_binop = {
   ['+'] = true,
   ['-'] = true,
   ['*'] = true,
   ['/'] = true,
   ['%'] = true
}
-- more? OLD?

local binop_to_mt = {
  ['+']  = '__add',
  ['-']  = '__sub',
  ['*']  = '__mul',
  ['/']  = '__div',
  ['^']  = '__pow',
  ['%']  = '__mod',
  ['..'] = '__concat',
  ['<']  = '__lt',
  ['<='] = '__le',
  ['>']  = '', -- negated __le
  ['>='] = '', -- negated __lt
  ['=='] = '__eq',
  ['~='] = '', -- negated __eq
}

local unop_to_mt = {
   ['Neg'] = '__neg',
   ['#']   = '__len',
}

local is_bool_binop = {
  ['<']  = true,
  ['<='] = true,
  ['>']  = true,
  ['>='] = true,
  ['=='] = true,
  ['~='] = true,
}

-- Macro handler function for C<TYPE> macro.
--
-- The C<TYPE> macro returns the static type of the lexical
-- C<id> as a string C<s>:
--
--   s = TYPE(id)
--
-- Example:
--
--   local function f() end
--   assert(TYPE(f) == 'function')
--
-- Note that the static type is bound to the variable
-- not the current value:
--
--   ...
--   f = 123  -- change value of f to a number
--   assert(TYPE(f) == 'function') -- unchanged
--
--[[UNUSED
local function macro_TYPE(ast)
  assert(ast.tag == 'ExpList')
  if #ast ~= 1 then error("expected one argument", 2) end
  local exp_ast = ast[1]

  if exp_ast.tag == 'Id' then
    local name = exp_ast[1]
    local var_ast = ast.scope[name]

    if not var_ast then
      error("lexical " .. tostring(name) .. " not in scope", 2)
    end

    --print('TYPE', name, var_ast, var_ast.stype)
    return M.object_to_ast(exp_ast.stype)
  else
    return M.object_to_ast(exp_ast.stype)
  end
end
--]]

local function create_compile_environment()
  local env = setmetatable({}, {__index = _G})
  env.mdofile = M.dofile
  env.mloadstring = M.loadstring
  env.mrequire = M.require
  env.MACRO = {}
  return env
end


-- Macro handler function for C<SETTYPE> macro.
--
-- The C<SETTYPE> macro sets the static type of the lexical
-- C<id> to the string C<s>:
--
--   s = TYPE(id)
--
-- Example:
--
--   local x = 2
--   SETTYPE(x, 'integer')
--   assert(TYPE(x) == 'integer')
-- 
--[[UNUSED
local function macro_SETTYPE(ast)
  --print("Set", ast)
  if #ast ~= 2 then error("expected two arguments", 2) end
  local id_ast, type_ast = ast[1], ast[2]

  if id_ast.tag ~= 'Id' then error("not identifier", 2) end

  local name, type = id_ast[1], type_ast[1]
  --print('r', name, ast.scope[name])
  local var_ast = ast.scope[name]
  if not var_ast then
    error("lexical " .. name .. " not in scope", 2)
  end

  --print("DEBUG:SETTYPE", name,type, var_ast)
  var_ast.stype = type
  return ASTNode{'Block'} -- no nothing
end
--]]

-- Macro handler function for C<AST> macro.
--
-- The C<AST> macro returns the AST of its argument expression C<exp>.
--
--   ast = TOAST(<exp>)
--
-- Example:
--
--   local ast = TOAST(1 + 2*x)
--   assert(ast.tag == '+')
--   assert(ast[2].tag == '*')
-- 
-- NOTE: possiblyu should be improved.  It retuns a B<copy>
-- of the AST, without metatables set.
--[[UNUSED
local function macro_TOAST(ast)
  if #ast ~= 1 then error("expected one argument", 2) end
  local exp_ast = ast[1]
  return 'value', M.object_to_ast(exp_ast)
end
--]]

local function resolve(env, id_ast)
  local obj_ast = id_ast
  if id_ast.tag == 'Id' then
    local obj_name = id_ast[1]
    obj_ast = id_ast.scope[obj_name]
    if not obj_ast then
      if env.ONGLOBAL then
        env.ONGLOBAL(obj_name)
      end
    end
  end
  if obj_ast then
    return obj_ast
  end
end

local function resolve_args(env, arg_ast)
  local args = {unpack(arg_ast)}
  for i,v in ipairs(args) do
    args[i] = resolve(env, v)
  end
  return args
end

-- similar to getbinhandler in Lua Reference Manual.
local function getbinhandler (op1, op2, event)
  return op1.stype and op1.stype[event] or
         op2.stype and op2.stype[event]
end

-- similar to getbinhandler but for unary op.
local function getunhandler (op1, event)
  return op1.stype and op1.stype[event]
end

local function context(ast)
  return string.format("At line %d column %d", ast.nline, ast.ncol)
end

local function mypcall(ast, caller, ...)
  local status, message = pcall(caller, ...)
  if not status then
    error(context(ast) .. ": " .. message)
  end
  return message -- result value
end

-- Walks the AST C<ast>, processing macros using macro definitions
-- C<macros>.  C<macros> is a table mapping macro names to macro
-- handler functions.
--
-- A macro handler function recieves as input an AST of the
-- expression list passed as arguments.  It returns
-- an AST representing the code that replaces the macro.
--
-- Note: the order of macro calls is depth-first search,
-- evaluating children then parents.
function M.process_macros(ast, env)
  local macros = env.MACRO

  -- evaluate children
  for i,v in ipairs(ast) do
    if type(v) == "table" then
      local is_replace, replaced = M.process_macros(v, env)
      if is_replace then
        ast[i] = replaced
      end
    end
  end

  -- Evaluate compile-time expressions.
  -- The order is to evalute children then parents.

  if ast.tag == 'Index' then
    local obj_ast, key_ast = resolve(env, ast[1]), ast[2]
    if obj_ast and obj_ast.stype and obj_ast.stype.__index then
      local result_type =
         mypcall(ast, obj_ast.stype.__index, obj_ast.stype, M.ast_to_object(key_ast))
      ast.stype = result_type -- may be nil
    end
  elseif ast.tag == 'Id' then
    local macro_name = ast[1]
    --print(macros.SETSQUARE,rawget(macros, 'SETSQUARE'),'ss', macro_name)

    local macro = macros[macro_name]
    if macro then
      local obj = {tag='Macro', macro}
      obj.scope = ast.scope -- copy
      obj.last = ast.last
      return true, obj
    end
    -- otherwise, non-macro Id
  elseif ast.tag == 'Call' then
    local macro = ast[1]
    if macro.tag == 'Macro' then -- a macro
      macro = macro[1]
      local arg_ast = ast[2]
      local args = resolve_args(env, arg_ast)
      local a,b = macro(unpack(args))
      if a == 'value' then a = M.object_to_ast(b)
      elseif a == nil then a = ASTNode{'Block'}end
      local res_ast = a
      --print ("M", macro, macro_name, arg_ast, res_ast)
      return true, res_ast
    end
    -- otherwise, non-macro call

    local obj_ast = resolve(env, ast[1])
    if obj_ast and obj_ast.stype then
      if obj_ast.stype then
        local caller = TFunction.isa(obj_ast.stype) and obj_ast.stype.__call or
           type(obj_ast.stype) == 'function' and obj_ast.stype
        if caller then
          local args = resolve_args(env, ast[2])
          ast.stype = mypcall(ast, caller, unpack(args)) -- may be nil
        else
          error("not callable " .. tostring(obj_ast) .. " " .. tostring(obj_ast.stype))
        end
      end
    end
  elseif ast.tag == 'Invoke' then  
    -- note: this is like an Index followed by a Call with "self"
    local obj_ast = resolve(env, ast[1])

    if obj_ast and obj_ast.stype and obj_ast.stype.__index then
      local key_ast = ast[2]
      local caller =
         obj_ast.stype.__index(obj_ast.stype, M.ast_to_object(key_ast)) -- may be il
      if caller then
        local args = resolve_args(env, ast[3])
        table.insert(args, 1, obj_ast) -- self
        ast.stype = mypcall(ast, caller, unpack(args)) -- may be nil
      end
    end
  elseif ast.tag == 'String' then
    ast.stype = TString.create(ast)
  elseif ast.tag == 'Number' then
    ast.stype = TNumber.create(ast)
  elseif ast.tag == 'Nil' then
    ast.stype = 'nil'  -- note: 'nil' different from nil.
  elseif ast.tag == 'True' or ast.tag == 'False' then
    ast.stype = 'boolean'
  elseif ast.tag == 'Op' then
    if #ast == 2 then -- unary op
      local a_ast = resolve(env, ast[2])
      local event = unop_to_mt[ast[1]]
      local h = getunhandler(a_ast, event)
      if h then
        ast.stype = h(a_ast.stype)
      else -- unknown type
        if a_ast.stype then
          error(tostring(ast) .. ' - operation not defined')
        end
      end
    else -- binary op
      local a_ast, b_ast = resolve(env, ast[2]), resolve(env, ast[3])
      if not a_ast or not b_ast then
        if not a_ast then
           print(context(ast) .. ": non-lexical ")
        end
        if not b_ast then
          print(context(ast) .. ": non-lexical ")
        end
      else
        if is_bool_binop[ast[1]] then
          ast.stype = 'boolean'
        else
          local event = binop_to_mt[ast[1]]
          local h = getbinhandler(a_ast, b_ast, event)
          if h then
            ast.stype = h(a_ast.stype, b_ast.stype)
          elseif ast[1] == 'And' or ast[1] == 'Or' and
                 a_ast.stype == b_ast.stype then
            ast.stype = a_ast.stype
          elseif a_ast.stype == 'number' or b_ast.stype == 'number' then
            ast.stype = 'number'
          else -- unknown type
            if a_ast.stype and b_ast.stype then
              error(tostring(ast) .. ' - operation not defined')
            end
          end
        end
      end
    end
  elseif ast.tag == 'Paren' then
    ast.stype = ast[1].stype
  elseif ast.tag == 'Local' then
    local vals_ast = ast[2]
    if vals_ast then
      for i=1,#vals_ast do
        local val_ast = vals_ast[i]
        --print(val_ast.stype)
        if ast[1][i] then
          ast[1][i].stype = val_ast.stype
          ast[1][i].typed = val_ast.typed
        end
      end
    end
  elseif ast.tag == 'Set' then
    local names_ast, vals_ast = ast[1], ast[2]
    for i=1,#vals_ast do
      local name_ast = resolve(env, names_ast[i])
      local val_ast = resolve(env, vals_ast[i])
      if name_ast and val_ast then
        if not name_ast.typed then
          name_ast.stype = val_ast.stype
          -- unused: name_ast.typed = val_ast.typed
        end
      end
    end
  end
end

-- Return a shallow copy of table t.
-- A helper function.
local function shallowcopytable(t)
  local t2 = {}
  for k,v in pairs(t) do t2[k] = v end
  return t2
end

-- helper for resolve_lexical_scope.
local function helper(ast, scope, last_local)
  -- note: unless this statement creates lexicals, the lexicals
  -- of the next sibling will be identical to that in the current statement.

  local childscope = scope
  local child_last_local = last_local

  if ast.tag == 'Local' then
    local names_ast = ast[1]
    scope = shallowcopytable(scope)
    for i=1,#names_ast do  -- new lexicals
      local name_ast = names_ast[i]
      local name = name_ast[1]
      --print('Local', name, scope)
      scope[name] = name_ast
      last_local = name_ast
    end
  elseif ast.tag == 'Function' or ast.tag == 'FunctionDef' or ast.tag == 'LocalFunctionDef' then
    local params_ast = ast.tag == 'Function' and ast[1] or ast[2]

    -- lexical function
    if ast.tag == 'LocalFunctionDef' then
      local funcname_ast = ast[1]
      local funcname = funcname_ast[1]
      scope = shallowcopytable(scope) -- new lexical
      scope[funcname] = funcname_ast
      last_local = funcname_ast
      funcname_ast.stype = TFunction.create()

      -- link variable name of lexical function
      -- to its lexical parameters (for param argument type checking)
      --OLD?
      funcname_ast.params_ast = params_ast
    end

    -- process params
    if #params_ast > 0 then
      local names_ast = params_ast
      childscope = shallowcopytable(scope)
      for i=1,#names_ast do  -- new lexicals
        local name_ast = names_ast[i]
        if name_ast.tag ~= 'VARARG' then
          local name = name_ast[1]
          childscope[name] = name_ast
          child_last_local = name_ast
        end
      end
    end
  elseif ast.tag == 'Fornum' or ast.tag == 'Forin' then
    local var_ast = ast[1]
    local var_name = var_ast[1]
    childscope = shallowcopytable(scope)
    childscope[var_name] = var_ast
    child_last_local = var_ast
  end

  ast.scope = scope  -- store scope information in AST node.
  ast.last = last_local -- store last defined local in AST node.

  --print("s",scope, scope.x)

  -- handle children
  local last_local = last_local
  for i,v in ipairs(ast) do
    if type(v) == "table" then
      childscope, child_last_local
        = helper(v, childscope, child_last_local)
    end
  end

  return scope, last_local
end

-- Walks the AST C<ast>, setting the field "scope" on each node
-- to contain a table of visible lexicals at that point.
-- This table maps a variable name to the AST node of the identifier
-- in the variable's declaration.
-- These tables may be shared amoung AST nodes (for efficiency).
function M.resolve_lexical_scope(ast)
  local scope = {}
  local last_local = nil
  helper(ast, scope, last_local)
end

--[[OLD
local function check_call(args_ast, params_ast)
  for i=1,#params_ast do
    local param_ast = params_ast[i]
    local param_name = param_ast[1]
    if param_ast.stype then
      --print("expectarg", param_ast.stype)
      local arg_ast = args_ast[i]
      if not arg_ast then
        error("missing argument " .. param_name, 2)
      end
      if arg_ast.stype ~= param_ast.stype then
        error("argument " .. param_name .. " type is " ..
              tostring(arg_ast.stype) .. " but expecting " ..
              param_ast.stype, 2)
      end
    end
  end
end
--]]

-- A replacement for C<loadstring> that processes macros.
--
--   f, msg = loadstring(code, [, chunkname [, is_debug]])
--
-- This has the same form as C<loadstring> but with an
-- additional optional C<is_debug> Boolean argument
-- that if true results in a debug debug of the AST
-- and code to be printed (default false).
function M.loadstring(code, chunkname, is_debug)
  local parser = LuaFishParser()

  local t = parser:parse(code)

  if is_debug then print('DEBUG[AST.IN][', t, ']') end

  local env = create_compile_environment()

  local function macro_ONCOMPILE(func_ast)
    assert(func_ast.tag == 'Function')

    local func_code = LuaFishSerializer.ast_to_code(func_ast)

    local func = assert(loadstring('return ' .. func_code,
                                   (chunkname or '') .. '(ONCOMPILE)',
                                    is_debug))
    setfenv(func, getfenv(1))
  
    func()(func_ast.scope)

    return ASTNode{'Block'} -- no nothing
  end
  setfenv(macro_ONCOMPILE, env)
  env.MACRO.ONCOMPILE = macro_ONCOMPILE

  local function macro_REQUIRE(name_ast)
    assert(name_ast.tag == 'String')
    local name = name_ast[1]

    --print(name_ast.last, 'L')

    --hack
    local stype
    if name == 'math' then
      stype = require 'luafish.math'
    elseif name == 'string' then
      stype = require 'luafish.string'
    elseif name == '_G' then
      stype = _G
    else
      local m = require(name)
      return m.init(env) --ok?
    end



    local result_ast = ASTNode{'Call', ASTNode{'Id', 'require'},
           ASTNode{'ExpList', ASTNode{'String', name}}}
    result_ast.stype = stype
    result_ast.typed = true
    return result_ast
  end
  env.MACRO.REQUIRE = macro_REQUIRE

  local function macro_NOGLOBALS()
    function env.ONGLOBAL(name)
      error('global "' .. name .. '" used when globals are disabled')
    end
  end
  env.MACRO.NOGLOBALS = macro_NOGLOBALS

  env.M = M

  M.resolve_lexical_scope(t)

  M.process_macros(t, env)

  if is_debug then print('DEBUG[AST.OUT][', t, ']') end

  -- FIX- this should return nil and error message not raise an error
  local code = LuaFishSerializer.ast_to_code(t)

  if is_debug then print('DEBUG[CODE.OUT][', code, ']') end
  local fh = io.open("out1", 'w')
  fh:write(code)
  fh:close()
  return loadstring(code, chunkname, is_debug)
end


-- A replacement for C<dofile> that processes macros.
--
--   ... = dofile(filename [, is_debug])
--
-- - filename is the file name. Type is string.
-- - is_debug is an optional Boolean indicating whether
--   debug dumps of the AST and code are printed (default false).
function M.dofile(name, is_debug)
  local fh = assert(io.open(name))
  local code = fh:read'*a'
  fh:close()

  local f, message = assert(M.loadstring(code, name, is_debug))
  if not f then error(message, 2) end

  return f()
end

-- A replacement for C<loadfile> that processes macros.
-- - is_debug is an optional Boolean indicating whether
--   debug dumps of the AST and code are printed (default false).
function M.loadfile(name, is_debug)
  local fh = assert(io.open(name))
  local code = fh:read'*a'
  fh:close()

  local f, message = M.loadstring(code, name, is_debug)
  return f, message
end

-- A replacement for C<require> that processes macros.
--
--   res = require(modname [, is_debug])
--
-- - modname is the module name. Type is string.
-- - is_debug is an optional Boolean indicating whether
--   debug dumps of the AST and code are printed (default false).
local sentinel = function() end
function M.require(name, is_debug)
  local pkg = package.loaded[name]
  if pkg then return pkg end

  local path = locate(package.path, name)
  if path then -- macro-enabled loaded
    package.loaded[name] = sentinel
    local old_arg = _G.arg; _G.arg = {name} -- localize
    pkg = M.dofile(path, is_debug)
    _G.arg = old_arg -- restore
    if pkg then
      package.loaded[name] = pkg
    end
    if package.loaded[name] == sentinel then
      package.loaded[name] = true
    end
    return package.loaded[name]
  else
    return require(name)  -- fallback
  end
end

function M.loader(name)
  local path = locate(package.path, name)
  if path then -- macro-enabled loaded
    return M.loadfile(path)
  end
end

function M.addloader()
  table.insert(package.loaders, 1, M.loader)
  return true
end

return M

--[=[

=NAME

luafish.macro - macro processing support

=SYNOPSIS

  See examples.

=DESCRIPTION

This module includes various utility for macro processing
on abstract syntax trees (ASTs).  Built-in macros are included
for static type checking.

=INTERFACE

See source code.

=STATUS

WARNING: This is only a prototype.  I am exploring various
design options.  The code is not complete.

=AUTHOR/CREDITS

David Manura. Licensed under the same terms as Lua itself.

--]=]


