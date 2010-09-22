-- luafish/parser.lua

local lpeg = require 'lpeg'

local C  = lpeg.C
local Cc = lpeg.Cc
local Cf = lpeg.Cf
local Cg = lpeg.Cg
local Cp = lpeg.Cp
local Ct = lpeg.Ct
local P  = lpeg.P
local R  = lpeg.R
local S  = lpeg.S
local V  = lpeg.V

local M = {}; M.__index = M

-- AST node type.
local ASTNode = {}
function ASTNode:__tostring()
  local ts = {}
  ts[#ts+1] = 'tag=' .. string.format("%q", self.tag)
  for i,v in ipairs(self) do
    if type(v) == "string" then
      ts[#ts+1] = string.format("%q", v)
    else
      ts[#ts+1] = tostring(v)
    end
  end
  return '{' .. table.concat(ts, ',') .. '}'
end
setmetatable(ASTNode, {__call = function(class, t) -- constructor
  if not t.tag then
    t.tag = table.remove(t, 1)
  end
  return setmetatable(t, ASTNode)
end})
M.ASTNode = ASTNode


--debug
local function short(s,i)
  local small = s:sub(i, i+40)
  if i+40 < #s then small = small .. "..." end
  return small
end
local debug = P(function(s,i)print("DEBUG", short(s,i)) return i end)


--## parser handlers

-- convert value to string, handling nested tables
local function rtostring(t)
  if type(t) == "table" then
    local ts = {}
    for i,v in ipairs(t) do
      ts[i] = rtostring(v)
    end
    return "{" .. table.concat(ts, ",") .. "}"
  else
    return string.format("%q", t)
  end
end

--## parser

-- callback for [=[ long strings ]=]
local longstring = #(P '[[' + (P '[' * P '=' ^ 0 * P '['))
local longstring = longstring * P(function(input, index)
   local level = input:match('^%[(=*)%[', index)
   if level then
      local _, stop = input:find(']' .. level .. ']', index, true)
      if stop then return stop + 1 end
   end
end)
-- strings
local singlequoted_string = P "'" * ((1 - S "'\r\n\f\\") + (P '\\' * 1)) ^ 0 * "'"
local doublequoted_string = P '"' * ((1 - S '"\r\n\f\\') + (P '\\' * 1)) ^ 0 * '"'
local luastring = singlequoted_string +
                  doublequoted_string +
                  longstring

-- comments
local singleline_comment = P'--' * (1 - S'\r\n\f')^0
local multiline_comment = P'--' * longstring
local comment = multiline_comment + singleline_comment

-- whitespace
local ws = (S('\r\n\f\t ') + comment)^1

local idsafe = R('AZ', 'az', '\127\255') + P '_'

local digit = R('09')

--TODO-check that this is ok.
local number_sign = S'+-'^-1
local number_decimal = digit^1
local number_hexadecimal = P'0' * S'xX' * R('09', 'AF', 'af')^1
local number_float = (P'.' * digit^1 +
                      digit^1 * (P'.' * digit^0)^-1 ) *
                     (S'eE' * number_sign * digit^1)^-1
local number = number_hexadecimal + number_float + number_decimal

-- capture value and current position
local function C2(value)
  return Cp() * Cc(value)
end

local anykeyword =
    P'and' +
    P'break' +
    P'do' +
    P'elseif' +
    P'else' +
    P'end' +
    P'false' +
    P'for' +
    P'function' +
    P'if' +
    P'in' +
    P'local' +
    P'nil' +
    P'not' +
    P'or' + 
    P'repeat' +
    P'return' +
    P'then' +
    P'true' +
    P'until' +
    P'while'

-- get line and column numbers from position i (1-based) in string s.
function M.get_linecol(s, i)
  local nline, ncol = 1, 0
  local was_nl = false
  for k=1,i do
    if was_nl then
      nline = nline + 1
      ncol = 0
      was_nl = false
    end
    if s:match('^\n', k) then
      was_nl = true
    end
    ncol = ncol + 1
  end
  return nline, ncol
end

function M.update_linecol(s, i, i_last, nline, ncol)
  i_last = i_last or 0
  nline = nline or 0
  ncol = ncol or 0

  while i_last > i do -- reverse, go to start of previous line
    repeat
      i_last = i_last - 1
    until i_last == 1 or s:match('^\n',i_last)
    nline = nline - 1
    ncol = 0
  end

  for k=i_last+1,i do
    if k == 1 or s:match('^\n',k-1) then
      nline = nline + 1
      ncol = 0
    end
    ncol = ncol + 1
  end
  return i, nline, ncol
end


local function build_grammar(self)
  local grammar = {'chunk'}

  local name_simple =
    ws^-1 * C(idsafe * (idsafe + digit)^0 - anykeyword * -idsafe)

  local name =
    (C2'Id' * name_simple)
    / self.handle_identifier

  local function keyword(name) return ws^-1 * P(name) * -idsafe end

  local function op(name) return ws^-1 * P(name) end

  -- capture op
  local function cop(name) return ws^-1 * C(P(name)) end
  
  local function binop_helper(a, pos, op, b)
    return self.handle_binop(pos, op, a, b)
  end

  local handle = {
    Index = self.handle_index,
    IndexShort = self.handle_indexshort,
    Call = self.handle_call,
    ColonCall = self.handle_coloncall
  }
  local function accum(a, pos, op, b, ...)
    if op == nil then
      return a
    elseif op == "ColonCall" then
      return accum(handle[op](pos, op, a, b, select(1, ...)), select(2, ...))
    else
      return accum((handle[op] or self.handle_binop)(pos, op, a, b), ...)
    end
  end

  local function postfix_helper(a, pos, op, ...)
    return handle[op](pos, op, a, ...)
  end

  -- right associative accumulate
  local function raccum(a, pos, op, b, ...)
    if op == nil then
      return a
    else
      return self.handle_binop(pos, op, a, raccum(b, ...))
    end
  end

  local function handle_unrecognized(i)
    error(function(s)
      local nline, ncol = M.get_linecol(s, i)
      return string.format("At line %d col %d unrecognized [%s]", nline, ncol, short(s,i))
    end)
  end

  -- modified
  grammar.chunk = (
    V'block' * ws^-1 * ((Cp() * P(1)) / handle_unrecognized)^-1
  ) / function(t) return ASTNode(t) end

  -- modified
  grammar.block = (
  --    P(function(s,i) print(s:sub(i)) end) +
      C2'Block' * (V'stat' * op';'^-1)^0 * (V'laststat' * op';'^-1)^-1
  ) / self.handle_block

  grammar.stat = (
    (C2'Assign' * V'varlist' * op'=' * V'explist')
        / self.handle_assign +
    V'functioncall' +
    (C2'Do' * keyword'do' * V'block' * keyword'end')
        / self.handle_do +
    (C2'While' * keyword'while' * V'exp' * keyword'do' *
        V'block' * keyword'end')
        / self.handle_while +
    (C2'Repeat' * keyword'repeat' * V'block' * keyword'until' * V'exp')
        / self.handle_repeat +
    (C2'If' * keyword'if' * V'exp' * keyword'then' * V'block' *
        (keyword'elseif' * V'exp' * keyword'then' * V'block')^0 *
        (keyword'else' * V'block')^-1 *
        keyword'end')
        / self.handle_if +
    (C2'For' * keyword'for' * name * op'=' * V'exp' * op',' * V'exp' *
        (op',' * V'exp')^-1 * keyword'do' * V'block' * keyword'end')
        / self.handle_for + 
    (C2'Forin' * keyword'for' * V'namelist' * keyword'in' * V'explist' *
         keyword'do' * V'block' * keyword'end')
         / self.handle_forin +
    (C2'FunctionDef' * keyword'function' * V'funcname' * V'funcbody')
         / self.handle_functiondef +
    (C2'LocalFunctionDef' * keyword'local' * keyword'function' * name *
         V'funcbody')
         / self.handle_localfunctiondef +
    (C2'Local' * keyword'local' * V'namelist' * (op'=' * V'explist')^-1)
         / self.handle_local
  ) / self.handle_stat

  grammar.laststat =
    (C2'Return' * keyword'return' * V'explist'^-1) / self.handle_return +
    (C2'Break' * keyword'break') / self.handle_break

  grammar.funcname =
    (C2'FuncName' * name * (op'.' * name)^0 * (cop':' * name)^-1)
        / self.handle_funcname

  grammar.varlist =
    (C2'VarList' * V'var' * (op',' * V'var')^0) / self.handle_varlist

  -- modified. note: was left-recursive
  grammar.var =
    (V'prefixexp' * V'endindex') / accum +
    name

  grammar.namelist =
    (C2'NameList' * name * (',' * name)^0) / self.handle_namelist

  grammar.explist =
    (C2'ExpList' * (V'exp' * op',')^0 * V'exp') / self.handle_explist

  --modified
  -- note: exp was left-recursive in binop.
  grammar.exp =
    Cf(Cg(V'orfactor') * Cg(C2'Or' * keyword'or' * V'orfactor')^0, binop_helper)

  grammar.orfactor =
    Cf(Cg(V'andfactor') * Cg(C2'And' * keyword'and' * V'andfactor')^0, binop_helper)

  grammar.andfactor =
    Cf(Cg(V'comparefactor') * Cg(V'compareop' * V'comparefactor')^0, binop_helper)

  grammar.comparefactor =
    (V'concatfactor' * (Cp() * cop'..' * V'concatfactor')^0) / raccum

  grammar.concatfactor =
    Cf(Cg(V'sumfactor') * Cg(V'sumop' * V'sumfactor')^-0, binop_helper)

  grammar.sumfactor =
    Cf(Cg(V'productfactor') * Cg(V'productop' * V'productfactor')^0, binop_helper)

  grammar.productfactor =
    (V'unaryop' * V'productfactor') / self.handle_unop +
    V'unaryfactor'

  --ok? productfactor usage allows x^-y^z
  grammar.unaryfactor =
    (V'term' * (Cp() * cop'^' * V'productfactor')^0) / raccum

  grammar.compareop = Cp() * (
    cop'<=' +
    cop'<' +   -- order important
    cop'>=' +
    cop'>' +   -- order important
    cop'~=' +
    cop'=='
  )

  grammar.sumop = Cp() * (
    cop'+' +
    cop'-'
  )

  grammar.productop = Cp() * (
    cop'*' +
    cop'/' +
    cop'%'
  )

  grammar.unaryop = Cp() * (
    Cc'Not' * keyword'not' +
    cop'#' +
    Cc'Neg' * op'-'
  )

  -- modified
  grammar.term =
    (C2'Nil' * keyword'nil') / self.handle_nil + 
    (C2'False' * keyword'false') / self.handle_false +
    (C2'True' * keyword'true') / self.handle_true +
    (C2'Number' * ws^-1 * C(number)) / self.handle_number +
    (C2'String' * ws^-1 * C(luastring)) / self.handle_string +
    (C2'Dots' * op'...') / self.handle_dots +
    V'function' +
    Cf(Cg(V'prefixexp') * Cg(V'postfix')^0, postfix_helper) +   -- modified
    V'tableconstructor'

  --modified
  grammar.prefixexp =
    name +
    (C2'Parens' * op'(' * V'exp' * op')') / self.handle_parens

  --modified
  grammar.postfixcall =
    C2'Call' * V'args' +
    C2'ColonCall' * op':' *
        ((C2'String' * name_simple) / self.handle_string) * V'args'

  -- modified
  grammar.postfixindex =
    C2'Index' * op'[' * V'exp' * op']' +
    C2'IndexShort' * op'.' * ((C2'String' * name_simple) / self.handle_string)

  --modified
  grammar.postfix =
    V'postfixcall' +
    V'postfixindex'

  --modified
  grammar.endcall =
    (V'postfix' * #V'postfix')^0 * V'postfixcall'

  --modified
  grammar.endindex =
    (V'postfix' * #V'postfix')^0 * V'postfixindex'

  -- modified (note: was left recusive)
  grammar.functioncall =
    (Cf(Cg(V'prefixexp') * Cg(V'postfix' * #V'postfix')^0, postfix_helper) * V'postfixcall') / postfix_helper

  grammar.args = (
    op'(' * (grammar.explist + C2'ExpList' / self.handle_explist) * op')' + -- improve style?
    (C2'ExpList' * V'tableconstructor') / self.handle_explist +  -- improve style?
      -- improve style?
    (C2'ExpList' * (C2'String' * ws^-1 * C(luastring) / self.handle_string)) /
        self.handle_explist
  ) / self.handle_args

  grammar['function'] = (
    C2'Function' * keyword'function' * V'funcbody'
  ) / self.handle_function

  grammar.funcbody =  -- improve style?
     op'(' * (V'parlist' + C2'NameList' / self.handle_namelist) * op')'
        * V'block' * keyword'end'

  grammar.parlist = (C2'NameList' * (
    name * (',' * name)^0
         * (op',' * (C2'VARARG' * op'...') / self.handle_vararg)^-1 +
    (C2'VARARG' * op'...') / self.handle_vararg
  )) / self.handle_namelist

  grammar.tableconstructor =
    (C2'Table' * op'{' * V'fieldlist'^-1 * op'}') / self.handle_table

  grammar.fieldlist =
    V'field' * (V'fieldsep' * V'field')^0 * V'fieldsep'^-1

  --FIX: handler call?
  grammar.field = Cp() * (
    op'[' * V'exp' * op']' * op'=' * V'exp' +
    ((C2'String' * name_simple) / self.handle_string * op'=' * V'exp') +
    Cc(true) * V'exp'
  )

  grammar.fieldsep =
    op',' + op';'

  self.grammar = grammar
end

-- constructor
setmetatable(M, {__call = function()
  local self = setmetatable({}, M)

  local function generic_handle(pos, ...)
    return ASTNode {pos=pos, ...}
  end
  local function identity_handle(...) return ... end
  local function eval_handle(pos, op, o)
    if o:match("[%[%\"%\']") then
      o = assert(loadstring('return ' .. o))()
    end
    return generic_handle(pos, op, o)
  end

  self.handle_stat = function(t)
    --print(rtostring(t))
    return t
  end
  self.handle_table = function(pos, id, ...)
    local t = {pos=pos,id}
    local i = 0
    local n = select('#', ...)
    for k=1,n,3 do
      local pos2,k,v = select(k, ...)
      if k == true then
        i = i + 1
        k = ASTNode{pos=pos2, 'Number', i} -- call handle_number?
      end
      t[#t+1] = ASTNode{pos=pos2, 'Field', k, v}
    end
    return ASTNode(t)
  end
  self.handle_number = function(pos, id, o)
    return generic_handle(pos, id, tonumber(o))
  end
  self.handle_string = eval_handle
  self.handle_nil = generic_handle
  self.handle_true = generic_handle
  self.handle_false = generic_handle
  self.handle_dots = generic_handle
  self.handle_binop = function(pos, ...) return generic_handle(pos, 'Op', ...) end
  self.handle_unop = function(pos, ...) return generic_handle(pos, 'Op', ...) end
  self.handle_parens = generic_handle
  self.handle_function = generic_handle
  self.handle_index = generic_handle
  self.handle_indexshort =
      function(pos, op, ...) return generic_handle(pos, "Index", ...) end
  self.handle_call = generic_handle
  self.handle_coloncall = generic_handle
  self.handle_identifier = generic_handle
  self.handle_block = generic_handle
  self.handle_local = generic_handle
  self.handle_localfunctiondef = generic_handle
  self.handle_functiondef = generic_handle
  self.handle_assign = generic_handle
  self.handle_do = generic_handle
  self.handle_while = generic_handle
  self.handle_repeat = generic_handle
  self.handle_if = generic_handle
  self.handle_for = generic_handle
  self.handle_forin = generic_handle
  self.handle_explist = generic_handle
  self.handle_varlist = generic_handle
  self.handle_namelist = generic_handle
  self.handle_break = generic_handle
  self.handle_return = generic_handle
  self.handle_funcname = generic_handle
  self.handle_vararg = generic_handle

  self.handle_args = identity_handle

  build_grammar(self)

  return self
end})

-- convert "pos" attribute (character position) to
-- "nline"/"ncols" attributes (line and column numbers)
-- in AST nodes.
local function mark_linecols(ast, s, linecol)
  if type(ast) == "table" then
    ast.nline, ast.ncol = linecol(s, ast.pos)
    for i,ast2 in ipairs(ast) do
      mark_linecols(ast2, s, linecol)
    end
  end
end

function M:parse(o)
  local text
  if io.type(o) then
    text = o:read"*a"
  elseif o == nil then
    text = io.read"*a"
  elseif type(o) == "table" then
    local filename = assert(o[1], "table must contain filename")
    local fh = assert(io.open(filename))
    text = fh:read"*a"
    fh:close()
  elseif type(o) == "string" then
    text = o
  else
    error(tostring(o) .. " not a recognized type", 2)
  end

  text = text:gsub("^#[^\n]*\n", "", 1)  -- remove any shebang line
                                         --FIX: and increase line number

  self.s = text
  
  local ok, result = pcall(lpeg.match, self.grammar, text)
  if not ok then
    if type(result) == 'function' then
      error(result(text))
    else
      error(result)
    end
  end

  -- add line numbers/columns
  local ipos, nline, ncol = 0
  local function linecol(s, i)
    ipos, nline, ncol = M.update_linecol(s, i, ipos, nline, ncol)
    return nline, ncol
  end
  mark_linecols(result, text, linecol)


  return result
end


if ... ~= 'luafish.parser' then

  local p = M()
  local result = p:parse(...) -- note: currently raises error on fail

  print('return ' .. tostring(result))
end


return M

--[=[
=NAME

luafish.parser - Lua 5.1 parser using LPeg.

=SYNOPSIS

  -- command line
  echo 'x=2+3*4; print(x)' | lua lib/luafish/parser.lua
  (the above prints an abstract syntax tree (AST) of the given code)

  -- module usage
  local Parser = require 'luafish.parser'
  local p = Parser()
  print(p:parse('x=2+3*4; print(x)'))

=DESCRIPTION

This parses Lua 5.1 source code using the Lpeg library.

During parsing, it calls call-back functions.  Those can be
overridden, but they currently build an abstract syntax tree (AST).

=DEPENDENCIES

This depends on
  * LPeg http://www.inf.puc-rio.br/~roberto/lpeg.html
    (version >= 0.9)

=STATUS

WARNING! This code is not fully tested.
It's complete enough to parse itself, Kepler, and the Lua 5.1 test
suite ( http://lua-users.org/lists/lua-l/2006-03/msg00716.html ).
The test suite for whether it builds correct ASTs is incomplete.
The AST nodes may change, possibly to make even more consistent with
Metalua.

TODO: report line numbers in error messages.

=INTERFACE

TODO - all the AST node types should be documented.
See source code.

=AUTHOR/CREDITS

David Manura. Licensed under the same terms as Lua itself.

A few of the lexing parts are based on Peter Odding's
Lua 5.1 Lpeg lexing code in http://lua-users.org/wiki/LuaGrammar .

--]=]
