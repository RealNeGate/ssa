inspect = require("inspect")

--------------------------
-- type system
--------------------------
-- { tag="int", min=number, max=number }
-- { tag="ptr", of=TYPE }
local type_pool = {}

-- converted to interned string so it can
-- be put into the pool
local function type_key(t)
    if t.tag == "int" then
        return string.format("int[%d,%d]", t.min, t.max)
    elseif t.tag == "ptr" then
        return string.format("ptr[%s]", t.of)
    else
        error("fuck")
    end
end

-- interns type
local function intern(t)
    local k = type_key(t)
    if type_pool[k] then
        return type_pool[k]
    else
        type_pool[k] = t
        return t
    end
end

local function int_type(a, b) return intern({ tag="int", min=a, max=b }) end
local function ptr_type(t)    return intern({ tag="ptr", of=t })         end

local uint8  = int_type(0, 255)
local uint16 = int_type(0, 65535)

--------------------------
-- node building
--------------------------
local counter = 0
local function make(ty, t, ins)
    counter = counter + 1
    local n = { tag=t, type=ty, gvn=counter, users={}, ins=ins }

    -- fill input list (and users)
    for i,v in ipairs(ins) do
        -- add user
        v.users[#v.users + 1] = n
    end

    return n
end

local function set_in(n, i, v)
    -- remove old user
    local old = n.ins[i]
    if old then
        for i,v in ipairs(old.users) do
            if v == n then
                -- remove swap
                old.users[i] = old.users[#old.users]
                old.users[#old.users] = nil
                break
            end
        end
    end

    n.ins[i] = v
    v.users[#v.users + 1] = n
end

local function node_str(n)
    local t = {}
    local visited = {}

    local function walk(t, n)
        if visited[n.gvn] then
            return
        end
        visited[n.gvn] = true

        for i,v in ipairs(n.ins) do
            walk(t, v)
        end

        t[#t + 1] = string.format("v%d\t%s\t%s\t[ ", n.gvn, n.tag, type_key(n.type))
        for i,v in ipairs(n.ins) do
            t[#t + 1] = string.format("v%d ", v.gvn)
        end

        if n.tag == "num" then
            t[#t + 1] = string.format("]\t%d\n", n.imm)
        else
            t[#t + 1] = "]\n"
        end
    end

    walk(t, n)
    return table.concat(t, "")
end

--------------------------
-- parsing
--------------------------
local code_i = 1
local code = {
    "y", ":=", 4, ";",
    "y", ":=", 16, "*", "y", ";",
}

-- latest definitions
local defs = {}

local function next_tok()
    local t = code[code_i]
    code_i = code_i + 1
    return t
end

local function parse_atom()
    local token = next_tok()
    if type(token) == "number" then
        local n = make(uint16, "num", {})
        n.imm = token
        return n
    elseif type(token) == "string" then
        if not defs[token] then
            error("no definition "..token)
        end

        return defs[token]
    else
        error("fuck "..token)
    end
end

-- EXPR3  ::= SYM | NUMBER
-- EXPR2  ::= EXPR3 (('+'|'-') EXPR3)*
-- EXPR1  ::= EXPR2 (('*'|'/') EXPR2)*
-- EXPR   ::= EXPR1
prec = {
    ["+"] = 1,
    ["-"] = 1,
    ["*"] = 2,
    ["/"] = 2,
}

local function parse_expr(min_prec)
    local lhs = parse_atom()
    while prec[code[code_i]] and prec[code[code_i]] >= min_prec do
        local op = next_tok()
        local rhs = parse_expr(prec[op] + 1)

        lhs = make(uint16, op, {lhs, rhs})
    end

    return lhs
end

-- DEF ::= SYM ':=' EXPR ';'
local function parse_def()
    if code[code_i + 1] ~= ":=" then
        return false
    end

    local name = next_tok()
    next_tok()

    local val = parse_expr(0)
    if next_tok() ~= ";" then
        error("expected semicolon after decl "..code[code_i - 1])
    end

    defs[name] = val
    return true
end

-- STMT ::= DEF | EXPR ';'
local function parse_stmt()
    if     parse_def() then
    elseif parse_expr(0) then
    else   error("bad parse") end
end

print("code:")

while code_i < #code do
    parse_stmt()
end

print(node_str(defs.y))
