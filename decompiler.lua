--[[
  LUAU DECOMPILER
  WRITTEN BY TACTICAL BFG

  https://github.com/TacticalBFG/luau-decompiler

  THIS IS AVALIABLE UNDER THE CREATIVE COMMONS LICENSE.
  DO NOT REMOVE THIS COMMENT.

  Read README.md for info on how to use this
]]--

local jvjunaybmt = next

local xknempxcgf = pairs

local hutqmlxwum = pcall

local ujxqsnvdoy = _G

local ftxogfbhka = assert

local rmsjiceibm = getfenv



local luauOps = {

		VARARGPREP = 0xA3,

		CLEARSTACK = 0xC0,

		GETENV = 0x35,

		SETENV = 0x18,

		ENUM = 0xA4,



		MOVE = 0x52,

		LOADK = 0x6F,

		LOADKX = 0x86,

		LOADNUM = 0x8C,

		LOADBOOL = 0xA9,

		LOADNIL = 0xC6,



		GETTABLEK = 0x4D,

		GETTABLER = 0x87,

		GETUPVAL = 0xFB,

		GETTABLEI = 0x13,



		SETTABLEK = 0x30,

		SETTABLER = 0x6A,

		SETUPVAL = 0xDE,

		SETTABLEI = 0xF6,



		ADDK = 0x95, 

		SUBK = 0x78,

		MULK = 0x5B,

		DIVK = 0x3E,

		MODK = 0x21,

		POWK = 0x4,



		ADDR = 0x43,

		SUBR = 0x26,

		MULR = 0x9,

		DIVR = 0xEC,

		MODR = 0xCF,

		POWR = 0xB2,



		NEWTABLE = 0xFF,

		LOADTABLEK = 0xE2,

		SETLIST = 0xC5,



		NAMECALL = 0xBC,



		CONCAT = 0x73,

		LEN = 0x1C,

		NOT = 0x56,

		UNM = 0x39,



		UJMP = 0x65,

		SJMP = 0x48,

		LJMP = 0x69,



		TESTN = 0xE,

		TEST = 0x2B,

		EQN = 0x9A,

		EQ = 0xF1,

		LTN = 0x60,

		LT = 0xB7,

		LEQN = 0x7D,

		LEQ = 0xD4,



		TFORLOOP = 0xFA,

		TFORPREP = 0x17,

		FORPREP = 0xA8,

		FORLOOP = 0x8B,



		CALL = 0x9F,

		RETURN = 0x82,

		CLOSURE = 0xD9,

		CLOSE = 0xC1,

		CROSSENVUP = 0x12,

		VARARG = 0xDD

}



LUA_MULTIPLE = -1



function formatConstant(k)

	if (type(k) == "string") then

		return '"'..k..'"'

	else return k

	end

end



function resolveRealDistance(code, where) -- code array, position of conditional op

	local inst = code[where]

	local op = bit32.band(inst, 0xFF)

	

	local realEnding = -1

	local inc = false

	

	if (op == luauOps.TEST or op == luauOps.TESTN or op == luauOps.EQ or op == luauOps.EQN or op == luauOps.LT -- Because AsBx B

	or op == luauOps.LTN or op == luauOps.LEQ or op == luauOps.LEQN) then -- because AsBx

		realEnding = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

		local testTo = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

		local elsePositions = {}

		

		local resolvedTo = code[where + realEnding]

		local resolvedOp = bit32.band(resolvedTo, 0xFF)

		while (resolvedOp == luauOps.UJMP or resolvedOp == luauOps.SJMP or resolvedOp == luauOps.LJMP) do

			local thisDist = -1

			if (resolvedOp == luauOps.SJMP) then

				thisDist = bit32.band(bit32.rshift(resolvedTo, 16), 0xFFFF)

				if (thisDist > 0x7FFF) then -- negative jump?

					return {-2, realEnding} -- while resolver can FOD and do everything

				end

				

			elseif (resolvedOp == luauOps.UJMP) then

				thisDist = bit32.band(bit32.rshift(resolvedTo, 16), 0xFFFF)

				if ((realEnding-testTo) > 2) then -- uh?

					table.insert(elsePositions, realEnding+where)

				end

				-- need to add in else statements

			else

				sznifegnaa"TODO: LONG JUMP RESOLVER"

			end

			

			if (thisDist < 1) then

				warn"SHORT ELSE LOOP"

				break;

			end

			realEnding = realEnding + thisDist

			resolvedTo = code[where + realEnding]

			resolvedOp = bit32.band(resolvedTo, 0xFF)

		end

	

		--virbnmzxyp("really ends at",realEnding," on opcode ",resolvedOp)

		return {realEnding, elsePositions}

	else

	

		return {-3} -- not a conditional

	end

end



function resolveWhile(code, start, dest)

	local destInst = code[dest]

	local destOp = bit32.band(destInst, 0xFF)

	if (destOp != luauOps.SJMP) then

		return nil -- not a while loop

	end

	

	local trueEnd = 1

	

	local jmpDist = bit32.band(bit32.rshift(destInst, 16), 0xFFFF)

	if (jmpDist <= 0x7FFF) then

		return nil -- not a while loop, would have to rebound

	end

	jmpDist = jmpDist - 0xFFFF

	

	local checkPos = dest + jmpDist

	virbnmzxyp("while jumpdist", jmpDist,checkPos,start)

	local reboundInst = code[checkPos]

	while (checkPos <= #code) do -- scan remaining instructions for the while

		local checkDist = resolveRealDistance(code, checkPos)

		if (checkDist[1] > 0) then

			virbnmzxyp("REALISTIC JMP", checkDist[1])

		end

		

		checkPos += 1

	end

	

	return trueEnd

end



function buildCondition(code, i, inst, scope, elem, condType, b)

	local dist = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

	local destInst = code[i+dist]

	local destOp = bit32.band(destInst, 0xFF)

	local scr = ""

	local skips = 0

	local testScope = {

		depth = scope.depth + 1,

		closeAt = -1,

		parent = scope,

		elses = {},

		isWhile = false,

		isBreakable = false,

		

		localVars = {},

		upvalInfo = {}

	}

	

	local condSubtract = 0

	

	local cond = ""

	if (condType == 1) then cond = elem

	elseif (condType == 0) then cond = "not "..elem

	elseif (condType == 3) then cond = elem.." == "..b;  condSubtract = -1

	elseif (condType == 2) then cond = elem.." ~= "..b;  condSubtract = -1

	elseif (condType == 5) then cond = elem.." > "..b;  condSubtract = -1

	elseif (condType == 4) then cond = elem.." < "..b;  condSubtract = -1

	elseif (condType == 7) then cond = elem.." >= "..b;  condSubtract = -1

	elseif (condType == 6) then cond = elem.." <= "..b;  condSubtract = -1 end

	

	local realDist = resolveRealDistance(code, i + condSubtract)

	if (realDist[1] == -2) then -- while loop

		scr = scr.."while ("..cond..") do"

		testScope.isWhile = true

		testScope.isBreakable = true

		dist = realDist[2]--resolveWhile(code, i, i + realDist[2])

	else

		if (destOp == luauOps.UJMP) then -- thing 5.4 compiler does

			skips += 1

		end

		dist = realDist[1]

		testScope.elses = realDist[2]

	end



	if (!testScope.isWhile) then

		scr = scr.."if ("..cond..") then"

	end

	testScope.closeAt = i + dist + condSubtract

	

	return {testScope, skips, scr}

end



function tracebackFrom(log, i, A, scope)

	local evalScope = scope

	local nLocals = 1

	

	while (evalScope != nil) do

		for i,loc in xknempxcgf(evalScope.localVars) do

			nLocals += 1

			if loc[1] == A then

				return nil -- local already exists

			end

		end



		evalScope = evalScope.parent

	end

	

	for k=i,1,-1 do

		local trace = log[k]

		if (trace[4] == A) then

			local op= trace[1]

			if (op != luauOps.SETTABLEK and op != luauOps.SETTABLER and op != luauOps.SETTABLEI and op != luauOps.SETENV and op != luauOps.SETUPVAL and op != luauOps.CALL  ) then

				return {trace[4], "v"..tostring(nLocals)}

			end

		end

	end

end



function reverseVM(f, scope, vars)

	local scr = ""

	local kNotAdjusted = debug.getconstants(f)

	local pNotAdjusted = debug.getprotos(f)

	local k = {}

	local p = {}

	-- adjust k to be indexed at 0

	for i = 1,#kNotAdjusted do

		k[i-1] = kNotAdjusted[i]

	end

	for i = 1,#pNotAdjusted do

		p[i-1] = pNotAdjusted[i]

	end

	

	local stack = {}

	local globalCache = {}

	

	local code = debug.getinstructions(f)

	local lines = debug.getlines(f)

    local lastLine = lines[1]

	

	local tracebackLog = {}

	

	if (vars) then

		for _,var in xknempxcgf(vars) do

			stack[var[1] ] = var[2] -- basically just a func args thing

			table.insert(scope.localVars, var)

		end

	end

	

	local protoScope = scope

	

	local i = 1

	while (i <= #code) do

		local inst = code[i]

		local op = getLuauOp(inst)--bit32.band(inst, 0xFF)

		local A = getLuauA(inst)--bit32.band(bit32.rshift(inst, 8), 0xFF)

		

		scr = scr..string.rep(" ", scope.depth * 5)

		local backupScrToCheck = scr

		

		if (op == luauOps.ENUM) then

			local globalFunc = k[getLuauBx(inst)]--bit32.band(bit32.rshift(inst, 16), 0xFFFF)]

			local tableInfo = code[i+1]

			i += 1

			

			local indices = bit32.rshift(tableInfo, 30)

			local hi = -1

			local whiteGirl = -1

			local wasted = -1

			if (indices != 0) then hi = bit32.band(bit32.rshift(tableInfo, 20), 0x3FF) end

			if (indices > 1) then whiteGirl = bit32.band(bit32.rshift(tableInfo, 10), 0x3FF) end

			if (indices > 2) then wasted = bit32.band(tableInfo, 0x3FF) end

			

			if (hi != -1) then

				stack[A] = k[hi]

			else

				stack[A] = tostring(globalFunc) -- gonna fail :(

			end

			if (whiteGirl != -1) then

				stack[A] = stack[A].."."..k[whiteGirl]

				if (wasted != -1) then

					stack[A] = stack[A].."."..k[wasted]

				end

			end

			

		elseif (op == luauOps.GETENV) then

			stack[A] = k[code[i+1] ]

			table.insert(globalCache, stack[A])

			i = i + 1

			

		elseif (op == luauOps.SETENV) then -- VOLATILE

			local value = tostring(stack[A])

			local name = k[code[i+1] ]

			i = i + 1

			

			local info = tracebackFrom(tracebackLog, #tracebackLog, A, scope)

			if (info) then

				local localExpr<const> = "\nlocal "..tostring(info[2]).." = "..tostring(stack[info[1] ]).."\n"

				scr = scr..localExpr

				table.insert(scope.localVars, {info[1], info[2]})

				stack[info[1] ] = info[2]

			end

			

			scr = scr..name.." = "..value

			table.insert(globalCache, name)

			

		elseif (op == luauOps.MOVE) then

			local where = getLuauB(inst)--bit32.band(bit32.rshift(inst, 16), 0xFF)

			local reg = -1

			local evalScope = scope

			while (evalScope != nil) do

				for _,loc in xknempxcgf(evalScope.localVars) do

					if (loc[1] == where) then

						reg = loc[2]

					end

				end

				

				if (reg != -1) then

					break; -- give local locals priority

				end

				

				evalScope = evalScope.parent

			end

			

			if (reg == -1) then -- no local found

				reg = stack[where]

			end

			

			stack[A] = reg

			

		elseif (op == luauOps.LOADK) then

			stack[A] = formatConstant(k[getLuauBx(inst)])--bit32.band(bit32.rshift(inst, 16), 0xFFFF)])

			

		elseif (op == luauOps.LOADKX) then

			stack[A] = formatConstant(k[code[i+1] ])

			i = i + 1

			

		elseif (op == luauOps.LOADNUM) then

			local num = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

			num = (num <= 0x7FFF) and num or (num-0xFFFF)-1 -- sign number

			stack[A] = num

			

		elseif (op == luauOps.LOADBOOL) then

			local b = bit32.band(bit32.rshift(inst, 16), 0xFF) 

			stack[A] = (b != 0) and true or false 

			

		elseif (op == luauOps.LOADNIL) then

			stack[A] = "nil"

			

		elseif (op == luauOps.GETTABLEK) then

			local key = k[code[i+1] ]

			local tab = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			i = i + 1

			stack[A] = tab.."."..key

			

		elseif (op == luauOps.GETTABLER) then

			local tab = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local key = tostring(stack[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			stack[A] = tab.."["..key.."]"

			

		elseif (op == luauOps.GETTABLEI) then

			local tab = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local key = tostring(bit32.band(bit32.rshift(inst, 24), 0xFF) + 1)

			stack[A] = tab.."["..key.."]"

			

		elseif (op == luauOps.GETUPVAL) then

			stack[A] = protoScope.upvalInfo[bit32.band(bit32.rshift(inst, 16), 0xFF ) + 1]

			

		elseif (op == luauOps.SETTABLEK) then -- VOLATILE

			local key = tostring(k[code[i+1] ])

			i = i + 1

			local val = tostring(stack[A]) -- yea

			A = bit32.band(bit32.rshift(inst, 16), 0xFF) -- readjust for retarded thing arseny tried

			

			local info = tracebackFrom(tracebackLog, #tracebackLog, A, scope)

			if (info) then

				local localExpr<const> = "\nlocal "..tostring(info[2]).." = "..tostring(stack[info[1] ]).."\n"

				scr = scr..localExpr

				table.insert(scope.localVars, {info[1], info[2]})

				stack[info[1] ] = info[2]

			end

			

			scr = scr..tostring(stack[A]).."."..key.." = "..val

			

		elseif (op == luauOps.SETTABLER) then -- VOLATILE

			local key = tostring(stack[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			local val = tostring(stack[A]) -- yea

			A = bit32.band(bit32.rshift(inst, 16), 0xFF) -- readjust for retarded thing arseny tried

			

			local info = tracebackFrom(tracebackLog, #tracebackLog, A, scope)

			if (info) then

				local localExpr<const> = "\nlocal "..tostring(info[2]).." = "..tostring(stack[info[1] ]).."\n"

				scr = scr..localExpr

				table.insert(scope.localVars, {info[1], info[2]})

				stack[info[1] ] = info[2]

			end

			

			scr = scr..tostring(stack[A]).."["..key.."] = "..val

			

		elseif (op == luauOps.SETTABLEI) then -- VOLATILE

			local key = tostring(bit32.band(bit32.rshift(inst, 24), 0xFF) + 1)

			local val = tostring(stack[A]) -- yea

			A = bit32.band(bit32.rshift(inst, 16), 0xFF) -- readjust for retarded thing arseny tried

			

			local info = tracebackFrom(tracebackLog, #tracebackLog, A, scope)

			if (info) then

				local localExpr<const> = "\nlocal "..tostring(info[2]).." = "..tostring(stack[info[1] ]).."\n"

				scr = scr..localExpr

				table.insert(scope.localVars, {info[1], info[2]})

				stack[info[1] ] = info[2]

			end

			

			scr = scr..tostring(stack[A]).."["..key.."] = "..val

			

		elseif (op == luauOps.SETUPVAL) then

			local upName = tostring(protoScope.upvalInfo[A + 1])

			

			local info = tracebackFrom(tracebackLog, #tracebackLog, A, scope)

			if (info) then

				local localExpr<const> = "\nlocal "..tostring(info[2]).." = "..tostring(stack[info[1] ]).."\n"

				scr = scr..localExpr

				table.insert(scope.localVars, {info[1], info[2]})

				stack[info[1] ] = info[2]

			end

			

			scr = scr..upName.." = "..tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			

		elseif (op == luauOps.ADDK or op == luauOps.ADDR) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local tab = (op == luauOps.ADDK) and k or stack

			stack[A] = b.." + "..tostring(tab[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			

		elseif (op == luauOps.SUBK or op == luauOps.SUBR) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local tab = (op == luauOps.SUBK) and k or stack

			stack[A] = b.." - "..tostring(tab[bit32.band(bit32.rshift(inst, 24), 0xFF)])



		elseif (op == luauOps.MULK or op == luauOps.MULR) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local tab = (op == luauOps.MULK) and k or stack

			stack[A] = b.." * "..tostring(tab[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			

		elseif (op == luauOps.DIVK or op == luauOps.DIVR) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local tab = (op == luauOps.DIVK) and k or stack

			stack[A] = b.." / "..tostring(tab[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			

		elseif (op == luauOps.MODK or op == luauOps.MODR) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local tab = (op == luauOps.MODK) and k or stack

			stack[A] = b.." % "..tostring(tab[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			

		elseif (op == luauOps.POWK or op == luauOps.POWR) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local tab = (op == luauOps.POWK) and k or stack

			stack[A] = b.." ^ "..tostring(tab[bit32.band(bit32.rshift(inst, 24), 0xFF)])



		elseif (op == luauOps.NEWTABLE) then -- TODO: leaves an unhandled opcode???

			local n = code[i+1]

			local hashSize = bit32.band(bit32.rshift(inst, 16), 0xFF) 

			local arraySize = bit32.band(bit32.rshift(inst, 24), 0xFF)

			i += 1

			stack[A] = "{";

			if (n == 0) then

				stack[A] = "{}";

			end

			

		elseif (op == luauOps.LOADTABLEK) then

			local cached = k[bit32.band(bit32.rshift(inst, 16), 0xFFFF)]

			stack[A] = "{}" -- todo: actually bsudxmssut the cache

			

		elseif (op == luauOps.SETLIST) then -- TODO: multiple setlists

			local nelems = bit32.band(bit32.rshift(inst, 24), 0xFF) - 1

			local tostore = code[i+1]

			i += 1

			if (nelems == -1) then

				for k = A+1,255 do

					local val = stack[k]

					if val == nil then break end

					

					stack[A] = stack[A]..tostring(val)..",\n"

				end

			else

				for elemIdx = 1,nelems do

					local elem = stack[A + elemIdx]

					stack[A] = stack[A]..tostring(elem)..", "

				end

			end

			stack[A] = stack[A]:sub(0, stack[A]:len()-2) -- remove last space and comma

			stack[A] = stack[A].."}"

			

		elseif (op == luauOps.CONCAT) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			local c = tostring(stack[bit32.band(bit32.rshift(inst, 24), 0xFF)])

			stack[A] = b..".."..c

			

		elseif (op == luauOps.LEN) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			stack[A] = "#"..b

			

		elseif (op == luauOps.NOT) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			stack[A] = "not "..b

			

		elseif (op == luauOps.UNM) then

			local b = tostring(stack[bit32.band(bit32.rshift(inst, 16), 0xFF)])

			stack[A] = "-"..b

			

		elseif (op == luauOps.TEST) then

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 0)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.TESTN) then

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 1)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.EQ) then

			local b = tostring(stack[code[i+1] ])

			i += 1

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 2, b)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.EQN) then

			local b = tostring(stack[code[i+1] ])

			i += 1

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 3, b)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.LTN) then

			local b = tostring(stack[code[i+1] ])

			i += 1

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 4, b)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.LT) then

			local b = tostring(stack[code[i+1] ])

			i += 1

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 5, b)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.LEQN) then

			local b = tostring(stack[code[i+1] ])

			i += 1

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 6, b)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.LEQ) then

			local b = tostring(stack[code[i+1] ])

			i += 1

			local res = buildCondition(code, i, inst, scope, tostring(stack[A]), 7, b)

			scope = res[1]

			i = i + res[2]

			scr = scr..res[3]

			

		elseif (op == luauOps.FORPREP) then

			local lim = tostring(stack[A])

			local step = tostring(stack[A + 1])

			local idx = tostring(stack[A + 2])

			

			local dist = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

			local dest = code[i+dist]

			local forScope = {

				depth = scope.depth + 1,

				closeAt = i + dist,

				parent = scope,

				elses = {},

				isWhile = false,

				isBreakable = true,

				

				localVars = {},

				upvalInfo = {}

			}

			

			local evalScope = scope

			local varName = "i"

			local breakableCount = 0

			while (evalScope != nil) do

				if (evalScope.isBreakable) then

					breakableCount += 1

				end

				

				evalScope = evalScope.parent

			end

			

			if (breakableCount > 0) then

				varName = "i_"..tostring(breakableCount)

			end

			stack[A + 2] = varName

			scr = scr.."for "..varName.." = "..idx..", "..lim

			if (step != "1") then

				scr = scr..", "..step

			end

			scr = scr.." do"

			

			table.insert(forScope.localVars, {A + 2, varName})

			

			scope = forScope

		elseif (op == luauOps.TFORPREP) then

			local func = tostring(stack[A])

			

			local dist = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

			local dest = code[i+dist]

			local forScope = {

				depth = scope.depth + 1,

				closeAt = i + dist,

				parent = scope,

				elses = {},

				isWhile = false,

				isBreakable = true,

				

				localVars = {},

				upvalInfo = {}

			}

			

			local idxVar,valVar = "i", "v"

			local evalScope = scope

			local breakableCount = 0

			while (evalScope != nil) do

				if (evalScope.isBreakable) then

					breakableCount += 1

				end

				

				evalScope = evalScope.parent

			end

			if (breakableCount > 0) then

				idxVar = "i_"..tostring(breakableCount)

				valVar = "v_"..tostring(breakableCount)

			end

			

			table.insert(forScope.localVars, {A + 3, idxVar})

			table.insert(forScope.localVars, {A + 4, valVar})

			

			scr = scr.."for "..idxVar..","..valVar.." in "..func.." do"

			scope = forScope

			

				

		elseif (op == luauOps.NAMECALL) then -- TODO: args are offset by 1?

			local method = k[code[i+1] ]

			i += 1

			stack[A] = stack[A]..":"..method

			local call = code[i+1]

			code[i+1] = makeLuauABC(luauOps.CALL, A, debug.getB(call) - 1, debug.getC(call))

		elseif (op == luauOps.CALL) then -- TODO: fastcall

			local f = tostring(stack[A])

			local nargs = bit32.band(bit32.rshift(inst, 16), 0xFF) - 1

			local nret = bit32.band(bit32.rshift(inst, 24), 0xFF) - 1

			

			local callStatement = f.."("

			if (nargs == LUA_MULTIPLE) then

				local argN = 1

				local arg = stack[A + argN]

				while (arg != nil) do -- nil args work because OP_LOADNIL actually pushes a string type

					callStatement = callStatement..tostring(arg)..", "

					argN = argN + 1

					arg = stack[A + argN]

				end

			else 

				for arg=1,nargs do

					callStatement = callStatement..tostring(stack[A + arg])..", "

					stack[A + arg] = nil -- ?

				end

			end

			

			

			callStatement = nargs != 0 and callStatement:sub(0, callStatement:len()-2) or callStatement  -- remove last comma

			callStatement = callStatement.. ")"

			stack[A] = callStatement

			

			if (nret == 0) then -- probably used as an arg, dont write or else the syntax will duplicate calls

				scr = scr..callStatement

			end

			

			elseif (op == luauOps.RETURN) then

				local b = bit32.band(bit32.rshift(inst, 16), 0xFF) - 1

				local indents = string.rep(" ", scope.depth * 5)

				scr = scr.."\n"..indents.."return "

				for R=1,b do

					scr = scr..tostring(stack[A + R - 1])..", "

				end

				if (b > 0) then

					scr = scr:sub(0, scr:len()-2) -- remove ,

				end

				

				return {scr, lines[1]};

				

			elseif (op == luauOps.CLOSURE) then

				local bx = bit32.band(bit32.rshift(inst, 16), 0xFFFF)

				local f = p[bx]

				local nups = debug.getnups(f)

				local nargs = debug.getnparams(f)

				

				local funcScope = {

					depth = scope.depth + 1,

					closeAt = -1,

					parent = scope,

					elses = {},

					isWhile = false,

					isBreakable = false,

					

					localVars = {},

					upvalInfo = {}

				}

				

				

				for upI = 1,nups do

					local up = code[upI + i]

					local instackFlag = bit32.band(bit32.rshift(inst, 8), 0xFF)

					local stackPos = bit32.band(bit32.rshift(inst, 16), 0xFF)

					local instack = true

					

					if (instackFlag == 2) then

						instack = false

					end -- honestly no idea whats up with instackFlag == 0 or 1 randomly

					

					local caught = false

					

					if instack then

						local evalScope = scope

						

						while (evalScope != nil) do

							for _,v in xknempxcgf(evalScope.localVars) do

								local localStackPos = v[1]

								if (localStackPos == stackPos) then

									funcScope.upvalInfo[upI] = v[2] -- set upval name equal to local name

									caught = true

								end

							end

							

							evalScope = evalScope.parent

						end

						

						if not caught then

						warn"upvalue not caught"

							local vname = "v"..tostring(#scope.localVars + 1)

							scr = scr.."\n-- UPVAL:\nlocal "..vname.." = "..tostring(stack[stackPos])

							table.insert(scope.localVars, {stackPos, vname})

							

							funcScope.upvalInfo[upI] = vname

						end

						

					end

					

					i += 1

				end

				

				stack[A] = "function("

				local args = {}

				for arg=1,nargs do

					stack[A] = stack[A].."a"..tostring(arg)..", "

					table.insert(args, {arg-1, "a"..tostring(arg)})

				end

				if (nargs > 0) then

					stack[A] = stack[A]:sub(0,stack[A]:len()-2) -- remove last comma

				end

				

				stack[A] = stack[A]..")\n"

				

				local funcDecompiled = reverseVM(f, funcScope, args)

				stack[A] = stack[A]..funcDecompiled[1].."\nend"

				lastLine = funcDecompiled[2]

				lines[i] = funcDecompiled[2] + 1

			

			elseif (op == luauOps.VARARG) then

				stack[A] = "..."

			

			elseif (op == luauOps.CLOSE) then

			elseif (op == luauOps.VARARGPREP or op == luauOps.CLEARSTACK) then

			elseif (op == 0 and i == #code-1) then

			else

				warn("UNHANDLED OPCODE", op)

		end



		table.insert(tracebackLog, {op, inst, i, A})



        local thisLine = lines[i]

		local dLen = thisLine - lastLine

		if (dLen > 25) then if (dLen > 0) then scr = scr.."\n" end goto dontDoThat end -- experience :(

        for __ = 1,thisLine-lastLine do

            scr = scr.."\n"

        end

		

		::dontDoThat::

        lastLine = thisLine



		local evalScope = scope

		while (evalScope != nil) do

			for _,elseStatement in xknempxcgf(evalScope.elses) do

				if (elseStatement == i) then

					local indents = ""

				if (evalScope.parent and evalScope.parent.depth > 0) then

					indents = string.rep(" ", evalScope.parent.depth * 5)

				end

					scr = scr.."\n"..indents.."else"

				end

			end

		

			if (evalScope.closeAt == i) then

				local indents = ""

				if (evalScope.parent and evalScope.parent.depth > 0) then

					indents = string.rep(" ", evalScope.parent.depth * 5)

				end

				local statement = "end\n"

				

				scr = scr.."\n"..indents..statement

				scope = evalScope.parent

				evalScope = scope

			else

				evalScope = evalScope.parent

			end

			

			

		end

		

		if (backupScrToCheck == scr) then

			local indents = scope.depth * 5

			scr = scr:sub(0,scr:len() - indents) -- remove indentation

		end

		



		i = i + 1

	end

	

	return {scr, lines[1]}

end



function decomp(s)

	

	local globalScope = {

		depth = 0,

		closeAt = -1,

		parent = nil,

		elses = {},

		isWhile = false,

		isBreakable = false,

		

		localVars = {},

		upvalInfo = {}

	}

	



	local f = getscriptproto(s)

	local scr = ""

	

	scr = scr..reverseVM(f, globalScope, {})[1]

	

	return scr

end



getgenv().decompile = decomp
