local class = require 'ext.class'
local table = require 'ext.table'
local range = require 'ext.range'
local number = require 'ext.number'
local math = require 'ext.math'

local BigNumber = class()

function BigNumber:init(n, base)
	self.negative = false
	self.nan = false
	self.infinity = false
	self.repeatFrom = nil
	self.repeatTo = nil
-- hmm... this causes errors in longIntDiv
-- but without this I can't make the assertion in toBase() that we are getting an integer	
--	self.minExp = 0
--	self.maxExp = 0
	self.base = base or 10
	assert(self.base > 1, "can't set to a base of 1 or lower")
	if type(n) == 'string' then
		if n == 'nan' then self.nan = true end
		if n == '0' then return end
		if n:sub(1,1) == '-' then
			n = n:sub(2)
			self.negative = true
		end
		for i=1,#n do
			self[i-1] = tonumber(n:sub(-i,-i), self.base)
		end
		self.minExp = 0
		self.maxExp = #n-1
		self:removeLeadingZeroes()
	elseif type(n) == 'number' then
--[[ original integer-only code
		n = math.floor(n)
		if n < 0 then
				self.negative = true
				n = -n
		end
		local i = 0
		while n > 0 do
				self[i] = n % self.base
				n = (n - self[i]) / self.base
				self.minExp = 0
				self.maxExp = i
				i = i + 1
		end
--]]
-- [[ my attempt for decimal numbers	
		if n < 0 then
			self.negative = true
			n = -n
		end
		-- TODO use longIntDiv, esp with non-10 bases, so we can take advantage of repeating digits
		-- TODO between this and toBase(), the code is very similar
		-- and toBase uses BigNumbers for accuracy...
		-- and this allows for fractional bases...
		-- TODO first convert the number in base-10 or base-2 or something
		-- (using ffi we can assign it to a double[1] and then use its bits to determine the number exactly)
		-- then use toBase to convert it to whatever base is desired
		-- and change that code to take decimals (and repeating decimals) into account
		if n > 0 then
			local i = math.floor(math.log(n, self.base))
			self.minExp = i
			self.maxExp = i
			local p = self.base^i
			while n > 0 do
				local d = math.floor((n / p) % self.base)
				if not math.isfinite(d) then 
					self.trailsoff = true
					break 
				end
				self[i] = d 
				n = n - d * p
				p = p / self.base
				self.minExp = i
				i = i - 1
			end
			
			-- TODO remove the constraint that minExp cannot be > 0
			for j=i,0,-1 do
				self[j] = 0
			end
			self.minExp = math.min(self.minExp, 0)
		end
--]]	
	elseif type(n) == 'table' then
		if BigNumber.is(n) then
			for k,v in pairs(n) do
				self[k] = v
			end
		else
			for i=1,#n do
				self[i-1] = tonumber(n[i]) % self.base
			end
			self.minExp = 0
			self.maxExp = #n-1
			self.negative = n.negative or false 
			self.nan = n.nan or false
			self.infinity = n.infinity or false
			
			--self.base = n.base or self.base
			if self.base ~= n.base then
				if self.nan or self.infinity then
					self.base = n.base
				else
					error("now you need to convert toBase in-place on constructor")
				end
			end
		end
		self:removeLeadingZeroes()
	elseif n == nil then
	else
		error("don't know how to handle the input of type "..type(n))
	end
end

function BigNumber.__sub(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if a.infinite then
		if b.infinite then
			if a.negative == b.negative then 
				return BigNumber{infinite = true, negative = a.negative}
			else
				return BigNumber.nan
			end
		end
	end
	-- if signs don't match, treat it like an addition
	if a.negative ~= b.negative then
		if a.negative and not b.negative then
			return -((-a) + b)
		elseif b.negative and not a.negative then
			return a + (-b)
		end
	end
	-- signs match, make sure abs biggest is 'a'
	if a.negative then
		if b < a then return -(b - a) end
	else
		if a < b then return -(b - a) end
	end
	if a:isZero() then return -b end
	if b:isZero() then return a end
	local c = BigNumber()
	c.negative = a.negative
	c.minExp = 0
	c.base = a.base
	local borrow = 0	-- or -1
	local i = 0
	while i <= a.maxExp or i <= b.maxExp do	-- shouldn't allow borrow past the end
		local digit = (a[i] or 0) - (b[i] or 0) + borrow
		borrow = 0
		if digit < 0 then 
			borrow = -1
			digit = digit + a.base
		end
		c[i] = digit
		c.maxExp = i
		i = i + 1
	end
	-- this fails for big(2.5, 2.5) / big(5)
	assert(borrow == 0)
	c:removeLeadingZeroes()
	return c
end

function BigNumber:removeLeadingZeroes()
	-- remove leading zeroes
	while true do
		self:calcMaxExp()
		assert(self:isZero() or self[self.maxExp] ~= nil)
		if self[self.maxExp] ~= 0 then break end
		self[self.maxExp] = nil
	end
	self:calcMaxExp()
end

-- in-place
function BigNumber:carry()
	local i = 0
	local carry = 0
	while true do
		self[i] = (self[i] or 0) + carry
		carry = 0
		while self[i] >= self.base do
			self[i] = self[i] - self.base
			carry = carry + 1
		end
		i = i + 1
		if i > self.maxExp then
			if carry == 0 and (self[i] == 0 or self[i] == nil) then break end
		end
	end
	self:removeLeadingZeroes()
end

--[[
an..a0.a-1..a(1-p)[a(-p)..a(-q)]
+ same thing with b's ...
a = sum_i=0..n B^i a_i + 10^(1-p) * sum_i=p..q a^i / (9...9) (p-q 9's)
b = same thing
a + b = 
--]]



function BigNumber.__add(a,b)
--print('BigNumber.__add',a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if b.base ~= a.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if a.infinite then
		if b.infinite then
			if a.negative == b.negative then 
				return BigNumber{infinite = true, negative = a.negative}
			else
				return BigNumber.nan
			end
		end
	end
	if a.negative ~= b.negative then
		if a.negative and not b.negative then
			return b - (-a)
		elseif b.negative and not a.negative then
			return a - (-b)
		end
	end
	if a:isZero() then return b end
	if b:isZero() then return a end
	local c = BigNumber()
	c.negative = a.negative	-- == b.negative
	c.minExp = 0
	if a.repeatFrom then c.minExp = math.min(c.minExp, a.repeatFrom+1) end
	if b.repeatFrom then c.minExp = math.min(c.minExp, b.repeatFrom+1) end
	c.base = a.base
	local carry = 0
	local i = c.minExp
	while i <= a.maxExp or i <= b.maxExp or carry > 0 do
		local digit = (a[i] or 0) + (b[i] or 0) + carry
--		local avalue = 0
--		local bvalue = 0
--		if a.repeatFrom and i > a.repeatFrom then avalue = a[i] or 0 end
--		if b.repeatFrom and i > b.repeatFrom then bvalue = b[i] or 0 end
--		local digit = avalue + bvalue + carry
		carry = math.floor(digit / a.base)
		c[i] = digit % a.base
		c.maxExp = i
		i = i + 1
	end

-- and now for something completely different
	if a.repeatFrom or b.repeatFrom then
		local aRep = a:getRepeatAsInteger()
		local bRep = b:getRepeatAsInteger()
print('aRep', aRep)
print('bRep', bRep)
		-- next pad up whichever has the larger (smaller abs) repeatTo until they're both even
		if a.repeatFrom and b.repeatFrom then
print'padding to align'
			if a.repeatFrom < b.repeatFrom then
print'padding a'				
				for i=a.minExp, a.maxExp do
					a[i+b.repeatFrom-a.repeatFrom] = a[i]
				end
				for i=0,minExp-1 do
					a[i] = 0
				end
			elseif b.repeatFrom < a.repeatFrom then
print'padding b'				
				for i=b.minExp, b.maxExp do
					b[i+a.repeatFrom-b.repeatFrom] = b[i]
				end
				for i=0,minExp-1 do
					b[i] = 0
				end
			end
		end
print('aRep', aRep)
print('bRep', bRep)
local aNines
if aRep == 0 then 
print('aNines is 1')
	aNines = 1
else 
	aNines = range(aRep.maxExp+1):map(function(i) return a.base-1 end)
print('aNines is from '..aNines:concat', ')	
	aNines.base = a.base
	aNines = BigNumber(aNines)
end
print('aNines',aNines)
local bNines
if bRep == 0 then
print('bNines is 1')
	bNines = 1
else
	bNines = range(bRep.maxExp+1):map(function(i) return b.base-1 end)
print('bNines is from '..bNines:concat', ')	
	bNines.base = b.base
	bNines = BigNumber(bNines)
end
print('bNines',bNines)
-- TODO denom, aNines, and bNines, can all be reduced by the # of nines they have in common
local cRep = (aRep * bNines + bRep * aNines) / (aNines * bNines) 
print('cRep', cRep, require'ext.tolua'(cRep))
-- what happens if cRep is bigger than aRep or bRep?  where do the extra digits go?  are they repeated or not?		
		local repeatTo
		if a.repeatTo and not b.repeatTo then 
			repeatTo = a.repeatTo
		elseif b.repeatTo and not a.repeatTo then
			repeatTo = b.repeatTo
		else
			repeatTo = math.min(a.repeatTo, b.repeatTo)
		end
print('repeatTo',repeatTo)		
print('c.minExp', c.minExp, 'c.maxExp', c.maxExp)
		for i=c.minExp, c.maxExp do
			c[i+repeatTo] = cRep[i]
print('i',i, 'i+repeatTo', i+repeatTo, 'c[i+repeatTo]', c[i+repeatTo])
		end
		c.repeatTo = repeatTo
		c.repeatFrom = repeatTo + (c.maxExp - c.minExp)
		assert(c.minExp >= c.repeatTo)
		c.minExp = c.repeatTo
print('c', c)
print('c', require'ext.tolua'(c))
	end
	return c
end

function BigNumber:getRepeatAsInteger()
	if not self.repeatFrom or not self.repeatTo then
		assert(not self.repeatFrom and not self.repeatTo)
		return BigNumber(0, self.base)
	end
	local digits = range(self.repeatTo, self.repeatFrom):mapi(function(i)
		return self[i], i-self.repeatTo+1
	end)
	digits.base = self.base
	return BigNumber(digits)
end

function BigNumber.__unm(a)
	a = BigNumber(a)
	a.negative = not a.negative
	return a
end

function BigNumber.simpleMul(a,b)	-- TODO better multiplication algorithm!
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigInit.nan end
	if a.maxExp == nil or b.maxExp == nil then return BigNumber() end
	local bWasNegative = b.negative
	if b.negative then
		b = -b
	end
	local c = BigNumber()
	c.base = a.base
	local counter = BigNumber()
	while counter ~= b do
		c = c + a
		counter = counter + BigNumber(1)
	end
	c.negative = a.negative ~= bWasNegative
	return c
end
function BigNumber.longMul(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if a.maxExp == nil or b.maxExp == nil then return BigNumber() end
	local c = BigNumber()
	c.base = a.base
	c.negative = a.negative ~= b.negative
	c.minExp = 0
	c.maxExp = a.maxExp + b.maxExp
	for i=0,c.maxExp do
		c[i] = 0
	end
	-- 1) add digits
	for i=0,a.maxExp do
		local ai = a[i] or 0
		for j=0,b.maxExp do
			local bj = b[j] or 0
			c[i+j] = c[i+j] + ai * bj
		end
	end
	-- 2) apply carries
	c:carry()
	return c
end
BigNumber.__mul = BigNumber.longMul

--[[
a^b = (an..a0)^b = (10^n an + ... + 10^0 a0)^b
 = (10^n an + ... + a0) * ... * (10^n an + ... + a0) (b times)
 
(a0 + x a1)^2 = a0^2 + x 2 a0 a1 + x^2 a1^2
(a0 + x a1 + x^2 a2)^2 = 
	a0 a0
	+ x * ((a0 + a1) + (a1 + a0))
	+ x^2 * (a0 a2 + a1 a1 + a2 a0)
	+ x^3 * (a2 a1 + a1 a2)
	+ x^4 * a2 a2

this one turns out to run *really* slow,
and breaks for n > 80 or so
--]]
function BigNumber.intPow_binomial(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if b:isZero() then return BigNumber{1, base=a.base} end
	if a:isZero() then return BigNumber{base=a.base} end
	if b.negative then error('no support for negative powers!') end
	--'b' iterators cycling through the coefficients of 'a'
	-- so iters will be sized by a's digits and increment until it reaches b
	local iters = table()
	local bNum = b:tonumber()	-- for our iterator table ... this limits this power method to only exponents of 4billion (or whatever the precision of doubles is)
	for i=1,bNum do
		iters[i] = a.minExp	
	end
	local c = BigNumber()
	c.base = a.base
	c.minExp = a.minExp ^ bNum
	c.maxExp = a.maxExp ^ bNum
	if b:intmod(BigNumber(2)) == 1 then c.negative = a.negative end
	local done = false
	while not done do
		--apply iteration
		local power = 0
		local coeff = 1
		for i=1,bNum do
			power = power + iters[i]
			coeff = coeff * a[iters[i]]
		end
		c[power] = (c[power] or 0) + coeff

		for i=1,bNum do
			iters[i] = iters[i] + 1
			if iters[i] <= a.maxExp then break end
			iters[i] = a.minExp
			if i == bNum then
				done = true
			end
		end
	end
	c:carry()
	return c
end
function BigNumber.intPow_simple(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if b.negative then error('no support for negative powers!') end
	local c = BigNumber(1)
	c.base = a.base
	local counter = BigNumber()
	while counter ~= b do
		c = c * a
		counter = counter + BigNumber(1)
	end
	return c
end

-- returns a table of digits, with the 0's digit in [1], and the n'th digit in [n+1]
-- TODO bignumbers of arbitrary bases
function BigNumber.toBase(n, base)
	assert(base > 1, "can't set to a base of 1 or lower")
	
	n = BigNumber(n)
	if n.base == base then return n end
--	assert(n.minExp == 0, "can only handle integers, but got a minExp "..n.minExp)	-- can't handle fractions yet

	-- construct 'p' as value 'base' in base 'n.base'
	-- don't rely on 'toBase' to convert it -- or we'll get a recursive call
	local p = BigNumber(base, n.base)
	
	local result = BigNumber()
	result.base = base
	result.minExp = 0
	local i = 0
	local b
	while n > BigNumber(0, n.base) do
		n, b = n:intdiv(p)
		result[i] = b:tonumber()
		result.maxExp = i
		i = i + 1
	end
	return result
end

-- convert a table of digits back to a number
-- use caching of powers-of-two to do this
local function bin2num(b)
	local res = BigNumber()
	local _2d = BigNumber(1)	--2^d
	for d,ni in ipairs(b) do	--d=1 == 0's digit
		if ni == 1 then
			res = res + _2d
		end
		_2d = _2d * 2
	end
	return res
end

-- a^b = a^sum_i b_i for b_i the powers-of-two of b
-- TODO use another power? other than 2?
-- this runs faster than intPow_simple for n>80 (and they're both quick enough for n<=80)
function BigNumber.intPow_binDigits(a,b)	
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.nan or b.nan then return BigNumber.nan end
	if b.negative then error('no support for negative powers!') end

	local bb = b:toBase(2)	-- binary form of 'b'. TODO arbitrary base bignumbers
	local res = BigNumber(1):toBase(a.base)
	local _2ToTheI = BigNumber(1):toBase(a.base)	-- 2^i
	local aToThe2ToThei = a	--a^(2^i)
	for i=0,#bb do
		local bbi = bb[i]
		if bbi == 1 then
			res = res * aToThe2ToThei
		end
		aToThe2ToThei = aToThe2ToThei * aToThe2ToThei
		_2ToTheI = _2ToTheI + _2ToTheI
	end
	return res
end

-- simple runs *very* fast for b<100
-- binomial *might* run faster later ... but probably not
-- binDigits runs faster than simple for b>80
-- ... and should be easily extendible to decimal numbers
--BigNumber.intpow = BigNumber.intPow_binomial
--BigNumber.intpow = BigNumber.intPow_simple
BigNumber.intpow = BigNumber.intPow_binDigits
BigNumber.__pow = BigNumber.intpow

BigNumber.nan = BigNumber()
BigNumber.nan.nan = true

function BigNumber.simpleIntDiv(a,b)	-- TODO negative support!
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if b.maxExp == nil then return BigNumber.nan end
	local n = BigNumber(b)
	local c = BigNumber(0, a.base)
	c.base = a.base
	while n <= a do
		n = n + b
		c = c + BigNumber(1, a.base)
	end
	n = b - (n - a)
	return c, n
end

function BigNumber.longIntDiv(a,b, getRepeatingDecimals)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if b.base ~= a.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return BigNumber.nan end
	if b:isZero() then
		if a:isZero() then return BigNumber.nan end
		return BigNumber{infinity=true, negative=a.negative}
	end
	local dividendDigits = BigNumber(a)
	local place = dividendDigits.maxExp or 0 
	local dividendCurrentDigits = BigNumber(dividendDigits[place], a.base)	-- most significant digit
	dividendDigits[place] = nil
	dividendDigits:calcMaxExp()
	local divisor = BigNumber(b)
	local results = BigNumber(0, a.base)
	results.maxExp = place
	results.minExp = place
	local remainder = BigNumber(0, a.base)
	local digitLastRemainderPairs = table()	-- keys are digit,lastRemainder as strings
	local repeatFrom, repeatTo
	for precision=1,math.huge do
		local digit = dividendCurrentDigits:simpleIntDiv(divisor)
		dividendCurrentDigits = dividendCurrentDigits - divisor * digit
		local lastRemainder = remainder
		remainder = dividendCurrentDigits
		--print('place',place,'digit', digit, 'last remainder', lastRemainder)
		local key = digit..','..lastRemainder
		if place < 0 then
			if digitLastRemainderPairs[key] then
				repeatFrom = digitLastRemainderPairs[key]
				repeatTo = place + 1
				break
			end
			digitLastRemainderPairs[key] = place 
		end
		assert(dividendCurrentDigits >= BigNumber(0, a.base))
		results[place] = digit[0] or 0
		results.minExp = place
		if place <= 0 and not getRepeatingDecimals then
			-- return int part and remainder
			break
		end
		place = place - 1
		dividendCurrentDigits = dividendCurrentDigits * BigNumber(a.base, a.base) + BigNumber(dividendDigits[place], a.base)
	end
	-- final check that we're not repeating zeroes ...
	if repeatFrom then
		if repeatFrom == repeatTo and results[repeatFrom] == 0 then
			results[repeatFrom] = nil
			results.minExp = repeatFrom+1 
			repeatFrom = nil
			repeatTo = nil
		end
	end
	-- and we're good to test
	if repeatFrom then
		local repeatingLength = repeatFrom - repeatTo + 1
		if not maxRepeatingLength or repeatingLength > maxRepeatingLength then
			maxRepeatingLength = repeatingLength
			maxRepeatingP = b
		end
	end
	results:removeLeadingZeroes()
	if getRepeatingDecimals then
		results.repeatFrom = repeatFrom
		results.repeatTo = repeatTo
	end
	return results, dividendCurrentDigits
end

function BigNumber.decdiv(a,b)
	return (BigNumber.longIntDiv(a,b,true))
end

BigNumber.intdiv = BigNumber.longIntDiv

function BigNumber.__div(a,b)
	--return (BigNumber.intdiv(a,b))	-- return only one value
	return (BigNumber.decdiv(a,b))	-- return only one value
end

function BigNumber.intmod(a,b)
	return select(2, BigNumber.intdiv(a,b))
end

BigNumber.__mod = BigNumber.intmod

function BigNumber.__eq(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return false end
	if a.maxExp == nil and b.maxExp == nil then return true end	--negative zero equals positive zero
	if a.maxExp ~= b.maxExp then return false end
	if a.negative ~= b.negative then return false end
	for i=0,a.maxExp do
		if a[i] ~= b[i] then return false end
	end
	return true
end
function BigNumber.__ne(a,b)
	return not a == b
end
function BigNumber.__lt(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return false end
	if a:isZero() then
		if b:isZero() then
			return false
		end
		return not b.negative
	end
	if b:isZero() then
		return a.negative
	end
	if a.negative and not b.negative then return true end
	if b.negative and not a.negative then return false end
	if a.maxExp < b.maxExp then return not a.negative end
	if a.maxExp > b.maxExp then return a.negative end
	for i=a.maxExp,0,-1 do
		if a[i] < b[i] then return not a.negative end
		if a[i] > b[i] then return a.negative end
	end
	return false -- equal
end
function BigNumber.__le(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.base ~= b.base then b = b:toBase(a.base) end
	if a.nan or b.nan then return false end
	if a:isZero() then
		if b:isZero() then
			return true
		end
		return not b.negative
	end
	if b:isZero() then
		return a.negative
	end
	if a.negative and not b.negative then return true end
	if b.negative and not a.negative then return false end
	if a.maxExp < b.maxExp then return not a.negative end
	if a.maxExp > b.maxExp then return a.negative end
	for i=a.maxExp,0,-1 do
		if a[i] < b[i] then return not a.negative end
		if a[i] > b[i] then return a.negative end
	end
	return true -- equal
end
function BigNumber.tonumber(n)
	if n.nan then return 0/0 end
	if n:isZero() then return 0 end
	local sum = 0
	for i=n.maxExp,0,-1 do
		sum = sum * n.base
		sum = sum + n[i]
	end
	if n.negative then sum = -sum end
	return sum
end
--[[
	number of digits that the integer form has
	zero has 0 digits
	nan has nan digits
	negatives are ignored
	decimal places are ignored
--]]
function BigNumber.calcMaxExp(n)
	if n.nan then return n end
	n.maxExp = nil
	for k,v in pairs(n) do
		if type(k) == 'number' then
			if n.maxExp == nil then 
				n.maxExp = k
			else
				n.maxExp = math.max(n.maxExp, k)
			end
		end
	end
	if n.maxExp == nil then n.minExp = nil end
	return n.maxExp
end
function BigNumber.calcMinExp(n)
	if n.nan then return n end
	n.minExp = nil 
	for k,v in pairs(n) do
		if type(k) == 'number' then
			if n.minExp == nil then
				n.minExp = k
			else
				n.minExp = math.min(n.minExp, k)
			end
		end
	end
	if n.minExp == nil then n.maxExp = nil end
	return n.minExp
end
function BigNumber.__tostring(n)
	if n.nan then return 'nan' end
	if n.infinity then 
		if n.negative then
			return '-inf'
		else
			return 'inf' 
		end
	end
	if n:isZero() then return '0' end
	local s = ''
	for i=n.maxExp,0,-1 do
		s = s .. number.charfor(math.floor(n[i]))
	end
	if n.minExp < 0 then
		s = s .. '.'
		for i=-1,n.minExp,-1 do
			if i == n.repeatFrom then s = s .. '[' end
			s = s .. number.charfor(math.floor(n[i] or 0))
			if i == n.repeatTo then s = s .. ']' end
		end
	end
	if n.negative then s = '-'..s end
	if n.base ~= 10 then
		if n.base == 2 then
			s = s .. 'b'
		elseif n.base == 8 then
			s = s .. 'o'
		elseif n.base == 16 then
			s = s .. 'h'
		else
			s = s .. 'base'..n.base
		end
	end
	if n.trailsoff then
		s = s .. '...'
	end
	return s
end
function BigNumber.__concat(a,b) 
	return tostring(a) .. tostring(b) 
end

function BigNumber.factorial(n)
	local f = BigNumber(1)
	local i = BigNumber(2)
	while i <= n do
		f = f * i
		i = i + BigNumber(1)
	end
	return f
end

-- returns a lua table, indexes 1-n, of all the digits
-- t[1] is the 1's place, t[2] is the 10's place, etc
function BigNumber:digits()
	local t = table()
	if self:isZero() then return t end
	for i=math.min(0,self.minExp or 0),math.max(0,self.maxExp or 0) do
		t:insert(self[i] or 0)
	end
	return t
end
-- ridiculously often used
function BigNumber:sumOfDigits()
	return self:digits():sum()
end

function BigNumber:isZero() return not self.maxExp end

return BigNumber
