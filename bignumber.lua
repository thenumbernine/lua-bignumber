local class = require 'ext.class'
local table = require 'ext.table'
local BigNumber = class()

function BigNumber:init(n)
	self.negative = false
	self.nan = false
	self.infinity = false
	self.repeatFrom = nil
	self.repeatTo = nil
	if type(n) == 'string' then
		if n == 'nan' then self.nan = true end
		if n == '0' then return end
		if n:sub(1,1) == '-' then
			n = n:sub(2)
			self.negative = true
		end
		for i=1,#n do
			self[i-1] = tonumber(n:sub(-i,-i))
		end
		self.minExp = 0
		self.maxExp = #n-1
		self:removeLeadingZeroes()
	elseif type(n) == 'number' then
		n = math.floor(n)
		if n < 0 then
			self.negative = true
			n = -n
		end
		local i = 0
		while n > 0 do
			self[i] = n % 10
			n = (n - self[i]) / 10
			self.minExp = 0
			self.maxExp = i
			i = i + 1
		end
		-- TODO support for fractions?
	elseif type(n) == 'table' then
		if BigNumber.is(n) then
			for k,v in pairs(n) do
				self[k] = v
			end
		else
			for i=1,#n do
				self[i-1] = tonumber(n[i]) % 10
			end
			self.minExp = 0
			self.maxExp = #n-1
			self.negative = n.negative or false 
			self.nan = n.nan or false
			self.infinity = n.infinity or false
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
	local borrow = 0	-- or -1
	local i = 0
	while i <= a.maxExp or i <= b.maxExp do	-- shouldn't allow borrow past the end
		local digit = (a[i] or 0) - (b[i] or 0) + borrow
		borrow = 0
		if digit < 0 then 
			borrow = -1
			digit = digit + 10
		end
		c[i] = digit
		c.maxExp = i
		i = i + 1
	end
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
		while self[i] >= 10 do
			self[i] = self[i] - 10
			carry = carry + 1
		end
		i = i + 1
		if i > self.maxExp then
			if carry == 0 and (self[i] == 0  or self[i] == nil) then break end
		end
	end
	self:removeLeadingZeroes()
end

function BigNumber.__add(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
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
	local carry = 0
	local i = 0
	while i <= a.maxExp or i <= b.maxExp or carry > 0 do
		local digit = (a[i] or 0) + (b[i] or 0) + carry
		carry = math.floor(digit / 10)
		c[i] = digit % 10
		c.maxExp = i
		i = i + 1
	end
	return c
end

function BigNumber.__unm(a)
	a = BigNumber(a)
	a.negative = not a.negative
	return a
end

function BigNumber.simpleMul(a,b)	-- TODO better multiplication algorithm!
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.nan or b.nan then return BigInit.nan end
	if a.maxExp == nil or b.maxExp == nil then return BigNumber() end
	local bWasNegative = b.negative
	if b.negative then
		b = -b
	end
	local c = BigNumber()
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
	if a.nan or b.nan then return BigNumber.nan end
	if a.maxExp == nil or b.maxExp == nil then return BigNumber() end
	local c = BigNumber()
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
	if a.nan or b.nan then return BigNumber.nan end
	if b:isZero() then return BigNumber(1) end
	if a:isZero() then return BigNumber() end
	if b.negative then error('no support for negative powers!') end
	--'b' iterators cycling through the coefficients of 'a'
	-- so iters will be sized by a's digits and increment until it reaches b
	local iters = table()
	local bNum = b:tonumber()	-- for our iterator table ... this limits this power method to only exponents of 4billion (or whatever the precision of doubles is)
	for i=1,bNum do
		iters[i] = a.minExp	
	end
	local c = BigNumber()
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
	if a.nan or b.nan then return BigNumber.nan end
	if b.negative then error('no support for negative powers!') end
	local c = BigNumber(1)
	local counter = BigNumber()
	while counter ~= b do
		c = c * a
		counter = counter + BigNumber(1)
	end
	return c
end

-- returns a table of digits, with the 0's digit in [1], and the n'th digit in [n+1]
-- TODO bignumbers of arbitrary bases
local function binDigits(n)
	n = BigNumber(n)
	-- TODO 'toBase' for the BigNumber
	-- look in ext.meta for an example of how to do this
	local binDigits = table()
	local i = BigNumber(1)	-- 2^digit
	local d = BigNumber(0)	-- digit
	while i <= n do	-- stop once i >= n
		d = d + 1
		i = i * 2
	end
	d = d - 1
	i = i / 2
	-- now work backwards and add digits, starting iwth the most significant
	while d >= 0 do
		if n >= i then
			binDigits[d:tonumber()+1] = 1
			n = n - i
		else
			binDigits[d:tonumber()+1] = 0
		end
		d = d - 1
		i = i / 2
	end
	return binDigits
end

-- convert a table of digits back to a number
-- use caching of powers-of-two to do this
local function bin2num(b)
	local res = BigNumber(0)
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
	local bb = binDigits(b)	-- binary form of 'b'.  TODO arbitrary base bignumbers
	local res = BigNumber(1)
	local _2ToTheI = BigNumber(1)	-- 2^i
	local aToThe2ToThei = a	--a^(2^i)
	for _,bbi in ipairs(bb) do
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
	if a.nan or b.nan then return BigNumber.nan end
	if b.maxExp == nil then return BigNumber.nan end
	local n = BigNumber(b)
	local c = BigNumber()
	while n <= a do
		n = n + b
		c = c + BigNumber(1)
	end
	n = b - (n - a)
	return c, n
end

function BigNumber.longIntDiv(a,b, getRepeatingDecimals)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
	if a.nan or b.nan then return BigNumber.nan end
	if b:isZero() then
		if a:isZero() then return BigNumber.nan end
		return BigNumber{infinity=true, negative = a.negative}
	end
	local dividendDigits = BigNumber(a)
	local place = dividendDigits.maxExp or 0 
	local dividendCurrentDigits = BigNumber(dividendDigits[place])	-- most significant digit
	dividendDigits[place] = nil
	dividendDigits:calcMaxExp()
	local divisor = BigNumber(b)
	local results = BigNumber()
	results.maxExp = place
	results.minExp = place
	local remainder = BigNumber()
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
		assert(dividendCurrentDigits >= BigNumber(0))
		results[place] = digit[0] or 0
		results.minExp = place
		if place <= 0 and not getRepeatingDecimals then
			-- return int part and remainder
			break
		end
		place = place - 1
		dividendCurrentDigits = dividendCurrentDigits * BigNumber(10) + BigNumber(dividendDigits[place])
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
	return (BigNumber.intdiv(a,b))	-- return only one value
end

function BigNumber.intmod(a,b)
	return select(2, BigNumber.intdiv(a,b))
end

BigNumber.__mod = BigNumber.intmod

function BigNumber.__eq(a,b)
	if not BigNumber.is(a) then a = BigNumber(a) end
	if not BigNumber.is(b) then b = BigNumber(b) end
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
		sum = sum * 10
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
	if n:isZero() then return '0' end
	local s = ''
	for i=n.maxExp,0,-1 do
		s = s .. math.floor(n[i])
	end
	if n.minExp < 0 then
		s = s .. '.'
		for i=-1,n.minExp,-1 do
			s = s .. (math.floor(n[i]) or 0)
		end
	end
	if n.negative then s = '-'..s end
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
--  t[1] is the 1's place, t[2] is the 10's place, etc
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
