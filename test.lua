#!/usr/bin/env lua
require 'ext'
local tolua = require 'ext.tolua'

local big = require 'bignumber'
local function asserteq(a,b, msg)
	print(a..' == '.. b)
	if a ~= b then
		print("expected "..tolua(a).." to equal "..tolua(b))
		error("expected "..a.." to equal "..b..(msg and ': '..msg or ''))
	end
end

-- base 10 integer operators

-- constructors

-- construct by string vs number
asserteq(big'1', big(1))
asserteq(big'2', big(2))
asserteq(big'10', big(10))

-- construct by string vs table
asserteq(big'1', big{[0]=1})
asserteq(big'2', big{[0]=2})
asserteq(big'10', big{[0]=0,1})

-- construct by number vs table
asserteq(big(1), big{[0]=1})
asserteq(big(2), big{[0]=2})
asserteq(big(10), big{[0]=0,1})

-- how should trailing zeros and minexp be handled?
asserteq(big'10'.maxExp, 1)
asserteq(big'10'.minExp, 1)

asserteq(big(10).maxExp, 1)
asserteq(big(10).minExp, 1)

asserteq(big{[0]=0,1}.maxExp, 1)
asserteq(big{[0]=0,1}.minExp, 1)

-- member functions
do
	local x = big(100)/7
	local p,q,r = x:getRepeatAsFrac()
	asserteq(p, big(285714))
	asserteq(q, big(999999))
	asserteq(r, big(14))
	asserteq(p / q + r, x)
end

-- unm
for i=-10,10 do
	asserteq(-big(i), big(-i))
end

-- add
for i=-10,10 do
	for j=-10,10 do
		asserteq(big(i) + big(j), big(i+j), big(i)..' + '..big(j))
	end
end

-- sub
for i=-10,10 do
	for j=-10,10 do
		asserteq(big(i) - big(j), big(i-j))
	end
end

-- multiply
for i=-10,10 do
	for j=-10,10 do
		asserteq(big(i) * big(j), big(i*j))
	end
end

-- divide
assert(big(0):isZero())
assert((big(0) / big(0)).nan)
asserteq(big(1) / big(0), big.constant.infinity)
asserteq(big(-1) / big(0), -big.constant.infinity)
asserteq(big(2) / big(2), big(1))
asserteq(big(10) / big(5), big(2))

-- modulo
asserteq(big(10) % big(3), big(1))

-- pow
for i=-10,10 do
	for j=0,10 do
		asserteq(big(i) ^ big(j), big(i^j))
	end
end

-- base 10 decimal operators

-- constructors

asserteq(big'1.5', big(1.5))

-- unm

asserteq(-big(1.5), big(-1.5))
asserteq(-big(-1.5), big(1.5))

-- add
-- using construction of numbers for decimals is not always accurate
asserteq(big'1.2', big{[0]=1, [-1]=2})
asserteq(big'1.3', big{[0]=1, [-1]=3})
asserteq(big'1.2' + big'1.3', big'2.5')

-- mul
asserteq(big'1.1' * big'1.1', big'1.21')

--[[ repeating decimal div .. but needs repeating decimal mul to work
for i=1,10 do
	for j=1,10 do
		asserteq((big(i)/big(j))*big(j), big(i), tostring(big(i))..'/'..tostring(big(j)))
	end
end
--]]

asserteq(big'11' / big'11', big'1')
asserteq(big'1.1' / big'1.1', big'1')

asserteq(big(1) / big(5), big'.2')
-- TODO constructor for repeated decimals?
asserteq(big(1) / big(7), setmetatable({minExp=-6, maxExp=-1, repeatTo=-6, repeatFrom=-1, [-1]=1, [-2]=4, [-3]=2, [-4]=8, [-5]=5, [-6]=7}, big))
asserteq(big(1) / big(3), setmetatable({minExp=-1, maxExp=-1, repeatTo=-1, repeatFrom=-1, [-1]=3}, big))
-- another way to input repeating decimals:
asserteq(big(1) / big(3), big{repeatTo=1, repeatFrom=1, 3}:shiftRight(2))
asserteq(big(1) / big(7), big{repeatTo=1, repeatFrom=6, 7, 5, 8, 2, 4, 1}:shiftRight(7))

-- repeating fractions

-- add
local _1_3 = big(1) / big(3)
asserteq(_1_3 + _1_3 + _1_3, big(1))

asserteq(big(100) / 7 + big(200) / 11, big(2500) / 77)

local _31_99 = big(31) / big(99)
local _321_999 = big(321) / big(999)
asserteq(_31_99 + _321_999, big(634452) / big(999999))

-- adding of one repeating and one non-repeating
asserteq(big(352)/big(1000) + big(634452) / big(999999000), big{repeatTo=-9, repeatFrom=-4, [-1]=3, [-2]=5, [-3]=2, [-4]=6, [-5]=3, [-6]=4, [-7]=4, [-8]=5, [-9]=2})

-- requires lcm between [2 digits] and [3 digits], and shifting the 2-dgitis by 1
asserteq(big(31)/big(990) + _321_999, big{repeatTo=-7, repeatFrom=-2, [-1]=3, [-2]=5, [-3]=2, [-4]=6, [-5]=3, [-6]=4, [-7]=4})

-- TODO require a shifting of >lcm=6 digits from either side

-- sub
asserteq(big(1) - _1_3, _1_3 + _1_3)
asserteq(_1_3 - big(1), -_1_3 - _1_3)
asserteq(big'.1' - _1_3, big{negative=true, repeatFrom=-2, repeatTo=-2, [-1]=2, [-2]=3})

-- mul
asserteq(_1_3*10, big{repeatFrom=-1, repeatTo=-1, [0]=3, [-1]=3})

asserteq(_1_3*.1, big{repeatFrom=-2, repeatTo=-2, [-2]=3})

asserteq(_1_3 * _1_3, big(1) / big(9))

-- div
--print(_1_3, _1_3:getRepeatAsFrac())	== 3, 9, 0
asserteq(_1_3/10, big{repeatFrom=-2, repeatTo=-2, [-2]=3})

-- divide
asserteq(_1_3 / _1_3, big(1))

-- base 2

-- constructors

asserteq(big('10', 2), big(2))
asserteq(big('10', 2), big(2):toBase(2))
asserteq(big('1.1', 2), big'1.5')

--
print(big(2):primeFactorization():mapi(tostring):concat', ')
print(big(10):primeFactorization():mapi(tostring):concat', ')
print(big(12):primeFactorization():mapi(tostring):concat', ')
print(big(24):primeFactorization():mapi(tostring):concat', ')
print(big(60):primeFactorization():mapi(tostring):concat', ')
print(big(120):sqrt())
print(big(120):primeFactorization():mapi(tostring):concat', ')
