#!/usr/bin/env lua
require 'ext'
local big = require 'bignumber'
local function asserteq(a,b)
	print(a..' == '.. b)
	if a ~= b then
		error("expected "..a.." to equal "..b)
	end
end


-- base 10 integer operators

-- constructors

-- construct by string vs number
asserteq(big'1', big(1))
asserteq(big'2', big(2))
asserteq(big'10', big(10))

-- construct by string vs table
asserteq(big'1', big{1})
asserteq(big'2', big{2})
asserteq(big'10', big{0,1})

-- construct by number vs table
asserteq(big(1), big{1})
asserteq(big(2), big{2})
asserteq(big(10), big{0,1})

-- how should trailing zeros and minexp be handled?
asserteq(big'10'.maxExp, 1)
asserteq(big'10'.minExp, 1)

asserteq(big(10).maxExp, 1)
asserteq(big(10).minExp, 1)

asserteq(big{0,1}.maxExp, 1)
asserteq(big{0,1}.minExp, 1)

-- unm
asserteq(-big(10), big(-10))
asserteq(-big(-10), big(10))
asserteq(-big(0), big(0))

-- add
asserteq(big(1) + big(1), big(2))

-- sub
asserteq(big(3) - big(3), big(0))
asserteq(big(10) - big(3), big(7))
asserteq(big(3) - big(10), big(-7))

-- multiply
asserteq(big(1) * big(1), big(1))
asserteq(big(2) * big(2), big(4))

-- divide
assert((big(0) / big(0)).nan)
asserteq(big(1) / big(0), big.constant.infinity)
asserteq(big(-1) / big(0), -big.constant.infinity)
asserteq(big(2) / big(2), big(1))
asserteq(big(10) / big(5), big(2))

-- modulo
asserteq(big(10) % big(3), big(1))

-- exponent
asserteq(big(2) ^ big(10), big(1024))

-- base 10 decimal operators

-- constructors

asserteq(big'1.5', big(1.5))

-- unm

asserteq(-big(1.5), big(-1.5))
asserteq(-big(-1.5), big(1.5))

-- add
-- using construction of numbers for decimals is not always accurate
asserteq(big'1.2', big{1, [0]=2})
asserteq(big'1.3', big{1, [0]=3})
asserteq(big'1.2' + big'1.3', big'2.5')

-- mul
asserteq(big'1.1' * big'1.1', big'1.21')

-- div
asserteq(big'11' / big'11', big'1')
asserteq(big'1.1' / big'1.1', big'1')

asserteq(big(1) / big(5), big'.2')
-- TODO constructor for repeated decimals?
asserteq(big(1) / big(7), setmetatable({minExp=-6, maxExp=-1, repeatTo=-6, repeatFrom=-1, [-1]=1, [-2]=4, [-3]=2, [-4]=8, [-5]=5, [-6]=7}, big))
asserteq(big(1) / big(3), setmetatable({minExp=-1, maxExp=-1, repeatTo=-1, repeatFrom=-1, [-1]=3}, big))

-- base 2 integer operations

-- constructors

asserteq(big('10', 2), big(2))
asserteq(big('10', 2), big(2, 2))

print(tolua(big'1':toBase(2)))
print(tolua(big('1.1', 2)))
print(tolua(big'1.5':toBase(2)))
print(tolua(big'2':toBase(2)))
asserteq(big('1.1', 2), big'1.5')
