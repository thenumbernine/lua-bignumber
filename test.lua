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
asserteq(big'1.2', big{[0]=1, [-1]=2})
asserteq(big'1.3', big{[0]=1, [-1]=3})
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
-- another way to input repeating decimals:
asserteq(big(1) / big(3), big{repeatTo=1, repeatFrom=1, 3}:shiftRight(2))
asserteq(big(1) / big(7), big{repeatTo=1, repeatFrom=6, 7, 5, 8, 2, 4, 1}:shiftRight(7))

-- repeating fractions

-- add
local _1_3 = big(1) / big(3)
asserteq(_1_3 + _1_3 + _1_3, big(1))

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
-- TODO how about repeatFrom/repeatTo when they are in the negative digits place?
asserteq(_1_3*10, big{repeatFrom=-1, repeatTo=-1, [0]=3, [-1]=3})

-- div
-- TODO FIXME dividing (even by 1000) removes the repeatFrom/repeatTo
asserteq(_1_3/10, big{repeatFrom=-2, repeatTo=-2, [-2]=3})

-- divide
asserteq(_1_3 / _1_3, big(1))

-- base 2

-- constructors

asserteq(big('10', 2), big(2))
asserteq(big('10', 2), big(2):toBase(2))
asserteq(big('1.1', 2), big'1.5')
