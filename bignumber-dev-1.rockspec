package = "bignumber"
version = "dev-1"
source = {
	url = "git+https://github.com/thenumbernine/lua-bignumber"
}
description = {
	detailed = "This is a big number class that I made for assistance with some Project Euler problems.",
	homepage = "https://github.com/thenumbernine/lua-bignumber",
	license = "MIT"
}
dependencies = {
	"lua >= 5.1",
}
build = {
	type = "builtin",
	modules = {
		bignumber = "bignumber.lua",
		["bignumber.tests.test"] = "tests/test.lua"
	},
}
