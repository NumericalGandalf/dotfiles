local M = { "williamboman/mason.nvim" }

M.build = ":MasonUpdate"

M.event = "VeryLazy"

function M.config()
	require("mason").setup()
end

return M
