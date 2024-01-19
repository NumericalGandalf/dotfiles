local M = { "williamboman/mason.nvim" }

M.build = ":MasonUPdate"

M.event = "VeryLazy"

function M.config()
	require("mason").setup()
end

return M
