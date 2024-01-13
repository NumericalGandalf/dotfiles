local M = { "williamboman/mason.nvim" }

M.build = ":MasonUPdate"

M.event = "VimEnter"

function M.config()
	require("mason").setup()
end

return M
