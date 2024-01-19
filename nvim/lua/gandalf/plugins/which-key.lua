local M = { "folke/which-key.nvim" }

M.cond = false

M.event = "VeryLazy"

function M.config()
	require("which-key").setup()
end

return M
