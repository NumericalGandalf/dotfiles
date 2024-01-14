local M = { "folke/which-key.nvim" }

M.cond = false

M.event = "VimEnter"

function M.config()
	require("which-key").setup()
end

return M
