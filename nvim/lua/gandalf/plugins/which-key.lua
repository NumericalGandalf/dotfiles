local M = { "folke/which-key.nvim" }

M.event = "VimEnter"

M.enabled = require("gandalf.prefs").enablings.whichkey

function M.config()
	require("which-key").setup()
end

return M
