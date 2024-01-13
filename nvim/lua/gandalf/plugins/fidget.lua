local M = { "j-hui/fidget.nvim" }

M.event = "VimEnter"

M.enabled = require("gandalf.prefs").enablings.fidget

function M.config()
	require("fidget").setup({
		notification = {
			override_vim_notify = true,
			window = {
				y_padding = 1,
			},
		},
	})
end

return M
