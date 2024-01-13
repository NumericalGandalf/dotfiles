local M = { "windwp/nvim-autopairs" }

M.event = "InsertEnter"

M.enabled = require("gandalf.prefs").enablings.autopairs

function M.config()
	require("nvim-autopairs").setup({
		disable_filetype = { "TelescopePrompt" },
	})
end

return M
