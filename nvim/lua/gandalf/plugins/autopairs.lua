local M = { "windwp/nvim-autopairs" }

M.event = "InsertEnter"

M.cond = false

function M.config()
	require("nvim-autopairs").setup({
		disable_filetype = { "TelescopePrompt" },
	})
end

return M
