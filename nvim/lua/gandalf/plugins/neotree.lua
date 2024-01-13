local M = { "nvim-neo-tree/neo-tree.nvim" }

M.dependencies = {
	"nvim-tree/nvim-web-devicons",
	"nvim-lua/plenary.nvim",
	"MunifTanjim/nui.nvim",
	"3rd/image.nvim",
}

M.lazy = false

M.enabled = require("gandalf.prefs").enablings.neotree

function M.config()
	require("neo-tree").setup({
		popup_border_style = "rounded",
		window = {
			width = "18%",
		},
		filesystem = {
			hijack_netrw_behavior = "open_current",
			filtered_items = {
				visible = true,
				hide_dotfiles = false,
				hide_gitignore = false,
				hide_hidden = false,
			},
		},
	})
	vim.keymap.set({ "n", "v" }, "<leader>ft", function()
		vim.cmd(":Neotree toggle")
	end)
end

return M
