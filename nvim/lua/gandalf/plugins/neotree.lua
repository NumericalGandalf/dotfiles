local M = { "nvim-neo-tree/neo-tree.nvim" }

M.dependencies = {
	"nvim-tree/nvim-web-devicons",
	"nvim-lua/plenary.nvim",
	"MunifTanjim/nui.nvim",
	"3rd/image.nvim",
}

M.cond = false

M.lazy = false

local toggle_tree = "<leader>ft"

if not M.cond then
	vim.keymap.set("n", toggle_tree, function()
		vim.cmd(":Lexplore")
	end)
end

function M.config()
	vim.g.loaded_netrw = 1
	vim.g.loaded_netrwPlugin = 1

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

	vim.keymap.set("n", toggle_tree, function()
		vim.cmd(":Neotree toggle")
	end)
end

return M
