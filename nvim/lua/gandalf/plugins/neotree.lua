return {
	"nvim-neo-tree/neo-tree.nvim",
	dependencies = {
		"nvim-tree/nvim-web-devicons",
		"nvim-lua/plenary.nvim",
		"MunifTanjim/nui.nvim",
		"3rd/image.nvim",
	},
	lazy = false,
	config = function()
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
	end,
}
