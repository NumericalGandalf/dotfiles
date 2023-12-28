return {
	{
		"ellisonleao/gruvbox.nvim",
		dependencies = { "folke/tokyonight.nvim" },
		lazy = false,
		config = function()
			vim.api.nvim_create_user_command("SetColorscheme", function(opts)
				local colorscheme = opts.fargs[1]
				if colorscheme == "gruvbox" then
					require("gruvbox").setup({
						terminal_colors = true,
						undercurl = true,
						underline = true,
						bold = true,
						italic = {
							strings = true,
							emphasis = true,
							comments = true,
							operators = true,
							folds = true,
						},
						strikethrough = true,
						invert_selection = false,
						invert_signs = false,
						invert_tabline = false,
						invert_intend_guides = false,
						inverse = true,
						contrast = "",
						palette_overrides = {},
						overrides = {},
						dim_inactive = false,
						transparent_mode = false,
					})
				elseif colorscheme == "tokyonight" then
					require("tokyonight").setup({
						style = "moon",
						transparent = false,
						terminal_colors = true,
						styles = {
							comments = { italic = true },
							keywords = { italic = false },
							functions = {},
							variables = {},
							sidebars = "dark",
							floats = "dark",
						},
						hide_inactive_statusline = true,
						dim_inactive = false,
						lualine_bold = false,
					})
				else
					error(string.format("Invalid colorscheme '%s'", colorscheme))
					return
				end
				vim.api.nvim_command(string.format("colorscheme %s", colorscheme))
			end, {
				nargs = 1,
				complete = function()
					return { "gruvbox", "tokyonight" }
				end,
			})
			vim.cmd.SetColorscheme("gruvbox")
		end,
	},
}
