local M = { "folke/tokyonight.nvim" }

M.lazy = false

M.priority = 100

function M.config()
  require("tokyonight").setup({
    style = "moon",
    transparent = true,
    terminal_colors = true,
    styles = {
      comments = { italic = true },
      keywords = { italic = true },
      functions = {},
      variables = {},
      sidebars = "transparent",
      floats = "transparent",
    },
    lualine_bold = true,
  })

  vim.api.nvim_command("colorscheme tokyonight")
end

return M
