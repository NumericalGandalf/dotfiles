local M = { "rose-pine/neovim" }

M.name = "rose-pine"

M.cond = true

M.lazy = false

M.priority = 61

function M.config()
  require("rose-pine").setup({
    styles = {
      transparency = true,
    },
  })

  vim.cmd(":colorscheme rose-pine")
end

return M
