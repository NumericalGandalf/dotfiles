local M = { "rose-pine/neovim" }

M.name = "rose-pine"

M.cond = false

M.lazy = false

M.priority = 61

function M.config()
  require("rose-pine").setup({
    styles = {
      italic = false,
      transparency = true,
    },
  })

  -- vim.cmd(":colorscheme rose-pine")
end

return M
