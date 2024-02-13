local M = {}

function M.setup()
  M.gandalfs = vim.api.nvim_create_augroup("Gandalfs", {
    clear = true,
  })

  vim.cmd(":colorscheme default")
  vim.cmd(":hi Normal guibg=None")

  require("gandalf.settings").setup()
  require("gandalf.lazy").setup()
  require("gandalf.commands").setup()
end

return M
