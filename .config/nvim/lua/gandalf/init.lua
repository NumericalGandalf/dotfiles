local M = {}

function M.setup()
  M.gandalfs = vim.api.nvim_create_augroup("Gandalfs", {
    clear = true,
  })

  vim.api.nvim_command("colorscheme dalf")

  require("gandalf.settings").setup()
  require("gandalf.lazy").setup()
  require("gandalf.commands").setup()
end

return M
