local M = { "nvimdev/lspsaga.nvim" }

M.cond = false

function M.config()
  require("lspsaga").setup({
    ui = {
      code_action = "A",
    },
    lightbulb = {
      virtual_text = false,
    },
  })
end

return M
