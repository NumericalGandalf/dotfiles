local M = { "nvimdev/lspsaga.nvim" }

M.cond = false

function M.attach_dynamic(opts)
  vim.keymap.set("n", "gd", function()
    vim.cmd(":Lspsaga goto_definition")
  end, opts)
  vim.keymap.set("n", "gt", function()
    vim.cmd(":Lspsaga goto_type_definition")
  end, opts)

  vim.keymap.set("n", "K", function()
    vim.cmd(":Lspsaga hover_doc")
  end, opts)
  vim.keymap.set("n", "<leader>rn", function()
    vim.cmd(":Lspsaga rename")
  end, opts)
  vim.keymap.set("n", "<leader>ca", function()
    vim.cmd(":Lspsaga code_action")
  end, opts)

  vim.keymap.set("n", "[d", function()
    vim.cmd(":Lspsaga diagnostic_jump_prev")
  end, opts)
  vim.keymap.set("n", "]d", function()
    vim.cmd(":Lspsaga diagnostic_jump_next")
  end, opts)

  vim.keymap.set("n", "gpd", function()
    vim.cmd(":Lspsaga peek_definition")
  end, opts)
  vim.keymap.set("n", "gpr", function()
    vim.cmd(":Lspsaga finder def+ref+imp")
  end, opts)
  vim.keymap.set("n", "gpt", function()
    vim.cmd(":Lspsaga peek_type_definition")
  end, opts)
end

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
