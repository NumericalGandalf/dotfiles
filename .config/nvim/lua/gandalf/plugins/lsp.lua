local M = { "neovim/nvim-lspconfig" }

M.dependencies = {
  "folke/neoconf.nvim",
  "folke/neodev.nvim",
  "williamboman/mason-lspconfig.nvim",
  "hrsh7th/cmp-nvim-lsp",
  "stevearc/conform.nvim",
  "mfussenegger/nvim-lint",
}

M.event = "BufEnter"

local function attach_generic(opts)
  local telescope = require("telescope.builtin")

  vim.keymap.set("n", "gr", telescope.lsp_references, opts)
  vim.keymap.set("n", "gi", vim.lsp.buf.implementation, opts)
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts)

  vim.keymap.set("n", "<leader>vd", vim.diagnostic.setqflist, opts)
  vim.keymap.set("n", "<leader>vD", vim.diagnostic.setloclist, opts)

  vim.keymap.set("n", "gS", telescope.lsp_document_symbols, opts)
  vim.keymap.set("n", "gs", telescope.lsp_workspace_symbols, opts)

  vim.keymap.set("n", "gh", function()
    vim.cmd(":ClangdSwitchSourceHeader")
  end, opts)
end

local function attach_dynamic(opts)
  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts)
  vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, opts)

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts)
  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts)
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts)

  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts)
end

function M.config()
  require("neoconf").setup()
  require("neodev").setup()

  local saga = require("gandalf.plugins.lspsaga")
  if saga.cond then
    require("lspsaga")
    attach_dynamic = saga.attach_dynamic
  end

  vim.api.nvim_create_autocmd("LspAttach", {
    group = require("gandalf").gandalfs,
    callback = function(ev)
      local opts = { buffer = ev.buf }
      vim.bo[opts.buffer].omnifunc = "v:lua.vim.lsp.omnifunc"
      attach_generic(opts)
      attach_dynamic(opts)
    end,
  })

  local lspconfig = require("lspconfig")
  local capabilities = require("cmp_nvim_lsp").default_capabilities()

  for _, server in ipairs({ "lua_ls", "pyright", "clangd", "jsonls" }) do
    lspconfig[server].setup({
      capabilities = capabilities,
    })
  end

  lspconfig.bashls.setup({
    filetypes = { "sh", "bash", "zsh" },
  })
end

return M
